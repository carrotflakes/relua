mod type_filter;

use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;

use type_filter::TfDnf;

use crate::ast;
use crate::r#type::{ConstData, Type, TypeTable};

thread_local! {
    static INFER_RESULT: std::cell::RefCell<Option<Vec<(ast::Span, Type)>>> = std::cell::RefCell::new(None);
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub location: ast::Span,
}

pub struct Context<'a> {
    parent: Result<&'a Context<'a>, RefCell<Vec<Error>>>,
    symbol_table: HashMap<String, Arc<Type>>,
    symbol_type_guarded: HashMap<*const Type, Type>,
    type_table: HashMap<String, Type>,
    type_guard_propagation: bool,
}

enum ReturnType {
    Fixed(Vec<Type>),
    Infer(Vec<Vec<Type>>),
}

impl<'a> Context<'a> {
    pub fn from_symbol_table(symbol_table: HashMap<String, Type>) -> Self {
        Self {
            parent: Err(RefCell::new(vec![])),
            symbol_table: symbol_table
                .into_iter()
                .map(|(name, type_)| (name.clone(), Arc::new(type_)))
                .collect(),
            symbol_type_guarded: HashMap::new(),
            type_table: HashMap::new(),
            type_guard_propagation: true,
        }
    }

    fn child(&'a self) -> Self {
        Self {
            parent: Ok(self),
            symbol_table: HashMap::new(),
            symbol_type_guarded: HashMap::new(),
            type_table: HashMap::new(),
            type_guard_propagation: true,
        }
    }

    fn insert(&mut self, name: String, type_: Type, span: Option<ast::Span>) {
        self.symbol_table
            .insert(name.clone(), Arc::new(type_.clone()));

        if let Some(span) = span {
            INFER_RESULT.with(|r| {
                if let Some(report) = &mut *r.borrow_mut() {
                    report.push((span, type_.clone()));
                }
            });
        }
    }

    fn get_symbol_variable_type(&self, name: &str) -> Option<&Arc<Type>> {
        self.symbol_table.get(name).or_else(|| {
            self.parent
                .as_ref()
                .ok()
                .and_then(|p| p.get_symbol_variable_type(name))
        })
    }

    fn get_symbol_type(&self, name: &str) -> Option<&Type> {
        let at = self.get_symbol_variable_type(name)?;
        let mut ctx = Some(self);
        while let Some(c) = ctx {
            if let Some(t) = c.symbol_type_guarded.get(&Arc::as_ptr(&at)) {
                return Some(t);
            }
            ctx = if c.type_guard_propagation {
                c.parent.clone().ok()
            } else {
                None
            };
        }
        Some(&**at)
    }

    fn all_symbols(&self) -> Vec<(&String, &Arc<Type>)> {
        let mut symbols = vec![];
        let mut ctx = Some(self);
        while let Some(c) = ctx {
            for (name, type_) in &c.symbol_table {
                if symbols.iter().all(|(n, _)| *n != name) {
                    symbols.push((name, type_));
                }
            }
            ctx = c.parent.clone().ok();
        }

        symbols
    }

    fn resolve_type(&self, type_: &Type) -> Type {
        match type_.resolve(&|name| {
            let mut ctx = Some(self);
            while let Some(c) = ctx {
                if let Some(t) = c.type_table.get(name) {
                    return Some(t.clone());
                }
                ctx = c.parent.clone().ok();
            }
            None
        }) {
            Ok(t) => t,
            Err(e) => {
                self.add_error(Error {
                    message: e,
                    location: ast::Span::default(), // FIXME
                });
                Type::Error // NOTE: Should I prevent to type check after this?
            }
        }
    }

    fn resolve_types<'b>(&self, iter: impl Iterator<Item = &'b Type>) -> Vec<Type> {
        let mut types = vec![];
        for t in iter {
            types.push(self.resolve_type(t));
        }
        types
    }

    fn add_error(&self, err: Error) {
        match self.parent {
            Ok(parent) => {
                parent.add_error(err);
            }
            Err(ref errors) => {
                errors.borrow_mut().push(err);
            }
        }
    }

    pub fn check_program(self, stmts: &[ast::SpannedStatement]) -> Result<Vec<Type>, Vec<Error>> {
        let mut return_types = ReturnType::Infer(vec![]);

        let _ = self.child().check_statements(stmts, &mut return_types);

        if let Err(e) = self.parent {
            if !e.borrow().is_empty() {
                return Err(e.into_inner());
            }
        }
        Ok(return_types.into_types())
    }

    pub fn type_inference(
        self,
        stmts: &[ast::SpannedStatement],
    ) -> (Vec<(ast::Span, Type)>, Vec<Error>) {
        INFER_RESULT.with(|r| {
            *r.borrow_mut() = Some(vec![]);
        });

        let res = self.check_program(stmts);

        let types = INFER_RESULT.with(|r| r.borrow_mut().take().unwrap());

        (types, res.err().unwrap_or_default())
    }

    fn check_statements(
        &mut self,
        stmts: &[ast::SpannedStatement],
        return_type: &mut ReturnType,
    ) -> Result<(), ()> {
        for stmt in stmts {
            match &**stmt {
                ast::Statement::Expression(expression) => {
                    self.check_expression(expression);
                }
                ast::Statement::Fn { name, function } => {
                    let mut ctx1 = self.child();
                    for p in &function.type_params {
                        ctx1.type_table
                            .insert(p.name().to_owned(), Type::Variable(p.clone()));
                    }

                    let params = ctx1.resolve_types(function.parameters.iter().map(|(_, t)| t));
                    let wrap_generics: &dyn Fn(Type) -> Type = if function.type_params.is_empty() {
                        &|t| t
                    } else {
                        &|t| Type::Generic(function.type_params.clone(), Box::new(t))
                    };

                    // Add function to bindings to allow recursion
                    ctx1.insert(
                        name.deref().clone(),
                        wrap_generics(Type::Function(
                            params.clone(),
                            if let Some(t) = &function.return_types {
                                ctx1.resolve_types(t.iter())
                            } else {
                                vec![]
                            }, // unknown?
                        )),
                        None,
                    );

                    if let Ok(ret_types) = ctx1.check_function(function) {
                        self.insert(
                            name.deref().clone(),
                            wrap_generics(Type::Function(params, ret_types.clone())),
                            Some(name.span.clone()),
                        );
                    } else {
                        self.insert(name.deref().clone(), Type::Error, Some(name.span.clone()));
                    }
                }
                ast::Statement::Let(vars, exprs) => {
                    if exprs.len() == 1 {
                        let actual = self.check_expression(&exprs[0]);
                        for i in 0..vars.len() {
                            let var = &vars[i];
                            let actual = actual.get(i).unwrap_or(&Type::Nil).clone();
                            if let Some(expect) = &var.1 {
                                let expect = self.resolve_type(expect);
                                self.type_match(&expect, &actual, &var.0.span);
                                self.insert(
                                    var.0.deref().clone(),
                                    expect.clone(),
                                    Some(var.0.span.clone()),
                                );
                            } else {
                                self.insert(
                                    var.0.deref().clone(),
                                    actual,
                                    Some(var.0.span.clone()),
                                );
                            }
                        }
                    } else {
                        for i in 0..vars.len() {
                            let var = &vars[i];
                            // TODO: Can we assume that the number of variables and expressions are the same?
                            let actual = if let Some(expr) = exprs.get(i) {
                                self.check_expression(expr)
                                    .first()
                                    .cloned()
                                    .unwrap_or(Type::Nil)
                            } else {
                                Type::Nil
                            };
                            if let Some(expect) = &var.1 {
                                let expect = self.resolve_type(expect);
                                self.type_match(&expect, &actual, &var.0.span);
                                self.insert(
                                    var.0.deref().clone(),
                                    expect.clone(),
                                    Some(var.0.span.clone()),
                                );
                            } else {
                                self.insert(
                                    var.0.deref().clone(),
                                    actual,
                                    Some(var.0.span.clone()),
                                );
                            }
                        }
                    }
                }
                ast::Statement::Assignment { vars, exprs } => {
                    let var_types = vars
                        .iter()
                        .map(|var| match var {
                            ast::LValue::Variable(name) => {
                                if let Some(t) = self.get_symbol_variable_type(name) {
                                    (**t).clone()
                                } else {
                                    self.add_error(Error {
                                        message: format!("Variable not found: {}", &**name),
                                        location: name.span.clone(),
                                    });
                                    Type::Error
                                }
                            }
                            ast::LValue::Index(table, index) => {
                                let table_type = self.check_expression(table)[0].clone();
                                match table_type {
                                    Type::Any => {
                                        self.check_expression(index);
                                        Type::Any
                                    }
                                    Type::Table(table_type) => {
                                        let index_type = self.check_expression(index)[0].clone();
                                        self.check_table(&table_type, &index_type, &stmt.span)
                                    }
                                    Type::Error => Type::Error,
                                    _ => {
                                        self.add_error(Error {
                                            message: format!("Not a table: {}", table_type),
                                            location: table.span.clone(),
                                        });
                                        Type::Error
                                    }
                                }
                            }
                        })
                        .collect::<Vec<_>>();

                    let actuals = if exprs.len() == 1 {
                        self.check_expression(&exprs[0])
                    } else {
                        exprs
                            .iter()
                            .map(|e| {
                                self.check_expression(e)
                                    .get(0)
                                    .cloned()
                                    .unwrap_or(Type::Nil)
                            })
                            .collect::<Vec<_>>()
                    };
                    for i in 0..var_types.len() {
                        let actual = actuals.get(i).unwrap_or(&Type::Nil);
                        let expect = self.resolve_type(&var_types[i]);
                        self.type_match(&expect, actual, &stmt.span);
                        match &vars[i] {
                            ast::LValue::Variable(v) => {
                                let v = self
                                    .get_symbol_variable_type(v)
                                    .expect("The variable was already checked");
                                self.symbol_type_guarded.insert(&**v, actual.clone());
                            }
                            ast::LValue::Index(_, _) => {} // TODO
                        }
                    }
                }
                ast::Statement::If {
                    condition,
                    then,
                    else_,
                } => {
                    self.check_expression(condition);
                    let (mut ctx1, mut ctx2) = self.conditioned(condition);
                    ctx1.check_statements(then, return_type)?;
                    ctx2.check_statements(else_, return_type)?;

                    // Update type guard:
                    // self = ctx1 | ctx2
                    let mut vs =
                        ctx1.symbol_type_guarded
                            .keys()
                            .filter(|v| !ctx1.symbol_table.values().any(|t| Arc::as_ptr(t) == **v))
                            .chain(ctx2.symbol_type_guarded.keys().filter(|v| {
                                !ctx2.symbol_table.values().any(|t| Arc::as_ptr(t) == **v)
                            }))
                            .collect::<Vec<_>>();
                    vs.sort_unstable();
                    vs.dedup();
                    let mut new_guard = self.symbol_type_guarded.clone();
                    for v in vs {
                        let t = self.symbol_type_guarded.get(v);
                        let t1 = ctx1.symbol_type_guarded.get(v).or(t);
                        let t2 = ctx2.symbol_type_guarded.get(v).or(t);
                        if let (Some(t1), Some(t2)) = (t1, t2) {
                            if t1 == t2 {
                                new_guard.insert(*v, t1.clone());
                            } else {
                                new_guard.insert(
                                    *v,
                                    Type::Union(vec![t1.clone(), t2.clone()]).normalize(),
                                );
                            }
                        }
                    }
                    self.symbol_type_guarded = new_guard;
                }
                ast::Statement::While { condition, body } => {
                    self.check_expression(condition);
                    let (mut ctx1, ctx2) = self.conditioned(condition);
                    ctx1.check_statements(body, return_type)?;

                    // Update type guard:
                    for (v, t) in ctx2.symbol_type_guarded.iter() {
                        self.symbol_type_guarded.insert(*v, t.clone());
                    }
                }
                ast::Statement::ForNumeric {
                    variable,
                    start,
                    end,
                    step,
                    body,
                } => {
                    let start_type = self.check_expression(start)[0].clone();
                    self.type_match(&Type::Number, &start_type, &stmt.span);
                    let end_type = self.check_expression(end)[0].clone();
                    self.type_match(&Type::Number, &end_type, &stmt.span);
                    if let Some(step) = step {
                        let step_type = self.check_expression(step)[0].clone();
                        self.type_match(&Type::Number, &step_type, &stmt.span);
                    }
                    let mut ctx = self.child();
                    ctx.insert(
                        variable.deref().clone(),
                        Type::Number,
                        Some(variable.span.clone()),
                    );
                    ctx.check_statements(body, return_type)?;
                }
                ast::Statement::ForGeneric {
                    variables,
                    exprs,
                    body,
                } => {
                    // TODO: type check
                    let expr_types = if exprs.len() == 1 {
                        self.check_expression(&exprs[0])
                    } else {
                        exprs
                            .iter()
                            .map(|e| {
                                self.check_expression(e)
                                    .get(0)
                                    .cloned()
                                    .unwrap_or(Type::Nil)
                            })
                            .collect()
                    };

                    let et0 = expr_types.get(0).cloned().unwrap_or(Type::Nil);
                    let et0 = self.resolve_type(&et0); // ?
                    self.type_match(
                        &Type::Function(vec![Type::Any, Type::Any], vec![Type::Any]), // TODO
                        &et0,
                        exprs.get(0).map(|e| &e.span).unwrap_or(&stmt.span),
                    );

                    let mut ctx = self.child();
                    for i in 0..variables.len() {
                        let (name, type_) = &variables[i];
                        let type_actual = if let Type::Function(_, rts) = &et0 {
                            rts.get(i).cloned().unwrap_or(Type::Nil)
                        } else {
                            Type::Any
                        };
                        ctx.type_match(
                            type_.as_ref().unwrap_or(&Type::Any),
                            &type_actual,
                            &name.span,
                        );
                        ctx.insert(
                            name.deref().clone(),
                            type_.clone().unwrap_or(type_actual),
                            Some(name.span.clone()),
                        );
                    }
                    ctx.check_statements(body, return_type)?;
                }
                ast::Statement::Return(exprs) => {
                    if exprs.len() == 1 {
                        let actual = self.check_expression(&exprs[0]);
                        match return_type {
                            ReturnType::Fixed(expect) => {
                                for i in 0..expect.len() {
                                    self.type_match(
                                        &expect[i],
                                        actual.get(i).unwrap_or(&Type::Nil),
                                        &stmt.span,
                                    );
                                }
                            }
                            ReturnType::Infer(typess) => {
                                typess.push(actual);
                            }
                        }
                    } else {
                        let mut actuals = vec![];
                        for expr in exprs {
                            let actual = self.check_expression(expr);
                            actuals.push(actual[0].clone());
                        }
                        match return_type {
                            ReturnType::Fixed(expect) => {
                                for i in 0..expect.len() {
                                    self.type_match(
                                        &expect[i],
                                        actuals.get(i).unwrap_or(&Type::Nil),
                                        &stmt.span,
                                    );
                                }
                            }
                            ReturnType::Infer(typess) => {
                                typess.push(actuals);
                            }
                        }
                    }
                }
                ast::Statement::Break => {}
                ast::Statement::TypeAlias(name, type_) => {
                    self.type_table
                        .insert(name.name().to_owned(), type_.clone());
                }
            }
        }
        Ok(())
    }

    fn check_function(mut self, function: &ast::Function) -> Result<Vec<Type>, ()> {
        self.type_guard_propagation = false;

        for (name, type_) in &function.parameters {
            self.insert((**name).clone(), self.resolve_type(type_), None);
        }

        let mut ret_types = match &function.return_types {
            Some(t) => ReturnType::Fixed(self.resolve_types(t.iter())),
            None => ReturnType::Infer(vec![]),
        };

        self.check_statements(&function.body, &mut ret_types)?;

        Ok(ret_types.into_types())
    }

    fn check_expression(&mut self, expr: &ast::SpannedExpression) -> Vec<Type> {
        match &**expr {
            ast::Expression::Literal(literal) => {
                let val = match literal {
                    ast::Literal::Number(n) => ConstData::try_from_f64(*n)
                        .map(Type::Const)
                        .unwrap_or(Type::Number),
                    ast::Literal::Bool(b) => Type::Const(ConstData::Bool(*b)),
                    ast::Literal::String(s) => Type::Const(ConstData::String(s.clone())),
                };
                vec![val]
            }
            ast::Expression::Nil => vec![Type::Nil],
            ast::Expression::Variable(name) => {
                if let Some(t) = self.get_symbol_type(name) {
                    vec![t.clone()]
                } else {
                    self.add_error(Error {
                        message: format!("Variable not found: {}", name),
                        location: expr.span.clone(),
                    });
                    vec![Type::Error]
                }
            }
            ast::Expression::Call {
                function,
                arguments,
            } => {
                let mut func_type = if let ast::Expression::Literal(ast::Literal::String(f)) =
                    &***function
                {
                    match f.as_str() {
                        "__eq" | "__ne" => {
                            Type::Function(vec![Type::Any, Type::Any], vec![Type::Bool])
                        }
                        "__lt" | "__le" | "__gt" | "__ge" => {
                            Type::Function(vec![Type::Number, Type::Number], vec![Type::Bool])
                        }
                        "__add" | "__sub" | "__mul" | "__div" | "__idiv" | "__mod" | "__pow" => {
                            Type::Function(vec![Type::Number, Type::Number], vec![Type::Number])
                        }
                        "__neg" => Type::Function(vec![Type::Number], vec![Type::Number]),
                        "__len" => Type::Function(
                            vec![Type::Union(vec![
                                Type::String,
                                Type::Table(TypeTable::any()),
                            ])],
                            vec![Type::Number],
                        ),
                        _ => self.check_expression(function)[0].clone(), // possiblly out of range
                    }
                } else {
                    self.check_expression(function)[0].clone()
                };

                if let Type::Any = func_type {
                    for arg in arguments {
                        self.check_expression(arg);
                    }
                    return vec![Type::Any; 16]; // TODO: We want to return [any, any, any, ...]
                }

                if let Type::Error = func_type {
                    for arg in arguments {
                        self.check_expression(arg);
                    }
                    return vec![Type::Error];
                }

                let arg_types: Vec<_> = if arguments.len() == 1 {
                    self.check_expression(&arguments[0])
                } else {
                    arguments
                        .iter()
                        .map(|arg| {
                            self.check_expression(arg)
                                .first()
                                .cloned()
                                .unwrap_or(Type::Nil)
                        })
                        .collect()
                };

                // Resolve generic types (if it's easy)
                'resolve: {
                    if let Type::Generic(tvs, t) = &func_type {
                        if let Type::Function(params, return_types) = &**t {
                            let mut ctx = self.child();
                            for tv in tvs.iter() {
                                if let Some(i) = params.iter().position(|p| {
                                    if let Type::Variable(v_) = p {
                                        tv == v_
                                    } else {
                                        false
                                    }
                                }) {
                                    let t = arg_types.get(i).unwrap_or(&Type::Nil);
                                    ctx.type_table.insert(tv.name().to_owned(), t.clone());
                                } else {
                                    break 'resolve;
                                }
                            }
                            func_type = Type::Function(
                                ctx.resolve_types(params.iter()).to_vec(),
                                ctx.resolve_types(return_types.iter()).to_vec(),
                            );
                        }
                    }
                }

                if let Type::Function(params, return_types) = func_type {
                    for i in 0..params.len() {
                        let param = self.resolve_type(&params[i]);
                        self.type_match(&param, arg_types.get(i).unwrap_or(&Type::Nil), &expr.span);
                    }
                    return return_types;
                }

                self.add_error(Error {
                    message: format!("Not a function: {}", func_type),
                    location: expr.span.clone(),
                });
                vec![Type::Error]
            }
            ast::Expression::Index { table, index } => {
                let table_types = self.check_expression(table);
                let table_type = &table_types[0];
                match table_type {
                    Type::Any => {
                        self.check_expression(index);
                        return vec![Type::Any];
                    }
                    Type::Table(table_type) => {
                        let index_types = self.check_expression(index);
                        let index_type = &index_types[0];
                        vec![self.check_table(&table_type, &index_type, &expr.span)]
                    }
                    Type::Union(ts) => {
                        let ts = ts
                            .iter()
                            .map(|t| match t {
                                Type::Any => {
                                    self.check_expression(index);
                                    Type::Any
                                }
                                Type::Table(table_type) => {
                                    let index_types = self.check_expression(index);
                                    let index_type = &index_types[0];
                                    self.check_table(&table_type, &index_type, &expr.span)
                                }
                                _ => {
                                    self.add_error(Error {
                                        message: format!("Not a table: {}", table_type),
                                        location: expr.span.clone(),
                                    });
                                    Type::Error
                                }
                            })
                            .collect::<Vec<_>>();
                        vec![Type::Union(ts).normalize()]
                    }
                    _ => {
                        self.add_error(Error {
                            message: format!("Not a table: {}", table_type),
                            location: expr.span.clone(),
                        });
                        vec![Type::Error]
                    }
                }
            }
            ast::Expression::Fn(function) => {
                if function.type_params.is_empty() {
                    let ret_types = self
                        .child()
                        .check_function(function)
                        .unwrap_or_else(|_| vec![Type::Error]);
                    vec![Type::Function(
                        function.parameters.iter().map(|(_, t)| t.clone()).collect(),
                        ret_types,
                    )]
                } else {
                    let mut ctx = self.child();
                    for p in &function.type_params {
                        ctx.type_table
                            .insert(p.name().to_owned(), Type::Variable(p.clone()));
                    }
                    let ret_types = ctx
                        .check_function(function)
                        .unwrap_or_else(|_| vec![Type::Error]);
                    vec![Type::Generic(
                        function.type_params.clone(),
                        Box::new(Type::Function(
                            function.parameters.iter().map(|(_, t)| t.clone()).collect(),
                            ret_types,
                        )),
                    )]
                }
            }
            ast::Expression::Table(vec) => {
                let mut consts = vec![];
                let mut number = vec![];
                let mut string = vec![];
                let mut bool = vec![];
                let mut table = vec![];
                let mut functions = vec![];

                for (key, value) in vec {
                    let value_types = self.check_expression(value);
                    let value_type = value_types[0].clone();
                    match key {
                        ast::TableKey::Literal(literal) => {
                            consts.push((literal.to_const_data(), value_type))
                        }
                        ast::TableKey::Expression(e) => {
                            let key_types = self.check_expression(e);
                            let key_type = key_types[0].clone();
                            match key_type {
                                Type::Number => number.push(value_type),
                                Type::String => string.push(value_type),
                                Type::Bool => bool.push(value_type),
                                Type::Const(const_data) => consts.push((const_data, value_type)),
                                Type::Table(_) => table.push(value_type),
                                Type::Function(_, _) => functions.push(value_type),
                                t => {
                                    self.add_error(Error {
                                        message: format!("Invalid table key type: {}", t),
                                        location: e.span.clone(),
                                    });
                                }
                            }
                        }
                    }
                }

                vec![Type::Table(TypeTable {
                    consts,
                    number: Type::from_types(number).map(Box::new),
                    string: Type::from_types(string).map(Box::new),
                    bool: Type::from_types(bool).map(Box::new),
                    table: Type::from_types(table).map(Box::new),
                    function: Type::from_types(functions).map(Box::new),
                })]
            }
            ast::Expression::LogicalAnd(a, b) => {
                let ta = self.check_expression(a);
                let type_filter = type_filter::expression_to_type_filter(a);
                let tb = if let Some(type_filter) = type_filter {
                    let all_symbols = self.all_symbols();
                    let type_filter = type_filter::type_filter_to_dnf(&type_filter);
                    let mut ctx = self.type_guarded(&all_symbols, &type_filter, false);
                    ctx.check_expression(b)
                } else {
                    self.check_expression(b)
                };
                match ta[0].is_truthy() {
                    Some(true) => tb,
                    Some(false) => ta,
                    None => {
                        vec![Type::Union(vec![ta[0].clone(), tb[0].clone()]).normalize()]
                    }
                }
            }
            ast::Expression::LogicalOr(a, b) => {
                let ta = self.check_expression(a);
                let type_filter = type_filter::expression_to_type_filter(a);
                let tb = if let Some(type_filter) = type_filter {
                    let all_symbols = self.all_symbols();
                    let type_filter = type_filter::type_filter_to_dnf(&type_filter);
                    let mut ctx = self.type_guarded(&all_symbols, &type_filter, true);
                    ctx.check_expression(b)
                } else {
                    self.check_expression(b)
                };
                match ta[0].is_truthy() {
                    Some(true) => ta,
                    Some(false) => tb,
                    None => {
                        vec![Type::Union(vec![ta[0].clone(), tb[0].clone()]).normalize()]
                    }
                }
            }
            ast::Expression::LogicalNot(e) => {
                self.check_expression(e);
                vec![Type::Bool]
            }
            ast::Expression::As(e, t) => {
                let actual = self.check_expression(e);
                let expect = self.resolve_type(t);
                self.type_match(&expect, &actual[0], &expr.span);
                vec![expect]
            }
            ast::Expression::TypeResolve(e, type_args) => {
                let ts = self.check_expression(e);
                let type_args = self.resolve_types(type_args.iter());
                if ts.len() != 1 {
                    // ?
                    self.add_error(Error {
                        message: format!("Type resolve expects one argument, got {}", ts.len()),
                        location: expr.span.clone(),
                    });
                    return vec![Type::Error];
                }
                match &ts[0] {
                    Type::Generic(params, t) => {
                        if params.len() != type_args.len() {
                            self.add_error(Error {
                                message: format!(
                                    "Type resolve expects same number of type arguments as generic parameters"
                                ),
                                location: expr.span.clone(),
                            });
                            return vec![Type::Error];
                        }
                        let mut ctx = self.child();
                        for (param, arg) in params.iter().zip(type_args.iter()) {
                            ctx.type_table.insert(param.name().to_owned(), arg.clone());
                        }
                        vec![ctx.resolve_type(t)]
                    }
                    _ => {
                        self.add_error(Error {
                            message: format!("Type resolve expects a generic type"),
                            location: expr.span.clone(),
                        });
                        vec![Type::Error]
                    }
                }
            }
        }
    }

    fn conditioned(&'a self, condition: &ast::SpannedExpression) -> (Self, Self) {
        if let Some(type_filter) = type_filter::expression_to_type_filter(condition) {
            let all_symbols = self.all_symbols();
            let type_filter = type_filter::type_filter_to_dnf(&type_filter);
            (
                self.type_guarded(&all_symbols, &type_filter, false),
                self.type_guarded(&all_symbols, &type_filter, true),
            )
        } else {
            (self.child(), self.child())
        }
    }

    fn type_guarded(
        &self,
        all_symbols: &Vec<(&String, &Arc<Type>)>,
        type_filter: &TfDnf,
        neg: bool,
    ) -> Context<'_> {
        Context {
            parent: Ok(self),
            symbol_table: HashMap::new(),
            symbol_type_guarded: all_symbols
                .iter()
                .filter_map(|(name, at)| {
                    let r#type = self.get_symbol_type(name).unwrap();
                    if !r#type.is_concrete() {
                        return None;
                    }
                    let type_ = type_filter::type_filter_apply(type_filter, name, &r#type, neg);
                    if r#type == &type_ {
                        None
                    } else {
                        Some((Arc::as_ptr(at), type_))
                    }
                })
                .collect(),
            type_table: HashMap::new(),
            type_guard_propagation: true,
        }
    }

    fn check_table(&mut self, table_type: &TypeTable, index_type: &Type, span: &ast::Span) -> Type {
        check_table(table_type, index_type).unwrap_or_else(|e| {
            self.add_error(Error {
                message: e,
                location: span.clone(),
            });
            Type::Error
        })
    }

    fn type_match(&mut self, expect: &Type, actual: &Type, span: &ast::Span) -> bool {
        if expect.is_error() || actual.is_error() {
            return true;
        }
        if expect.include(actual) {
            true
        } else {
            self.add_error(Error {
                message: format!("Type mismatch, expected {}, got {}", expect, actual),
                location: span.clone(),
            });
            false
        }
    }
}

fn check_table(table_type: &TypeTable, index_type: &Type) -> Result<Type, String> {
    if index_type.is_error() {
        return Ok(Type::Error);
    }

    match index_type {
        Type::Number => {
            if let Some(t) = &table_type.number {
                return Ok((**t).clone());
            }
        }
        Type::String => {
            if let Some(t) = &table_type.string {
                return Ok((**t).clone());
            }
        }
        Type::Bool => {
            if let Some(t) = &table_type.bool {
                return Ok((**t).clone());
            }
        }
        Type::Table(_) => {
            if let Some(t) = &table_type.table {
                return Ok((**t).clone());
            }
        }
        Type::Function(_, _) => {
            if let Some(t) = &table_type.function {
                return Ok((**t).clone());
            }
        }
        Type::Const(const_data) => {
            for (cd, ty) in table_type.consts.iter() {
                if cd == const_data {
                    return Ok(ty.clone());
                }
            }
            return check_table(table_type, &const_data.r#type());
        }
        Type::Union(ts) => {
            let ts = ts
                .iter()
                .map(|t| check_table(table_type, t))
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(Type::from_types(ts).unwrap());
        }
        Type::Any => {
            // TODO
            return Ok(Type::Any);
        }
        _ => {
            // TODO
        }
    }

    // According to the Lua specification, It returns nil if the index is not found.
    // Ok(Type::Nil)
    Err(format!("Invalid index type: {}", index_type))
}

impl ReturnType {
    fn into_types(self) -> Vec<Type> {
        match self {
            ReturnType::Fixed(ts) => ts,
            ReturnType::Infer(typess) => {
                let mut types = vec![];
                for i in 0..typess.iter().map(|ts| ts.len()).max().unwrap_or(0) {
                    let ts = typess
                        .iter()
                        .map(|types| types.get(i).cloned().unwrap_or(Type::Nil))
                        .collect();
                    types.push(Type::from_types(ts).unwrap());
                }
                types
            }
        }
    }
}
