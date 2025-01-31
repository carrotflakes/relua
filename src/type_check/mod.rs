mod type_filter;

use std::collections::HashMap;
use std::sync::Arc;

use crate::ast;
use crate::r#type::{ConstData, Type, TypeTable};

thread_local! {
    static SOURCE: std::cell::RefCell<*const str> = std::cell::RefCell::new("");
}

pub struct Context<'a> {
    parent: Option<&'a Context<'a>>,
    symbol_table: HashMap<String, Arc<Type>>,
    symbol_type_guarded: HashMap<*const Type, Type>,
    type_table: HashMap<String, Type>,
}

enum ReturnType {
    Fixed(Vec<Type>),
    Infer(Vec<Vec<Type>>),
}

impl<'a> Context<'a> {
    pub fn from_symbol_table(symbol_table: HashMap<String, Type>) -> Self {
        Self {
            parent: None,
            symbol_table: symbol_table
                .into_iter()
                .map(|(name, type_)| (name.clone(), Arc::new(type_)))
                .collect(),
            symbol_type_guarded: HashMap::new(),
            type_table: HashMap::new(),
        }
    }

    fn child(&'a self) -> Self {
        Self {
            parent: Some(self),
            symbol_table: HashMap::new(),
            symbol_type_guarded: HashMap::new(),
            type_table: HashMap::new(),
        }
    }

    fn insert(&mut self, name: String, type_: Type) {
        self.symbol_table.insert(name.clone(), Arc::new(type_));
    }

    fn get_symbol_variable_type(&self, name: &str) -> Option<&Arc<Type>> {
        self.symbol_table
            .get(name)
            .or_else(|| self.parent.and_then(|p| p.get_symbol_variable_type(name)))
    }

    fn get_symbol_type(&self, name: &str) -> Option<&Type> {
        let at = self.get_symbol_variable_type(name)?;
        let mut ctx = Some(self);
        while let Some(c) = ctx {
            if let Some(t) = c.symbol_type_guarded.get(&Arc::as_ptr(&at)) {
                return Some(t);
            }
            ctx = c.parent;
        }
        Some(&**at)
    }

    fn resolve_type(&self, type_: &Type) -> Result<Type, String> {
        type_.resolve(&|name| {
            let mut ctx = Some(self);
            while let Some(c) = ctx {
                if let Some(t) = c.type_table.get(name) {
                    return Some(t.clone());
                }
                ctx = c.parent;
            }
            None
        })
    }

    fn resolve_types<'b>(&self, iter: impl Iterator<Item = &'b Type>) -> Result<Vec<Type>, String> {
        let mut types = vec![];
        for t in iter {
            types.push(self.resolve_type(t)?);
        }
        Ok(types)
    }

    pub fn check_program(
        &self,
        stmts: &[ast::SpannedStatement],
        source: &str,
    ) -> Result<Vec<Type>, String> {
        SOURCE.with(|s| {
            *s.borrow_mut() = source;
        });

        let mut return_types = ReturnType::Infer(vec![]);

        self.child().check_statements(stmts, &mut return_types)?;

        SOURCE.with(|s| {
            *s.borrow_mut() = "";
        });

        Ok(return_types.into_types())
    }

    fn check_statements(
        &mut self,
        stmts: &[ast::SpannedStatement],
        return_type: &mut ReturnType,
    ) -> Result<(), String> {
        for stmt in stmts {
            match &**stmt {
                ast::Statement::Expression(expression) => {
                    self.check_expression(expression)?;
                }
                ast::Statement::Fn { name, function } => {
                    let mut ctx1 = self.child();
                    for p in &function.type_params {
                        ctx1.type_table.insert(p.clone(), Type::Variable(p.clone()));
                    }

                    let params = ctx1.resolve_types(function.parameters.iter().map(|(_, t)| t))?;
                    let wrap_generics: &dyn Fn(Type) -> Type = if function.type_params.is_empty() {
                        &|t| t
                    } else {
                        &|t| Type::Generic(function.type_params.clone(), Box::new(t))
                    };

                    // Add function to bindings to allow recursion
                    ctx1.insert(
                        name.clone(),
                        wrap_generics(Type::Function(
                            params.clone(),
                            if let Some(t) = &function.return_types {
                                ctx1.resolve_types(t.iter())?
                            } else {
                                vec![]
                            }, // unknown?
                        )),
                    );

                    let ret_types = ctx1.check_function(function)?;
                    self.insert(
                        name.clone(),
                        wrap_generics(Type::Function(params, ret_types.clone())),
                    );
                }
                ast::Statement::Let(vars, exprs) => {
                    if exprs.len() == 1 {
                        let actual = self.check_expression(&exprs[0])?;
                        for i in 0..vars.len() {
                            let var = &vars[i];
                            let actual = actual.get(i).unwrap_or(&Type::Nil).clone();
                            if let Some(expect) = &var.1 {
                                let expect = self.resolve_type(expect)?;
                                type_match(&expect, &actual, &stmt.span)?;
                                self.insert(var.0.clone(), expect.clone());
                            } else {
                                self.insert(var.0.clone(), actual);
                            }
                        }
                    } else {
                        for i in 0..vars.len() {
                            let var = &vars[i];
                            let actual = self.check_expression(&exprs[i])?;
                            if let Some(expect) = &var.1 {
                                let expect = self.resolve_type(expect)?;
                                type_match(&expect, &actual[0], &stmt.span)?;
                                self.insert(var.0.clone(), expect.clone());
                            } else {
                                self.insert(var.0.clone(), actual[0].clone());
                            }
                        }
                    }
                }
                ast::Statement::Assignment { vars, exprs } => {
                    let var_types = vars
                        .iter()
                        .map(|var| match var {
                            ast::LValue::Variable(name) => self
                                .get_symbol_variable_type(name)
                                .ok_or_else(|| format!("Variable not found: {}", name))
                                .map_err(append_loc(&stmt.span))
                                .map(|t| (**t).clone()),
                            ast::LValue::Index(table, index) => {
                                let table_type = self.check_expression(table)?[0].clone();
                                match table_type {
                                    Type::Any => {
                                        self.check_expression(index)?;
                                        Ok(Type::Any)
                                    }
                                    Type::Table(table_type) => {
                                        let index_type = self.check_expression(index)?[0].clone();
                                        check_table(&table_type, &index_type)
                                            .map_err(append_loc(&stmt.span))
                                    }
                                    _ => Err(format!("Not a table: {}", table_type))
                                        .map_err(append_loc(&stmt.span)),
                                }
                            }
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    if exprs.len() == 1 {
                        // let x, y = f()
                        let actual = self.check_expression(&exprs[0])?;
                        for i in 0..var_types.len() {
                            let expect = self.resolve_type(&var_types[i])?;
                            let actual = actual.get(i).unwrap_or(&Type::Nil);
                            type_match(&expect, actual, &stmt.span)?;
                            match &vars[i] {
                                ast::LValue::Variable(v) => {
                                    let v = self.get_symbol_variable_type(v).unwrap();
                                    self.symbol_type_guarded.insert(&**v, actual.clone());
                                }
                                ast::LValue::Index(_, _) => {} // TODO
                            }
                        }
                    } else {
                        // let x, y = 1, 2
                        for i in 0..var_types.len().max(exprs.len()) {
                            let actual = if i < exprs.len() {
                                self.check_expression(&exprs[i])?[0].clone()
                            } else {
                                Type::Nil
                            };
                            let expect = self.resolve_type(&var_types[i])?;
                            type_match(&expect, &actual, &stmt.span)?;
                            match &vars[i] {
                                ast::LValue::Variable(v) => {
                                    let v = self.get_symbol_variable_type(v).unwrap();
                                    self.symbol_type_guarded.insert(&**v, actual.clone());
                                }
                                ast::LValue::Index(_, _) => {} // TODO
                            }
                        }
                    }
                }
                ast::Statement::If {
                    condition,
                    then,
                    else_,
                } => {
                    self.check_expression(condition)?;
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
                    self.check_expression(condition)?;
                    // TODO: type guard
                    self.child().check_statements(body, return_type)?;
                }
                ast::Statement::ForNumeric {
                    variable,
                    start,
                    end,
                    step,
                    body,
                } => {
                    let start_type = self.check_expression(start)?[0].clone();
                    type_match(&Type::Number, &start_type, &stmt.span)?;
                    let end_type = self.check_expression(end)?[0].clone();
                    type_match(&Type::Number, &end_type, &stmt.span)?;
                    if let Some(step) = step {
                        let step_type = self.check_expression(step)?[0].clone();
                        type_match(&Type::Number, &step_type, &stmt.span)?;
                    }
                    let mut ctx = self.child();
                    ctx.insert(variable.clone(), Type::Number);
                    ctx.check_statements(body, return_type)?;
                }
                ast::Statement::ForGeneric {
                    variables,
                    exprs,
                    body,
                } => {
                    // TODO: type check
                    for expr in exprs {
                        self.check_expression(expr)?;
                    }
                    let mut ctx = self.child();
                    for (name, type_) in variables {
                        ctx.insert(name.clone(), type_.clone().unwrap_or(Type::Any));
                    }
                    ctx.check_statements(body, return_type)?;
                }
                ast::Statement::Return(exprs) => {
                    if exprs.len() == 1 {
                        let actual = self.check_expression(&exprs[0])?;
                        match return_type {
                            ReturnType::Fixed(expect) => {
                                for i in 0..expect.len() {
                                    type_match(
                                        &expect[i],
                                        actual.get(i).unwrap_or(&Type::Nil),
                                        &stmt.span,
                                    )?;
                                }
                            }
                            ReturnType::Infer(typess) => {
                                typess.push(actual);
                            }
                        }
                    } else {
                        let mut actuals = vec![];
                        for expr in exprs {
                            let actual = self.check_expression(expr)?;
                            actuals.push(actual[0].clone());
                        }
                        match return_type {
                            ReturnType::Fixed(expect) => {
                                for i in 0..expect.len() {
                                    type_match(
                                        &expect[i],
                                        actuals.get(i).unwrap_or(&Type::Nil),
                                        &stmt.span,
                                    )?;
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
                    self.type_table.insert(name.clone(), type_.clone());
                }
            }
        }
        Ok(())
    }

    fn check_function(&self, function: &ast::Function) -> Result<Vec<Type>, String> {
        let mut ctx = self.child();

        for (name, type_) in &function.parameters {
            ctx.insert(name.clone(), self.resolve_type(type_)?);
        }

        let mut ret_types = match &function.return_types {
            Some(t) => ReturnType::Fixed(self.resolve_types(t.iter())?),
            None => ReturnType::Infer(vec![]),
        };

        ctx.check_statements(&function.body, &mut ret_types)?;

        Ok(ret_types.into_types())
    }

    fn check_expression(&self, expr: &ast::SpannedExpression) -> Result<Vec<Type>, String> {
        Ok(match &**expr {
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
                let val = self
                    .get_symbol_type(name)
                    .ok_or_else(|| format!("Variable not found: {}", name))
                    .map_err(append_loc(&expr.span))?
                    .clone();
                vec![val]
            }
            ast::Expression::Call {
                function,
                arguments,
            } => {
                let func_type = if let ast::Expression::Literal(ast::Literal::String(f)) =
                    &***function
                {
                    match f.as_str() {
                        "__eq" => Type::Function(vec![Type::Any, Type::Any], vec![Type::Bool]),
                        "__lt" | "__le" => {
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
                        _ => self.check_expression(function)?[0].clone(),
                    }
                } else {
                    self.check_expression(function)?[0].clone()
                };

                if let Type::Any = func_type {
                    for arg in arguments {
                        self.check_expression(arg)?;
                    }
                    return Ok(vec![Type::Any]); // TODO
                }

                if let Type::Function(params, return_types) = func_type {
                    if arguments.len() == 1 {
                        let actual = self.check_expression(&arguments[0])?;
                        for i in 0..params.len() {
                            let param = self.resolve_type(&params[i])?;
                            type_match(&param, actual.get(i).unwrap_or(&Type::Nil), &expr.span)?;
                        }
                    } else {
                        for i in 0..params.len() {
                            let param = self.resolve_type(&params[i])?;
                            let actual = if let Some(arg) = arguments.get(i) {
                                self.check_expression(arg)?[0].clone()
                            } else {
                                Type::Nil
                            };
                            type_match(&param, &actual, &expr.span)?;
                        }
                    }
                    return_types
                } else {
                    return Err(format!("Not a function: {}", func_type))
                        .map_err(append_loc(&expr.span));
                }
            }
            ast::Expression::Index { table, index } => {
                let table_types = self.check_expression(table)?;
                let table_type = &table_types[0];
                match table_type {
                    Type::Any => {
                        self.check_expression(index)?;
                        return Ok(vec![Type::Any]);
                    }
                    Type::Table(table_type) => {
                        let index_types = self.check_expression(index)?;
                        let index_type = &index_types[0];
                        vec![check_table(&table_type, &index_type)
                            .map_err(append_loc(&expr.span))?]
                    }
                    Type::Union(ts) => {
                        let ts = ts
                            .iter()
                            .map(|t| match t {
                                Type::Any => {
                                    self.check_expression(index)?;
                                    Ok(Type::Any)
                                }
                                Type::Table(table_type) => {
                                    let index_types = self.check_expression(index)?;
                                    let index_type = &index_types[0];
                                    Ok(check_table(&table_type, &index_type)
                                        .map_err(append_loc(&expr.span))?)
                                }
                                _ => Err(format!("Not a table: {}", table_type))
                                    .map_err(append_loc(&expr.span)),
                            })
                            .collect::<Result<Vec<_>, _>>()?;
                        vec![Type::Union(ts).normalize()]
                    }
                    _ => {
                        return Err(format!("Not a table: {}", table_type))
                            .map_err(append_loc(&expr.span))
                    }
                }
            }
            ast::Expression::Fn(function) => {
                if function.type_params.is_empty() {
                    let ret_types = self.check_function(function)?;
                    vec![Type::Function(
                        function.parameters.iter().map(|(_, t)| t.clone()).collect(),
                        ret_types,
                    )]
                } else {
                    let mut ctx = self.child();
                    for p in &function.type_params {
                        ctx.type_table.insert(p.clone(), Type::Variable(p.clone()));
                    }
                    let ret_types = ctx.check_function(function)?;
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
                    let value_types = self.check_expression(value)?;
                    let value_type = value_types[0].clone();
                    match key {
                        ast::TableKey::Literal(literal) => {
                            consts.push((literal.to_const_data(), value_type))
                        }
                        ast::TableKey::Expression(e) => {
                            let key_types = self.check_expression(e)?;
                            let key_type = key_types[0].clone();
                            match key_type {
                                Type::Number => number.push(value_type),
                                Type::String => string.push(value_type),
                                Type::Bool => bool.push(value_type),
                                Type::Const(const_data) => consts.push((const_data, value_type)),
                                Type::Table(_) => table.push(value_type),
                                Type::Function(_, _) => functions.push(value_type),
                                t => {
                                    return Err(format!("Invalid table key type: {}", t))
                                        .map_err(append_loc(&expr.span));
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
                let a = self.check_expression(a)?;
                let b = self.check_expression(b)?;
                vec![Type::from_types(vec![a[0].clone(), b[0].clone()]).unwrap()]
            }
            ast::Expression::LogicalOr(a, b) => {
                let a = self.check_expression(a)?;
                let b = self.check_expression(b)?;
                vec![Type::from_types(vec![a[0].clone(), b[0].clone()]).unwrap()]
            }
            ast::Expression::LogicalNot(e) => {
                self.check_expression(e)?;
                vec![Type::Bool]
            }
            ast::Expression::TypeResolve(e, type_args) => {
                let ts = self.check_expression(e)?;
                let type_args = self.resolve_types(type_args.iter())?;
                if ts.len() != 1 {
                    return Err(format!("Type resolve expects one argument",))
                        .map_err(append_loc(&expr.span));
                }
                match &ts[0] {
                    Type::Generic(params, t) => {
                        if params.len() != type_args.len() {
                            return Err(format!("Type resolve expects same number of type arguments as generic parameters")).map_err(append_loc(&expr.span));
                        }
                        let mut ctx = self.child();
                        for (param, arg) in params.iter().zip(type_args.iter()) {
                            ctx.type_table.insert(param.clone(), arg.clone());
                        }
                        vec![ctx.resolve_type(t)?]
                    }
                    _ => {
                        return Err(format!("Type resolve expects a generic type",))
                            .map_err(append_loc(&expr.span));
                    }
                }
            }
        })
    }

    fn conditioned(&'a self, condition: &ast::SpannedExpression) -> (Self, Self) {
        let type_filter = type_filter::expression_to_type_filter(condition);
        if let Some(type_filter) = type_filter {
            let type_filter = type_filter::type_filter_to_dnf(&type_filter);
            let ctx1 = Context {
                parent: Some(self),
                symbol_table: HashMap::new(),
                symbol_type_guarded: self
                    .symbol_table
                    .iter()
                    .filter_map(|(name, at)| {
                        let r#type = self.get_symbol_type(name).unwrap();
                        let type_ =
                            type_filter::type_filter_apply(&type_filter, name, r#type, false);
                        if r#type == &type_ {
                            None
                        } else {
                            Some((Arc::as_ptr(at), type_))
                        }
                    })
                    .collect(),
                type_table: HashMap::new(),
            };
            let ctx2 = Context {
                parent: Some(self),
                symbol_table: HashMap::new(),
                symbol_type_guarded: self
                    .symbol_table
                    .iter()
                    .filter_map(|(name, at)| {
                        let r#type = self.get_symbol_type(name).unwrap();
                        let type_ =
                            type_filter::type_filter_apply(&type_filter, name, r#type, true);
                        if r#type == &type_ {
                            None
                        } else {
                            Some((Arc::as_ptr(at), type_))
                        }
                    })
                    .collect(),
                type_table: HashMap::new(),
            };
            (ctx1, ctx2)
        } else {
            (self.child(), self.child())
        }
    }
}

fn check_table(table_type: &TypeTable, index_type: &Type) -> Result<Type, String> {
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
        _ => {}
    }
    Err(format!("Invalid index type: {}", index_type))
}

fn type_match(expect: &Type, actual: &Type, span: &ast::Span) -> Result<(), String> {
    if expect.include(actual) {
        Ok(())
    } else {
        Err(append_loc(span)(format!(
            "Type mismatch, expected {}, got {}",
            expect, actual
        )))
    }
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

fn append_loc(span: &ast::Span) -> impl Fn(String) -> String {
    let pos = span.0.start;
    move |msg| {
        let src = SOURCE.with(|s| unsafe { &**s.borrow() });
        let loc = row_and_col(src, pos);
        format!("{} at {:?}", msg, loc)
    }
}

fn row_and_col(src: &str, pos: usize) -> (usize, usize) {
    let mut row = 1;
    let mut col = 1;
    for (i, c) in src.char_indices() {
        if i == pos {
            break;
        }
        if c == '\n' {
            row += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (row, col)
}
