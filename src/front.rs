use crate::ast::*;
use crate::r#type::{ConstData, Type, TypeTable, Variable as TypeVariable};

// NOTE: We cannot add "type" to the forbidden identifiers because it is a function in Lua.
const FORBIDDEN_IDENTIFIERS: &[&str] = &[
    "fn", "let", "if", "else", "while", "for", "in", "return", "break", "true", "false", "as", "declare",
];

// https://docs.rs/peg/latest/peg/
peg::parser!(pub grammar parser() for str {
    pub rule program() -> Vec<SpannedStatement>
        = statements()

    rule statements() -> Vec<SpannedStatement>
        = _ ss:(statement() ** _) _ rs:ret_stat()? {
            let mut ss = ss;
            if let Some(r) = rs {
                ss.push(r);
            }
            ss
        }

    rule ret_stat() -> SpannedStatement
        = spanned_stmt(<
            "return" es:((_ e:expression() _ { e }) ** ",") { Statement::Return(es) } / "break" { Statement::Break }
        >)

    rule statement() -> SpannedStatement
        = spanned_stmt(<
            let_stmt()
            / if_else()
            / while_loop()
            / for_numeric()
            / for_generic()
            / assignment()
            / def_function()
            / type_alias()
            / declare_let()
            / e:expression() { Statement::Expression(e) }
        >)

    rule let_stmt() -> Statement
        = "let" vs:((_ name:spanned_identifier() _ t:(":" _ t:type_() _ { t })? { (name, t) }) ++ ",") "=" es:((_ e:expression() _ { e }) ++ ",") { Statement::Let(vs, es) }

    rule if_else() -> Statement
        = "if" _ e:expression() _ "{" _
        then_body:statements() _ "}"
        else_body:(_ "else" _ "{" _ ss:statements() _ "}" { ss })?
        { Statement::If { condition: e, then: then_body, else_: else_body.unwrap_or_default() } }

    rule while_loop() -> Statement
        = "while" _ e:expression() _ "{" _
        loop_body:statements() _ "}"
        { Statement::While { condition: e, body: loop_body } }

    rule for_numeric() -> Statement
        = "for" _ i:spanned_identifier() _ "=" _ s:expression() _ "," _ e:expression() step:(_ "," _ e:expression() _ { e })? _ "{" _
        body:statements() _ "}"
        { Statement::ForNumeric { variable: i, start: s, end: e, step, body } }

    rule for_generic() -> Statement
        = "for" _ vs:((_ name:spanned_identifier() _ t:(":" _ t:type_() _ { t })? { (name, t) }) ++ ",") _ "in" _ es:((_ e:expression() _ { e }) ++ ",") _ "{" _
        body:statements() _ "}"
        { Statement::ForGeneric { variables: vs, exprs: es, body } }

    rule assignment() -> Statement
        = vs:((_ l:lval() _ { l }) ++ ",") _ "=" _ es:((_ e:expression() _ { e }) ++ ",") {
            Statement::Assignment {
                vars: vs,
                exprs: es,
            }
        }

    rule def_function() -> Statement
        = "fn" _ name:spanned_identifier() _ tps:type_params_opt() _
        "(" params:((_ i:spanned_identifier() _ ":" _ t:type_() {(i, t)}) ** ",") ")" _
        rt:ret_types()?
        "{" _ stmts:statements() _ "}"
        { Statement::Fn {name, function: Function { type_params: tps, parameters: params, return_types: rt, body: stmts } } }

    rule type_alias() -> Statement
        = "type" _ name:spanned_identifier() _ "=" _ t:type_() { Statement::TypeAlias(TypeVariable::from_spanned_str(name), t) }

    rule declare_let() -> Statement
        = "declare" _ "let" _ vs:((_ name:spanned_identifier() _ ":" _ t:type_() _ { (name, t) }) ++ ",") { Statement::DeclareLet(vs) }

    rule lval() -> LValue
        = t:expression() {?
            match t.node {
                Expression::Index { table, index } => Ok(LValue::Index(table, index)),
                Expression::Variable(i) => Ok(LValue::Variable(spanned(t.span.start, t.span.end, i))),
                _ => Err("expected index or variable expression"),
            }
        }

    rule expression() -> SpannedExpression = precedence!{
        a:(@) _ "||" _ b:@ { spanned(a.span.start, b.span.end, Expression::LogicalOr(Box::new(a), Box::new(b))) }
        --
        a:(@) _ "&&" _ b:@ { spanned(a.span.start, b.span.end, Expression::LogicalAnd(Box::new(a), Box::new(b))) }
        --
        a:(@) _ "==" _ b:@ { bin_op_expr("__eq", a, b) }
        a:(@) _ "!=" _ b:@ { spanned(a.span.start, b.span.end, Expression::LogicalNot(Box::new(bin_op_expr("__eq", a, b)))) }
        a:(@) _ !type_args() "<"  _ b:@ { bin_op_expr("__lt", a, b) }
        a:(@) _ "<=" _ b:@ { bin_op_expr("__le", a, b) }
        a:(@) _ ">"  _ b:@ { spanned(a.span.start, b.span.end, Expression::LogicalNot(Box::new(bin_op_expr("__le", a, b)))) }
        a:(@) _ ">=" _ b:@ { spanned(a.span.start, b.span.end, Expression::LogicalNot(Box::new(bin_op_expr("__lt", a, b)))) }
        --
        a:(@) _ "+" _ b:@ { bin_op_expr("__add", a, b) }
        a:(@) _ "-" _ b:@ { bin_op_expr("__sub", a, b) }
        --
        a:(@) _ "*" _ b:@ { bin_op_expr("__mul", a, b) }
        a:(@) _ "/" _ b:@ { bin_op_expr("__div", a, b) }
        a:(@) _ idiv_op() _ b:@ { bin_op_expr("__idiv", a, b) }
        a:(@) _ "%" _ b:@ { bin_op_expr("__mod", a, b) }
        --
        a:(@) _ "as" _ t:type_() pe:position!() { spanned(a.span.start, pe, Expression::As(Box::new(a), t)) }
        --
        pb:position!() "-" _ e:@ { spanned(pb, e.span.end, function_expr("__neg", vec![e])) }
        pb:position!() "!" _ e:@ { spanned(pb, e.span.end, Expression::LogicalNot(Box::new(e))) }
        --
        a:@ _ "^" _ b:(@) { bin_op_expr("__pow", a, b) }
        --
        pb:position!() "len!" _ a:@ { spanned(pb, a.span.end, function_expr("__len", vec![a])) }
        a:@ _ "[" _ b:expression() _ "]" pe:position!() { spanned(a.span.start, pe, Expression::Index { table: Box::new(a), index: Box::new(b) }) }
        a:@ _ "(" _ args:((_ e:expression() _ {e}) ** ",") ")" pe:position!() { spanned(a.span.start, pe, Expression::Call { function: Box::new(a), arguments: args }) }
        a:@ _ ts:type_args() pe:position!() { spanned(a.span.start, pe, Expression::TypeResolve(Box::new(a), ts)) }
        a:@ _ "." _ pi:position!() b:identifier() pe:position!() { spanned(a.span.start, pe, Expression::Index { table: Box::new(a), index: Box::new(spanned(pi, pe, Expression::Literal(Literal::String(b)))) }) }
        "(" _ e:expression() _ ")" { e }
        u:spanned_expr(<unary_op()>) { u }
    }

    rule idiv_op() -> ()
        = "//" {? if cfg!(feature = "idiv") {Ok(())} else {Err("// operator is not supported.")} }

    rule unary_op() -> Expression
        = t:table_items() { Expression::Table(t) }
        / f:function() { Expression::Fn(f) }
        / i:identifier() { Expression::Variable(i) }
        / l:literal() { Expression::Literal(l) }
        / "()" { Expression::Nil }

    rule table_items() -> Vec<(TableKey, SpannedExpression)>
        = "{" _ ts:((_ t:table_entry() _ { t }) ** ",") ","? _ "}" {
            let mut tes = vec![];
            let mut i = 1;
            for (k, v) in ts {
                if let Some(k) = k {
                    tes.push((k, v));
                } else {
                    tes.push((TableKey::Literal(Literal::Number(i as f64)), v));
                    i += 1;
                }
            }
            tes
        }

    rule table_entry() -> (Option<TableKey>, SpannedExpression)
        = k:literal() _ ":" _ v:expression() { (Some(TableKey::Literal(k)), v) }
        / k:(k:key() _ ":" { TableKey::Literal(Literal::String(k)) })? _ v:expression() { (k, v) }
        / "[" _ k:expression() _ "]" _ ":" _ v:expression() { (Some(TableKey::Expression(k)), v) }

    rule function() -> Function
        = "fn" _ tps:type_params_opt() _
        "(" params:((_ i:spanned_identifier() _ ":" _ t:type_() {(i, t)}) ** ",") _ ")" _
        rt:ret_types()?
        "{" _ stmts:statements() _ "}"
        { Function { type_params: tps, parameters: params, return_types: rt, body: stmts } }

    rule identifier() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) {? if !FORBIDDEN_IDENTIFIERS.contains(&n) { Ok(n.to_owned()) } else { Err("Keyword is not a identifier") } } }
        / expected!("identifier")

    rule spanned_identifier() -> Spanned<String>
        = pb:position!() i:identifier() pe:position!() { spanned(pb, pe, i) }

    rule key() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { n.to_owned() } }
        / expected!("key")

    rule literal() -> Literal
        = n:$(['0'..='9']+ ("." ['0'..='9']*)?) { Literal::Number(n.parse().unwrap()) }
        / "true" { Literal::Bool(true) }
        / "false" { Literal::Bool(false) }
        / s:string() { Literal::String(s) }

    rule type_() -> Type = precedence!{
        a:(@) _ "|" _ b:@ { Type::Union(vec![a, b]).normalize() }
        --
        tps:type_params() _ a:@ { Type::Generic(tps, Box::new(a)) }
        --
        t:type_table() { t }
        a:type_function() { a }
        a:type_atom() { a }
        i:spanned_identifier() {
            Type::Variable(TypeVariable::from_spanned_str(i))
        }
    }

    rule spanned_stmt(f: rule<Statement>) -> SpannedStatement
        = pb:position!() s:f() pe:position!() { spanned(pb, pe, s) }

    rule spanned_expr(f: rule<Expression>) -> SpannedExpression
        = pb:position!() e:f() pe:position!() { spanned(pb, pe, e) }

    rule type_table_entry() -> (Option<Literal>, Type)
        = k:literal() _ ":" _ v:type_() { (Some(k), v) }
        / i:key() _ ":" _ t:type_() { (Some(Literal::String(i)), t) }
        / t:type_() { (None, t) }

    rule type_table() -> Type
        // TODO: {, [str]: num}
        = "{" _ cs:((_ t:type_table_entry() _ { t }) ** ",") ","? ts:((_ "[" k:$("num" / "str" / "bool" / "table" / "fn") "]" _ ":" _ v:type_() _ { (k, v) }) ** ",") ","? _ "}"
        {
            let mut tes = vec![];
            let mut i = 1;
            for (k, v) in cs {
                if let Some(k) = k {
                    tes.push((k.to_const_data(), v));
                } else {
                    tes.push((ConstData::try_from_f64(i as f64).unwrap(), v));
                    i += 1;
                }
            }
            let number = ts.iter().find(|(k, _)| *k == "num").map(|(_, v)| Box::new(v.clone()));
            let string = ts.iter().find(|(k, _)| *k == "str").map(|(_, v)| Box::new(v.clone()));
            let bool = ts.iter().find(|(k, _)| *k == "bool").map(|(_, v)| Box::new(v.clone()));
            let table = ts.iter().find(|(k, _)| *k == "table").map(|(_, v)| Box::new(v.clone()));
            let function = ts.iter().find(|(k, _)| *k == "fn").map(|(_, v)| Box::new(v.clone()));
            Type::Table(TypeTable { consts: tes, number, string, bool, table, function })
        }

    rule type_function() -> Type
        = "(" _ ps:((_ t:type_() _ { t }) ** ",") ")" _ rt:ret_types() { Type::Function(ps, rt) }

    rule ret_types() -> Vec<Type>
        = "->" _ t:type_() _ { vec![t] }
        / "->" _ "(" ts:((_ t:type_() _ { t }) ** ",") ")" _ { ts }

    rule type_atom() -> Type
        = "num" { Type::Number }
        / "str" { Type::String }
        / "bool" { Type::Bool }
        / "()" { Type::Nil }
        / "unknown" { Type::Unknown }
        / "any" { Type::Any }
        / "never" { Type::never() }
        / l:literal() { Type::Const(l.to_const_data()) }

    rule type_params() -> Vec<TypeVariable>
        = "<" _ ts:((_ t:spanned_identifier() _ { TypeVariable::from_spanned_str(t) }) ++ ",") ">" _ { ts }

    rule type_params_opt() -> Vec<TypeVariable>
        = type_params() / { vec![] }

    rule type_args() -> Vec<Type>
        = "<" _ ts:((_ t:type_() _ { t }) ++ ",") ">" _ { ts }

    rule string() -> String
        = "\"" s:$(("\\" ['"' | '\'' | 'n' | 'r' | 't'] / [^'"'])*) "\"" { unescape(s) }
        / "'" s:$(("\\" ['"' | '\'' | 'n' | 'r' | 't'] / [^'\''])*) "'" { unescape(s) }

    rule _() = quiet!{([' ' | '\t' | '\n'] / "#" [^'\n']*)*}
});

fn function_expr(name: &str, args: Vec<SpannedExpression>) -> Expression {
    Expression::Call {
        function: Box::new(Spanned::none(Expression::Literal(Literal::String(
            name.to_owned(),
        )))),
        arguments: args,
    }
}

fn bin_op_expr(name: &str, a: SpannedExpression, b: SpannedExpression) -> SpannedExpression {
    spanned(
        a.span.start,
        b.span.end,
        Expression::Call {
            function: Box::new(Spanned::none(Expression::Literal(Literal::String(
                name.to_owned(),
            )))),
            arguments: vec![a, b],
        },
    )
}

fn spanned<T>(start: usize, end: usize, value: T) -> Spanned<T> {
    Spanned {
        node: value,
        span: start..end,
    }
}

fn unescape(s: &str) -> String {
    let mut r = String::new();
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next().unwrap() {
                '"' => r.push('"'),
                '\'' => r.push('\''),
                'n' => r.push('\n'),
                'r' => r.push('\r'),
                't' => r.push('\t'),
                c => r.push(c),
            }
        } else {
            r.push(c);
        }
    }
    r
}

#[test]
fn test_parser() {
    fn test_program(program: &str) {
        let defs = parser::program(program).unwrap();
        gilder::assert_golden!(format!("{:?}", defs));
    }

    test_program(
        r#"
let a: num = 1
let b: num = 1.23
fn main(a: num) {
}
main(2)
"#,
    );

    test_program(
        r#"
fn main(a: num) -> num {
    let b: num = 1
    return a * 2 + b
}
"#,
    );

    test_program(
        r#"
fn main(a: num) -> num {
    fn(a: num) -> num {
        return a * 2
    }
}
"#,
    );

    test_program(
        r#"
fn main() -> {bool, bool, [str]: num} {
    return {
        a: 1,
        b: 2,
        true,
        false,
    }
}"#,
    );

    test_program(
        r#"
let a = {}[1][2]
"#,
    );

    test_program(
        r#"
fn main() {
    if true {
        return 1
    }
    if true {
        return 2
    } else {
        return 3
    }
    while true {
        return 4
    }
}
"#,
    );

    test_program(
        r#"
let a: {f: (num) -> num} = {
    f: fn(a: num) -> num {
        return a * 2
    }
}
"#,
    );

    test_program(
        r#"
fn f() {
    {1: 2, 3: 4}[1].a = 1
}
"#,
    );

    test_program(
        r#"
# comment
fn f() { # a
  # b
}
"#,
    );

    test_program(
        r#"
fn f<T>(a: T) -> T {
    return a
}
let a: num = f<num>(1)
"#,
    );

    test_program(r#"len!x + 1"#);

    test_program(r#"while true {break}break"#);

    test_program(r#"let input = 0"#);

    test_program(r#"let a = "abc\"\'\n\r\t" + 'abc\"\'\n\r\t' "#);
}

#[test]
fn peg_test() {
    peg::parser!(pub grammar parser() for str {
        pub rule binary_op() -> String = precedence!{
            a:(@) "!" b:@ { format!("{} ! {}", a, b) }
            --
            a:@ "!a!" { format!("{} !a!", a) }
            "a" { format!("a") }
        }
    });
    dbg!(parser::binary_op("a!a!").err());
}

#[test]
fn peg_test2() {
    peg::parser!(pub grammar parser() for str {
        pub rule binary_op() -> String = precedence!{
            a:(@) "+" b:@ { format!("({} + {})", a, b) }
            a:(@) "-" b:@ { format!("({} - {})", a, b) }
            --
            a:(@) "*" b:@ { format!("({} * {})", a, b) }
            a:(@) "/" b:@ { format!("({} / {})", a, b) }
            --
            s:$(['a'..'z']+) { s.to_owned() }
        }
    });
    dbg!(parser::binary_op("a+b*c").unwrap());
    dbg!(parser::binary_op("a*b+c").unwrap());
}
