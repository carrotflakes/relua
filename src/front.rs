use crate::ast::*;
use crate::r#type::{ConstData, Type, TypeTable};

// https://docs.rs/peg/latest/peg/
peg::parser!(pub grammar parser() for str {
    pub rule program() -> Vec<Definition>
        = _ defs:(definition() ** _) _ { defs }

    rule definition() -> Definition
        = def_function()
        / def_var()
        / e:expression() _ { Definition::Expression(e) }

    rule def_function() -> Definition
        = _ "fn" _ name:identifier() _
        "(" params:((_ i:identifier() _ ":" _ t:type_() {(i, t)}) ** ",") ")" _
        rt:(
            "->" _ t:type_() _ { t }
        )?
        "{" _
        stmts:statements()
        _ "}" _
        { Definition::Function {name, function: Function { parameters: params, return_type: rt.unwrap_or(Type::Nil), body: stmts } } }

    rule def_var() -> Definition
        = _ "let" _ name:identifier() _ t:(":" _ t:type_() _ { t })? "=" _ e:expression() _
        { Definition::Variable(Variable { name, type_: t, expr: e }) }

    rule statements() -> Vec<Statement>
        = s:(statement()*) { s }

    rule statement() -> Statement
        = _ "return" _ e:expression() _ { Statement::Return(Some(e)) }
        / _ "let" _ name:identifier() _ t:(":" _ t:type_() _ { t })? "=" _ e:expression() _
            { Statement::Let(Variable { name, type_: t, expr: e }) }
        / _ e:expression() _ { Statement::Expression(e) }
        / if_else()
        / while_loop()
        / assignment()
        / e:binary_op()  { Statement::Expression(e) }

    rule if_else() -> Statement
        = "if" _ e:expression() _ "{"
        then_body:statements() _ "}" _ "else" _ "{"
        else_body:statements() _ "}"
        { Statement::If { condition: e, then: then_body, else_: else_body } }

    rule while_loop() -> Statement
        = "while" _ e:expression() _ "{"
        loop_body:statements() _ "}"
        { Statement::While { condition: e, body: loop_body } }

    rule assignment() -> Statement
        = i:identifier() _ "=" _ e:expression() {Statement::Assignment {
            target: i,
            e,
        }}

    rule expression() -> Expression
        = binary_op()

    rule binary_op() -> Expression = precedence!{
        a:(@) _ "&&" _ b:@ { Expression::LogicalAnd(Box::new(a), Box::new(b)) }
        a:(@) _ "||" _ b:@ { Expression::LogicalOr(Box::new(a), Box::new(b)) }
        --
        a:@ _ "==" _ b:(@) { Expression::Call { function: "__eq".to_owned(), arguments: vec![a, b] } }
        a:@ _ "!=" _ b:(@) { Expression::LogicalNot(Box::new(Expression::Call { function: "__eq".to_owned(), arguments: vec![a, b] })) }
        a:@ _ "<"  _ b:(@) { Expression::Call { function: "__lt".to_owned(), arguments: vec![a, b] } }
        a:@ _ "<=" _ b:(@) { Expression::Call { function: "__le".to_owned(), arguments: vec![a, b] } }
        a:@ _ ">"  _ b:(@) { Expression::LogicalNot(Box::new(Expression::Call { function: "__le".to_owned(), arguments: vec![a, b] })) }
        a:@ _ ">=" _ b:(@) { Expression::LogicalNot(Box::new(Expression::Call { function: "__lt".to_owned(), arguments: vec![a, b] })) }
        --
        a:(@) _ "+" _ b:@ { Expression::Call { function: "__add".to_owned(), arguments: vec![a, b] } }
        a:(@) _ "-" _ b:@ { Expression::Call { function: "__sub".to_owned(), arguments: vec![a, b] } }
        --
        a:(@) _ "*" _ b:@ { Expression::Call { function: "__mul".to_owned(), arguments: vec![a, b] } }
        a:(@) _ "/" _ b:@ { Expression::Call { function: "__div".to_owned(), arguments: vec![a, b] } }
        a:(@) _ "%" _ b:@ { Expression::Call { function: "__mod".to_owned(), arguments: vec![a, b] } }
        --
        a:(@) _ "[" _ b:expression() _ "]" { Expression::Index { table: Box::new(a), index: Box::new(b) } }
        u:unary_op() { u }
    }

    rule unary_op() -> Expression
        = "(" _ e:expression() _ ")" { e }
        / "{" _ ts:((_ t:table_entry() _ { t }) ** ",") ","? _ "}" {
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
            Expression::Table(tes)
        }
        / "!" _ e:expression() { Expression::LogicalNot(Box::new(e)) }
        / f:function() { Expression::Fn(f) }
        / i:identifier() _ "(" args:((_ e:expression() _ {e}) ** ",") ")" { Expression::Call { function: i, arguments: args } }
        / i:identifier() { Expression::Variable(i) }
        / l:literal() { Expression::Literal(l) }

    rule table_entry() -> (Option<TableKey>, Expression)
        = k:literal() _ ":" _ v:expression() { (Some(TableKey::Literal(k)), v) }
        / k:(k:identifier() _ ":" { TableKey::Literal(Literal::String(k)) })? _ v:expression() { (k, v) }
        // TODO: TableKey::Expression

    rule function() -> Function
        = _ "fn" _
        "(" params:((_ i:identifier() _ ":" _ t:type_() {(i, t)}) ** ",") ")" _
        "->" _
        rt:type_() _
        "{" _
        stmts:statements()
        _ "}" _
        { Function { parameters: params, return_type: rt, body: stmts } }

    rule identifier() -> String
        = quiet!{ !keyword() n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { n.to_owned() } }
        / expected!("identifier")

    rule keyword() -> ()
        = "fn" / "let"/ "if" / "else" / "while" / "return" / "true" / "false"

    rule literal() -> Literal
        = n:$(['0'..='9']+ ("." ['0'..='9']*)?) { Literal::Number(n.parse().unwrap()) }
        / "true" { Literal::Bool(true) }
        / "false" { Literal::Bool(false) }
        // / "&" i:identifier() { Expression::GlobalDataAddr(i) }

    rule type_() -> Type = precedence!{
        a:@ _ "|" _ b:(@) { Type::Union(vec![a, b]).normalize() }
        --
        // TODO: {, [str]: num}
        "{" _ cs:((_ t:type_table_entry() _ { t }) ** ",") ","? ts:((_ "[" k:$("num" / "str" / "bool") "]" _ ":" _ v:type_() _ { (k, v) }) ** ",") ","? _ "}"
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
            Type::Table(TypeTable { consts: tes, number, string, bool })
        }
        a:type_atom() { a }
    }

    rule type_table_entry() -> (Option<Literal>, Type)
        = k:(k:literal() _ ":" { k })? _ v:type_() { (k, v) }

    rule type_atom() -> Type
        = "num" { Type::Number }
        / "str" { Type::String }
        / "bool" { Type::Bool }
        / "()" { Type::Nil }
        / "unknown" { Type::Unknown }
        / l:literal() { Type::Const(l.to_const_data()) }

    rule _() =  quiet!{[' ' | '\t' | '\n']*}
});

#[test]
fn test_parser() {
    let programs = [
        r#"
let a: num = 1
let b: num = 1.23
fn main(a: num) {
}
main(2)
"#,
        r#"
fn main(a: num) -> num {
    let b: num = 1
    return a * 2 + b
}
"#,
        r#"
fn main(a: num) -> num {
    fn(a: num) -> num {
        return a * 2
    }
}
"#,
        r#"
fn main() -> {bool, bool, [str]: num} {
    return {
        a: 1,
        b: 2,
        true,
        false,
    }
}"#,
r#"
let a = {}[1][2]
"#,
    ];
    for program in programs.iter() {
        let defs = parser::program(program).unwrap();
        gilder::assert_golden!(format!("{:?}", defs));
    }
}
