use crate::ast::*;
use crate::r#type::Type;

// https://docs.rs/peg/latest/peg/
peg::parser!(pub grammar parser() for str {
    pub rule program() -> Vec<Definition>
        = _ defs:(definition() ** _) _ { defs }

    rule definition() -> Definition
        = def_function()
        / def_var()

    rule def_function() -> Definition
        = _ "fn" _ name:identifier() _
        "(" params:((_ i:identifier() _ ":" _ t:type_() {(i, t)}) ** ",") ")" _
        "->" _
        rt:type_() _
        "{" _
        stmts:statements()
        _ "}" _
        { Definition::Function {name, function: Function { parameters: params, return_type: rt, body: stmts } } }

    rule def_var() -> Definition
        = _ "let" _ name:identifier() _ ":" _ t:type_() _ "=" _ e:expression() _
        { Definition::Variable(Variable { name, type_: t, expr: e }) }

    rule statements() -> Vec<Statement>
        = s:(statement()*) { s }

    rule statement() -> Statement
        = _ "return" _ e:expression() _ { Statement::Return(Some(e)) }
        / _ "let" _ name:identifier() _ ":" _ t:type_() _ "=" _ e:expression() _
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
        u:unary_op() { u }
    }

    rule unary_op() -> Expression
        = "(" _ e:expression() _ ")" { e }
        / "{" _ ts:((_ t:table_entry() _ { t }) ** ",") ","? _ "}" { Expression::Table(ts) }
        / "!" _ e:expression() { Expression::LogicalNot(Box::new(e)) }
        / f:function() { Expression::Fn(f) }
        / i:identifier() _ "(" args:((_ e:expression() _ {e}) ** ",") ")" { Expression::Call { function: i, arguments: args } }
        / i:identifier() { Expression::Variable(i) }
        / l:literal() { l }

    rule table_entry() -> TableEntry
        = k:identifier() _ ":" _ v:expression() { TableEntry::Field(k, v) }
        / v:expression() { TableEntry::Value(v) }

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

    rule literal() -> Expression
        = n:$(['0'..='9']+) { Expression::Literal(Literal::Number(n.parse().unwrap())) }
        / "true" { Expression::Literal(Literal::Bool(true)) }
        / "false" { Expression::Literal(Literal::Bool(false)) }
        // / "&" i:identifier() { Expression::GlobalDataAddr(i) }

    rule type_() -> Type = precedence!{
        a:@ _ "|" _ b:(@) { Type::Union(vec![a, b]) }
        --
        a:type_atom() { a }
    }

    rule type_atom() -> Type
        = "num" { Type::Number }
        / "str" { Type::String }
        / "bool" { Type::Bool }
        / "table" { Type::Table }
        / "()" { Type::Nil }
        / "unknown" { Type::Unknown }
        / "(" _ t:type_() _ "," _ ts:(type_() ** ",") _ ")" { Type::Tuple(vec![vec![t], ts].concat()) }
        / "[" _ t:type_() _ "]" { Type::Array(Box::new(t)) }

    rule _() =  quiet!{[' ' | '\t' | '\n']*}
});

#[test]
fn test_parser() {
    let programs = [
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
fn main() -> table {
    return {
        a: 1,
        b: 2,
    }
}"#,
    ];
    for program in programs.iter() {
        let defs = parser::program(program).unwrap();
        gilder::assert_golden!(format!("{:?}", defs));
    }
}
