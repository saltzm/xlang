extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use std::collections::HashMap;
use std::fs;
use std::vec::Vec;

#[derive(Parser)]
#[grammar = "cx.pest"]
pub struct CxParser;

#[derive(Debug)]
pub enum InfixOperator {}

#[derive(Debug)]
pub enum AstNode {
    Plus,
    Minus,
    Multiply,
    Divide,
    // Non-decomposable
    Identifier(String),
    Integer(String), // TODO could turn into numeric types
    Decimal(String),
    StringLiteral(String),
    // Decomposable
    Constant(Box<AstNode>),
    PostfixExpression {
        operator: String,
        argument_list: Box<AstNode>,
    },
    InfixExpression {
        left: Box<AstNode>,
        operator: Box<AstNode>,
        right: Box<AstNode>,
    },
    BlockExpression(Vec<Box<AstNode>>),
    ArgumentExpressionList(Vec<Box<AstNode>>),
    VariableDeclaration {
        name: String,
        value: Box<AstNode>,
    },
    TypedVariable {
        name: String,
        typ: String,
    },
    TypedVariableList(Vec<Box<AstNode>>),
    TypeDeclaration {
        name: String,
        fields: Box<AstNode>,
    },
    FunctionDeclaration {
        name: String,
        input_parameters: Box<AstNode>,
        output_parameters: Box<AstNode>,
        body: Box<AstNode>,
    },
    EndOfInput
}

pub struct Enum {}

pub struct Struct {}

pub struct Function {
    name: String,
    input_parameters: Vec<(String, String)>,
    output_type: String,
}

pub struct Module {
    next_anon_struct_id: u64,
    structs: HashMap<String, Struct>,
    functions: HashMap<String, Function>,
    enums: HashMap<String, Enum>,
    //    constants: HashMap<String, VariableAssignment>
}

pub struct Program {
    base_module: Module,
    modules: HashMap<String, Module>,
    entry_points: Vec<String>,
}

pub fn parse_typelist(parsed_typelist: pest::iterators::Pair<Rule>) -> Vec<(String, String)> {
    let mut typelist = Vec::new();
    for typedvar in parsed_typelist.into_inner() {
        match typedvar.as_rule() {
            Rule::TypedVariableList => {
                let mut typedvar_iter = typedvar.into_inner();
                let field_name_str = typedvar_iter.next().unwrap().as_str();
                let field_type_str = typedvar_iter.next().unwrap().as_str();
                typelist.push((String::from(field_name_str), String::from(field_type_str)));
                // println!("\t{:} {:},", field_type_str, field_name_str);
            }
            _ => unreachable!(),
        }
    }
    return typelist;
}

pub fn print_struct(struct_name: &str, fields: String) {
    println!(
        "typedef struct {:} {{\n\t{:};\n}} {:};",
        struct_name, fields, struct_name
    );
}

pub fn parse_namedtypelist(stmt: pest::iterators::Pair<Rule>) {
    let mut parsed_struct = stmt.into_inner();
    let struct_name = parsed_struct.next().unwrap().as_str();
    let parsed_fields = parsed_struct.next().unwrap();
    let fields = parse_typelist(parsed_fields);
    let fields_string = fields
        .iter()
        .map(|(a, b)| -> String {
            return format!("{:} {:}", b, a);
        })
        .collect::<Vec<String>>()
        .join(";\n\t");

    print_struct(struct_name, fields_string);
}

pub fn make_anon_struct(field_tokens: pest::iterators::Pair<Rule>) -> String {
    let fields = parse_typelist(field_tokens);
    let fields_string = fields
        .iter()
        .map(|(a, b)| -> String {
            return format!("{:} {:}", b, a);
        })
        .collect::<Vec<String>>()
        .join(";\n\t");
    print_struct("anonstruct", fields_string);
    return String::from("anonstruct");
}

pub fn parse_function(scope: &mut Scope, stmt: pest::iterators::Pair<Rule>) -> Function {
    let mut parsed_func = stmt.into_inner();
    let fn_name = parsed_func.next().unwrap().as_str();
    let parsed_input_params = parsed_func.next().unwrap();
    let input_args = parse_typelist(parsed_input_params);

    let output = parsed_func.next().unwrap();
    let output_type: String = match output.as_rule() {
        Rule::Identifier => String::from(output.as_str()),
        Rule::TypedVariableList => make_anon_struct(output),
        _ => unreachable!("{:}", output.as_str()),
    };

    let input_arg_string = input_args
        .iter()
        .map(|(a, b)| -> String {
            return format!("{:} {:}", b, a);
        })
        .collect::<Vec<String>>()
        .join(", ");

    println!(
        "{:} {:}({:}) {{}}\n",
        output_type, fn_name, input_arg_string
    );

    let mut func_scope = scope.clone();

    for (name, typ) in input_args.iter() {
        func_scope.vars.insert(
            name.clone(),
            Variable {
                name: name.clone(),
                typ: typ.clone(),
                val: name.clone(),
            },
        );
    }

    //parse_block(&func_scope, parsed_func.next().unwrap().into_inner());

    return Function {
        name: String::from(fn_name),
        input_parameters: input_args,
        output_type: String::from(output_type),
    };
}

#[derive(Debug, Clone)]
pub struct Variable {
    name: String,
    typ: String,
    val: String,
}

#[derive(Debug, Clone)]
pub struct Scope {
    vars: HashMap<String, Variable>,
}

//impl Clone for Scope {
//    fn clone(self: &Scope) -> Scope {
//        return Scope { vars: self.vars.clone() };
//    }
//}

pub struct PrefixOp {
    opname: String,
    input_arguments: Vec<(String, String)>,
}

//pub fn parse_valexpr(scope: &Scope, mut valexpr: pest::iterators::Pairs<Rule>) -> String {
//    println!("found variable assign valexpr: {:?}", valexpr.as_str());
//    return String::from("todovalexpr");
//}
//
//pub fn parse_atom(scope: &Scope, mut atom: pest::iterators::Pairs<Rule>) -> String {
//    println!("found variable assign atom: {:?}", atom.as_str());
//    let parsed_atom = atom.next().unwrap();
//    let parsed_atom_rule = parsed_atom.as_rule();
//    let atom_contents = parsed_atom.into_inner().next().unwrap();
//    return match parsed_atom_rule {
//        Rule::Constant => match atom_contents.as_rule() {
//            Rule::Integer => String::from("int"),
//            Rule::Decimal => String::from("double"),
//            Rule::StringLiteral => String::from("char*"),
//            _ => unreachable!(),
//        },
//            Rule::Identifier => match scope.vars.get(atom_contents.as_str()) {
//                Some(var) => var.typ.clone(),
//                None => panic!("Variable {:} not yet defined!!!!!", atom_contents.as_str()),
//            },
//        _ => unreachable!(),
//    };
//    //return String::new();
//}
//
//pub fn parse_assignment(
//    scope: &mut Scope,
//    mut assignexpr: pest::iterators::Pairs<Rule>,
//) -> Variable {
//    println!("found assignexpr: {:?}", assignexpr.as_str());
//    let name = String::from(assignexpr.next().unwrap().as_str());
//    let parsed_val = assignexpr.next().unwrap();
//    let val = String::from(parsed_val.as_str());
//    let typ = match parsed_val.as_rule() {
//        Rule::Expression => parse_valexpr(&scope, parsed_val.into_inner()),
//        Rule::PrimaryExpression => parse_atom(&scope, parsed_val.into_inner()),
//        _ => unreachable!(),
//    };
//    return Variable { name, typ, val };
//}
//
//pub fn parse_expr(scope: &mut Scope, expr: pest::iterators::Pair<Rule>) {
//    match expr.as_rule() {
//        Rule::=> println!("found valexpr: {:?}", expr.as_str()),
//        Rule::assignexpr => {
//            let var = parse_assignment(scope, expr.into_inner());
//            scope.vars.insert(var.name.clone(), var);
//        }
//        _ => unreachable!(),
//    }
//}
//
//pub fn parse_block(scope: &Scope, block: pest::iterators::Pairs<Rule>) {
//    let mut subscope: Scope = scope.clone();
//    for expr in block {
//        match expr.as_rule() {
//            Rule::expr => {
//                println!("found expr in block : {:?}", expr.as_str());
//                parse_expr(&mut subscope, expr.into_inner().next().unwrap());
//                println!("scope vars : {:?}", scope.vars);
//            }
//            Rule::blockexpr => {
//                parse_block(&mut subscope, expr.into_inner());
//            }
//            _ => unreachable!(),
//        }
//    }
//}

//pub fn parse_statement(scope: &mut Scope, stmt: pest::iterators::Pair<Rule>) {
//    // println!("found stmt: {:?}", stmt.as_str());
//    match stmt.as_rule() {
//        Rule::expr => {
//            println!("found expr: {:?}", stmt.as_str());
//            parse_expr(scope, stmt.into_inner().next().unwrap())
//        }
//        Rule::blockexpr => {
//            println!("found block: {:?}", stmt.as_str());
//            parse_block(&scope, stmt.into_inner());
//        }
//        Rule::namedtypelist => parse_namedtypelist(stmt),
//        Rule::typealias => println!("found typealias: {:?}", stmt.as_str()),
//        Rule::ctypealias => println!("found ctypealias: {:?}", stmt.as_str()),
//        Rule::cfunction => {}
//        Rule::function => {
//            println!("found function: {:?}", stmt.as_str());
//            let _new_fn = parse_function(scope, stmt);
//            //            program.base_module.functions.insert(new_fn.name.clone(), new_fn);
//        }
//        Rule::COMMENT=> {
//            println!("found comment: {:?}", stmt.as_str());
//        }
//        Rule::EOI => {
//            println!("found EOI");
//        }
//        _ => unreachable!(),
//    }
//}
pub fn parse_list(stmt: pest::iterators::Pairs<Rule>) -> Vec<Box<AstNode>> {
    return stmt
        .map(|x| {
            return Box::new(parse(x));
        })
        .collect::<Vec<Box<AstNode>>>();
}

pub fn parse(stmt: pest::iterators::Pair<Rule>) -> AstNode {
    //println!("found stmt: {:?}", stmt);
    let rule = stmt.as_rule();
    let stmt_str = stmt.as_str();
    let mut inner = stmt.into_inner();
    match rule {
        Rule::Plus => return AstNode::Plus,
        Rule::Minus => return AstNode::Minus,
        Rule::Multiply => return AstNode::Multiply,
        Rule::Divide => return AstNode::Divide,
        Rule::InfixOperator => return parse(inner.next().unwrap()),
        Rule::Identifier => return AstNode::Identifier(String::from(stmt_str)),
        Rule::Integer => return AstNode::Integer(String::from(stmt_str)),
        Rule::Decimal => return AstNode::Decimal(String::from(stmt_str)),
        Rule::StringLiteral => return AstNode::StringLiteral(String::from(stmt_str)),
        Rule::Constant => return parse(inner.next().unwrap()),
        Rule::PrimaryExpression => return parse(inner.next().unwrap()),
        Rule::PostfixExpression => {
            return AstNode::PostfixExpression {
                operator: String::from(inner.next().unwrap().as_str()),
                argument_list: Box::new(parse(inner.next().unwrap())),
            };
        }
        Rule::InfixExpression => {
            return AstNode::InfixExpression {
                left: Box::new(parse(inner.next().unwrap())),
                operator: Box::new(parse(inner.next().unwrap())),
                right: Box::new(parse(inner.next().unwrap())),
            };
        }
        Rule::BlockExpression => return AstNode::BlockExpression(parse_list(inner)),
        Rule::ArgumentExpressionList => return AstNode::ArgumentExpressionList(parse_list(inner)),
        Rule::VariableDeclaration => {
            return AstNode::VariableDeclaration {
                name: String::from(inner.next().unwrap().as_str()),
                value: Box::new(parse(inner.next().unwrap())),
            };
        }
        Rule::TypedVariable => {
            return AstNode::TypedVariable {
                name: String::from(inner.next().unwrap().as_str()),
                typ: String::from(inner.next().unwrap().as_str()),
            };
        }
        Rule::TypedVariableList => return AstNode::TypedVariableList(parse_list(inner)),
        Rule::TypeDeclaration => {
            return AstNode::TypeDeclaration {
                name: String::from(inner.next().unwrap().as_str()),
                fields: Box::new(parse(inner.next().unwrap())),
            };
        }
        Rule::FunctionDeclaration => {
            return AstNode::FunctionDeclaration {
                name: String::from(inner.next().unwrap().as_str()),
                input_parameters: Box::new(parse(inner.next().unwrap())),
                output_parameters: Box::new(parse(inner.next().unwrap())),
                body: Box::new(parse(inner.next().unwrap())),
            };
        }
        Rule::EOI => return AstNode::EndOfInput,  // TODO
        Rule::COMMENT => unreachable!(),
        // Silent rules
        Rule::WHITESPACE => unreachable!(),
        Rule::Expression => unreachable!(),
        Rule::Statement => unreachable!(),
        Rule::Program => unreachable!(),
    }
}

pub fn print_ast_node() {}

fn main() {
    let unparsed_file = fs::read_to_string("test.cx").expect("cannot read file");
    let file = CxParser::parse(Rule::Program, &unparsed_file).expect("unsuccessful parse"); // unwrap the parse result

    let mut _program = Program {
        base_module: Module {
            next_anon_struct_id: 0,
            structs: HashMap::new(),
            functions: HashMap::new(),
            enums: HashMap::new(),
        },
        modules: HashMap::new(),
        entry_points: Vec::<String>::new(),
    };

    let mut scope = Scope {
        vars: HashMap::new(),
    };

    let ast = file
        .map(|stmt| {
            return parse(stmt);
        })
        .collect::<Vec<AstNode>>();

    println!("{:#?}", ast);

    //   let successful_parse = CxParser::parse(Rule::typelist, "(a: U64, b: String)");

    //   let unsuccessful_parse = CxParser::parse(Rule::function, "fn Add = (a: u64, b: u64) -> u64 { a + b }");
    //   println!("{:?}", unsuccessful_parse);
}
