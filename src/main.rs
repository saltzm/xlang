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

#[derive(Debug, Clone)]
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
        typ: Option<String>,
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
    EndOfInput,
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
                val: None,
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
    val: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    vars: HashMap<String, Variable>,
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

pub fn ast_list_to_c(scope: &mut Scope, list: Vec<Box<AstNode>>) -> Vec<String> {
    return list
        .iter()
        .map(|node| -> String {
            // TODO get rid of clone...
            // TODO Assuming no list types have lists as elements... i think that's safe? but hacky
            // anyways.
            return ast_to_c(scope, node.clone()).join("");
        })
        .collect::<Vec<String>>();
}

pub fn get_type() -> String {
    return String::from("ThisAintNoType");
}

pub fn ast_to_c(scope: &mut Scope, node: Box<AstNode>) -> Vec<String> {
    match *node {
        AstNode::Plus => return vec![String::from("+")],
        AstNode::Minus => return vec![String::from("-")],
        AstNode::Multiply => return vec![String::from("*")],
        AstNode::Divide => return vec![String::from("/")],
        // Non-decomposable
        AstNode::Identifier(s) => return vec![s],
        AstNode::Integer(s) => return vec![s],
        AstNode::Decimal(s) => return vec![s],
        AstNode::StringLiteral(s) => return vec![s],

        // Decomposable
        AstNode::Constant(node) => return ast_to_c(scope, node),
        AstNode::PostfixExpression {
            operator,
            argument_list,
        } => {
            return vec![format!(
                "{:} ({:})",
                operator,
                ast_to_c(scope, argument_list).join(", ")
            )];
        }
        AstNode::InfixExpression {
            left,
            operator,
            right,
        } => {
            return vec![format!(
                "{:} {:} {:}",
                ast_to_c(scope, left).join(""),
                ast_to_c(scope, operator).join(""),
                ast_to_c(scope, right).join("")
            )];
        }
        // TODO this is going to be harder than anticipated, since BlockExpressions can return
        // stuff
        AstNode::BlockExpression(list) => {
            return vec![format!(
                "{{\n    {:};\n}}",
                ast_list_to_c(scope, list).join(";\n    ")
            )]
        }
        AstNode::ArgumentExpressionList(list) => {
            return ast_list_to_c(scope, list);
        }
        AstNode::VariableDeclaration { name, typ, value } => {
            return vec![format!(
                "{:} {:} = {:};",
                typ.unwrap(),
                name,
                ast_to_c(scope, value).join("")
            )]
        }
        AstNode::TypedVariable { name, typ } => {
            return vec![format!("{:} {:}", typ, name)];
        }
        AstNode::TypedVariableList(list) => return ast_list_to_c(scope, list),
        AstNode::TypeDeclaration { name, fields } => {
            return vec![format!(
                "typedef struct {:} {{\n    {:}; \n}} {:};",
                name,
                ast_to_c(scope, fields).join(";\n    "),
                name
            )];
        }
        AstNode::FunctionDeclaration {
            name,
            input_parameters,
            output_parameters,
            body,
        } => {
            // TODO no me gusta
            let output_type: String = match *output_parameters {
                AstNode::Identifier(s) => String::from(s),
                AstNode::TypedVariableList(list) => String::from("AnonymousStruct"), // TODO make_anon_struct(list),
                _ => unreachable!(),
            };

//            let mut func_scope = scope.clone();
//            match *input_parameters.clone() {
//                // TODO
//                AstNode::TypedVariableList(list) => {
//                    for var in list {
//                        match *var {
//                            AstNode::TypedVariable { name, typ } => {
//                                func_scope.vars.insert(
//                                    name.clone(),
//                                    Variable {
//                                        name: name.clone(),
//                                        typ: typ.clone(),
//                                        val: None,
//                                    },
//                                );
//                            }
//                            _ => unreachable!(),
//                        }
//                    }
//                }
//                _ => unreachable!(),
//            };

            return vec![format!(
                "{:} {:} ({:}) {:};",
                output_type,
                name,
                ast_to_c(scope, input_parameters).join(", "),
                ast_to_c(scope, body).join("")
            )];
        }
        AstNode::EndOfInput => {
            return vec![String::from("")];
        }
    }
}

pub fn build_program_metadata(program_metadata: &mut Program, node: Box<AstNode>) {
    match *node {
        AstNode::FunctionDeclaration {
            name,
            input_parameters,
            output_parameters,
            body,
        } => {
            let input_parameters = match *input_parameters {
                AstNode::TypedVariableList(list) => list
                    .iter()
                    .map(|var| {
                        return match *var.clone() {
                            AstNode::TypedVariable { name, typ } => (name, typ),
                            _ => unreachable!(),
                        };
                    })
                    .collect::<Vec<(String, String)>>(),
                _ => unreachable!(),
            };

            let output_type: String = match *output_parameters {
                AstNode::Identifier(s) => String::from(s),
                AstNode::TypedVariableList(list) => String::from("AnonymousStruct"), // TODO make_anon_struct(list),
                _ => unreachable!(),
            };

            let _ = program_metadata.base_module.functions.insert(
                name.clone(),
                Function {
                    name,
                    input_parameters,
                    output_type,
                },
            );
        }
        _ => (),
    }
}

pub fn resolve_expr_type(
    program_metadata: &mut Program,
    scope: &mut Scope,
    node: Box<AstNode>,
) -> String {
    match *node {
        AstNode::Identifier(s) => {
            return scope.vars.get(&s).expect("Variable is not defined").typ.clone();
        }
        AstNode::Integer(s) => return String::from("int"),
        AstNode::Decimal(s) => return String::from("double"),
        AstNode::StringLiteral(s) => return String::from("char*"),
        AstNode::PostfixExpression {
            operator,
            argument_list,
        } => match program_metadata.base_module.functions.get(&operator) {
            Some(func) => return func.output_type.clone(),
            None => panic!("Function not found: {:}", operator),
        },
        AstNode::InfixExpression {
            left,
            operator,
            right,
        } => {
            let left_type = resolve_expr_type(program_metadata, scope, left);
            let right_type = resolve_expr_type(program_metadata, scope, right);
            // TODO this is all jank
            if left_type != "int" && left_type != "double" {
                panic!("Operator + not supported for type: {:}", left_type)
            }
            if right_type != "int" && right_type != "double" {
                panic!("Operator + not supported for type: {:}", left_type)
            }
            if left_type == "int" && right_type == "int" {
                return String::from("int");
            } else {
                return String::from("double");
            }
        }
        AstNode::BlockExpression(list) => {
            let mut block_scope = scope.clone();

            for node in list.clone() {
                // TODO CLONE ugh
                deduce_variable_types(program_metadata, &mut block_scope, node);
            }

            return resolve_expr_type(program_metadata, &mut block_scope, list.last().unwrap().clone());
        }
        _ => unreachable!(),
    }
}

pub fn deduce_variable_types_list(
    program_metadata: &mut Program,
    scope: &mut Scope,
    list: Vec<Box<AstNode>>,
) -> Vec<Box<AstNode>> {
    return list
        .iter()
        .map(|x| {
            return Box::new(deduce_variable_types(program_metadata, scope, x.clone()));
        })
        .collect::<Vec<Box<AstNode>>>();
}

pub fn deduce_variable_types(
    program_metadata: &mut Program,
    scope: &mut Scope,
    node: Box<AstNode>,
) -> AstNode {
    match *node {
        AstNode::Plus => *node,
        AstNode::Minus => *node,
        AstNode::Multiply => *node,
        AstNode::Divide => *node,
        // Non-decomposable
        AstNode::Identifier(_) => *node,
        AstNode::Integer(_) => *node,
        AstNode::Decimal(_) => *node,
        AstNode::StringLiteral(_) => *node,
        AstNode::Constant(_) => *node,
        AstNode::PostfixExpression {
            operator,
            argument_list,
        } => {
            return AstNode::PostfixExpression {
                operator,
                argument_list: Box::new(deduce_variable_types(
                    program_metadata,
                    scope,
                    argument_list,
                )),
            }
        }
        AstNode::InfixExpression {
            left,
            operator,
            right,
        } => {
            return AstNode::InfixExpression {
                left: Box::new(deduce_variable_types(program_metadata, scope, left)),
                operator,
                right: Box::new(deduce_variable_types(program_metadata, scope, right)),
            }
        }
        AstNode::BlockExpression(list) => {
            let mut block_scope: Scope = scope.clone();
            return AstNode::BlockExpression(deduce_variable_types_list(
                program_metadata,
                &mut block_scope,
                list,
            ));
        }
        AstNode::ArgumentExpressionList(list) => {
            return AstNode::ArgumentExpressionList(deduce_variable_types_list(
                program_metadata,
                scope,
                list,
            ));
        }
        AstNode::VariableDeclaration { name, typ, value } => {
            let new_typ = Some(resolve_expr_type(program_metadata, scope, value.clone()));
            scope.vars.insert(
                name.clone(),
                Variable {
                    name: name.clone(),
                    typ: new_typ.clone().unwrap(),
                    val: None,
                },
            );
            return AstNode::VariableDeclaration {
                name,
                typ: new_typ,
                value: Box::new(deduce_variable_types(program_metadata, scope, value)),
            };
        }
        AstNode::TypedVariable { name, typ } => {
            scope.vars.insert(
                name.clone(),
                Variable {
                    name: name.clone(),
                    typ: typ.clone(),
                    val: None,
                },
            );
            return AstNode::TypedVariable { name, typ };
        }
        AstNode::TypedVariableList(list) => {
            return AstNode::ArgumentExpressionList(deduce_variable_types_list(
                program_metadata,
                scope,
                list,
            ));
        }
        AstNode::TypeDeclaration { name, fields } => {
            return AstNode::TypeDeclaration { name, fields }
        }
        AstNode::FunctionDeclaration {
            name,
            input_parameters,
            output_parameters,
            body,
        } => {
            let mut func_scope = scope.clone();
            return AstNode::FunctionDeclaration {
                name: name,
                input_parameters: Box::new(deduce_variable_types(
                    program_metadata,
                    &mut func_scope,
                    input_parameters,
                )),
                output_parameters: output_parameters,
                body: Box::new(deduce_variable_types(
                    program_metadata,
                    &mut func_scope,
                    body,
                )),
            };
        }
        AstNode::EndOfInput => {
            return AstNode::EndOfInput;
        }
    }
}

// TODO handle not-top-level fns
//pub fn resolve_anonymous_struct(node: Box<AstNode>) -> Option<Box<AstNode>> {
//    match *node {
//        AstNode::FunctionDeclaration {
//            name,
//            input_parameters,
//            output_parameters,
//            body,
//        } => {
//            // TODO no me gusta
//            return match *output_parameters {
//                AstNode::Identifier(s) => None,
//                AstNode::TypedVariableList(list) => {
//                    return Some(Box::new(AstNode::TypeDeclaration {
//                        name: String::from("anon"),
//                        fields: Box::new(AstNode::TypedVariableList(list)),
//                    }))
//                }
//                _ => unreachable!(),
//            };
//        }
//        _ => None,
//    }
//}

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
                typ: None,
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
        Rule::EOI => return AstNode::EndOfInput, // TODO
        // Silent rules
        Rule::COMMENT => unreachable!(),
        Rule::WHITESPACE => unreachable!(),
        Rule::Expression => unreachable!(),
        Rule::Statement => unreachable!(),
        Rule::Program => unreachable!(),
    }
}

fn main() {
    let unparsed_file = fs::read_to_string("test.cx").expect("cannot read file");
    let file = CxParser::parse(Rule::Program, &unparsed_file).expect("unsuccessful parse");

    let mut program_metadata = Program {
        base_module: Module {
            next_anon_struct_id: 0,
            structs: HashMap::new(),
            functions: HashMap::new(),
            enums: HashMap::new(),
        },
        modules: HashMap::new(),
        entry_points: Vec::<String>::new(),
    };

    let ast = parse_list(file);
//    println!("{:#?}", ast);

    for sub in ast.clone() {
        build_program_metadata(&mut program_metadata, sub);
    }

    let mut scope = Scope {
        vars: HashMap::new(),
    };

    let new_ast = ast.iter().map(|node| {
        return Box::new(deduce_variable_types(&mut program_metadata, &mut scope, node.clone()));
    }).collect::<Vec<Box<AstNode>>>();

    println!("{:#?}", new_ast);

    //    let mut ast_with_anon_structs = ast.clone();
    //    for sub in ast {
    //        match resolve_anonymous_struct(sub) {
    //            Some(s) => ast_with_anon_structs.push(s),
    //            None => ()
    //        }
    //    }

    for sub in new_ast {
        println!("{:}", ast_to_c(&mut scope, sub).join("\n"));
    }
}
