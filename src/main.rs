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
    VariableIdentifier(String),
    TypeIdentifier(String),
    FunctionIdentifier(String),
    Integer(String), // TODO could turn into numeric types
    Decimal(String),
    StringLiteral(String),
    // Decomposable
    Constant(Box<AstNode>),
    PostfixExpression {
        operator: String,
        argument_list: Box<AstNode>,
    },
    StructFieldAccessExpression {
        field: String,
        subfields: Box<AstNode>,
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

pub struct Struct {
    name: String,
    fields: HashMap<String, String>,
}

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

pub fn parse_list(stmt: pest::iterators::Pairs<Rule>) -> Vec<Box<AstNode>> {
    return stmt
        .map(|x| {
            return Box::new(parse(x));
        })
        .collect::<Vec<Box<AstNode>>>();
}

pub fn ast_list_to_c(
    program_metadata: &Program,
    scope: &mut Scope,
    list: Vec<Box<AstNode>>,
) -> Vec<String> {
    return list
        .iter()
        .map(|node| -> String {
            // TODO get rid of clone...
            // TODO Assuming no list types have lists as elements... i think that's safe? but hacky
            // anyways.
            return ast_to_c(program_metadata, scope, node.clone()).join("");
        })
        .collect::<Vec<String>>();
}

pub fn ast_to_c(program_metadata: &Program, scope: &mut Scope, node: Box<AstNode>) -> Vec<String> {
    match *node {
        AstNode::Plus => return vec![String::from("+")],
        AstNode::Minus => return vec![String::from("-")],
        AstNode::Multiply => return vec![String::from("*")],
        AstNode::Divide => return vec![String::from("/")],
        // Non-decomposable
        AstNode::VariableIdentifier(s) => return vec![s],
        AstNode::TypeIdentifier(s) => return vec![s],
        AstNode::FunctionIdentifier(s) => return vec![s],
        AstNode::Integer(s) => return vec![s],
        AstNode::Decimal(s) => return vec![s],
        AstNode::StringLiteral(s) => return vec![s],

        // Decomposable
        AstNode::Constant(node) => return ast_to_c(program_metadata, scope, node),
        AstNode::PostfixExpression {
            operator,
            argument_list,
        } => {
            return vec![format!(
                "{:} ({:})",
                operator,
                ast_to_c(program_metadata, scope, argument_list).join(", ")
            )];
        }
        AstNode::StructFieldAccessExpression { field, subfields } => {
            return vec![format!(
                "{:}.{:}",
                field,
                ast_to_c(program_metadata, scope, subfields).join(".")
            )];
        }
        AstNode::InfixExpression {
            left,
            operator,
            right,
        } => {
            return vec![format!(
                "{:} {:} {:}",
                ast_to_c(program_metadata, scope, left).join(""),
                ast_to_c(program_metadata, scope, operator).join(""),
                ast_to_c(program_metadata, scope, right).join("")
            )];
        }
        // TODO this is going to be harder than anticipated, since BlockExpressions can return
        // stuff
        AstNode::BlockExpression(list) => {
            return vec![format!(
                "{{\n    {:};\n}}",
                ast_list_to_c(program_metadata, scope, list).join(";\n    ")
            )]
        }
        AstNode::ArgumentExpressionList(list) => {
            return ast_list_to_c(program_metadata, scope, list);
        }
        AstNode::VariableDeclaration { name, typ, value } => {
            return match *value {
                AstNode::ArgumentExpressionList(_) => vec![format!(
                    "{:} {:} = {{{:}}};",
                    typ.unwrap(),
                    name,
                    ast_to_c(program_metadata, scope, value).join(", ")
                )],
                _ => vec![format!(
                    "{:} {:} = {:};",
                    typ.unwrap(),
                    name,
                    ast_to_c(program_metadata, scope, value).join("")
                )],
            }
        }
        AstNode::TypedVariable { name, typ } => {
            return vec![format!("{:} {:}", typ, name)];
        }
        AstNode::TypedVariableList(list) => return ast_list_to_c(program_metadata, scope, list),
        AstNode::TypeDeclaration { name, fields } => {
            return vec![format!(
                "typedef struct {:} {{\n    {:}; \n}} {:};",
                name,
                ast_to_c(program_metadata, scope, fields).join(";\n    "),
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
            let output_type: String = program_metadata
                .base_module
                .functions
                .get(&name)
                .expect("Function not found")
                .output_type
                .clone();

            return vec![format!(
                "{:} {:} ({:}) {:};",
                output_type,
                name,
                ast_to_c(program_metadata, scope, input_parameters).join(", "),
                ast_to_c(program_metadata, scope, body).join("")
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
                AstNode::TypeIdentifier(s) => String::from(s),
                AstNode::TypedVariableList(list) => {
                    if list.is_empty() {
                        String::from("void")
                    } else {
                        String::from("AnonymousStruct") // TODO make_anon_struct(list),
                    }
                }
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
        AstNode::TypeDeclaration { name, fields } => {
            let fields = match *fields {
                AstNode::TypedVariableList(list) => {
                    let fields = list
                        .iter()
                        .map(|var| {
                            return match *var.clone() {
                                AstNode::TypedVariable { name, typ } => (name, typ),
                                _ => unreachable!(),
                            };
                        })
                        .collect::<Vec<(String, String)>>();
                    let mut field_map = HashMap::new();
                    for (name, typ) in fields {
                        let _ = field_map.insert(name, typ);
                    }
                    field_map
                }
                AstNode::TypeIdentifier(s) => match program_metadata.base_module.structs.get(&s) {
                    Some(t) => t.fields.clone(),
                    None => panic!(
                        "Cannot create type alias for type that is not defined: {:}",
                        s
                    ),
                },
                _ => unreachable!(),
            };

            let _ = program_metadata
                .base_module
                .structs
                .insert(name.clone(), Struct { name, fields });
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
        AstNode::VariableIdentifier(s) => match scope.vars.get(&s) {
            Some(var) => return var.typ.clone(),
            None => panic!("Variable is not defined: {}", s),
        },
        AstNode::Integer(_) => return String::from("int"),
        AstNode::Decimal(_) => return String::from("double"),
        AstNode::StringLiteral(_) => return String::from("char*"),
        AstNode::PostfixExpression {
            operator,
            argument_list,
        } => match program_metadata.base_module.functions.get(&operator) {
            Some(func) => return func.output_type.clone(),
            None => panic!("Function not found: {:}", operator),
        },
        AstNode::StructFieldAccessExpression { field, subfields } => {
            let struct_type = match scope.vars.get(&field) {
                Some(var) => var.typ.clone(),
                None => panic!("Variable is not defined: {}", field),
            };
            let struct_fields = program_metadata
                .base_module
                .structs
                .get(&struct_type)
                .expect("no such struct")
                .fields
                .clone();
            let mut struct_scope = Scope {
                vars: HashMap::new(),
            };
            for (name, typ) in struct_fields {
                println!("inserting {:}", name);
                struct_scope.vars.insert(
                    name.clone(),
                    Variable {
                        name: name,
                        typ: typ,
                        val: None,
                    },
                );
            }

            return resolve_expr_type(program_metadata, &mut struct_scope, subfields);
        }
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

            // TODO CLONE ugh
            for node in list.clone() {
                deduce_variable_types(program_metadata, &mut block_scope, node);
            }

            return resolve_expr_type(
                program_metadata,
                &mut block_scope,
                list.last().unwrap().clone(),
            );
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
        AstNode::FunctionIdentifier(_) => *node,
        AstNode::TypeIdentifier(_) => *node,
        AstNode::VariableIdentifier(s) => match scope.vars.get(&s) {
            Some(_) => AstNode::VariableIdentifier(s),
            None => panic!(format!("Variable is not defined: {:}", s)),
        },
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
        AstNode::StructFieldAccessExpression { field, subfields } => {
            AstNode::StructFieldAccessExpression { field, subfields }
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
            let type_string = match typ {
                Some(t) => {
                    match *value {
                        // Skip type deduction for AEL's
                        AstNode::ArgumentExpressionList(_) => t,
                        _ => {
                            // TODO for struct decls, resolve_expr_type is going to come back None... or some
                            // special value "anonstruct" as a hack?
                            let deduced_type =
                                Some(resolve_expr_type(program_metadata, scope, value.clone()));
                            assert_eq!(t,
                                        deduced_type.clone().unwrap(),
                                        "Type mismatch detected for variable: name: {:}, declared type: {:}, deduced type: {:}",
                                           name, t, deduced_type.unwrap()
                                   );
                            t
                        }
                    }
                }
                None => {
                    match *value {
                        // Skip type deduction for AEL's
                        AstNode::ArgumentExpressionList(_) =>
                            panic!("Type specifier must be included for struct assignment to variable: {:}", name),
                        _ => resolve_expr_type(program_metadata, scope, value.clone()),
                    }
                }
            };

            scope.vars.insert(
                name.clone(),
                Variable {
                    name: name.clone(),
                    typ: type_string.clone(),
                    val: None,
                },
            );
            return AstNode::VariableDeclaration {
                name,
                typ: Some(type_string),
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
            return AstNode::TypedVariableList(deduce_variable_types_list(
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
        Rule::VariableIdentifier => return AstNode::VariableIdentifier(String::from(stmt_str)),
        Rule::TypeIdentifier => return AstNode::TypeIdentifier(String::from(stmt_str)),
        Rule::FunctionIdentifier => return AstNode::FunctionIdentifier(String::from(stmt_str)),
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
        Rule::StructFieldAccessExpression => {
            return AstNode::StructFieldAccessExpression {
                field: String::from(inner.next().unwrap().as_str()),
                subfields: Box::new(parse(inner.next().unwrap())),
            }
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
        Rule::TypedVariableDeclaration => {
            return AstNode::VariableDeclaration {
                name: String::from(inner.next().unwrap().as_str()),
                typ: Some(String::from(inner.next().unwrap().as_str())),
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
    let unparsed_file = fs::read_to_string("codegen_test.cx").expect("cannot read file");
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

    let new_ast = ast
        .iter()
        .map(|node| {
            return Box::new(deduce_variable_types(
                &mut program_metadata,
                &mut scope,
                node.clone(),
            ));
        })
        .collect::<Vec<Box<AstNode>>>();

    println!("{:#?}", new_ast);

    let c_program = new_ast
        .iter()
        .map(|sub| {
            return ast_to_c(&program_metadata, &mut scope, sub.clone()).join("\n\n");
        })
        .collect::<Vec<String>>()
        .join("\n");

    fs::write(
        "main.c",
        format!("#include <stdio.h>\n#include <stdint.h>\n {:}", c_program),
    )
    .unwrap();

    use std::process::Command;
    let output = Command::new("gcc")
        .arg("main.c")
        .output()
        .expect("failed to execute process");
    println!("{:#?}", output);
}
