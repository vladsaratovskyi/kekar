use ast::Type;
use crate::ast;

pub fn parse_type(type_name: &str) -> Type {
    match type_name {
        "num" | "Num" => Type::Num,
        "string" | "String" => Type::String,
        "bool" | "Bool" => Type::Bool,
        "" => Type::None,
        s => Type::Identifier(s.to_string()),
    }
}
