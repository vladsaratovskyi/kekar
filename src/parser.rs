use crate::lexer::Token;


trait Expression {
    
}

struct Binary {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
    operator: Token
}

struct Unary {
    operand: Box<dyn Expression>,
    operator: Token
}

impl Expression for Binary {

}

impl Expression for Unary {

}

pub struct Parser {
}

impl Parser {
    pub fn new(&self) -> Parser {
        Parser {
            
        }
    }

    pub fn parse(&self, tokens: Vec<Token>) {

    }
}