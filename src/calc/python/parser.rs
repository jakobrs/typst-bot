use thiserror::Error;

use crate::calc::{ArithOp, ExpressionTree, Operator};

use super::lexer::Token;

#[derive(Error, Debug)]
pub enum PythonParserError {
    #[error("Expected {expected}, found {got}")]
    Expected { expected: Token, got: Token },
    #[error("Expected {expected}, found end of file")]
    UnexpectedEof { expected: Token },
    #[error("Expected (something, I haven't bothered tracking this yet), found end of file")]
    UnexpectedEofUnknown,
    #[error("Expected (something), found {got}")]
    Unexpected { got: Token },
    #[error("Didn't really expect anything, got {got} instead")]
    ExpectedEof { got: Token },
}

struct ParserContext {
    left: Vec<Token>,
    pos: usize,
}

impl ParserContext {
    fn new(mut tokens: Vec<Token>) -> Self {
        tokens.reverse();
        Self {
            left: tokens,
            pos: 0,
        }
    }

    fn advance(&mut self) {
        self.left.pop();
        self.pos += 1;
    }

    fn peek(&mut self) -> Option<&Token> {
        self.left.last()
    }

    fn next(&mut self) -> Option<Token> {
        self.left.pop().and_then(|token| {
            self.pos += 1;
            Some(token)
        })
    }

    fn expect(&mut self, token: Token) -> Result<()> {
        match self.next() {
            Some(token1) if token1 == token => Ok(()),
            Some(token1) => Err(PythonParserError::Expected {
                expected: token,
                got: token1,
            }),
            None => Err(PythonParserError::UnexpectedEof { expected: token }),
        }
    }
}

type Result<T, E = PythonParserError> = std::result::Result<T, E>;

pub fn parse(tokens: impl Into<Vec<Token>>) -> Result<ExpressionTree> {
    fn parse_unit(ctx: &mut ParserContext) -> Result<ExpressionTree> {
        match ctx.next() {
            Some(Token::LeftParen) => {
                let unit = parse_expression(ctx)?;
                ctx.expect(Token::RightParen)?;
                Ok(unit)
            }
            Some(Token::Global(name)) => {
                if matches!(ctx.peek(), Some(Token::LeftParen)) {
                    ctx.advance();
                    // Function application
                    let mut args = vec![];

                    loop {
                        if matches!(ctx.peek(), Some(Token::RightParen)) {
                            ctx.advance();
                            break;
                        }

                        args.push(parse_expression(ctx)?);

                        if matches!(ctx.peek(), Some(Token::Comma)) {
                            ctx.advance();
                        }
                        if matches!(ctx.peek(), Some(Token::RightParen)) {
                            ctx.advance();
                            break;
                        }
                    }

                    Ok(ExpressionTree::Apply(Operator::Named(name), args))
                } else {
                    Ok(ExpressionTree::Constant(name))
                }
            }
            Some(Token::Lit(value)) => Ok(ExpressionTree::Value(value)),
            Some(token) => Err(PythonParserError::Unexpected { got: token }),
            None => Err(PythonParserError::UnexpectedEofUnknown),
        }
    }

    fn parse_factor(ctx: &mut ParserContext) -> Result<ExpressionTree> {
        let mut base = parse_unit(ctx)?;

        while matches!(ctx.peek(), Some(Token::ArithOp(ArithOp::Pow))) {
            ctx.advance();

            let exp = parse_unit(ctx)?;

            base = ExpressionTree::Apply(Operator::ArithOp(ArithOp::Pow), vec![base, exp]);
        }

        Ok(base)
    }

    fn parse_term(ctx: &mut ParserContext) -> Result<ExpressionTree> {
        let mut sign = false;
        loop {
            match ctx.peek() {
                Some(Token::ArithOp(ArithOp::Plus)) => ctx.advance(),
                Some(Token::ArithOp(ArithOp::Minus)) => {
                    ctx.advance();
                    sign = !sign;
                }
                _ => break,
            }
        }

        let mut factor = parse_factor(ctx)?;

        while matches!(
            ctx.peek(),
            Some(Token::ArithOp(
                ArithOp::Mul | ArithOp::Div | ArithOp::FloorDiv
            ))
        ) {
            let Some(Token::ArithOp(op)) = ctx.next() else { unreachable!() };

            let next_factor = parse_factor(ctx)?;

            factor = ExpressionTree::Apply(Operator::ArithOp(op), vec![factor, next_factor]);
        }

        Ok(if sign {
            ExpressionTree::Apply(Operator::Named("negate".to_string()), vec![factor])
        } else {
            factor
        })
    }

    fn parse_expression(ctx: &mut ParserContext) -> Result<ExpressionTree> {
        let mut term = parse_term(ctx)?;

        while matches!(
            ctx.peek(),
            Some(Token::ArithOp(ArithOp::Plus | ArithOp::Minus))
        ) {
            let Some(Token::ArithOp(op)) = ctx.next() else { unreachable!() };

            let next_term = parse_term(ctx)?;

            term = ExpressionTree::Apply(Operator::ArithOp(op), vec![term, next_term]);
        }

        Ok(term)
    }

    let mut ctx = ParserContext::new(tokens.into());
    let expr = parse_expression(&mut ctx)?;
    match ctx.peek() {
        None => Ok(expr),
        Some(token) => Err(PythonParserError::ExpectedEof { got: token.clone() }),
    }
}
