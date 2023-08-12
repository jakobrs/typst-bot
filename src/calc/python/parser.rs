use crate::calc::{ArithOp, ExpressionTree, Operator};

use super::lexer::Token;

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

    fn is_finished(&self) -> bool {
        self.left.is_empty()
    }

    fn advance(&mut self) {
        self.left.pop();
        self.pos += 1;
    }

    fn peek(&mut self) -> Option<&Token> {
        self.left.last()
    }

    fn next(&mut self) -> Token {
        let token = self.left.pop();
        self.pos += 1;
        token.unwrap()
    }

    fn expect(&mut self, token: Token) {
        assert!(self.next() == token);
    }
}

pub fn parse(tokens: impl Into<Vec<Token>>) -> ExpressionTree {
    fn parse_unit(ctx: &mut ParserContext) -> ExpressionTree {
        match ctx.next() {
            Token::LeftParen => {
                let unit = parse_expression(ctx);
                ctx.expect(Token::RightParen);
                unit
            }
            Token::Global(name) => {
                if matches!(ctx.peek(), Some(Token::LeftParen)) {
                    ctx.advance();
                    // Function application
                    let mut args = vec![];

                    loop {
                        if matches!(ctx.peek(), Some(Token::RightParen)) {
                            ctx.advance();
                            break;
                        }

                        args.push(parse_expression(ctx));

                        if matches!(ctx.peek(), Some(Token::Comma)) {
                            ctx.advance();
                        }
                        if matches!(ctx.peek(), Some(Token::RightParen)) {
                            ctx.advance();
                            break;
                        }
                    }

                    ExpressionTree::Apply(Operator::Named(name), args)
                } else {
                    ExpressionTree::Constant(name)
                }
            }
            Token::Lit(value) => ExpressionTree::Value(value),
            token => panic!("Unexpected token {token:?}"),
        }
    }

    fn parse_factor(ctx: &mut ParserContext) -> ExpressionTree {
        let mut base = parse_unit(ctx);

        while matches!(ctx.peek(), Some(Token::ArithOp(ArithOp::Pow))) {
            ctx.advance();

            let exp = parse_unit(ctx);

            base = ExpressionTree::Apply(Operator::ArithOp(ArithOp::Pow), vec![base, exp]);
        }

        base
    }

    fn parse_term(ctx: &mut ParserContext) -> ExpressionTree {
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

        let mut factor = parse_factor(ctx);

        while matches!(
            ctx.peek(),
            Some(Token::ArithOp(
                ArithOp::Mul | ArithOp::Div | ArithOp::FloorDiv
            ))
        ) {
            let Token::ArithOp(op) = ctx.next() else { unreachable!() };

            let next_factor = parse_factor(ctx);

            factor = ExpressionTree::Apply(Operator::ArithOp(op), vec![factor, next_factor]);
        }

        if sign {
            ExpressionTree::Apply(Operator::Named("negate".to_string()), vec![factor])
        } else {
            factor
        }
    }

    fn parse_expression(ctx: &mut ParserContext) -> ExpressionTree {
        let mut term = parse_term(ctx);

        while matches!(
            ctx.peek(),
            Some(Token::ArithOp(ArithOp::Plus | ArithOp::Minus))
        ) {
            let Token::ArithOp(op) = ctx.next() else { unreachable!() };

            let next_term = parse_term(ctx);

            term = ExpressionTree::Apply(Operator::ArithOp(op), vec![term, next_term]);
        }

        term
    }

    let mut ctx = ParserContext::new(tokens.into());
    let expr = parse_expression(&mut ctx);
    assert!(ctx.is_finished());
    expr
}
