use whair::loc::{Span, Spanned};
use whair::ParseBuffer;

struct Token {
    span: Span,
    pub ty: TokenTy,
}

impl whair::TypeOfToken for Token {
    type TT = TokenTy;

    fn ty(&self) -> Self::TT {
        self.ty
    }
}

impl Spanned for Token {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(PartialEq, Eq, Copy, Clone)]
enum TokenTy {
    Plus,
    Minus,
    Star,
    Slash,

    Number,
}

#[derive(Debug)]
enum Expr {
    BinOp(ExprBinOp),
    UInt(ExprUInt),
}

#[derive(Debug)]
struct ExprBinOp {
    operands: Box<(Expr, Expr)>,
    ty: BinOpTy,
}

#[derive(Debug)]
enum BinOpTy {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
struct ExprUInt(u32);

fn parse_add_sub(buf: &mut ParseBuffer<Token>) -> Result<Expr, ()> {
    let mut lhs = parse_mul_div(buf)?;
    while let Some(tok) =
        buf.consume_pred(|tok| tok.ty == TokenTy::Plus || tok.ty == TokenTy::Minus)
    {
        let ty = match tok.ty {
            TokenTy::Plus => BinOpTy::Add,
            TokenTy::Minus => BinOpTy::Sub,
            _ => unreachable!(),
        };
        let rhs = parse_mul_div(buf)?;
        lhs = Expr::BinOp(ExprBinOp {
            operands: Box::new((lhs, rhs)),
            ty,
        });
    }
    Ok(lhs)
}

fn parse_mul_div(buf: &mut ParseBuffer<Token>) -> Result<Expr, ()> {
    let mut lhs = Expr::UInt(parse_uint(buf)?);
    while let Some(tok) =
        buf.consume_pred(|tok| tok.ty == TokenTy::Star || tok.ty == TokenTy::Slash)
    {
        let ty = match tok.ty {
            TokenTy::Star => BinOpTy::Mul,
            TokenTy::Slash => BinOpTy::Div,
            _ => unreachable!(),
        };
        let rhs = Expr::UInt(parse_uint(buf)?);
        lhs = Expr::BinOp(ExprBinOp {
            operands: Box::new((lhs, rhs)),
            ty,
        });
    }
    Ok(lhs)
}

fn parse_uint(buf: &mut ParseBuffer<Token>) -> Result<ExprUInt, ()> {
    if let Some(tok) = buf.consume(TokenTy::Number) {
        let span = tok.span;
        if span.is_empty() {
            Err(())
        } else {
            match buf.slice(span).parse::<u32>() {
                Ok(v) => Ok(ExprUInt(v)),
                Err(_) => unreachable!(),
            }
        }
    } else {
        Err(())
    }
}

fn main() {
    let mut binop = ParseBuffer::new(
        vec![
            Token {
                span: Span(0, 1),
                ty: TokenTy::Number,
            },
            Token {
                span: Span(1, 2),
                ty: TokenTy::Plus,
            },
            Token {
                span: Span(2, 3),
                ty: TokenTy::Number,
            },
            Token {
                span: Span(3, 4),
                ty: TokenTy::Star,
            },
            Token {
                span: Span(4, 5),
                ty: TokenTy::Number,
            }
        ],
        "1+2*3",
    );
    let res = parse_add_sub(&mut binop);
    dbg!(res);
}
