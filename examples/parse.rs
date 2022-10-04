#![feature(iter_advance_by)]
use std::iter::Peekable;
use std::str::Chars;

use whair::loc::{Span, Spanned};
use whair::ParseBuffer;

#[derive(Debug)]
struct Token {
    span: Span,
    pub ty: TokenTy,
}

impl Token {
    pub fn ty(&self) -> TokenTy {
        self.ty
    }
}

impl Spanned for Token {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum TokenTy {
    Plus,
    Minus,
    Star,
    Slash,

    Number,

    Ident,
    Keyword,

    LParen,
    RParen,
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

#[derive(Debug)]
struct ParseError {
    span: Option<Span>,
    output: &'static str,
}

impl ParseError {
    pub fn maybe_span(&self) -> Option<Span> {
        self.span
    }
}

fn parse_add_sub(buf: &mut ParseBuffer<Token>) -> Result<Expr, ParseError> {
    let mut lhs = parse_mul_div(buf)?;
    while let Some(tok) =
        buf.consume(|tok| tok.ty == TokenTy::Plus || tok.ty == TokenTy::Minus)
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

fn parse_mul_div(buf: &mut ParseBuffer<Token>) -> Result<Expr, ParseError> {
    let mut lhs = Expr::UInt(parse_uint(buf)?);
    while let Some(tok) =
        buf.consume(|tok| tok.ty == TokenTy::Star || tok.ty == TokenTy::Slash)
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

fn parse_uint(buf: &mut ParseBuffer<Token>) -> Result<ExprUInt, ParseError> {
    match buf.next() {
        Some(tok) if tok.ty() == TokenTy::Number => {
            let span = tok.span();
            match buf.slice(span).parse::<u32>() {
                Ok(v) => Ok(ExprUInt(v)),
                Err(_) => unreachable!(),
            }
        }
        Some(tok) => Err(ParseError {
            span: Some(tok.span()),
            output: "Expected number",
        }),
        None => Err(ParseError {
            span: Some(Span(buf.len(), buf.len() + 1)),
            output: "Expected number but got EOF",
        }),
    }
}

struct Tokenizer<'a> {
    chars: Peekable<Chars<'a>>,
    orig: &'a str,
    idx: usize,
}

macro_rules! matched_tok {
    ($s:ident, $e:expr, $begin:expr) => {{
        $s.advance();
        Ok($crate::Token {
            ty: $e,
            span: whair::loc::Span($begin, $s.idx),
        })
    }};
    ($s:ident, $e:expr, $len:expr, $begin:expr) => {{
        $Ok($crate::Token {
            ty: $e,
            span: whair::loc::Span($begin, $s.idx),
        })
    }};
}

impl<'a> Tokenizer<'a> {
    const KEYWORDS: [&'static str; 1] = ["fun"];

    pub fn new(input: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            chars: input.chars().peekable(),
            orig: input,
            idx: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.idx >= self.orig.len()
    }

    pub fn token(&mut self) -> Result<Token, ParseError> {
        let begin = self.idx;
        let c = self.chars.peek().ok_or(ParseError {
            output: "Reached EOF but expected token.",
            span: Some(Span(0, self.idx)),
        })?;

        fn is_whitespace(c: char) -> bool {
            matches!(c, '\n' | '\r' | ' ')
        }

        if is_whitespace(*c) {
            self.advance();
            return self.token();
        }

        if *c >= '0' && *c <= '9' {
            self.skip_num();
            return Ok(Token {
                ty: TokenTy::Number,
                span: Span(begin, self.idx),
            });
        }

        if (*c >= 'a' && *c <= 'z') || (*c >= 'A' && *c <= 'Z') {
            self.skip_ident();
            let ident_str = &&self.orig[begin..self.idx];
            return Ok(Token {
                ty: if Self::KEYWORDS.contains(ident_str) {
                    TokenTy::Keyword
                } else {
                    TokenTy::Ident
                },
                span: Span(begin, self.idx),
            });
        }

        match c {
            '+' => matched_tok!(self, TokenTy::Plus, begin),
            '-' => matched_tok!(self, TokenTy::Minus, begin),
            '*' => matched_tok!(self, TokenTy::Star, begin),
            '/' => matched_tok!(self, TokenTy::Slash, begin),
            '(' => matched_tok!(self, TokenTy::LParen, begin),
            ')' => matched_tok!(self, TokenTy::RParen, begin),
            _ => Err(ParseError {
                output: "Invalid character",
                span: Some(Span(begin, begin + 1)),
            }),
        }
    }

    fn advance(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                self.idx += 1;
                Some(c)
            }
            None => None,
        }
    }

    fn advance_n(&mut self, n: usize) -> Option<char> {
        match self.chars.nth(n) {
            Some(c) => {
                self.idx += n;
                Some(c)
            }
            None => None,
        }
    }

    fn skip_ident(&mut self) {
        while let Some(c) = self.chars.peek() {
            if (*c < 'a' || *c > 'z') && (*c < 'A' || *c > 'Z') {
                break;
            }
            self.advance();
        }
    }

    fn skip_num(&mut self) {
        while let Some(c) = self.chars.peek() {
            if *c <= '0' || *c >= '9' {
                break;
            }
            self.advance();
        }
    }
}

fn highlight_oneliner(input: &str, span: Span) -> String {
    let mut buf = input.to_string();
    assert!(!buf.contains('\n')); // This function only works for one-liners.

    buf.push('\n');
    for _ in 0..span.0 {
        buf.push(' ');
    }

    let diff = span.1 - span.0;
    for _ in 0..diff {
        buf.push('^');
    }

    let left = buf.len() - span.1;
    for _ in 0..left {
        buf.push(' ');
    }
    buf
}

fn find_line_bounds(input: &str, index: usize) -> Span {
    fn is_newline((_i, c): &(usize, char)) -> bool {
        *c == '\n'
    }

    let mut search_iter = input.chars().enumerate();
    search_iter.advance_by(index).unwrap();

    let mut rev_search_iter = input.chars().rev().enumerate();
    rev_search_iter.advance_by(input.len() - index).unwrap();

    let start = match rev_search_iter.find(is_newline) {
        Some((i, _c)) => index - i + 1,
        None => 0,
    };

    let end = search_iter.find(is_newline).map(|(i, _c)| i).unwrap_or(input.len());
    Span(start, end)
}

fn main() {
    // let input = include_str!("/home/edvin/repo/whair/lang");
    let input = "1*2*";

    let mut tokenizer = Tokenizer::new(input);
    let mut tokens = vec![];
    while !tokenizer.is_empty() {
        let res = tokenizer.token();
        match res {
            Ok(tok) => tokens.push(tok),
            Err(e) => {
                dbg!(e);
                break;
            }
        }
    }
    dbg!(&tokens);

    let mut binop = ParseBuffer::new(tokens, input);
    let res = parse_add_sub(&mut binop);
    match res {
        Ok(ast) => {
            dbg!(ast);
        }
        Err(e) => {
            match e.maybe_span() {
                None => eprintln!("ERROR: {}", e.output),
                Some(span) => {
                    eprintln!("ERROR ({}..{}): {}", span.0, span.1, e.output);
                    let line_span = find_line_bounds(input, span.0);

                    // TODO: Make this able to highlight
                    // multiple lines
                    let line = &input[line_span.0..line_span.1];
                    let rel_span = Span(span.0 - line_span.0, span.1 - line_span.0);

                    eprintln!("{}", highlight_oneliner(line, rel_span));
                }
            }
        }
    }
}
