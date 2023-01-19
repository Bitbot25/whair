#![feature(trait_alias)]
pub mod fmt;
pub mod loc;

pub struct ParseBuffer<'a, T> {
    index: usize,
    tokens: Vec<T>,
    orig: &'a str,
}

impl<'a, T> ParseBuffer<'a, T> {
    pub fn new(tokens: Vec<T>, orig: &'a str) -> ParseBuffer<'a, T> {
        ParseBuffer {
            index: 0,
            tokens,
            orig,
        }
    }

    pub fn len(&self) -> usize {
        self.orig.len()
    }

    pub fn is_empty(&self) -> bool {
        self.index >= self.tokens.len()
    }
    pub fn peek(&self) -> Option<&T> {
        self.tokens.get(self.index)
    }

    pub fn next(&mut self) -> Option<&T> {
        match self.tokens.get(self.index) {
            Some(c) => {
                self.index += 1;
                Some(c)
            }
            None => None,
        }
    }

    pub fn consume(&mut self, pred: impl FnOnce(&T) -> bool) -> Option<&T> {
        match self.tokens.get(self.index) {
            Some(tok) if pred(tok) => {
                self.index += 1;
                Some(tok)
            }
            _ => None,
        }
    }

    pub fn seek_span(&mut self, mut pred: impl FnMut(&T) -> bool) -> loc::Span {
        let begin = self.index;
        let mut end = begin;
        while match self.peek() {
            Some(tok) => pred(tok),
            None => false,
        } {
            end += 1;
        }
        loc::Span(begin, end)
    }

    pub fn all_tokens(&self) -> &Vec<T> {
        &self.tokens
    }

    pub fn slice(&self, span: loc::Span) -> &'a str {
        &self.orig[span.0..span.1]
    }
}
