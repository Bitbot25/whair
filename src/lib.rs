pub mod loc;

pub trait TypeOfToken {
    type TT: Copy + Clone + Eq + PartialEq;
    fn ty(&self) -> Self::TT;
}

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

    pub fn consume(&mut self, tgt: T::TT) -> Option<&T>
    where
        T: TypeOfToken,
    {
        match self.tokens.get(self.index) {
            Some(tok) if tok.ty() == tgt => {
                self.index += 1;
                Some(tok)
            }
            _ => None,
        }
    }

    pub fn consume_pred(&mut self, pred: impl FnOnce(&T) -> bool) -> Option<&T> {
        // match self.peek() {
        // Some(tok) if pred(tok) => {
        // self.index += 1;
        // Some(tok)
        // }
        // _ => None,
        // }
        match self.tokens.get(self.index) {
            // TODO: What the fuck is this? When i manually inline
            // the function it works but when i instead call
            // self.peek() it complains about borrowing self.index?
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
