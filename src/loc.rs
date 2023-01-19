pub struct SourceSpan<'a> {
    src: &'a str,
    span: Span,
}

#[derive(Copy, Clone, Debug)]
pub struct Span(pub usize, pub usize);

impl Span {
    pub fn len(&self) -> usize {
        self.1 - self.0
    }

    pub fn is_empty(&self) -> bool {
        self.0 == self.1
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}
