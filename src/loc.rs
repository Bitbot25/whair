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

    pub fn is_subrange_of(&self, other: Span) -> bool {
        let begin_is_subrange = self.0 >= other.0 && self.0 <= other.1;
        let end_is_subrange = self.1 >= other.0 && self.1 <= other.1;
        begin_is_subrange && end_is_subrange
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}
