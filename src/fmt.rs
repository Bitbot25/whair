use std::{borrow::Cow, fmt as stdfmt, ops::RangeBounds};

use crate::loc::Span;

pub struct SelectOverAdapter<T: stdfmt::Display> {
    pub span: Span,
    pub inner: T,
}

impl<T: stdfmt::Display> stdfmt::Display for SelectOverAdapter<T> {
    fn fmt(&self, f: &mut stdfmt::Formatter<'_>) -> stdfmt::Result {
        let inner = format!("{}", self.inner);
        let mut begin_bound = 0;
        for line in inner.lines() {
            let end_bound = begin_bound + line.len() + 1; // +1 for newline
            let span_begin_intersects = self.span.0 >= begin_bound && self.span.0 < end_bound;
            let span_end_intersects = self.span.1 >= begin_bound && self.span.1 < end_bound;
            if span_begin_intersects || span_end_intersects {
                writeln!(f, "{line}")?;
            }
            begin_bound = end_bound;
        }
        Ok(())
    }
}
pub struct SelectLinesAdapter<T: stdfmt::Display, R: RangeBounds<usize>> {
    range: R,
    inner: T,
}

impl<T: stdfmt::Display, R: RangeBounds<usize>> stdfmt::Display for SelectLinesAdapter<T, R> {
    fn fmt(&self, f: &mut stdfmt::Formatter<'_>) -> stdfmt::Result {
        let inner = format!("{}", self.inner);
        for (idx, line) in inner.lines().enumerate() {
            if self.range.contains(&idx) {
                writeln!(f, "{line}")?;
            }
        }
        Ok(())
    }
}

pub struct LineNumberAdapter<T: stdfmt::Display>(pub T);

impl<T: stdfmt::Display> stdfmt::Display for LineNumberAdapter<T> {
    fn fmt(&self, f: &mut stdfmt::Formatter<'_>) -> stdfmt::Result {
        let inner = format!("{}", self.0);
        let line_count = inner.lines().count();
        let line_count_max_char_len = (line_count.checked_ilog10().unwrap_or(0) + 1) as usize;

        for (idx, line) in inner.lines().enumerate() {
            writeln!(
                f,
                "{:width$} | {}",
                idx + 1,
                line,
                width = line_count_max_char_len
            )?;
        }
        Ok(())
    }
}

pub trait UnderlineFunction = Fn(UnderlineFunctionData) -> Option<Cow<str>>;

#[derive(Debug)]
pub struct UnderlineFunctionData<'a> {
    pub line_index: usize,
    pub line_span: Span,
    pub line_contents: &'a str,
}

pub struct UnderlineAdapter<T: stdfmt::Display, F: UnderlineFunction> {
    pub inner: T,
    pub function: F,
}

impl<T: stdfmt::Display, F: UnderlineFunction> stdfmt::Display for UnderlineAdapter<T, F> {
    fn fmt(&self, f: &mut stdfmt::Formatter<'_>) -> stdfmt::Result {
        let inner = format!("{}", self.inner);
        let mut line_begin = 0;
        for (idx, line) in inner.lines().enumerate() {
            writeln!(f, "{}", line)?;
            let line_end = line_begin + line.len() + 1;
            match (self.function)(UnderlineFunctionData {
                line_index: idx,
                line_span: Span(line_begin, line_end),
                line_contents: line,
            }) {
                Some(out) => writeln!(f, "{out}")?,
                None => (),
            };
            line_begin = line_end;
        }
        Ok(())
    }
}

pub struct SetLineCharactersAdapter<'a, T: stdfmt::Display> {
    pub source: &'a str,
    pub span: Span,
    pub line: usize,
    pub inner: T,
}

impl<'a, T: stdfmt::Display> stdfmt::Display for SetLineCharactersAdapter<'a, T> {
    fn fmt(&self, f: &mut stdfmt::Formatter<'_>) -> stdfmt::Result {
        let inner = format!("{}", self.inner);
        for (idx, line) in inner.lines().enumerate() {
            if idx == self.line {
                // FIXME: This is assuming there are no strange characters in the line.
                let mut new = String::from(line);
                new.replace_range(self.span.0..self.span.1, self.source);
                writeln!(f, "{}", new)?;
            } else {
                writeln!(f, "{}", line)?;
            }
        }
        Ok(())
    }
}
