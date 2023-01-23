use bitflags::bitflags;
use std::borrow::Cow;

use crate::loc::Span;

#[derive(Debug, Copy, Clone)]
pub enum Color {
    Red,
    Default,
}

impl FmtBufRender for Color {
    fn fmt_render_to_string(&self, buf: &mut String) {
        buf.push_str(match self {
            Color::Red => ANSI_RED,
            Color::Default => ANSI_RESET,
        });
    }
}

macro_rules! ansi_escape {
    ($n:literal) => {
        concat!("\u{001b}[", $n, "m")
    };
}

pub const ANSI_RED: &'static str = ansi_escape!(31);
pub const ANSI_RESET: &'static str = ansi_escape!(0);
pub const ANSI_BOLD: &'static str = ansi_escape!(1);
pub const ANSI_ITALIC: &'static str = ansi_escape!(3);

#[derive(Debug, Copy, Clone)]
pub struct Styles {
    color: Color,
    attrs: StyleAttributes,
}

impl Styles {
    pub fn new(color: Color, attrs: StyleAttributes) -> Self {
        Styles { color, attrs }
    }

    pub fn none() -> Self {
        Styles {
            color: Color::Default,
            attrs: StyleAttributes::empty(),
        }
    }
}

bitflags! {
    pub struct StyleAttributes: u8 {
        const ITALIC = 0b00000001;
        const BOLD   = 0b00000010;
    }
}

impl FmtBufRender for StyleAttributes {
    fn fmt_render_to_string(&self, buf: &mut String) {
        if self.intersects(StyleAttributes::ITALIC) {
            buf.push_str(ANSI_ITALIC);
        }
        if self.intersects(StyleAttributes::BOLD) {
            buf.push_str(ANSI_BOLD);
        }
    }
}

impl FmtBufRender for Styles {
    fn fmt_render_to_string(&self, buf: &mut String) {
        self.color.fmt_render_to_string(buf);
        self.attrs.fmt_render_to_string(buf);
    }
}

pub trait FmtRender {
    fn fmt_render(&self, buf: &mut FmtBuffer);
}

pub trait FmtBufRender {
    fn fmt_render_to_string(&self, buf: &mut String);
}

#[derive(Copy, Clone, Debug)]
pub struct FmtChar {
    c: char,
    style: Styles,
}

impl FmtChar {
    pub fn new(c: char, style: Styles) -> Self {
        FmtChar { c, style }
    }
}

impl FmtRender for FmtChar {
    fn fmt_render(&self, buf: &mut FmtBuffer) {
        buf.putc(*self);
    }
}

impl FmtBufRender for FmtChar {
    fn fmt_render_to_string(&self, buf: &mut String) {
        self.style.fmt_render_to_string(buf);
        buf.push(self.c);
        // TODO: This can be more efficient
        // Reset styles after each char
        Styles::none().fmt_render_to_string(buf);
    }
}

#[derive(Debug, Clone)]
pub struct FmtLine {
    chars: Vec<FmtChar>,
}

impl FmtBufRender for FmtLine {
    fn fmt_render_to_string(&self, buf: &mut String) {
        for c in &self.chars {
            c.fmt_render_to_string(buf);
        }
        buf.push('\n');
    }
}

#[derive(Debug, Clone, Default)]
pub struct FmtBuffer {
    lines: Vec<FmtLine>,
    max_written_pos: (usize, usize),
}

impl FmtBuffer {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn max_written_pos(&self) -> (usize, usize) {
        self.max_written_pos
    }

    #[inline]
    pub fn free_row(&self) -> usize {
        let (_max_written_col, max_written_row) = self.max_written_pos;
        max_written_row + 1
    }

    #[inline]
    pub fn putc(&mut self, c: FmtChar) {
        if self.lines.is_empty() {
            self.new_line();
        } else {
            self.lines.last_mut().unwrap().chars.push(c);
        }
    }

    #[inline]
    pub fn new_line(&mut self) {
        self.lines.push(FmtLine { chars: vec![] });
    }
}

impl FmtBufRender for FmtBuffer {
    fn fmt_render_to_string(&self, buf: &mut String) {
        for line in &self.lines {
            line.fmt_render_to_string(buf);
        }
    }
}

pub enum LineAnnotation {
    Line { line_begin: usize, line_end: usize },
    MultiLineOpening { line_begin: usize, line_end: usize },
    MultiLineClosing { line_begin: usize, line_end: usize },
    MultiLineLeg,
}

pub enum BorderStyle {
    Vertical,
    Corner,
    Closing(String),
}

pub struct Renderer {
    fmt_buf: FmtBuffer,
}

impl Renderer {
    fn outer_gutter_line_number(&mut self, number_padding: usize, line_number: usize) {
        let fmt = format!("{:width$} ", line_number, width = number_padding);
        for c in fmt.chars() {
            self.fmt_buf.putc(FmtChar::new(c, Styles::none()));
        }
    }

    fn left_border(&mut self) {
        self.fmt_buf.putc(FmtChar::new('|', Styles::none()));
        self.fmt_buf.putc(FmtChar::new(' ', Styles::none()));
    }

    fn write_str(&mut self, string: &str) {
        for c in string.chars() {
            self.fmt_buf.putc(FmtChar::new(c, Styles::none()));
        }
    }

    pub fn code_line(
        &mut self,
        number_padding: usize,
        line_number: usize,
        source: &str,
        annotations: &[LineAnnotation],
    ) {
        self.outer_gutter_line_number(number_padding, line_number);
        self.left_border();

        for annotation in annotations {
            match annotation {
                LineAnnotation::Line {
                    line_begin,
                    line_end,
                } => {
                    self.write_str();
                }
            }
        }
    }
}
