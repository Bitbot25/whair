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

pub struct NoExtraOptions;

pub trait FmtRender {
    type ExOptions = NoExtraOptions;
    fn fmt_render(&self, buf: &mut FmtBuffer, options: RenderOptions, ex_options: Self::ExOptions);
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
    fn fmt_render(&self, buf: &mut FmtBuffer, options: RenderOptions, _ex_options: NoExtraOptions) {
        match options.location() {
            RenderLocation::Header => buf.putc(0, 0, *self),
            RenderLocation::ExactCR { column, row } => buf.putc(*column, *row, *self),
            RenderLocation::Annotation(..) => todo!(), // Will panic if the span is more than 1 chars
        }
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
    pub fn putc_with_count(&mut self, col: usize, row: usize, c: FmtChar, count: &mut usize) {
        self.putc(col, row, c);
        *count += 1;
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

    pub fn putc(&mut self, col: usize, row: usize, c: FmtChar) {
        if row >= self.lines.len() {
            self.lines.resize(row + 1, FmtLine { chars: vec![] });
        }
        let line = &mut self.lines[row];
        if col >= line.chars.len() {
            line.chars
                .resize(col + 1, FmtChar::new(' ', Styles::none()));
        }
        if line.chars[col].c != ' ' && cfg!(debug) {
            panic!("cannot overwite existing characters in buffer");
        }
        line.chars[col] = c;

        let (max_col, max_row) = self.max_written_pos;
        if col > max_col || (col == max_col && row > max_row) {
            self.max_written_pos = (col, row);
        }
    }
}

impl FmtBufRender for FmtBuffer {
    fn fmt_render_to_string(&self, buf: &mut String) {
        for line in &self.lines {
            line.fmt_render_to_string(buf);
        }
    }
}

#[derive(Copy, Clone)]
pub enum RenderLocation {
    Header,
    Annotation(Option<Span>),
    ExactCR { column: usize, row: usize },
}

impl RenderLocation {
    pub fn resolve_to_exact_cr(&self) -> (usize, usize) {
        match self {
            Self::ExactCR { column, row } => (*column, *row),
            Self::Annotation(..) => todo!(),
            Self::Header => (0, 0),
        }
    }
}

pub struct RenderOptions {
    location: RenderLocation,
}

pub struct RenderOptionsBuilder {
    location: Option<RenderLocation>,
}

impl RenderOptionsBuilder {
    pub fn new() -> Self {
        RenderOptionsBuilder { location: None }
    }

    pub fn builder_of(opt: &RenderOptions) -> Self {
        RenderOptionsBuilder {
            location: Some(opt.location),
        }
    }

    pub fn location(mut self, loc: RenderLocation) -> RenderOptionsBuilder {
        self.location = Some(loc);
        self
    }

    pub fn build(self) -> RenderOptions {
        RenderOptions {
            location: self.location.expect("location to render was not specified"),
        }
    }
}

impl RenderOptions {
    pub fn builder() -> RenderOptionsBuilder {
        RenderOptionsBuilder::new()
    }

    pub fn location(&self) -> &RenderLocation {
        &self.location
    }
}

pub struct MultipartText<'a> {
    strs: &'a [Text<'a>],
}

impl<'a> MultipartText<'a> {
    pub fn new(strs: &'a [Text<'a>]) -> Self {
        MultipartText { strs: strs.into() }
    }
}

impl<'a> FmtRender for MultipartText<'a> {
    fn fmt_render(&self, buf: &mut FmtBuffer, options: RenderOptions, _ex_options: NoExtraOptions) {
        let (mut column, row) = options.location().resolve_to_exact_cr();
        for str in self.strs.iter() {
            str.fmt_render(
                buf,
                RenderOptionsBuilder::builder_of(&options)
                    .location(RenderLocation::ExactCR { column, row })
                    .build(),
                NoExtraOptions,
            );
            column += str.str.len();
        }
    }
}

pub struct Text<'a> {
    str: Cow<'a, str>,
    style: Styles,
}

impl<'a> Text<'a> {
    pub fn new<S: Into<Cow<'a, str>>>(str: S, style: Styles) -> Self {
        Text {
            str: str.into(),
            style,
        }
    }
}

impl<'a> FmtRender for Text<'a> {
    fn fmt_render(&self, buf: &mut FmtBuffer, options: RenderOptions, _ex_options: NoExtraOptions) {
        fn simple_render(
            this: &Text,
            buf: &mut FmtBuffer,
            options: &RenderOptions,
            mut column: usize,
            row: usize,
        ) {
            for c in this.str.chars() {
                let fmt_char = FmtChar::new(c, this.style);
                fmt_char.fmt_render(
                    buf,
                    RenderOptionsBuilder::builder_of(options)
                        .location(RenderLocation::ExactCR { column, row })
                        .build(),
                    NoExtraOptions,
                );
                column += 1;
            }
        }
        match options.location() {
            RenderLocation::Header => simple_render(self, buf, &options, 0, 0),
            RenderLocation::ExactCR { column, row } => {
                simple_render(self, buf, &options, *column, *row)
            }
            RenderLocation::Annotation(..) => todo!(),
        }
    }
}

pub struct CodeLine<'a> {
    line_number: usize,
    /// The [`source`] does not contain a newline.
    source: &'a str,
}

impl<'a> CodeLine<'a> {
    pub fn new(line_number: usize, source: &'a str) -> Self {
        assert!(!source.contains('\n'), "source cannot contain a newline");
        CodeLine {
            line_number,
            source,
        }
    }
}

pub struct CodeLinePadding {
    width: usize,
}

impl<'a> FmtRender for CodeLine<'a> {
    type ExOptions = CodeLinePadding;
    fn fmt_render(&self, buf: &mut FmtBuffer, options: RenderOptions, padding: CodeLinePadding) {
        // FIXME: Actually use the line number and pad
        let text = Text::new(
            format!(
                "{:width$} | {}",
                self.line_number,
                self.source,
                width = padding.width
            ),
            Styles::none(),
        );
        text.fmt_render(buf, options, NoExtraOptions);
    }
}

pub struct CodeBlock<'a> {
    lines: Vec<CodeLine<'a>>,
}

impl<'a> CodeBlock<'a> {
    pub fn new(lines: Vec<CodeLine<'a>>) -> Self {
        CodeBlock { lines }
    }
}

impl<'a> FmtRender for CodeBlock<'a> {
    fn fmt_render(&self, buf: &mut FmtBuffer, options: RenderOptions, _ex_options: NoExtraOptions) {
        fn simple_render(
            this: &CodeBlock,
            buf: &mut FmtBuffer,
            options: &RenderOptions,
            column: usize,
            mut row: usize,
        ) {
            let max_line_number = match this
                .lines
                .iter()
                .map(|line| line.line_number)
                .max()
                .unwrap_or(1)
            {
                0 => 1,
                n => n,
            };
            let max_width = 1 + max_line_number.ilog10();
            for line in &this.lines {
                line.fmt_render(
                    buf,
                    RenderOptionsBuilder::builder_of(options)
                        .location(RenderLocation::ExactCR { column, row })
                        .build(),
                    CodeLinePadding {
                        width: max_width as usize,
                    },
                );
                row += 1;
            }
        }

        match options.location() {
            RenderLocation::Header => simple_render(self, buf, &options, 0, 0),
            RenderLocation::ExactCR { column, row } => {
                simple_render(self, buf, &options, *column, *row)
            }
            RenderLocation::Annotation(..) => todo!(),
        }
    }
}
