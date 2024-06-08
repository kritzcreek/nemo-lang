use ariadne::{Color, Config, Label, Report, ReportKind, Source};
use line_index::{LineCol, LineIndex};
use std::{fmt, str};
use text_size::TextRange;

#[derive(Debug)]
pub struct ParseError {
    pub it: String,
    pub at: TextRange,
}

impl ParseError {
    pub fn display<'err, 'src>(
        &'err self,
        source: &'src str,
        colors: bool,
    ) -> ParseErrorDisplay<'src, 'err> {
        ParseErrorDisplay {
            source,
            parse_error: self,
            colors,
        }
    }

    pub fn line_col(&self, line_index: &LineIndex) -> (LineCol, LineCol) {
        let start = line_index.line_col(self.at.start());
        let end = line_index.line_col(self.at.end());
        (start, end)
    }
}

pub struct ParseErrorDisplay<'src, 'err> {
    source: &'src str,
    parse_error: &'err ParseError,
    colors: bool,
}

impl fmt::Display for ParseErrorDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        render_parse_error(self.source, self.parse_error, self.colors, f);
        Ok(())
    }
}

pub fn render_parse_error(
    source: &str,
    error: &ParseError,
    colors: bool,
    output: &mut fmt::Formatter,
) {
    let file_name = "source";

    let out = Color::Fixed(81);
    let cache = (file_name, Source::from(source));

    // TODO avoid this allocation
    let mut out_buf = Vec::new();
    Report::build(ReportKind::Error, file_name, 12)
        .with_code("Parsing error")
        .with_message(&error.it)
        .with_label(
            Label::new((file_name, error.at.start().into()..error.at.end().into()))
                .with_message(error.it.to_string())
                .with_color(out),
        )
        .with_config(Config::default().with_color(colors))
        .finish()
        .write(cache, &mut out_buf)
        .unwrap();

    writeln!(output, "{}", str::from_utf8(&out_buf).unwrap()).unwrap();
}