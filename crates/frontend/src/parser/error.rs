use ariadne::{Color, Label, Report, ReportKind, Source};
use camino::Utf8Path;
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
        path: &'src Utf8Path,
        source: &'src str,
    ) -> ParseErrorDisplay<'src, 'err> {
        ParseErrorDisplay {
            path,
            source,
            parse_error: self,
        }
    }

    pub fn line_col(&self, line_index: &LineIndex) -> (LineCol, LineCol) {
        let start = line_index.line_col(self.at.start());
        let end = line_index.line_col(self.at.end());
        (start, end)
    }
}

pub struct ParseErrorDisplay<'src, 'err> {
    path: &'src Utf8Path,
    source: &'src str,
    parse_error: &'err ParseError,
}

impl fmt::Display for ParseErrorDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        render_parse_error(self.path, self.source, self.parse_error, f);
        Ok(())
    }
}

fn render_parse_error(
    path: &Utf8Path,
    source: &str,
    error: &ParseError,
    output: &mut fmt::Formatter,
) {
    let out = Color::Fixed(81);
    let cache = (path, Source::from(source));

    // TODO avoid this allocation
    let mut out_buf = Vec::new();
    Report::build(ReportKind::Error, (path, error.at.into()))
        .with_code("Parsing error")
        .with_message(&error.it)
        .with_label(
            Label::new((path, error.at.start().into()..error.at.end().into()))
                .with_message(error.it.to_string())
                .with_color(out),
        )
        .finish()
        .write(cache, &mut out_buf)
        .unwrap();

    writeln!(output, "{}", str::from_utf8(&out_buf).unwrap()).unwrap();
}
