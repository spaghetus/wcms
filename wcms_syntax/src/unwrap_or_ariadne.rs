use std::{fmt::Display, hash::Hash};

use chumsky::prelude::*;
pub trait UnwrapOrAriadne<OK> {
	fn unwrap_or_ariadne(self, src: &str) -> OK;
}

impl<OK, E> UnwrapOrAriadne<OK> for Result<OK, Vec<Simple<E>>>
where
	E: Hash + Eq + Display,
{
	fn unwrap_or_ariadne(self, src: &str) -> OK {
		use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind, Source};

		let mut colors = ColorGenerator::new();

		let error = match self {
			Ok(v) => {
				return v;
			}
			Err(e) => e,
		};

		let mut report = Report::build(
			ReportKind::Error,
			"input",
			error
				.iter()
				.map(|e| e.span().start)
				.min()
				.unwrap_or(0)
				.min(src.len() - 2),
		);
		for error in error {
			let mut span = error.span();
			span.start = span.start.min(src.len() - 2);
			span.end = span.end.min(src.len() - 1);
			eprintln!("{error}");
			report = report.with_label(
				Label::new(("input", error.span()))
					.with_message(error.to_string().fg(colors.next())),
			);
		}
		report.finish().print(("input", Source::from(src))).unwrap();
		panic!("Failed to parse document");
	}
}
