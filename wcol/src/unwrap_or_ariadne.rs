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

impl<OK> UnwrapOrAriadne<OK> for Result<OK, serde_yaml::Error> {
	fn unwrap_or_ariadne(self, src: &str) -> OK {
		use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind, Source};

		#[allow(clippy::range_plus_one)]
		let (msg, span) = match self {
			Ok(ok) => return ok,
			Err(e) => (
				e.to_string(),
				e.location()
					.map_or(0..1, |loc| loc.index()..loc.index() + 1),
			),
		};

		let mut colors = ColorGenerator::new();

		Report::build(ReportKind::Error, "input", span.start)
			.with_label(Label::new(("input", span)).with_message(msg.fg(colors.next())))
			.finish()
			.print(("input", Source::from(src)))
			.unwrap();
		panic!("Failed to parse document")
	}
}
