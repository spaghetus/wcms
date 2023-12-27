use chumsky::{
	prelude::*,
	text::{newline, whitespace},
	Parser,
};
use std::{
	collections::{HashMap, HashSet},
	convert::Into,
	fmt::Display,
	iter::Peekable,
	ops::{Deref, Range},
};
use strum_macros::EnumDiscriminants;

#[cfg(feature = "ariadne")]
pub mod unwrap_or_ariadne;

/// A string, delimited in one of several ways
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Text {
	/// A bare string, without spaces.
	Bare(String),
	/// A quoted string
	Quoted(String),
	/// A parenthesized string
	Paren(String),
}

fn text() -> impl Parser<char, Text, Error = Simple<char>> {
	choice((
		none_of(" \r\n=(\"]")
			.repeated()
			.at_least(1)
			.collect()
			.map(Text::Bare),
		just('"')
			.ignore_then(none_of('"').repeated().collect())
			.then_ignore(just('"'))
			.map(Text::Quoted),
		just('(')
			.ignore_then(none_of(')').repeated().collect())
			.then_ignore(just(')'))
			.map(Text::Paren),
	))
}

impl Deref for Text {
	type Target = str;

	fn deref(&self) -> &Self::Target {
		match self {
			Text::Bare(v) | Text::Quoted(v) | Text::Paren(v) => v.as_str(),
		}
	}
}

#[test]
fn test_text_parsing() {
	let samples = [
		["bare", "bare"],
		["\"quoted string with spaces\"", "quoted string with spaces"],
		[
			"(parenthesized string with spaces)",
			"parenthesized string with spaces",
		],
	];
	let parser = text();
	for [code, truth] in samples {
		assert_eq!(&*parser.parse(code).unwrap(), truth);
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Argument(Text, Option<Text>);

#[must_use]
fn argument() -> impl Parser<char, Argument, Error = Simple<char>> {
	text()
		.then_ignore(just('='))
		.then(text())
		.map(|(k, v)| Argument(k, Some(v)))
		.or(text().map(|k| Argument(k, None)))
}

#[test]
fn test_arguments() {
	let parser = argument()
		.separated_by(whitespace())
		.allow_leading()
		.allow_trailing();
	let parsed = parser
		.parse(r#"bare (parenthesized argument) bare=yes (paren)="12" "spooky scary" 1=2"#)
		.unwrap();
	assert_eq!(
		parsed,
		[
			Argument(Text::Bare("bare".to_string()), None,),
			Argument(Text::Paren("parenthesized argument".to_string()), None,),
			Argument(
				Text::Bare("bare".to_string()),
				Some(Text::Bare("yes".to_string()),),
			),
			Argument(
				Text::Paren("paren".to_string()),
				Some(Text::Quoted("12".to_string()),),
			),
			Argument(Text::Quoted("spooky scary".to_string()), None,),
			Argument(
				Text::Bare("1".to_string()),
				Some(Text::Bare("2".to_string()),),
			),
		]
	);
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockHeader {
	pub name: Text,
	pub pairs: HashMap<Text, Text>,
	pub tags: HashSet<Text>,
}

fn block_header_inner() -> impl Parser<char, BlockHeader, Error = Simple<char>> {
	text()
		.then(
			argument()
				.separated_by(whitespace())
				.allow_leading()
				.allow_trailing(),
		)
		.map(|(name, args)| {
			let mut block = BlockHeader {
				name,
				pairs: HashMap::new(),
				tags: HashSet::new(),
			};

			for Argument(key, value) in args {
				if let Some(value) = value {
					block.pairs.insert(key, value);
				} else {
					block.tags.insert(key);
				}
			}

			block
		})
}

fn block_header() -> impl Parser<char, BlockHeader, Error = Simple<char>> {
	just('[')
		.ignore_then(block_header_inner())
		.then_ignore(just(']'))
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GroupHeader(pub BlockHeader);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GroupFooter(pub Text);

fn group_header() -> impl Parser<char, GroupHeader, Error = Simple<char>> {
	just("[+")
		.ignore_then(block_header_inner())
		.then_ignore(just(']'))
		.map(GroupHeader)
}

fn group_footer() -> impl Parser<char, GroupFooter, Error = Simple<char>> {
	just("[-")
		.ignore_then(text())
		.then_ignore(just(']'))
		.map(GroupFooter)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Body(String);

fn body() -> impl Parser<char, Body, Error = Simple<char>> {
	take_until(newline())
		.map(|(chars, ())| chars)
		.collect()
		.map(Body)
}

#[derive(Debug, PartialEq, Eq, Clone, strum_macros::EnumDiscriminants)]
#[strum_discriminants(derive(Hash))]
pub enum Token {
	BlockHeader(BlockHeader, Range<usize>),
	GroupHeader(GroupHeader, Range<usize>),
	GroupFooter(GroupFooter, Range<usize>),
	BodyLine(Body, Range<usize>),
	ParseError(Range<usize>),
}

impl Display for TokenDiscriminants {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{self:?}")
	}
}

impl Token {
	#[must_use]
	pub fn span(&self) -> Range<usize> {
		match self {
			Token::BlockHeader(_, span)
			| Token::GroupHeader(_, span)
			| Token::GroupFooter(_, span)
			| Token::BodyLine(_, span)
			| Token::ParseError(span) => span.clone(),
		}
	}
	#[must_use]
	pub fn name(&self) -> &'static str {
		match self {
			Token::BlockHeader(_, _) => "BlockHeader",
			Token::GroupHeader(_, _) => "GroupHeader",
			Token::GroupFooter(_, _) => "GroupFooter",
			Token::BodyLine(_, _) => "BodyLine",
			Token::ParseError(_) => "ParseError",
		}
	}
}

fn tokens() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
	group_header()
		.map_with_span(Token::GroupHeader)
		.or(group_footer().map_with_span(Token::GroupFooter))
		.or(block_header().map_with_span(Token::BlockHeader))
		.or(body().map_with_span(Token::BodyLine))
		.recover_with(skip_parser(
			newline()
				.not()
				.repeated()
				.at_least(1)
				.map_with_span(|_, span| Token::ParseError(span)),
		))
		.repeated()
		.then_ignore(end())
}

#[test]
fn stress_test_objects() {
	use unwrap_or_ariadne::UnwrapOrAriadne;
	let parser = tokens();
	let input = include_str!("../../test.wcms");
	let parsed = parser.parse(input).unwrap_or_ariadne(input);
	println!("{parsed:#?}");
}

#[derive(Debug, Clone, EnumDiscriminants)]
pub enum Section {
	Block {
		header: BlockHeader,
		content: String,
		span: Range<usize>,
	},
	Group {
		header: BlockHeader,
		content: Vec<Section>,
		span: Range<usize>,
	},
}

impl Section {
	#[must_use]
	pub fn span(&self) -> Range<usize> {
		match self {
			Section::Block {
				header: _,
				content: _,
				span,
			}
			| Section::Group {
				header: _,
				content: _,
				span,
			} => span.clone(),
		}
	}

	pub(crate) fn consume_from_iter<I: Iterator<Item = Token>>(
		iter: &mut Peekable<I>,
	) -> Result<Option<Section>, Simple<TokenDiscriminants>> {
		let Some(first) = iter.next() else {
			return Ok(None);
		};
		match first {
			Token::BodyLine(text, _span) if text.0.trim().is_empty() => Ok(None),
			Token::BlockHeader(header, mut span) => {
				let mut content = Vec::new();
				while iter.peek().map(Into::into) == Some(TokenDiscriminants::BodyLine) {
					let Some(Token::BodyLine(Body(text), line_span)) = iter.next() else {
						unreachable!()
					};
					content.push(text);
					span.end = span.end.max(line_span.end);
				}
				let content = content.join("\n").trim().to_string();
				Ok(Some(Section::Block {
					header,
					content,
					span,
				}))
			}
			Token::GroupHeader(GroupHeader(header), mut span) => {
				let mut content = Vec::new();
				loop {
					if let Some(Token::GroupFooter(GroupFooter(text), footer_span)) = iter.peek() {
						span.end = span.end.max(footer_span.end);
						if text == &header.name {
							iter.next();
							return Ok(Some(Section::Group {
								header,
								content,
								span,
							}));
						}

						return Err(Simple::expected_input_found(
							span.start..footer_span.end,
							[Some(TokenDiscriminants::GroupFooter)],
							Some(TokenDiscriminants::GroupFooter),
						));
					}
					if let Some(next_section) = Section::consume_from_iter(iter)? {
						span.end = span.end.max(next_section.span().end);
						content.push(next_section);
					}
					if iter.peek().is_none() {
						return Err(Simple::expected_input_found(
							span,
							[
								Some(TokenDiscriminants::GroupFooter),
								Some(TokenDiscriminants::BlockHeader),
								Some(TokenDiscriminants::GroupHeader),
							],
							None,
						));
					}
				}
			}
			other => Err(Simple::expected_input_found(
				other.span(),
				[
					Some(TokenDiscriminants::BlockHeader),
					Some(TokenDiscriminants::GroupHeader),
				],
				Some(other.into()),
			)),
		}
	}
}

#[test]
fn stress_test_sections() {
	use unwrap_or_ariadne::UnwrapOrAriadne;
	let input = include_str!("../../test.wcms");
	let tokens = dbg!(string_to_tokens(input).unwrap_or_ariadne(input));
	let _sections = dbg!(tokens_to_sections(&tokens).unwrap_or_ariadne(input));
}

/// Parse a string to a set of tokens.
///
/// # Errors
/// Returns a list of parsing errors, if we can.
pub fn string_to_tokens(input: &str) -> Result<Vec<Token>, Vec<Simple<char>>> {
	tokens().parse(input)
}

/// Parse a string to a set of tokens.
///
/// # Errors
/// Returns a list of parsing errors, if we can.
pub fn tokens_to_sections(
	input: &[Token],
) -> Result<Vec<Section>, Vec<Simple<TokenDiscriminants>>> {
	let mut input = input.iter().cloned().peekable();
	let mut sections = Vec::new();
	let mut errors = Vec::new();

	while input.peek().is_some() {
		match Section::consume_from_iter(&mut input) {
			Ok(Some(section)) => sections.push(section),
			Err(e) => errors.push(e),
			_ => {}
		}
	}

	if errors.is_empty() {
		Ok(sections)
	} else {
		Err(errors)
	}
}
