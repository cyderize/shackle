use std::{
	path::{Path, PathBuf, MAIN_SEPARATOR_STR},
	str::FromStr,
};

use lsp_types::Uri;
use miette::{SourceCode, SpanContents};
use shackle_compiler::hir::{db::Hir, ids::NodeRef};

pub fn span_contents_to_range(r: &dyn SpanContents) -> lsp_types::Range {
	let mut range = lsp_types::Range::default();
	range.start.line = r.line() as u32;
	range.start.character = r.column() as u32;
	range.end.line = range.start.line;
	range.end.character = range.start.character;

	let mut iter = r.data().iter().copied().peekable();
	while let Some(char) = iter.next() {
		if matches!(char, b'\r' | b'\n') {
			range.end.line += 1;
			range.end.character = 0;
			if char == b'\r' {
				let _ = iter.next_if_eq(&b'\n');
			}
		} else {
			range.end.character += 1;
		}
	}
	range
}

pub fn node_ref_to_location<T: Into<NodeRef>>(
	db: &dyn Hir,
	node: T,
) -> Option<lsp_types::Location> {
	let (src, span) = node.into().source_span(db);
	let span_contents = src.read_span(&span, 0, 0).ok()?;
	let uri = path_to_uri(src.path()?);
	let range = span_contents_to_range(&*span_contents);
	Some(lsp_types::Location { uri, range })
}

pub fn uri_to_path(uri: &Uri) -> PathBuf {
	// TODO: Replace with less ad-hoc implementation
	assert_eq!(
		uri.scheme()
			.expect("Not a file path")
			.as_str()
			.to_lowercase(),
		"file"
	);
	let mut p = PathBuf::new();
	if let Some(auth) = uri.authority() {
		let h = auth.host().as_str();
		if h != "localhost" && !h.is_empty() {
			p.push(format!(
				"{}{}{}{}",
				MAIN_SEPARATOR_STR, MAIN_SEPARATOR_STR, h, MAIN_SEPARATOR_STR
			));
		}
	}
	for segment in uri.path().segments() {
		let s = segment.decode().into_string_lossy().to_string();
		if s.ends_with(":") {
			p.push(format!("{}{}", s, MAIN_SEPARATOR_STR));
		} else {
			p.push(s);
		}
	}
	eprintln!("{:?}", p);
	p
}

pub fn path_to_uri(path: &Path) -> Uri {
	// TODO: Replace with less ad-hoc implementation
	Uri::from_str(path.as_os_str().to_str().unwrap()).unwrap_or_else(|_| {
		let p = path.to_string_lossy().replace("\\", "/");
		let url = format!("file://{}{}", if p.starts_with("/") { "" } else { "/" }, p);
		eprintln!("{:?}", url);
		Uri::from_str(&url).unwrap()
	})
}
