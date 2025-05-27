use std::{
	path::{MAIN_SEPARATOR_STR, Path, PathBuf},
	str::FromStr,
};

use lsp_types::{Position, Uri};
use miette::{SourceCode, SpanContents};
use shackle_hir::{db::Db, ids::NodeRef};

pub(crate) fn span_contents_to_range(r: &dyn SpanContents) -> lsp_types::Range {
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

pub(crate) fn node_ref_to_location<'db, T: Into<NodeRef<'db>>>(
	db: &'db dyn Db,
	node: T,
) -> Option<lsp_types::Location> {
	let (src, span) = node.into().source_span(db);
	let span_contents = src.read_span(&span, 0, 0).ok()?;
	let uri = path_to_uri(src.path()?);
	let range = span_contents_to_range(&*span_contents);
	Some(lsp_types::Location { uri, range })
}

pub(crate) fn uri_to_path(uri: &Uri) -> PathBuf {
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
	p
}

pub(crate) fn path_to_uri(path: &Path) -> Uri {
	// TODO: Replace with less ad-hoc implementation
	Uri::from_str(path.as_os_str().to_str().unwrap()).unwrap_or_else(|_| {
		let p = path.to_string_lossy().replace("\\", "/");
		let url = format!("file://{}{}", if p.starts_with("/") { "" } else { "/" }, p);
		Uri::from_str(&url).unwrap()
	})
}

pub(crate) fn position_to_byte_offset(s: &str, position: Position) -> Option<usize> {
	let mut line = 0;
	let mut col = 0;

	for (byte_idx, ch) in s.char_indices() {
		if line == position.line && col == position.character {
			return Some(byte_idx);
		}

		if ch == '\n' {
			line += 1;
			col = 0;
		} else {
			col += 1;
		}
	}

	// Handle position at end of string
	if line == position.line && col == position.character {
		return Some(s.len());
	}

	None
}
