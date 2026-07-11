//! MiniZinc documentation comment handling.

/// Convert a raw MiniZinc documentation comment to Markdown suitable for an
/// editor hover.
pub fn documentation_markdown(raw: &str) -> String {
	let body = raw
		.strip_prefix("/**")
		.and_then(|s| s.strip_suffix("*/"))
		.unwrap_or(raw)
		.replace('\r', "");
	let body = dedent(&body);
	let mut parameters = Vec::new();
	let mut lines = Vec::new();
	for line in body.lines() {
		let trimmed = line.trim_start();
		if let Some(rest) = trimmed.strip_prefix("@group ") {
			let rest = rest.trim_start();
			let description = rest
				.find(char::is_whitespace)
				.map(|i| rest[i..].trim_start())
				.unwrap_or("");
			if !description.is_empty() {
				lines.push(description.to_owned());
			}
		} else if let Some(rest) = trimmed.strip_prefix("@param ") {
			let (name, description) = rest.split_once(':').unwrap_or((rest, ""));
			parameters.push((name.trim().to_owned(), description.trim().to_owned()));
		} else if !trimmed.starts_with("@groupdef ") {
			lines.push(line.to_owned());
		}
	}

	let mut markdown = convert_markup(lines.join("\n").trim());
	if !parameters.is_empty() {
		if !markdown.is_empty() {
			markdown.push_str("\n\n");
		}
		markdown.push_str("**Parameters**\n");
		for (name, description) in parameters {
			markdown.push_str("\n- `");
			markdown.push_str(&name);
			markdown.push('`');
			if !description.is_empty() {
				markdown.push_str(": ");
				markdown.push_str(&convert_markup(&description));
			}
		}
	}
	markdown.trim().to_owned()
}

fn dedent(input: &str) -> String {
	let input = input.trim_matches('\n');
	let indent = input
		.lines()
		.skip(1)
		.filter(|line| !line.trim().is_empty())
		.map(|line| line.len() - line.trim_start_matches([' ', '\t']).len())
		.min()
		.unwrap_or(0);
	input
		.lines()
		.enumerate()
		.map(|(i, line)| {
			if i == 0 {
				line.trim_start()
			} else {
				&line[line
					.char_indices()
					.nth(indent)
					.map_or(line.len(), |(i, _)| i)..]
			}
		})
		.collect::<Vec<_>>()
		.join("\n")
}

fn convert_markup(input: &str) -> String {
	let mut output = String::new();
	let mut rest = input;
	while !rest.is_empty() {
		if let Some(after) = rest.strip_prefix("``")
			&& let Some(end) = after.find("``")
		{
			output.push('`');
			output.push_str(&after[..end]);
			output.push('`');
			rest = &after[end + 2..];
		} else if let Some(after) = rest.strip_prefix("\\(")
			&& let Some(end) = after.find("\\)")
		{
			output.push_str(&convert_math(after[..end].trim()));
			rest = &after[end + 2..];
		} else if let Some(after) = rest.strip_prefix("\\[")
			&& let Some(end) = after.find("\\]")
		{
			output.push('`');
			output.push_str(after[..end].trim());
			output.push('`');
			rest = &after[end + 2..];
		} else if (rest.starts_with("\\a ") || rest.starts_with("\\p "))
			&& let Some((argument, tail)) = take_argument(&rest[3..])
		{
			output.push('`');
			output.push_str(argument);
			output.push('`');
			rest = tail;
		} else {
			let ch = rest.chars().next().unwrap();
			output.push(ch);
			rest = &rest[ch.len_utf8()..];
		}
	}
	output
}

fn take_argument(input: &str) -> Option<(&str, &str)> {
	let end = input
		.char_indices()
		.find(|(_, c)| !c.is_alphanumeric() && *c != '_')
		.map_or(input.len(), |(i, _)| i);
	(end > 0).then(|| (&input[..end], &input[end..]))
}

fn convert_math(input: &str) -> String {
	let mut result = input.replace("\\a ", "").replace("\\p ", "");
	for command in ["\\text{", "\\mbox{"] {
		while let Some(start) = result.find(command) {
			let content = start + command.len();
			let Some(end) = result[content..].find('}').map(|i| content + i) else {
				break;
			};
			let replacement = result[content..end].to_owned();
			result.replace_range(start..=end, &replacement);
		}
	}
	for (command, symbol) in [
		("\\leftrightarrow", "↔"),
		("\\rightarrow", "→"),
		("\\bigwedge", "⋀"),
		("\\bigvee", "⋁"),
		("\\subseteq", "⊆"),
		("\\supseteq", "⊇"),
		("\\setminus", "∖"),
		("\\emptyset", "∅"),
		("\\notin", "∉"),
		("\\forall", "∀"),
		("\\exists", "∃"),
		("\\oplus", "⊕"),
		("\\ldots", "…"),
		("\\lceil", "⌈"),
		("\\rceil", "⌉"),
		("\\lfloor", "⌊"),
		("\\rfloor", "⌋"),
		("\\sqrt", "√"),
		("\\land", "∧"),
		("\\lor", "∨"),
		("\\lnot", "¬"),
		("\\wedge", "∧"),
		("\\vee", "∨"),
		("\\neq", "≠"),
		("\\leq", "≤"),
		("\\geq", "≥"),
		("\\neg", "¬"),
		("\\not", "¬"),
		("\\le", "≤"),
		("\\ge", "≥"),
		("\\ne", "≠"),
		("\\in", "∈"),
		("\\subset", "⊂"),
		("\\cup", "∪"),
		("\\cap", "∩"),
		("\\sum", "∑"),
		("\\tanh", "tanh"),
		("\\tan", "tan"),
		("\\sinh", "sinh"),
		("\\sin", "sin"),
		("\\cosh", "cosh"),
		("\\cos", "cos"),
		("\\log", "log"),
		("\\ln", "ln"),
	] {
		result = result.replace(command, symbol);
	}
	for (script, unicode) in [
		("_{10}", "₁₀"),
		("_{2}", "₂"),
		("_{i}", "ᵢ"),
		("_{j}", "ⱼ"),
		("_{x}", "ₓ"),
		("_i", "ᵢ"),
		("_j", "ⱼ"),
		(" ^ {x}", "ˣ"),
		(" ^ {y}", "ʸ"),
	] {
		result = result.replace(script, unicode);
	}
	// Preserve escaped set braces while removing LaTeX grouping braces.
	result = result
		.replace("\\{", "\u{e000}")
		.replace("\\}", "\u{e001}")
		.replace("\\ ", " ")
		.replace(['{', '}'], "")
		.replace('\u{e000}', "{")
		.replace('\u{e001}', "}");
	result.split_whitespace().collect::<Vec<_>>().join(" ")
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn converts_standard_library_documentation() {
		assert_eq!(
			documentation_markdown(
				r#"/** @group stdlib.optiontypes.int True if \a x had zero.
    Equivalent to \( \text{occurs}(x) \wedge \text{deopt}(x) = 0 \)
    @param x: The ``optional`` value. */"#,
			),
			"True if `x` had zero.\nEquivalent to occurs(x) ∧ deopt(x) = 0\n\n**Parameters**\n\n- `x`: The `optional` value."
		);
	}

	#[test]
	fn preserves_unknown_math_commands() {
		assert_eq!(
			documentation_markdown(r#"/** \(x \unknown y\) */"#),
			"x \\unknown y"
		);
	}

	#[test]
	fn converts_standard_library_operators() {
		assert_eq!(
			documentation_markdown(r#"/** Return truth value of \(\bigwedge_i \a x[i]\) */"#),
			"Return truth value of ⋀ᵢ x[i]"
		);
		assert_eq!(
			documentation_markdown(
				r#"/** \((\bigvee_i \a x[i]) \lor (\bigvee_j \lnot \a y[j])\) */"#
			),
			"(⋁ᵢ x[i]) ∨ (⋁ⱼ ¬ y[j])"
		);
		assert_eq!(
			documentation_markdown(r#"/** \(\a c = \sum_i \a as[i] * \a bs[i]\) */"#),
			"c = ∑ᵢ as[i] * bs[i]"
		);
	}
}
