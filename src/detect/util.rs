use aho_corasick::{AhoCorasick, Anchored, Input, StartKind};
use once_cell::sync::Lazy;

pub(crate) fn get_lines(text: &str, lines: usize) -> &str {
    if lines == 0 {
        return text;
    }
    static LINE_SEARCHER: Lazy<AhoCorasick> = Lazy::new(|| AhoCorasick::new(["\n"]).unwrap());
    match LINE_SEARCHER.find_iter(text).nth(lines) {
        Some(mat) => &text[..mat.end()],
        None => text,
    }
}

pub(crate) fn find(
    text: &str,
    lines: usize,
    case_sensitive: bool,
    pattern: impl AsRef<[u8]>,
) -> bool {
    findany(text, lines, case_sensitive, [pattern])
}

pub(crate) fn findany<I, P>(text: &str, lines: usize, case_sensitive: bool, patterns: I) -> bool
where
    I: IntoIterator<Item = P>,
    P: AsRef<[u8]>,
{
    AhoCorasick::builder()
        .ascii_case_insensitive(!case_sensitive)
        .build(patterns)
        .unwrap()
        .is_match(get_lines(text, lines))
}

pub(crate) fn starts_with_any<I, P>(line: &str, case_sensitive: bool, patterns: I) -> bool
where
    I: IntoIterator<Item = P>,
    P: AsRef<[u8]>,
{
    AhoCorasick::builder()
        .ascii_case_insensitive(!case_sensitive)
        .start_kind(StartKind::Anchored)
        .build(patterns)
        .unwrap()
        .is_match(Input::new(line).anchored(Anchored::Yes))
}

pub(crate) fn next_non_blank(text: &str, start_num: usize) -> Option<&str> {
    text.lines()
        .skip(start_num)
        .find(|line| !line.trim_start().is_empty())
}
