use std::path::Path;

use aho_corasick::AhoCorasick;
use lazy_regex::{lazy_regex, regex, regex_is_match};
use once_cell::sync::Lazy;
use regex::Regex;

use self::{
    file_extension::FILE_EXTENSION, filename::FILENAME, path_suffix::PATH_SUFFIX, pattern::PATTERN,
};
use crate::FileType;

mod file_extension;
mod filename;
mod path_suffix;
mod pattern;
mod util;

/// Same as [`try_detect`] but automatically falling back to [`FileType::Text`] where
/// [`try_detect`] would return [`None`].
///
/// # Example
/// ```
/// use tft::FileType;
///
/// assert_eq!(FileType::Rust, tft::detect("main.rs", ""));
/// assert_eq!(FileType::Text, tft::detect("test.txt", ""));
/// assert_eq!(FileType::Text, tft::detect("unsupported.filetype", ""));
/// ```
pub fn detect(path: impl AsRef<Path>, content: &str) -> FileType {
    try_detect(path, content).unwrap_or(FileType::Text)
}

/// Try to detect a [`FileType`] given a file's path and content.
///
/// # Example
/// ```
/// use tft::FileType;
///
/// assert_eq!(Some(FileType::Rust), tft::try_detect("main.rs", ""));
/// assert_eq!(Some(FileType::Text), tft::try_detect("test.txt", ""));
/// assert_eq!(None, tft::try_detect("unsupported.filetype", ""));
/// ```
pub fn try_detect(path: impl AsRef<Path>, content: &str) -> Option<FileType> {
    let path = path.as_ref();

    // path suffix
    for (suffix, resolver) in PATH_SUFFIX {
        if path.ends_with(suffix) {
            if let Some(ft) = resolver.resolve(path, content) {
                return Some(ft);
            }
        }
    }

    // filename
    if let Some(resolver) = path
        .file_name()
        .and_then(|os_name| os_name.to_str())
        .and_then(|filename| FILENAME.get(filename))
    {
        if let Some(ft) = resolver.resolve(path, content) {
            return Some(ft);
        }
    }

    // patterns (non-negative priority)
    let mut negative_prio_start_index = 0;
    for (index, (match_full_path, regex, pat)) in PATTERN.iter().enumerate() {
        if pat.priority.map_or(false, |prio| prio < 0) {
            negative_prio_start_index = index;
            break;
        }
        if match match_full_path {
            true => path.to_str(),
            false => path.file_name().and_then(|os_name| os_name.to_str()),
        }
        .map_or(true, |haystack| !regex.is_match(haystack))
        {
            continue;
        }
        if let Some(ft) = pat.resolver.resolve(path, content) {
            return Some(ft);
        }
    }

    // file extension
    if let Some(resolver) = path
        .extension()
        .and_then(|os_ext| os_ext.to_str())
        .and_then(|ext| FILE_EXTENSION.get(ext))
    {
        if let Some(ft) = resolver.resolve(path, content) {
            return Some(ft);
        }
    }

    // patterns (negative priority)
    for (match_full_path, regex, pat) in PATTERN.iter().skip(negative_prio_start_index) {
        if match match_full_path {
            true => path.to_str(),
            false => path.file_name().and_then(|os_name| os_name.to_str()),
        }
        .map_or(true, |haystack| !regex.is_match(haystack))
        {
            continue;
        }
        if let Some(ft) = pat.resolver.resolve(path, content) {
            return Some(ft);
        }
    }

    // file contents
    // TODO: try to guess from content (make public as separate function)

    None
}

fn asa(_path: &Path, _content: &str) -> Option<FileType> {
    // TODO: user defined preferred asa filetype
    Some(FileType::AspVbs)
}

fn asm(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred asm syntax
    match util::findany(
        content,
        10,
        true,
        [".title", ".ident", ".macro", ".subtitle", ".library"],
    ) {
        true => Some(FileType::Vmasm),
        false => Some(FileType::Asm),
    }
}

fn asp(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred asp filetype
    match util::find(content, 3, false, "perlscript") {
        true => Some(FileType::AspPerl),
        false => Some(FileType::AspVbs),
    }
}

fn bak(path: &Path, content: &str) -> Option<FileType> {
    // for files like `main.rs.bak` retry search without the `.bak` extension
    try_detect(path.with_extension(""), content)
}

const VISUAL_BASIC_CONTENT: &[&str] = &[
    "vb_name",
    "begin vb.form",
    "begin vb.mdiform",
    "begin vb.usercontrol",
];

fn bas(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred bas filetype
    // Most frequent FreeBASIC-specific keywords in distro files
    let fb_keywords = regex!(
        r"^\s*(extern|var|enum|private|scope|union|byref|operator|constructor|delete|namespace|public|property|with|destructor|using)\b(\s*[:=(])\@!"i
    );
    let fb_preproc = regex!(
        r"^\s*(#\s*\a+|option\s+(byval|dynamic|escape|(no)?gosub|nokeyword|private|static)\b|(''|rem)\s*\$lang\b|def(byte|longint|short|ubyte|uint|ulongint|ushort)\b)"i
    );

    let fb_comment = regex!(r"^\s*/'");
    // OPTION EXPLICIT, without the leading underscore, is common to many dialects
    let qb64_preproc = regex!(r"^\s*($\a+|option\s+(_explicit|_?explicitarray)\b)"i);

    for line in content.lines().take(100) {
        if util::findany(line, 0, false, VISUAL_BASIC_CONTENT) {
            return Some(FileType::Vb);
        } else if fb_comment.is_match(line)
            || fb_preproc.is_match(line)
            || fb_keywords.is_match(line)
        {
            return Some(FileType::FreeBasic);
        } else if qb64_preproc.is_match(line) {
            return Some(FileType::Qb64);
        }
    }
    Some(FileType::Basic)
}

fn bindzone(content: &str, default: Option<FileType>) -> Option<FileType> {
    match regex_is_match!(
        r"^; <<>> DiG [0-9\.]+.* <<>>|\$ORIGIN|\$TTL|IN\s+SOA",
        util::get_lines(content, 4)
    ) {
        true => Some(FileType::Bindzone),
        false => default,
    }
}

fn btm(_path: &Path, _content: &str) -> Option<FileType> {
    // TODO: user defined dosbatch for btm
    Some(FileType::Btm)
}

fn cfg(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred cfg filetype
    match regex_is_match!(
        r"(eio|mmc|moc|proc|sio|sys):cfg"i,
        util::get_lines(content, 1)
    ) {
        true => Some(FileType::Rapid),
        false => Some(FileType::Cfg),
    }
}

/// If the first line starts with # or ! it's probably a ch file.
/// If one of the first ten lines starts with `@` it's probably a change file.
/// If one of the first ten lines contains `MODULE` it's probably a CHILL file.
/// If a line has `main`, `#include`, or `//` it's probably ch.
/// Otherwise CHILL is assumed.
fn change(_path: &Path, content: &str) -> Option<FileType> {
    if regex_is_match!(r"^(#|!)", util::get_lines(content, 1)) {
        return Some(FileType::Ch);
    }
    for line in content.lines().take(10) {
        if line.starts_with('@') {
            return Some(FileType::Change);
        }
        if util::find(line, 0, true, "MODULE") {
            return Some(FileType::Chill);
        }
        if regex_is_match!(r"main\s*\(|#\s*include|//"i, line) {
            return Some(FileType::Ch);
        }
    }
    Some(FileType::Chill)
}

fn changelog(_path: &Path, content: &str) -> Option<FileType> {
    match util::find(content, 1, false, "; urgency=") {
        true => Some(FileType::DebChangelog),
        false => Some(FileType::Changelog),
    }
}

fn cls(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred cls filetype
    let first_line = util::get_lines(content, 1);
    if regex_is_match!(r"^[%\\]", first_line) {
        Some(FileType::Tex)
    } else if first_line.starts_with('#')
        && AhoCorasick::builder()
            .ascii_case_insensitive(true)
            .build(["rexx"])
            .unwrap()
            .is_match(first_line)
    {
        Some(FileType::Rexx)
    } else if first_line == "VERSION 1.0 CLASS" {
        Some(FileType::Vb)
    } else {
        Some(FileType::St)
    }
}

fn cmd(_path: &Path, content: &str) -> Option<FileType> {
    match content.starts_with("/*") {
        true => Some(FileType::Rexx),
        false => Some(FileType::DosBatch),
    }
}

fn control(_path: &Path, content: &str) -> Option<FileType> {
    match content.starts_with("Source:") {
        true => Some(FileType::DebControl),
        false => None,
    }
}

fn copyright(_path: &Path, content: &str) -> Option<FileType> {
    match content.starts_with("Format:") {
        true => Some(FileType::DebCopyright),
        false => None,
    }
}

fn cpp(_path: &Path, _content: &str) -> Option<FileType> {
    // TODO: user defined cynlib for cpp
    Some(FileType::Cpp)
}

fn cpy(_path: &Path, content: &str) -> Option<FileType> {
    match content.starts_with("##") {
        true => Some(FileType::Python),
        false => Some(FileType::Cobol),
    }
}

fn csh(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred csh filetype
    // TODO: user defined preferred shell filetype
    shell(content, FileType::Csh)
}

fn dat(path: &Path, content: &str) -> Option<FileType> {
    if path
        .file_name()
        .and_then(|os_name| os_name.to_str())
        .map_or(
            false,
            |name| regex_is_match!(r"^((.*\.)?upstream\.dat|upstream\..*\.dat)$"i, name),
        )
    {
        return Some(FileType::UpstreamDat);
    }
    // TODO: user defined preferred dat filetype
    match util::next_non_blank(content, 0)
        .map_or(false, |line| regex_is_match!(r"^\s*(&\w+|defdat\b)"i, line))
    {
        true => Some(FileType::Krl),
        false => None,
    }
}

fn decl(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(3) {
        if regex_is_match!(r"^<!sgml"i, line) {
            return Some(FileType::SgmlDecl);
        }
    }
    None
}

fn dep3patch(path: &Path, content: &str) -> Option<FileType> {
    let filename = path.file_name()?.to_str()?;
    if filename == "series" {
        return None;
    }
    for line in content.lines().take(100) {
        if util::starts_with_any(
            line,
            true,
            [
                "Description:",
                "Subject:",
                "Origin:",
                "Bug:",
                "Forwarded:",
                "Author:",
                "From:",
                "Reviewed-by:",
                "Acked-by:",
                "Last-Updated:",
                "Applied-Upstream:",
            ],
        ) {
            return Some(FileType::Dep3Patch);
        } else if line.starts_with("---") {
            // end of headers found. stop processing
            return None;
        }
    }
    None
}

fn dsl(_path: &Path, content: &str) -> Option<FileType> {
    match regex_is_match!(r"^\s*<!", util::get_lines(content, 1)) {
        true => Some(FileType::Dsl),
        false => Some(FileType::Structurizr),
    }
}

fn dtrace(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(100) {
        if regex_is_match!(r"^(module|import)\b"i, line) {
            return Some(FileType::D);
        } else if regex_is_match!(r"'^#!\S+dtrace|#pragma\s+D\s+option|:\S-:\S-:", line) {
            return Some(FileType::DTrace);
        }
    }
    Some(FileType::D)
}

fn e(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred euphoria filetype
    for line in content.lines().take(100) {
        if regex_is_match!(r"^\s*<'\s*$|^\s*'>\s*$", line) {
            return Some(FileType::SpecMan);
        }
    }
    Some(FileType::Eiffel)
}

fn edn(_path: &Path, content: &str) -> Option<FileType> {
    match regex_is_match!(r"^\s*\(\s*edif\b"i, util::get_lines(content, 1)) {
        true => Some(FileType::Edif),
        false => Some(FileType::Clojure),
    }
}

fn ent(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(5) {
        if regex_is_match!(r"^\s*[#{]", line) {
            return Some(FileType::Cl);
        } else if !line.trim_start().is_empty() {
            // not a blank line, not a comment, and not a block start,
            // so doesn't look like valid cl code
            break;
        }
    }
    Some(FileType::Dtd)
}

fn euphoria(_path: &Path, _content: &str) -> Option<FileType> {
    // TODO: user defined preferred euphoria filetype
    Some(FileType::Euphoria3)
}

fn ex(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred euphoria filetype
    for line in content.lines().take(100) {
        if regex_is_match!(r"^(--|ifdef\b|include\b)", line) {
            return Some(FileType::Euphoria3);
        }
    }
    Some(FileType::Elixir)
}

fn foam(_path: &Path, content: &str) -> Option<FileType> {
    let mut foam_file = false;
    for line in content.lines().take(15) {
        if line.starts_with("FoamFile") {
            foam_file = true;
        } else if foam_file && line.trim_start().starts_with("object") {
            return Some(FileType::Foam);
        }
    }
    None
}

fn frm(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred frm filetype
    match util::findany(content, 5, false, VISUAL_BASIC_CONTENT) {
        true => Some(FileType::Vb),
        false => Some(FileType::Form),
    }
}

fn fs(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred fs filetype
    for line in content.lines().take(100) {
        if line.starts_with([':', '(', '\\']) {
            return Some(FileType::Forth);
        }
    }
    Some(FileType::FSharp)
}

fn fvwm(path: &Path, _content: &str) -> Option<FileType> {
    match path.extension().map_or(false, |ext| ext == "m4") {
        true => Some(FileType::Fvwm2M4),
        false => Some(FileType::Fvwm2),
    }
}

fn git(_path: &Path, content: &str) -> Option<FileType> {
    match regex_is_match!(r"^[a-fA-F0-9]{40,}\b|^ref: ", util::get_lines(content, 1)) {
        true => Some(FileType::Git),
        false => None,
    }
}

fn header(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(200) {
        if regex_is_match!(r"^@(interface|end|class)"i, line) {
            // TODO: allow setting C or C++
            return Some(FileType::ObjC);
        }
    }
    // TODO: user defined preferred header filetype
    Some(FileType::C)
}

fn hook(_path: &Path, content: &str) -> Option<FileType> {
    match util::get_lines(content, 1) == "[Trigger]" {
        true => Some(FileType::Conf),
        false => None,
    }
}

fn html(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(10) {
        if regex_is_match!(r"\bDTD\s+XHTML\s", line) {
            return Some(FileType::Xhtml);
        } else if regex_is_match!(r"\{%\s*(extends|block|load)\b|\{#\s+"i, line) {
            return Some(FileType::HtmlDjango);
        }
    }
    Some(FileType::Html)
}

fn hw(_path: &Path, content: &str) -> Option<FileType> {
    match util::find(content, 1, false, "<?php") {
        true => Some(FileType::Php),
        false => Some(FileType::Virata),
    }
}

fn idl(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(50) {
        if regex_is_match!(r#"^\s*import\s+"(unknwn|objidl)"\.idl"#i, line) {
            return Some(FileType::Msidl);
        }
    }
    Some(FileType::Idl)
}

fn in_(path: &Path, content: &str) -> Option<FileType> {
    if path
        .file_name()
        .map_or(false, |name| name == "configure.in")
    {
        return bak(path, content);
    }
    None
}

static PASCAL_KEYWORDS: Lazy<Regex> =
    lazy_regex!(r"^\s*(program|unit|library|uses|begin|procedure|function|const|type|var)\b"i);
static PASCAL_COMMENTS: Lazy<Regex> = lazy_regex!(r"^\s*(\{|\(\*|//)");

fn inc(path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred inc filetype
    let lines = util::get_lines(content, 3);
    if util::find(lines, 0, false, "perlscript") {
        Some(FileType::AspPerl)
    } else if util::find(lines, 0, false, "<%") {
        Some(FileType::AspVbs)
    } else if util::find(lines, 0, false, "<?") {
        Some(FileType::Php)
    } else if regex_is_match!(r"^\s(\{|\(\*)"i, lines) || PASCAL_KEYWORDS.is_match(lines) {
        Some(FileType::Pascal)
    } else if regex_is_match!(
        // TODO: is this regex correct?
        r"^\s*(inherit|require|[A-Z][\w_:${}]*\s+\??[?:+]?=) "i,
        lines
    ) {
        Some(FileType::Bitbake)
    } else if let Some(ft) = asm(path, content) {
        match ft {
            FileType::Asm => Some(FileType::Pov),
            _ => Some(ft),
        }
    } else {
        Some(FileType::Pov)
    }
}

fn inp(_path: &Path, content: &str) -> Option<FileType> {
    if content.starts_with('*') {
        return Some(FileType::Abaqus);
    }
    for line in content.lines().take(500) {
        if util::starts_with_any(line, false, ["header surface data"]) {
            return Some(FileType::Trasys);
        }
    }
    None
}

fn install(_path: &Path, content: &str) -> Option<FileType> {
    match util::find(content, 1, false, "<?php") {
        true => Some(FileType::Php),
        false => sh(content, Some(FileType::Bash)),
    }
}

fn log(path: &Path, _content: &str) -> Option<FileType> {
    let path = path.to_str();
    if path.map_or(
        false,
        |path| regex_is_match!(r"upstream([.-].*)?\.log|.*\.upstream\.log"i, path),
    ) {
        Some(FileType::UpstreamLog)
    } else if path.map_or(false, |path| {
        regex_is_match!(
            r"upstreaminstall(\..*)?\.log|.*\.upstreaminstall\.log"i,
            path
        )
    }) {
        Some(FileType::UpstreamInstallLog)
    } else if path.map_or(
        false,
        |path| regex_is_match!(r"usserver(\..*)?\.log|.*\.usserver\.log"i, path),
    ) {
        Some(FileType::UsServerLog)
    } else if path.map_or(
        false,
        |path| regex_is_match!(r"usw2kagtlog(\..*)?\.log|.*\.usw2kagtlog\.log"i, path),
    ) {
        Some(FileType::Usw2KagtLog)
    } else {
        None
    }
}

fn lpc(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined allow lpc
    for line in content.lines().take(12) {
        if util::starts_with_any(
            line,
            true,
            [
                "//",
                "inherit",
                "private",
                "protected",
                "nosave",
                "string",
                "object",
                "mapping",
                "mixed",
            ],
        ) {
            return Some(FileType::Lpc);
        }
    }
    Some(FileType::C)
}

fn lsl(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred lsl filetype
    match util::next_non_blank(content, 0)
        .map_or(false, |line| regex_is_match!(r"^\s*%|:\s*trait\s*$", line))
    {
        true => Some(FileType::Larch),
        false => Some(FileType::Lsl),
    }
}

fn m(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred m filetype
    let octave_block_terminators = regex!(
        r"(^|;)\s*\bend(_try_catch|classdef|enumeration|events|methods|parfor|properties)\b"i
    );
    let objc_preprocessor =
        regex!(r"^\s*#\s*(import|include|define|if|ifn?def|undef|line|error|pragma)\b"i);

    let mut saw_comment = false;
    for line in content.lines().take(100) {
        let trimmed_line = line.trim_start();
        if trimmed_line.starts_with("/*") {
            // /* ... */ is a comment in Objective C and Murphi, so we can't conclude
            // it's either of them yet, but track this as a hint in case we don't see
            // anything more definitive.
            saw_comment = true;
        }
        if trimmed_line.starts_with("//")
            || util::starts_with_any(trimmed_line, false, ["@import"])
            || objc_preprocessor.is_match(line)
        {
            return Some(FileType::ObjC);
        } else if util::starts_with_any(trimmed_line, false, ["#", "%%!", "unwind_protect"])
            || octave_block_terminators.is_match(line)
        {
            return Some(FileType::Octave);
        } else if trimmed_line.starts_with("%%") {
            return Some(FileType::Matlab);
        } else if trimmed_line.starts_with("(*") {
            return Some(FileType::Mma);
        } else if regex_is_match!(r"^\s*((type|var)\b|--)"i, line) {
            return Some(FileType::Murphi);
        }
    }

    match saw_comment {
        // We didn't see anything definitive, but this looks like either Objective C
        // or Murphi based on the comment leader. Assume the former as it is more
        // common.
        true => Some(FileType::ObjC),
        // default is Matlab
        false => Some(FileType::Matlab),
    }
}

fn m4_ext(path: &Path, _content: &str) -> Option<FileType> {
    match !path.to_str().map_or(false, |p| p.ends_with("html.m4"))
        && !path.to_str().map_or(false, |p| p.contains("fvwm2rc"))
    {
        true => Some(FileType::M4),
        false => None,
    }
}

fn mc(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(20) {
        let trimmed_line = line.trim_start();
        if util::starts_with_any(trimmed_line, false, ["#", "dnl"]) {
            return Some(FileType::M4);
        } else if trimmed_line.starts_with(';') {
            return Some(FileType::MsMessages);
        }
    }
    Some(FileType::M4)
}

fn me(path: &Path, _content: &str) -> Option<FileType> {
    match path.file_name().map_or(false, |name| {
        name.eq_ignore_ascii_case("read.me") || name.eq_ignore_ascii_case("click.me")
    }) {
        true => None,
        false => Some(FileType::Nroff),
    }
}

fn mm(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(20) {
        if regex_is_match!(r"^\s*(#\s*(include|import)\b|@import\b|/\*)"i, line) {
            return Some(FileType::ObjCpp);
        }
    }
    Some(FileType::Nroff)
}

fn mms(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(20) {
        let trimmed_line = line.trim_start();
        if util::starts_with_any(trimmed_line, true, ["%", "//", "*"]) {
            return Some(FileType::Mmix);
        } else if trimmed_line.starts_with('#') {
            return Some(FileType::Make);
        }
    }
    Some(FileType::Mmix)
}

fn is_lprolog(content: &str) -> bool {
    for line in content.lines().take(500) {
        let trimmed_line = line.trim_start();
        if !trimmed_line.is_empty() && !trimmed_line.starts_with('%') {
            return regex_is_match!(r"\bmodule\s+\w+\s*\.\s*(%|$)"i, line);
        }
    }
    false
}

fn is_rapid(content: &str) -> bool {
    util::next_non_blank(content, 0).map_or(
        false,
        |line| regex_is_match!(r"^\s*(%{3}|module\s+\w+\s*(\(|$))"i, line),
    )
}

fn mod_(path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred mod filetype
    if path
        .file_name()
        .map_or(false, |name| name.eq_ignore_ascii_case("go.mod"))
    {
        Some(FileType::GoMod)
    } else if is_lprolog(content) {
        Some(FileType::LambdaProlog)
    } else if util::next_non_blank(content, 0).map_or(false, |line| {
        regex_is_match!(r"(\bMODULE\s+\w+\s*;|^\s*\(\*)", line)
    }) {
        Some(FileType::Modula2)
    } else if is_rapid(content) {
        Some(FileType::Rapid)
    } else {
        Some(FileType::Modsim3)
    }
}

fn news(_path: &Path, content: &str) -> Option<FileType> {
    match util::find(content, 1, false, "; urgency=") {
        true => Some(FileType::DebChangelog),
        false => None,
    }
}

fn nroff(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(5) {
        if line.starts_with('.') {
            return Some(FileType::Nroff);
        }
    }
    None
}

fn patch(_path: &Path, content: &str) -> Option<FileType> {
    match regex_is_match!(
        r"^From [a-fA-F0-9]{40}+ Mon Sep 17 00:00:00 2001$",
        util::get_lines(content, 1)
    ) {
        true => Some(FileType::GitSendEmail),
        false => Some(FileType::Diff),
    }
}

fn perl(path: &Path, content: &str) -> Option<FileType> {
    match (path.extension().map_or(false, |ext| ext == "t")
        && path
            .parent()
            .and_then(|p| p.file_name())
            .map_or(false, |dir| dir == "t" || dir == "xt"))
        || (content.starts_with('#') && util::find(content, 1, false, "perl"))
        || content
            .lines()
            .take(30)
            .any(|line| util::starts_with_any(line.trim_start(), false, ["use"]))
    {
        true => Some(FileType::Perl),
        false => None,
    }
}

fn pl(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred pl filetype
    match util::next_non_blank(content, 0).map_or(
        false,
        |line| regex_is_match!(r":-|\bprolog\b|^\s*(%+(\s|$)|/\*)"i, line),
    ) {
        true => Some(FileType::Prolog),
        false => Some(FileType::Perl),
    }
}

fn pm(_path: &Path, content: &str) -> Option<FileType> {
    let line = util::get_lines(content, 1);
    if line.contains("XPM2") {
        Some(FileType::Xpm2)
    } else if line.contains("XPM") {
        Some(FileType::Xpm)
    } else {
        Some(FileType::Perl)
    }
}

fn pp(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred pp filetype
    match util::next_non_blank(content, 0).map_or(false, |line| {
        PASCAL_COMMENTS.is_match(line) || PASCAL_KEYWORDS.is_match(line)
    }) {
        true => Some(FileType::Pascal),
        false => Some(FileType::Puppet),
    }
}

fn prg(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred prg filetype
    match is_rapid(content) {
        true => Some(FileType::Rapid),
        false => Some(FileType::Clipper),
    }
}

fn progress_asm(path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred i filetype
    for line in content.lines().take(10) {
        let trimmed_line = line.trim_start();
        if trimmed_line.starts_with(';') {
            return asm(path, content);
        } else if trimmed_line.starts_with("/*") {
            break;
        }
    }
    Some(FileType::Progress)
}

fn progress_cweb(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred w filetype
    match util::starts_with_any(content, false, ["&analyze"])
        || content
            .lines()
            .take(3)
            .any(|line| util::starts_with_any(line, false, ["&global-define"]))
    {
        true => Some(FileType::Progress),
        false => Some(FileType::Cweb),
    }
}

fn progress_pascal(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred p filetype
    for line in content.lines().take(10) {
        if PASCAL_COMMENTS.is_match(line) || PASCAL_KEYWORDS.is_match(line) {
            return Some(FileType::Pascal);
        } else if line.trim_start().starts_with("/*") {
            break;
        }
    }
    Some(FileType::Progress)
}

/// Distinguish between `default`, Prolog, and Cproto files.
fn proto(content: &str, default: FileType) -> Option<FileType> {
    // Cproto files have a comment in the first line and a function prototype in
    // the second line, it always ends in `;`. Indent files may also have
    // comments, thus we can't match comments to see the difference.
    // IDL files can have a single `;` in the second line, require at least one
    // character before the `;`.
    if regex_is_match!(r".;$", util::get_lines(content, 2)) {
        // second line ends with `;`
        return Some(FileType::Cpp);
    }
    // recognize Prolog by specific text in the first non-empty line;
    // require a blank after the `%` because Perl uses `%list` and `%translate`
    match util::next_non_blank(content, 0).map_or(
        false,
        |line| regex_is_match!(r":-|\bprolog\b|^\s*(%+(\s|$)|/\*)"i, line),
    ) {
        true => Some(FileType::Prolog),
        false => Some(default),
    }
}

fn psf(_path: &Path, content: &str) -> Option<FileType> {
    let trimmed_line = util::get_lines(content, 1).trim();
    match [
        "distribution",
        "installed_software",
        "root",
        "bundle",
        "product",
    ]
    .into_iter()
    .any(|pat| trimmed_line.eq_ignore_ascii_case(pat))
    {
        true => Some(FileType::Psf),
        false => None,
    }
}

fn r(_path: &Path, content: &str) -> Option<FileType> {
    // Rebol is easy to recognize, check for that first
    if regex_is_match!(r"\brebol\b"i, util::get_lines(content, 50)) {
        return Some(FileType::Rebol);
    }

    for line in content.lines().take(50) {
        let trimmed_line = line.trim_start();
        // R has # comments
        if trimmed_line.starts_with('#') {
            return Some(FileType::R);
        }
        // Rexx has /* comments */
        if trimmed_line.starts_with("/*") {
            return Some(FileType::Rexx);
        }
    }
    // TODO: user defined preferred r filetype
    Some(FileType::R)
}

fn rc(path: &Path, _content: &str) -> Option<FileType> {
    match path
        .to_str()
        .map_or(false, |str| str.contains("/etc/Muttrc.d/"))
    {
        true => None,
        false => Some(FileType::Rc),
    }
}

fn redif(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(5) {
        if util::starts_with_any(line, false, ["template-type:"]) {
            return Some(FileType::Redif);
        }
    }
    None
}

fn reg(_path: &Path, content: &str) -> Option<FileType> {
    match regex_is_match!(
        r"^regedit[0-9]*\s*$|^windows registry editor version \d*\.\d*\s*$"i,
        util::get_lines(content, 1)
    ) {
        true => Some(FileType::Registry),
        false => None,
    }
}

fn rul(_path: &Path, content: &str) -> Option<FileType> {
    match util::find(content, 6, false, "installshield") {
        true => Some(FileType::InstallShield),
        false => Some(FileType::Diva),
    }
}

fn rules(path: &Path, _content: &str) -> Option<FileType> {
    let utf8_path = path.to_str();
    if utf8_path.map_or(
        false,
        |p| regex_is_match!(r"/(etc|(usr/)?lib)/udev/(rules\.d/)?.*\.rules$"i, p),
    ) {
        Some(FileType::UdevRules)
    } else if path.starts_with("/etc/ufw") {
        Some(FileType::Conf)
    } else if utf8_path.map_or(
        false,
        |p| regex_is_match!(r"/(etc|usr/share)/polkit-1/rules\.d/"i, p),
    ) {
        Some(FileType::JavaScript)
    } else {
        // TODO: maybe try to read udev.conf for other paths
        Some(FileType::Hog)
    }
}

fn sc(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(25) {
        if regex_is_match!(r"(class)?var\s<|\^this.*|\|\w+\||\+\s\w*\s\{|\*ar\s", line) {
            return Some(FileType::Supercollider);
        }
    }
    Some(FileType::Scala)
}

fn scd(_path: &Path, content: &str) -> Option<FileType> {
    match regex_is_match!(
        r#"^\S+\(\d[0-9A-Za-z]*\)(\s+"[^"]*"]){0,2}"#,
        util::get_lines(content, 1)
    ) {
        true => Some(FileType::Scdoc),
        false => Some(FileType::Supercollider),
    }
}

fn sgml(_path: &Path, content: &str) -> Option<FileType> {
    let lines = util::get_lines(content, 5);
    if lines.contains("linuxdoc") {
        Some(FileType::Smgllnx)
    } else if regex_is_match!(r"<!DOCTYPE[\s\S]*DocBook", lines) {
        Some(FileType::DocBookSgml4)
    } else {
        Some(FileType::Sgml)
    }
}

fn sh(content: &str, dialect: Option<FileType>) -> Option<FileType> {
    let dialect = dialect.unwrap_or_else(|| {
        let first_line = util::get_lines(content, 1);
        // try to detect from shebang
        if !regex_is_match!(r"^\s*#!", first_line) {
            FileType::Sh
        } else if regex_is_match!(r"\bcsh\b"i, first_line) {
            FileType::Csh
        } else if regex_is_match!(r"\btcsh\b"i, first_line) {
            FileType::Tcsh
        } else if regex_is_match!(r"\bzsh\b"i, first_line) {
            FileType::Zsh
        } else if regex_is_match!(r"\bksh\b"i, first_line) {
            FileType::Ksh
        } else if regex_is_match!(r"\b(bash|bash2)\b"i, first_line) {
            FileType::Bash
        } else {
            FileType::Sh
        }
    });
    shell(content, dialect)
}

fn shell(content: &str, dialect: FileType) -> Option<FileType> {
    let mut prev_line = "";
    for (line_num, line) in content.lines().enumerate().take(1000) {
        // skip the first line
        if line_num == 0 {
            prev_line = line;
            continue;
        }

        if regex_is_match!(r"\s*exec\s+(\S*/)?(tclsh|wish)"i, line)
            && !regex_is_match!(r"^\s*#.*\\$"i, prev_line)
        {
            // found an "exec" line with `tclsh` or `wish` after a comment with continuation
            return Some(FileType::Tcl);
        }

        prev_line = line;
    }
    Some(dialect)
}

fn sig(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred sig filetype
    let line = util::next_non_blank(content, 0)?;
    if regex_is_match!(r"^\s*(/\*|%|sig\s+[a-zA-Z])", line) {
        Some(FileType::LambdaProlog)
    } else if regex_is_match!(r"^\s*(\(\*|(signature|structure)\s+[a-zA-Z])", line) {
        Some(FileType::Sml)
    } else {
        None
    }
}

fn sil(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(100) {
        let trimmed_line = line.trim_start();
        if trimmed_line.starts_with(['\\', '%']) {
            return Some(FileType::Sile);
        } else if !trimmed_line.is_empty() {
            return Some(FileType::Sil);
        }
    }
    Some(FileType::Sil)
}

fn smi(_path: &Path, content: &str) -> Option<FileType> {
    match regex_is_match!(r"\bsmil\b"i, util::get_lines(content, 1)) {
        true => Some(FileType::Smil),
        false => Some(FileType::Mib),
    }
}

fn smil(_path: &Path, content: &str) -> Option<FileType> {
    match regex_is_match!(r"<\?\s*xml.*\?>", util::get_lines(content, 1)) {
        true => Some(FileType::Xml),
        false => Some(FileType::Smil),
    }
}

fn sql(_path: &Path, _content: &str) -> Option<FileType> {
    // TODO: user defined preferred sql filetype
    Some(FileType::Sql)
}

fn src(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred src filetype
    match util::next_non_blank(content, 0).map_or(
        false,
        |line| regex_is_match!(r"^\s*(&\w+|(global\s+)?def(fct)?\b)"i, line),
    ) {
        true => Some(FileType::Krl),
        false => None,
    }
}

fn sys(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred sys filetype
    match is_rapid(content) {
        true => Some(FileType::Rapid),
        false => Some(FileType::Bat),
    }
}

/// Choose context, plaintex, or tex (LaTeX)
fn tex(path: &Path, content: &str) -> Option<FileType> {
    let first_line = util::get_lines(content, 1);
    if regex_is_match!(r"^%&\s*plain(tex)?", first_line) {
        Some(FileType::PlainTex)
    } else if regex_is_match!(r"^%&\s*context", first_line)
        || path
            .to_str()
            .map_or(false, |p| regex_is_match!(r"tex/context/.*/.*\.tex"i, p))
    {
        Some(FileType::Context)
    } else {
        let latex_regex =
            regex!(r"^\s*\\(documentclass\b|usepackage\b|begin\{|newcommand\b|renewcommand\b)"i);
        let context_regex = regex!(
            r"^\s*\\(start[a-zA-Z]+|setup[a-zA-Z]+|usemodule|enablemode|enableregime|setvariables|useencoding|usesymbols|stelle[a-zA-Z]+|verwende[a-zA-Z]+|stel[a-zA-Z]+|gebruik[a-zA-Z]+|usa[a-zA-Z]+|imposta[a-zA-Z]+|regle[a-zA-Z]+|utilisemodule\b)"i
        );

        for line in content
            .lines()
            .skip_while(|line| regex_is_match!(r"^\s*%\S", line))
            .take(1000)
        {
            if latex_regex.is_match(line) {
                return Some(FileType::Tex);
            } else if context_regex.is_match(line) {
                return Some(FileType::Context);
            }
        }

        Some(FileType::Tex)
    }
}

fn tf(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines() {
        let trimmed_line = line.trim_start();
        if !trimmed_line.is_empty() && !trimmed_line.starts_with([';', '/']) {
            return Some(FileType::Terraform);
        }
    }
    Some(FileType::Tf)
}

fn tmp(path: &Path, content: &str) -> Option<FileType> {
    // for files like `main.rs~` retry search without the `~` suffix
    path.file_name()
        .and_then(|os_str| os_str.to_str())
        .and_then(|name| try_detect(path.with_file_name(&name[..name.len() - 1]), content))
}

fn ts(_path: &Path, content: &str) -> Option<FileType> {
    match regex_is_match!(r"<\?\s*xml", util::get_lines(content, 1)) {
        true => Some(FileType::Xml),
        false => Some(FileType::Smil),
    }
}

fn ttl(_path: &Path, content: &str) -> Option<FileType> {
    match regex_is_match!(r"^@?(prefix|base)", util::get_lines(content, 1)) {
        true => Some(FileType::Turtle),
        false => Some(FileType::Teraterm),
    }
}

fn txt(_path: &Path, content: &str) -> Option<FileType> {
    // vim helpfiles match *.txt but should have a modeline as last line
    match regex_is_match!(
        r"vim:.*ft=help",
        content.lines().next_back().unwrap_or(content)
    ) {
        true => Some(FileType::VimHelp),
        false => Some(FileType::Text),
    }
}

fn typ(_path: &Path, content: &str) -> Option<FileType> {
    // TODO: user defined preferred typ filetype
    for line in content.lines().take(200) {
        if regex_is_match!(r"^(CASE\s*=\s*(SAME|LOWER|UPPER|OPPOSITE)$|TYPE\s)", line) {
            return Some(FileType::Sql);
        }
    }
    Some(FileType::Typst)
}

fn v(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(200) {
        if !line.trim_start().starts_with('/') {
            if regex_is_match!(r";\s*($|/)", line) {
                return Some(FileType::Verilog);
            } else if regex_is_match!(r"\.\s*($|\(\*)", line) {
                return Some(FileType::Coq);
            }
        }
    }
    Some(FileType::V)
}

fn web(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(5) {
        if line.starts_with('%') {
            return Some(FileType::Web);
        }
    }
    Some(FileType::WinBatch)
}

fn xfree86(_path: &Path, content: &str) -> Option<FileType> {
    match regex_is_match!(r"\bXConfigurator\b", util::get_lines(content, 1)) {
        true => Some(FileType::XF86Conf3),
        false => Some(FileType::XF86Conf),
    }
}

fn xml(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(100) {
        if regex_is_match!(r"<!DOCTYPE.*DocBook", line) {
            return Some(FileType::DocBookXml4);
        } else if util::find(line, 0, false, " xmlns=\"http://docbook.org/ns/docbook\"") {
            return Some(FileType::DocBookXml5);
        } else if util::find(line, 0, false, "xmlns:xbl=\"http://www.mozilla.org/xbl\"") {
            return Some(FileType::Xbl);
        }
    }
    Some(FileType::Xml)
}

fn xpm(_path: &Path, content: &str) -> Option<FileType> {
    match util::find(content, 1, true, "XPM2") {
        true => Some(FileType::Xpm2),
        false => Some(FileType::Xpm),
    }
}

fn y(_path: &Path, content: &str) -> Option<FileType> {
    for line in content.lines().take(100) {
        if line.trim_start().starts_with('%') {
            return Some(FileType::Yacc);
        } else if regex_is_match!(r"^\s*(#|class\b)"i, line)
            && !regex_is_match!(r"^\s*#\s*include"i, line)
        {
            return Some(FileType::Racc);
        }
    }
    Some(FileType::Yacc)
}
