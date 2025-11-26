use std::ops::Range;

use fea_rs::typed::{AstNode as _, GlyphOrClass};
use smol_str::SmolStr;

use crate::AsFea;

const FEA_KEYWORDS: [&str; 52] = [
    "anchor",
    "anchordef",
    "anon",
    "anonymous",
    "by",
    "contour",
    "cursive",
    "device",
    "enum",
    "enumerate",
    "excludedflt",
    "exclude_dflt",
    "feature",
    "from",
    "ignore",
    "ignorebaseglyphs",
    "ignoreligatures",
    "ignoremarks",
    "include",
    "includedflt",
    "include_dflt",
    "language",
    "languagesystem",
    "lookup",
    "lookupflag",
    "mark",
    "markattachmenttype",
    "markclass",
    "nameid",
    "null",
    "parameters",
    "pos",
    "position",
    "required",
    "righttoleft",
    "reversesub",
    "rsub",
    "script",
    "sub",
    "substitute",
    "subtable",
    "table",
    "usemarkfilteringset",
    "useextension",
    "valuerecorddef",
    "base",
    "gdef",
    "head",
    "hhea",
    "name",
    "vhea",
    "vmtx",
];

/// A single glyph name, such as `cedilla`.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct GlyphName {
    /// The name itself as a string
    pub name: SmolStr,
}

impl std::fmt::Debug for GlyphName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GlyphName({})", self.name)
    }
}

impl GlyphName {
    /// Creates a new `GlyphName`, representing a single named glyph.
    pub fn new(name: &str) -> Self {
        Self {
            name: SmolStr::new(name),
        }
    }

    /// Returns an iterator over the glyph names in this `GlyphName`.
    pub fn glyphset(&self) -> impl Iterator<Item = &SmolStr> {
        std::iter::once(&self.name)
    }
}
impl AsFea for GlyphName {
    fn as_fea(&self, _indent: &str) -> String {
        if FEA_KEYWORDS.contains(&self.name.as_str()) {
            format!("\\{}", self.name)
        } else {
            self.name.to_string()
        }
    }
}

/// A glyph class literal, such as `[a b c]` or `[a-z A-Z]`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlyphClass {
    /// The glyphs in the class literal
    pub glyphs: Vec<GlyphContainer>,
    /// The location of the glyph class in the source feature file
    pub location: Range<usize>,
}
impl GlyphClass {
    /// Creates a new `GlyphClass` with the given glyph containers and location.
    pub fn new(glyphs: Vec<GlyphContainer>, location: Range<usize>) -> Self {
        Self { glyphs, location }
    }
}
impl AsFea for GlyphClass {
    fn as_fea(&self, _indent: &str) -> String {
        let inner: Vec<String> = self.glyphs.iter().map(|g| g.as_fea("")).collect();
        format!("[{}]", inner.join(" "))
    }
}
impl From<fea_rs::typed::GlyphClass> for GlyphClass {
    fn from(val: fea_rs::typed::GlyphClass) -> Self {
        match val {
            fea_rs::typed::GlyphClass::Named(glyph_class_name) => {
                let members = vec![GlyphContainer::GlyphClassName(SmolStr::new(
                    glyph_class_name.text(),
                ))];
                GlyphClass::new(members, glyph_class_name.range())
            }
            fea_rs::typed::GlyphClass::Literal(glyph_class_literal) => glyph_class_literal.into(),
        }
    }
}
impl From<fea_rs::typed::GlyphClassLiteral> for GlyphClass {
    fn from(val: fea_rs::typed::GlyphClassLiteral) -> Self {
        let members: Vec<GlyphContainer> = val
            .node()
            .iter_children()
            .flat_map(|child| {
                if let Some(gc) = fea_rs::typed::GlyphOrClass::cast(child) {
                    Some(gc.into())
                } else if let Some(gr) = fea_rs::typed::GlyphRange::cast(child) {
                    let start = gr
                        .iter()
                        .find_map(fea_rs::typed::GlyphName::cast)
                        .map(|gn| SmolStr::new(gn.text()))
                        .unwrap();
                    let end = gr
                        .iter()
                        .skip_while(|t| t.kind() != fea_rs::Kind::Hyphen)
                        .find_map(fea_rs::typed::GlyphName::cast)
                        .map(|gn| SmolStr::new(gn.text()))
                        .unwrap();

                    Some(GlyphContainer::GlyphRange(GlyphRange::new(start, end)))
                // "GlyphNameOrRange" doesn't go into the typed AST, we have to handle it ourselves
                } else if child.kind() == fea_rs::Kind::GlyphNameOrRange {
                    Some(GlyphContainer::GlyphNameOrRange(
                        child.token_text().unwrap().into(),
                    ))
                } else {
                    None
                }
            })
            .collect();
        GlyphClass::new(members, val.node().range())
    }
}

/// A glyph range, such as `a-z` or `A01-A05`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlyphRange {
    /// Start glyph name of the range
    pub start: SmolStr,
    /// End glyph name of the range
    pub end: SmolStr,
}
impl GlyphRange {
    /// Creates a new `GlyphRange` with the given start and end glyph names.
    pub fn new(start: SmolStr, end: SmolStr) -> Self {
        Self { start, end }
    }

    /// Returns an iterator over the glyph names in this `GlyphRange`.
    pub fn glyphset(&self) -> impl Iterator<Item = SmolStr> {
        // OK, the rules are:
        // <firstGlyph> and <lastGlyph> must be the same length and can differ only in one of the following ways:
        // By a single letter from A-Z, either uppercase or lowercase.
        // By up to 3 decimal digits in a contiguous run
        // So first we find a common prefix and suffix
        let start_bytes = self.start.as_bytes();
        let end_bytes = self.end.as_bytes();
        let mut prefix_len = 0;
        let mut suffix_len = 0;
        for (start_byte, end_byte) in start_bytes.iter().zip(end_bytes.iter()) {
            if start_byte == end_byte {
                prefix_len += 1;
            } else {
                break;
            }
        }
        for (start_byte, end_byte) in start_bytes.iter().rev().zip(end_bytes.iter().rev()) {
            if start_byte == end_byte {
                suffix_len += 1;
            } else {
                break;
            }
        }
        let start_core = &self.start[prefix_len..self.start.len() - suffix_len];
        let end_core = &self.end[prefix_len..self.end.len() - suffix_len];
        if start_core.len() != end_core.len() {
            // invalid range
            return vec![self.start.clone(), self.end.clone()].into_iter();
        }
        if start_core.len() == 1 {
            // Check if both cores are a single lower case or upper case letter
            let start_char = start_core.chars().next().unwrap();
            let end_char = end_core.chars().next().unwrap();
            if (start_char.is_ascii_lowercase() && end_char.is_ascii_lowercase())
                || (start_char.is_ascii_uppercase() && end_char.is_ascii_uppercase())
            {
                let range = start_char as u8..=end_char as u8;
                let glyphs: Vec<SmolStr> = range
                    .map(|b| {
                        SmolStr::new(format!(
                            "{}{}{}",
                            &self.start[..prefix_len],
                            b as char,
                            &self.start[self.start.len() - suffix_len..]
                        ))
                    })
                    .collect();
                return glyphs.into_iter();
            } else {
                // invalid range
                return vec![self.start.clone(), self.end.clone()].into_iter();
            }
        }
        // So it should be a 1 to 3 digit number
        let start_num: Option<usize> = start_core.parse().ok();
        let end_num: Option<usize> = end_core.parse().ok();
        if let (Some(start_num), Some(end_num)) = (start_num, end_num) {
            let range = start_num..=end_num;
            // Format each number to the correct width with leading zeros
            let glyphs: Vec<SmolStr> = range
                .map(|n| {
                    let mut s = String::new();
                    s.push_str(&self.start[..prefix_len]);
                    s.push_str(&format!("{:0width$}", n, width = start_core.len()));
                    s.push_str(&self.start[self.start.len() - suffix_len..]);
                    SmolStr::new(s)
                })
                .collect();
            return glyphs.into_iter();
        }
        // invalid range
        vec![self.start.clone(), self.end.clone()].into_iter()
    }
}
impl From<Range<SmolStr>> for GlyphRange {
    fn from(val: Range<SmolStr>) -> Self {
        GlyphRange::new(val.start, val.end)
    }
}
impl AsFea for GlyphRange {
    fn as_fea(&self, _indent: &str) -> String {
        format!("{} - {}", self.start.as_str(), self.end.as_str())
    }
}

/// A container for glyphs in various forms: single glyph names, glyph classes,
/// glyph ranges, or glyph name/range literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GlyphContainer {
    /// A single glyph name
    GlyphName(GlyphName),
    /// A glyph class literal
    GlyphClass(GlyphClass),
    /// A named glyph class
    GlyphClassName(SmolStr),
    /// A glyph range
    GlyphRange(GlyphRange),
    /// An ambiguity: either a glyph name or range literal, up to the user to resolve
    GlyphNameOrRange(SmolStr),
}

impl AsFea for GlyphContainer {
    fn as_fea(&self, _indent: &str) -> String {
        match self {
            GlyphContainer::GlyphName(gn) => gn.as_fea(_indent),
            GlyphContainer::GlyphClass(gcs) => {
                let inner: Vec<String> = gcs.glyphs.iter().map(|g| g.as_fea("")).collect();
                format!("[{}]", inner.join(" "))
            }
            GlyphContainer::GlyphClassName(name) => {
                if FEA_KEYWORDS.contains(&name.as_str()) {
                    format!("\\{}", name)
                } else {
                    name.to_string()
                }
            }
            GlyphContainer::GlyphRange(range) => range.as_fea(""),
            GlyphContainer::GlyphNameOrRange(name_or_range) => name_or_range.to_string(),
        }
    }
}

impl From<fea_rs::typed::GlyphOrClass> for GlyphContainer {
    fn from(val: fea_rs::typed::GlyphOrClass) -> Self {
        match val {
            GlyphOrClass::Glyph(glyph) => GlyphContainer::GlyphName(GlyphName::new(glyph.text())),
            GlyphOrClass::Class(glyph_class_literal) => {
                GlyphContainer::GlyphClass(glyph_class_literal.into())
            }
            GlyphOrClass::Cid(_cid) => todo!(),
            GlyphOrClass::NamedClass(glyph_class_name) => {
                GlyphContainer::GlyphClassName(SmolStr::new(glyph_class_name.text()))
            }
            GlyphOrClass::Null(_) => GlyphContainer::GlyphName(GlyphName::new("NULL")),
        }
    }
}

impl From<fea_rs::typed::GlyphClass> for GlyphContainer {
    fn from(val: fea_rs::typed::GlyphClass) -> Self {
        match val {
            fea_rs::typed::GlyphClass::Named(glyph_class_name) => {
                GlyphContainer::GlyphClassName(SmolStr::new(glyph_class_name.text()))
            }
            fea_rs::typed::GlyphClass::Literal(glyph_class_literal) => {
                GlyphContainer::GlyphClass(glyph_class_literal.into())
            }
        }
    }
}

impl GlyphContainer {
    /// Creates a new `GlyphContainer` representing a glyph class literal
    /// containing the given glyph names.
    pub fn new_class(glyph_names: &[&str]) -> Self {
        let members: Vec<GlyphContainer> = glyph_names
            .iter()
            .map(|name| GlyphContainer::GlyphName(GlyphName::new(name)))
            .collect();
        GlyphContainer::GlyphClass(GlyphClass::new(
            members,
            0..0, // location is not relevant here
        ))
    }

    /// Returns true if this `GlyphContainer` is an empty glyph class.
    pub fn is_empty(&self) -> bool {
        match self {
            GlyphContainer::GlyphClass(gcs) => gcs.glyphs.is_empty(),
            _ => false,
        }
    }
}

/// The name of a mark class
///
/// Note that this differs from the Python `fontTools` representation. In
/// Python, a `MarkClass` object contains `MarkClassDefinition` objects
/// for the glyphs in the class, and the `MarkClassDefinition` objects
/// recursively refer to the `MarkClass` they belong to. In Rust, the
/// `MarkClass` is just a name, and the relationship between the class name
/// and the glyphs and their anchor points is stored at the feature file level.
///
/// The name should not begin with `@`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MarkClass {
    /// The name of the mark class, without the leading `@`
    pub name: SmolStr,
}
impl MarkClass {
    /// Creates a new `MarkClass` with the given name.
    pub fn new(name: &str) -> Self {
        Self {
            name: SmolStr::new(name),
        }
    }
}
impl From<fea_rs::typed::GlyphClassDef> for MarkClass {
    fn from(val: fea_rs::typed::GlyphClassDef) -> Self {
        let label = val
            .iter()
            .find_map(fea_rs::typed::GlyphClassName::cast)
            .unwrap();
        MarkClass::new(label.text().trim_start_matches('@'))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_glyphclass() {
        const FEA: &str = "@foo = [a-b c d-e @bar];";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let definition = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::GlyphClassDef::cast)
            .unwrap();
        let literal = definition
            .node()
            .iter_children()
            .find_map(fea_rs::typed::GlyphClassLiteral::cast)
            .unwrap();
        println!("{:#?}", literal);
        let glyphs: GlyphClass = literal.into();
        assert_eq!(glyphs.glyphs.len(), 4);
    }

    #[test]
    fn test_glyphrange() {
        let range = GlyphRange::new(SmolStr::new("a01"), SmolStr::new("a05"));
        let glyphs: Vec<SmolStr> = range.glyphset().collect();
        assert_eq!(
            glyphs,
            vec![
                SmolStr::new("a01"),
                SmolStr::new("a02"),
                SmolStr::new("a03"),
                SmolStr::new("a04"),
                SmolStr::new("a05"),
            ]
        );

        let range = GlyphRange::new(SmolStr::new("B"), SmolStr::new("F"));
        let glyphs: Vec<SmolStr> = range.glyphset().collect();
        assert_eq!(
            glyphs,
            vec![
                SmolStr::new("B"),
                SmolStr::new("C"),
                SmolStr::new("D"),
                SmolStr::new("E"),
                SmolStr::new("F"),
            ]
        );

        let range = GlyphRange::new(SmolStr::new("cat"), SmolStr::new("cet"));
        let glyphs: Vec<SmolStr> = range.glyphset().collect();
        assert_eq!(
            glyphs,
            vec![
                SmolStr::new("cat"),
                SmolStr::new("cbt"),
                SmolStr::new("cct"),
                SmolStr::new("cdt"),
                SmolStr::new("cet"),
            ]
        );
    }
}
