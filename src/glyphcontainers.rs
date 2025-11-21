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
    pub fn new(name: &str) -> Self {
        Self {
            name: SmolStr::new(name),
        }
    }

    pub fn glyphset(&self) -> impl Iterator<Item = &SmolStr> {
        std::iter::once(&self.name)
    }

    pub fn as_fea(&self) -> String {
        if FEA_KEYWORDS.contains(&self.name.as_str()) {
            format!("\\{}", self.name)
        } else {
            self.name.to_string()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlyphClass {
    pub glyphs: Vec<GlyphContainer>,
    pub location: Range<usize>,
}
impl GlyphClass {
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

                    Some(GlyphContainer::GlyphRange(start..end))
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GlyphContainer {
    GlyphName(GlyphName),
    GlyphClass(GlyphClass),
    GlyphClassName(SmolStr),
    GlyphRange(Range<SmolStr>),
    GlyphNameOrRange(SmolStr),
}

impl AsFea for GlyphContainer {
    fn as_fea(&self, _indent: &str) -> String {
        match self {
            GlyphContainer::GlyphName(gn) => gn.as_fea(),
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
            GlyphContainer::GlyphRange(range) => {
                format!("{} - {}", range.start.as_str(), range.end.as_str())
            }
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
            GlyphOrClass::Cid(cid) => todo!(),
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
    pub name: SmolStr,
}
impl MarkClass {
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
}
