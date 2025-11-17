use std::ops::Range;

use fea_rs::typed::{AstNode as _, Tag};

use crate::{from_anchor, Anchor, AsFea, GlyphClass};

/// A named anchor definition. (2.e.viii)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnchorDefinition {
    pub x: i16,
    pub y: i16,
    pub contourpoint: Option<u16>,
    pub name: String,
    pub location: Range<usize>,
}
impl AnchorDefinition {
    pub fn new(
        x: i16,
        y: i16,
        contourpoint: Option<u16>,
        name: String,
        location: Range<usize>,
    ) -> Self {
        Self {
            x,
            y,
            contourpoint,
            name,
            location,
        }
    }
}
impl AsFea for AnchorDefinition {
    fn as_fea(&self, _indent: &str) -> String {
        let mut res = format!("anchorDef {} {}", self.x, self.y);
        if let Some(cp) = self.contourpoint {
            res.push_str(&format!(" contourpoint {}", cp));
        }
        res.push_str(&format!(" {};", self.name));
        res
    }
}
impl From<fea_rs::typed::AnchorDef> for AnchorDefinition {
    fn from(val: fea_rs::typed::AnchorDef) -> Self {
        let anchor_node = val
            .iter()
            .filter_map(fea_rs::typed::Anchor::cast)
            .next()
            .unwrap();
        let our_anchor: Anchor = from_anchor(anchor_node).unwrap();
        let name = val
            .iter()
            .find(|t| t.kind() == fea_rs::Kind::Ident)
            .unwrap();
        AnchorDefinition::new(
            our_anchor.x,
            our_anchor.y,
            our_anchor.contourpoint,
            name.token_text().unwrap().to_string(),
            val.node().range(),
        )
    }
}

/// A comment in a feature file
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Comment {
    pub text: String,
}
impl Comment {
    pub fn new(text: String) -> Self {
        Self { text }
    }
}
impl AsFea for Comment {
    fn as_fea(&self, _indent: &str) -> String {
        self.text.clone()
    }
}
impl From<&str> for Comment {
    fn from(text: &str) -> Self {
        Self::new(text.to_string())
    }
}

/// Example: `feature salt;`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FeatureReferenceStatement {
    pub feature_name: String,
}
impl FeatureReferenceStatement {
    pub fn new(feature_name: String) -> Self {
        Self { feature_name }
    }
}
impl AsFea for FeatureReferenceStatement {
    fn as_fea(&self, _indent: &str) -> String {
        format!("feature {};", self.feature_name)
    }
}
impl From<fea_rs::typed::FeatureRef> for FeatureReferenceStatement {
    fn from(feature: fea_rs::typed::FeatureRef) -> Self {
        Self::new(
            feature
                .iter()
                .find_map(Tag::cast)
                .unwrap()
                .text()
                .to_string(),
        )
    }
}

/// A `head` table `FontRevision` statement.
///
/// `revision` should be a number, and will be formatted to three
/// significant decimal places.
#[derive(Debug, Clone)]
pub struct FontRevisionStatement {
    pub revision: f32,
}
impl FontRevisionStatement {
    pub fn new(revision: f32) -> Self {
        Self { revision }
    }
}
impl AsFea for FontRevisionStatement {
    fn as_fea(&self, _indent: &str) -> String {
        format!("FontRevision {:.3};", self.revision)
    }
}
impl From<fea_rs::typed::HeadFontRevision> for FontRevisionStatement {
    fn from(val: fea_rs::typed::HeadFontRevision) -> Self {
        let revision_token = val
            .iter()
            .find(|t| t.kind() == fea_rs::Kind::Float)
            .unwrap();
        FontRevisionStatement {
            revision: revision_token.as_token().unwrap().text.parse().unwrap(),
        }
    }
}
impl PartialEq for FontRevisionStatement {
    fn eq(&self, other: &Self) -> bool {
        (self.revision * 1000.0).round() == (other.revision * 1000.0).round()
    }
}
impl Eq for FontRevisionStatement {}

/// A glyph class definition
///
/// Example: `@UPPERCASE = [A-Z];`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlyphClassDefinition {
    /// class name as a string, without initial ``@``
    pub name: String,
    pub glyphs: GlyphClass,
    pub location: Range<usize>,
}
impl GlyphClassDefinition {
    pub fn new(name: String, glyphs: GlyphClass, location: Range<usize>) -> Self {
        Self {
            name,
            glyphs,
            location,
        }
    }
}
impl AsFea for GlyphClassDefinition {
    fn as_fea(&self, _indent: &str) -> String {
        format!("{} = {};", self.name, self.glyphs.as_fea(""))
    }
}
impl From<fea_rs::typed::GlyphClassDef> for GlyphClassDefinition {
    fn from(val: fea_rs::typed::GlyphClassDef) -> Self {
        let label = val
            .iter()
            .find_map(fea_rs::typed::GlyphClassName::cast)
            .unwrap();
        let members: fea_rs::typed::GlyphClassLiteral = val
            .iter()
            .find_map(fea_rs::typed::GlyphClassLiteral::cast)
            .unwrap();
        GlyphClassDefinition {
            name: label.text().trim_start_matches('@').to_string(),
            glyphs: members.into(),
            location: val.node().range(),
        }
    }
}

/// A ``language`` statement within a feature
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanguageStatement {
    pub tag: String,
}
impl LanguageStatement {
    pub fn new(tag: String) -> Self {
        Self { tag }
    }
}
impl AsFea for LanguageStatement {
    fn as_fea(&self, _indent: &str) -> String {
        format!("language {};", self.tag)
    }
}
impl From<fea_rs::typed::Language> for LanguageStatement {
    fn from(language: fea_rs::typed::Language) -> Self {
        Self::new(
            language
                .iter()
                .find_map(Tag::cast)
                .unwrap()
                .text()
                .to_string(),
        )
    }
}

/// A top-level ``languagesystem`` statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanguageSystemStatement {
    pub script: String,
    pub language: String,
}
impl LanguageSystemStatement {
    pub fn new(script: String, language: String) -> Self {
        Self { script, language }
    }
}
impl AsFea for LanguageSystemStatement {
    fn as_fea(&self, _indent: &str) -> String {
        format!(
            "languagesystem {} {};",
            self.script,
            self.language.trim_ascii_end()
        )
    }
}
impl From<fea_rs::typed::LanguageSystem> for LanguageSystemStatement {
    fn from(langsys: fea_rs::typed::LanguageSystem) -> Self {
        let mut tags = langsys.iter().filter_map(Tag::cast);
        let script = tags.next().unwrap().text().to_string();
        let language = tags.next().unwrap().text().to_string();
        Self::new(script, language)
    }
}

/// Represents a ``lookup ...;`` statement to include a lookup in a feature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LookupReferenceStatement {
    pub lookup_name: String, // unlike in Python
    pub location: Range<usize>,
}
impl LookupReferenceStatement {
    pub fn new(lookup_name: String, location: Range<usize>) -> Self {
        Self { lookup_name, location }
    }
}
impl AsFea for LookupReferenceStatement {
    fn as_fea(&self, _indent: &str) -> String {
        format!("lookup {};", self.lookup_name)
    }
}
impl From<fea_rs::typed::LookupRef> for LookupReferenceStatement {
    fn from(lookup_ref: fea_rs::typed::LookupRef) -> Self {
        Self::new(
            lookup_ref
                .iter()
                .find(|t| t.kind() == fea_rs::Kind::Ident)
                .unwrap()
                .token_text()
                .unwrap()
                .to_string(),
            lookup_ref.node().range(),
        )
    }
}
    

/// A ``script`` statement
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScriptStatement {
    pub tag: String,
}
impl ScriptStatement {
    pub fn new(tag: String) -> Self {
        Self { tag }
    }
}
impl AsFea for ScriptStatement {
    fn as_fea(&self, _indent: &str) -> String {
        format!("script {};", self.tag)
    }
}
impl From<fea_rs::typed::Script> for ScriptStatement {
    fn from(script: fea_rs::typed::Script) -> Self {
        Self::new(
            script
                .iter()
                .find_map(Tag::cast)
                .unwrap()
                .text()
                .to_string(),
        )
    }
}

/// Represents a subtable break
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SubtableStatement;
impl SubtableStatement {
    pub fn new() -> Self {
        Self {}
    }
}
impl AsFea for SubtableStatement {
    fn as_fea(&self, _indent: &str) -> String {
        "subtable;".to_string()
    }
}

/// A ``lookupflag`` statement
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LookupFlagStatement {
    pub value: u16,
    pub mark_attachment: Option<GlyphClass>,
    pub mark_filtering_set: Option<GlyphClass>,
    pub location: Range<usize>,
}

impl LookupFlagStatement {
    pub fn new(
        value: u16,
        mark_attachment: Option<GlyphClass>,
        mark_filtering_set: Option<GlyphClass>,
        location: Range<usize>,
    ) -> Self {
        Self {
            value,
            mark_attachment,
            mark_filtering_set,
            location,
        }
    }
}

impl AsFea for LookupFlagStatement {
    fn as_fea(&self, _indent: &str) -> String {
        let mut res = Vec::new();
        let flags = ["RightToLeft", "IgnoreBaseGlyphs", "IgnoreLigatures", "IgnoreMarks"];
        let mut curr = 1u16;
        for flag in &flags {
            if self.value & curr != 0 {
                res.push(flag.to_string());
            }
            curr <<= 1;
        }
        if let Some(mark_attachment) = &self.mark_attachment {
            res.push(format!("MarkAttachmentType {}", mark_attachment.as_fea("")));
        }
        if let Some(mark_filtering_set) = &self.mark_filtering_set {
            res.push(format!("UseMarkFilteringSet {}", mark_filtering_set.as_fea("")));
        }
        if res.is_empty() {
            res.push("0".to_string());
        }
        format!("lookupflag {};", res.join(" "))
    }
}

impl From<fea_rs::typed::LookupFlag> for LookupFlagStatement {
    fn from(val: fea_rs::typed::LookupFlag) -> Self {
        let mut value = 0u16;
        let mut mark_attachment = None;
        let mut mark_filtering_set = None;

        for item in val.iter() {
            match item.kind() {
                fea_rs::Kind::RightToLeftKw => value |= 1,
                fea_rs::Kind::IgnoreBaseGlyphsKw => value |= 2,
                fea_rs::Kind::IgnoreLigaturesKw => value |= 4,
                fea_rs::Kind::IgnoreMarksKw => value |= 8,
                _ => {}
            }
        }

        // Collect all items and process MarkAttachment and UseMarkFilteringSet
        let items: Vec<_> = val.iter().collect();
        
        let mut i = 0;
        while i < items.len() {
            match items[i].kind() {
                fea_rs::Kind::MarkAttachmentTypeKw => {
                    if i + 1 < items.len()
                        && let Some(gc) = fea_rs::typed::GlyphClass::cast(items[i + 1])
                    {
                        mark_attachment = Some(gc.into());
                        i += 1; // Skip the glyph class
                    }
                }
                fea_rs::Kind::UseMarkFilteringSetKw => {
                    if i + 1 < items.len()
                        && let Some(gc) = fea_rs::typed::GlyphClass::cast(items[i + 1])
                    {
                        mark_filtering_set = Some(gc.into());
                        i += 1; // Skip the glyph class
                    }
                }
                _ => {}
            }
            i += 1;
        }

        LookupFlagStatement::new(value, mark_attachment, mark_filtering_set, val.range())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{GlyphContainer, GlyphName};

    #[test]
    fn test_roundtrip_lookupflag_simple() {
        const FEA: &str = "lookup test { lookupflag RightToLeft; } test;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let lookup = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::LookupBlock::cast)
            .unwrap();
        let lookupflag = lookup
            .node()
            .iter_children()
            .find_map(fea_rs::typed::LookupFlag::cast)
            .unwrap();
        let stmt = LookupFlagStatement::from(lookupflag);
        assert_eq!(stmt.value, 1);
        assert_eq!(stmt.as_fea(""), "lookupflag RightToLeft;");
    }

    #[test]
    fn test_roundtrip_lookupflag_multiple() {
        const FEA: &str = "lookup test { lookupflag RightToLeft IgnoreMarks; } test;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let lookup = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::LookupBlock::cast)
            .unwrap();
        let lookupflag = lookup
            .node()
            .iter_children()
            .find_map(fea_rs::typed::LookupFlag::cast)
            .unwrap();
        let stmt = LookupFlagStatement::from(lookupflag);
        assert_eq!(stmt.value, 9); // 1 + 8
        assert_eq!(stmt.as_fea(""), "lookupflag RightToLeft IgnoreMarks;");
    }

    #[test]
    fn test_roundtrip_lookupflag_zero() {
        const FEA: &str = "lookup test { lookupflag 0; } test;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let lookup = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::LookupBlock::cast)
            .unwrap();
        let lookupflag = lookup
            .node()
            .iter_children()
            .find_map(fea_rs::typed::LookupFlag::cast)
            .unwrap();
        let stmt = LookupFlagStatement::from(lookupflag);
        assert_eq!(stmt.value, 0);
        assert_eq!(stmt.as_fea(""), "lookupflag 0;");
    }

    #[test]
    fn test_generate_lookupflag() {
        let stmt = LookupFlagStatement::new(
            10, // IgnoreBaseGlyphs (2) + IgnoreMarks (8)
            None,
            None,
            0..0,
        );
        assert_eq!(stmt.as_fea(""), "lookupflag IgnoreBaseGlyphs IgnoreMarks;");
    }

    #[test]
    fn test_generate_lookupflag_with_mark_attachment() {
        let stmt = LookupFlagStatement::new(
            0,
            Some(GlyphClass::new(
                vec![
                    GlyphContainer::GlyphName(GlyphName::new("acute")),
                    GlyphContainer::GlyphName(GlyphName::new("grave")),
                ],
                0..0,
            )),
            None,
            0..0,
        );
        assert_eq!(stmt.as_fea(""), "lookupflag MarkAttachmentType [acute grave];");
    }

    // AnchorDefinition tests
    #[test]
    fn test_roundtrip_anchordef_simple() {
        const FEA: &str = "anchorDef 300 100 ANCHOR_1;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let anchor_def = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::AnchorDef::cast)
            .unwrap();
        let stmt = AnchorDefinition::from(anchor_def);
        assert_eq!(stmt.x, 300);
        assert_eq!(stmt.y, 100);
        assert_eq!(stmt.name, "ANCHOR_1");
        assert_eq!(stmt.contourpoint, None);
        assert_eq!(stmt.as_fea(""), "anchorDef 300 100 ANCHOR_1;");
    }

    #[test]
    fn test_roundtrip_anchordef_contourpoint() {
        const FEA: &str = "anchorDef 300 100 contourpoint 5 ANCHOR_1;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let anchor_def = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::AnchorDef::cast)
            .unwrap();
        let stmt = AnchorDefinition::from(anchor_def);
        assert_eq!(stmt.x, 300);
        assert_eq!(stmt.y, 100);
        assert_eq!(stmt.contourpoint, Some(5));
        assert_eq!(stmt.name, "ANCHOR_1");
        assert_eq!(stmt.as_fea(""), "anchorDef 300 100 contourpoint 5 ANCHOR_1;");
    }

    #[test]
    fn test_generation_anchordef() {
        let stmt = AnchorDefinition::new(150, -50, None, "BASE".to_string(), 0..0);
        assert_eq!(stmt.as_fea(""), "anchorDef 150 -50 BASE;");
    }

    // FeatureReferenceStatement tests
    #[test]
    fn test_roundtrip_featurereference() {
        const FEA: &str = "feature test { feature salt; } test;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let feature = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .unwrap();
        let feature_ref = feature
            .node()
            .iter_children()
            .find_map(fea_rs::typed::FeatureRef::cast)
            .unwrap();
        let stmt = FeatureReferenceStatement::from(feature_ref);
        assert_eq!(stmt.feature_name, "salt");
        assert_eq!(stmt.as_fea(""), "feature salt;");
    }

    #[test]
    fn test_generation_featurereference() {
        let stmt = FeatureReferenceStatement::new("liga".to_string());
        assert_eq!(stmt.as_fea(""), "feature liga;");
    }

    // FontRevisionStatement tests
    #[test]
    fn test_roundtrip_fontrevision() {
        const FEA: &str = "table head { FontRevision 2.500; } head;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::HeadTable::cast)
            .unwrap();
        let font_rev = table
            .node()
            .iter_children()
            .find_map(fea_rs::typed::HeadFontRevision::cast)
            .unwrap();
        let stmt = FontRevisionStatement::from(font_rev);
        assert_eq!(stmt.revision, 2.5);
        assert_eq!(stmt.as_fea(""), "FontRevision 2.500;");
    }

    #[test]
    fn test_generation_fontrevision() {
        let stmt = FontRevisionStatement::new(1.125);
        assert_eq!(stmt.as_fea(""), "FontRevision 1.125;");
    }

    // GlyphClassDefinition tests
    #[test]
    fn test_roundtrip_glyphclassdef() {
        const FEA: &str = "@UPPERCASE = [A B C D E F];";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let glyph_class_def = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::GlyphClassDef::cast)
            .unwrap();
        let stmt = GlyphClassDefinition::from(glyph_class_def);
        assert_eq!(stmt.name, "UPPERCASE");
        assert_eq!(stmt.glyphs.glyphs.len(), 6);
        assert_eq!(stmt.as_fea(""), "UPPERCASE = [A B C D E F];");
    }

    #[test]
    fn test_generation_glyphclassdef() {
        let glyphs = GlyphClass::new(
            vec![
                GlyphContainer::GlyphName(GlyphName::new("a")),
                GlyphContainer::GlyphName(GlyphName::new("b")),
                GlyphContainer::GlyphName(GlyphName::new("c")),
            ],
            0..0,
        );
        let stmt = GlyphClassDefinition::new("lowercase".to_string(), glyphs, 0..0);
        assert_eq!(stmt.as_fea(""), "lowercase = [a b c];");
    }

    // LanguageStatement tests
    #[test]
    fn test_roundtrip_language() {
        const FEA: &str = "feature test { language TRK; } test;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let feature = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .unwrap();
        let lang = feature
            .node()
            .iter_children()
            .find_map(fea_rs::typed::Language::cast)
            .unwrap();
        let stmt = LanguageStatement::from(lang);
        // Note: tag includes any trailing spaces from the source
        assert_eq!(stmt.as_fea(""), "language TRK;");
    }

    #[test]
    fn test_generation_language() {
        let stmt = LanguageStatement::new("DEU ".to_string());
        assert_eq!(stmt.as_fea(""), "language DEU ;");
    }

    // LanguageSystemStatement tests
    #[test]
    fn test_roundtrip_languagesystem() {
        const FEA: &str = "languagesystem latn dflt;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let langsys = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::LanguageSystem::cast)
            .unwrap();
        let stmt = LanguageSystemStatement::from(langsys);
        assert_eq!(stmt.script, "latn");
        assert_eq!(stmt.language, "dflt");
        assert_eq!(stmt.as_fea(""), "languagesystem latn dflt;");
    }

    #[test]
    fn test_generation_languagesystem() {
        let stmt = LanguageSystemStatement::new("cyrl".to_string(), "SRB ".to_string());
        assert_eq!(stmt.as_fea(""), "languagesystem cyrl SRB;");
    }

    // ScriptStatement tests
    #[test]
    fn test_roundtrip_script() {
        const FEA: &str = "feature test { script latn; } test;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let feature = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .unwrap();
        let script = feature
            .node()
            .iter_children()
            .find_map(fea_rs::typed::Script::cast)
            .unwrap();
        let stmt = ScriptStatement::from(script);
        assert_eq!(stmt.tag, "latn");
        assert_eq!(stmt.as_fea(""), "script latn;");
    }

    #[test]
    fn test_generation_script() {
        let stmt = ScriptStatement::new("arab".to_string());
        assert_eq!(stmt.as_fea(""), "script arab;");
    }

    // SubtableStatement tests
    #[test]
    fn test_generation_subtable() {
        let stmt = SubtableStatement::new();
        assert_eq!(stmt.as_fea(""), "subtable;");
    }

    // LookupReferenceStatement tests
    #[test]
    fn test_roundtrip_lookupreference() {
        const FEA: &str = "feature test { lookup myLookup; } test;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let feature = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .unwrap();
        let lookup_ref = feature
            .node()
            .iter_children()
            .find_map(fea_rs::typed::LookupRef::cast)
            .unwrap();
        let stmt = LookupReferenceStatement::from(lookup_ref);
        assert_eq!(stmt.lookup_name, "myLookup");
        assert_eq!(stmt.as_fea(""), "lookup myLookup;");
    }

    #[test]
    fn test_generation_lookupreference() {
        let stmt = LookupReferenceStatement::new("anotherLookup".to_string(), 0..0);
        assert_eq!(stmt.as_fea(""), "lookup anotherLookup;");
    }
}
