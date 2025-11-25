use std::ops::Range;

use fea_rs::typed::AstNode as _;

use crate::{AsFea, Comment, GlyphContainer, Statement};

/// A ``GDEF`` table ``Attach`` statement
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttachStatement {
    pub glyphs: GlyphContainer,
    pub contour_points: Vec<usize>,
    pub location: Range<usize>,
}
impl AttachStatement {
    pub fn new(glyphs: GlyphContainer, contour_points: Vec<usize>, location: Range<usize>) -> Self {
        Self {
            glyphs,
            contour_points,
            location,
        }
    }
}
impl AsFea for AttachStatement {
    fn as_fea(&self, _indent: &str) -> String {
        let points = self
            .contour_points
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        format!("Attach {} {};", self.glyphs.as_fea(""), points)
    }
}
impl From<fea_rs::typed::GdefAttach> for AttachStatement {
    fn from(val: fea_rs::typed::GdefAttach) -> Self {
        let glyphs = val
            .iter()
            .find_map(fea_rs::typed::GlyphOrClass::cast)
            .unwrap();
        let contour_points: Vec<usize> = val
            .iter()
            .filter(|t| t.kind() == fea_rs::Kind::Number)
            .map(|t| t.as_token().unwrap().text.parse().unwrap())
            .collect();
        AttachStatement::new(glyphs.into(), contour_points, val.node().range())
    }
}

/// A ``GDEF`` table ``GlyphClassDef`` statement
///
/// Example: ``GlyphClassDef [a b c], [f_f_i f_f_l], [acute grave], [n.sc t.sc];``
/// Or with named classes: ``GlyphClassDef @BASE, @LIGATURES, @MARKS, @COMPONENT;``
///
/// The four parameters represent base glyphs, ligature glyphs, mark glyphs,
/// and component glyphs respectively. Any parameter can be None.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlyphClassDefStatement {
    pub base_glyphs: Option<GlyphContainer>,
    pub ligature_glyphs: Option<GlyphContainer>,
    pub mark_glyphs: Option<GlyphContainer>,
    pub component_glyphs: Option<GlyphContainer>,
    pub location: Range<usize>,
}

impl GlyphClassDefStatement {
    pub fn new(
        base_glyphs: Option<GlyphContainer>,
        ligature_glyphs: Option<GlyphContainer>,
        mark_glyphs: Option<GlyphContainer>,
        component_glyphs: Option<GlyphContainer>,
        location: Range<usize>,
    ) -> Self {
        Self {
            base_glyphs,
            ligature_glyphs,
            mark_glyphs,
            component_glyphs,
            location,
        }
    }
}

impl AsFea for GlyphClassDefStatement {
    fn as_fea(&self, _indent: &str) -> String {
        let base = self
            .base_glyphs
            .as_ref()
            .map(|g| g.as_fea(""))
            .unwrap_or_default();
        let liga = self
            .ligature_glyphs
            .as_ref()
            .map(|g| g.as_fea(""))
            .unwrap_or_default();
        let mark = self
            .mark_glyphs
            .as_ref()
            .map(|g| g.as_fea(""))
            .unwrap_or_default();
        let comp = self
            .component_glyphs
            .as_ref()
            .map(|g| g.as_fea(""))
            .unwrap_or_default();
        format!("GlyphClassDef {}, {}, {}, {};", base, liga, mark, comp)
    }
}

impl From<fea_rs::typed::GdefClassDef> for GlyphClassDefStatement {
    fn from(val: fea_rs::typed::GdefClassDef) -> Self {
        // Extract the 4 glyph class entries in order: base, ligature, mark, component
        let mut entries = val
            .iter()
            .filter(|t| t.kind() == fea_rs::Kind::GdefClassDefEntryNode)
            .filter_map(fea_rs::typed::GdefClassDefEntry::cast);

        // Helper to extract GlyphContainer from an entry (handles both literal classes and named class references)
        let extract_container =
            |entry: fea_rs::typed::GdefClassDefEntry| -> Option<GlyphContainer> {
                entry
                    .iter()
                    .find_map(fea_rs::typed::GlyphOrClass::cast)
                    .map(Into::into)
            };

        let base_glyphs = entries.next().and_then(extract_container);
        let ligature_glyphs = entries.next().and_then(extract_container);
        let mark_glyphs = entries.next().and_then(extract_container);
        let component_glyphs = entries.next().and_then(extract_container);

        GlyphClassDefStatement::new(
            base_glyphs,
            ligature_glyphs,
            mark_glyphs,
            component_glyphs,
            val.range(),
        )
    }
}

/// A ``GDEF`` table ``LigatureCaretByIndex`` statement
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LigatureCaretByIndexStatement {
    pub glyphs: GlyphContainer,
    pub carets: Vec<usize>,
    pub location: Range<usize>,
}

impl LigatureCaretByIndexStatement {
    pub fn new(glyphs: GlyphContainer, carets: Vec<usize>, location: Range<usize>) -> Self {
        Self {
            glyphs,
            carets,
            location,
        }
    }
}

impl AsFea for LigatureCaretByIndexStatement {
    fn as_fea(&self, _indent: &str) -> String {
        let carets = self
            .carets
            .iter()
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        format!(
            "LigatureCaretByIndex {} {};",
            self.glyphs.as_fea(""),
            carets
        )
    }
}

impl From<fea_rs::typed::GdefLigatureCaret> for LigatureCaretByIndexStatement {
    fn from(val: fea_rs::typed::GdefLigatureCaret) -> Self {
        let glyphs = val
            .iter()
            .find_map(fea_rs::typed::GlyphOrClass::cast)
            .unwrap();

        // Extract the caret indices as unsigned integers
        let carets: Vec<usize> = val
            .iter()
            .filter(|t| t.kind() == fea_rs::Kind::Number)
            .map(|t| t.as_token().unwrap().text.parse().unwrap())
            .collect();

        LigatureCaretByIndexStatement::new(glyphs.into(), carets, val.node().range())
    }
}

/// A ``GDEF`` table ``LigatureCaretByPos`` statement
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LigatureCaretByPosStatement {
    pub glyphs: GlyphContainer,
    pub carets: Vec<i16>,
    pub location: Range<usize>,
}

impl LigatureCaretByPosStatement {
    pub fn new(glyphs: GlyphContainer, carets: Vec<i16>, location: Range<usize>) -> Self {
        Self {
            glyphs,
            carets,
            location,
        }
    }
}

impl AsFea for LigatureCaretByPosStatement {
    fn as_fea(&self, _indent: &str) -> String {
        let carets = self
            .carets
            .iter()
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        format!("LigatureCaretByPos {} {};", self.glyphs.as_fea(""), carets)
    }
}

impl From<fea_rs::typed::GdefLigatureCaret> for LigatureCaretByPosStatement {
    fn from(val: fea_rs::typed::GdefLigatureCaret) -> Self {
        let glyphs = val
            .iter()
            .find_map(fea_rs::typed::GlyphOrClass::cast)
            .unwrap();

        // Extract the caret positions as signed integers
        let carets: Vec<i16> = val
            .iter()
            .filter(|t| t.kind() == fea_rs::Kind::Number)
            .map(|t| t.as_token().unwrap().text.parse().unwrap())
            .collect();

        LigatureCaretByPosStatement::new(glyphs.into(), carets, val.node().range())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GdefStatement {
    Attach(AttachStatement),
    GlyphClassDef(GlyphClassDefStatement),
    LigatureCaretByIndex(LigatureCaretByIndexStatement),
    LigatureCaretByPos(LigatureCaretByPosStatement),
    Comment(Comment),
    // Include(IncludeStatement),
}
impl AsFea for GdefStatement {
    fn as_fea(&self, indent: &str) -> String {
        match self {
            GdefStatement::Attach(stmt) => stmt.as_fea(indent),
            GdefStatement::GlyphClassDef(stmt) => stmt.as_fea(indent),
            GdefStatement::LigatureCaretByIndex(stmt) => stmt.as_fea(indent),
            GdefStatement::LigatureCaretByPos(stmt) => stmt.as_fea(indent),
            GdefStatement::Comment(cmt) => cmt.as_fea(indent),
            // GdefStatement::Include(stmt) => stmt.as_fea(indent),
        }
    }
}
impl From<GdefStatement> for Statement {
    fn from(val: GdefStatement) -> Self {
        match val {
            GdefStatement::Attach(stmt) => Statement::GdefAttach(stmt),
            GdefStatement::GlyphClassDef(stmt) => Statement::GdefClassDef(stmt),
            GdefStatement::LigatureCaretByIndex(stmt) => Statement::GdefLigatureCaretByIndex(stmt),
            GdefStatement::LigatureCaretByPos(stmt) => Statement::GdefLigatureCaretByPos(stmt),
            GdefStatement::Comment(cmt) => Statement::Comment(cmt),
            // GdefStatement::Include(stmt) => Statement::Include(stmt),
        }
    }
}
impl TryFrom<Statement> for GdefStatement {
    type Error = crate::Error;
    fn try_from(value: Statement) -> Result<Self, Self::Error> {
        match value {
            Statement::GdefAttach(stmt) => Ok(GdefStatement::Attach(stmt)),
            Statement::GdefClassDef(stmt) => Ok(GdefStatement::GlyphClassDef(stmt)),
            Statement::GdefLigatureCaretByIndex(stmt) => {
                Ok(GdefStatement::LigatureCaretByIndex(stmt))
            }
            Statement::GdefLigatureCaretByPos(stmt) => Ok(GdefStatement::LigatureCaretByPos(stmt)),
            Statement::Comment(cmt) => Ok(GdefStatement::Comment(cmt)),
            // Statement::Include(stmt) => Ok(GdefStatement::Include(stmt)),
            _ => Err(crate::Error::CannotConvert),
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{GlyphClass, GlyphName};

    #[test]
    fn test_roundtrip_ligature_caret_by_index() {
        const FEA: &str = "table GDEF { LigatureCaretByIndex f_f_i 2 3; } GDEF;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let gdef_table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::GdefTable::cast)
            .unwrap();
        let ligature_caret = gdef_table
            .node()
            .iter_children()
            .find_map(fea_rs::typed::GdefLigatureCaret::cast)
            .unwrap();
        let stmt = LigatureCaretByIndexStatement::from(ligature_caret);
        assert_eq!(stmt.glyphs.as_fea(""), "f_f_i");
        assert_eq!(stmt.carets, vec![2, 3]);
        assert_eq!(stmt.as_fea(""), "LigatureCaretByIndex f_f_i 2 3;");
    }

    #[test]
    fn test_roundtrip_ligature_caret_by_pos() {
        const FEA: &str = "table GDEF { LigatureCaretByPos f_f_i 200 400; } GDEF;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let gdef_table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::GdefTable::cast)
            .unwrap();
        let ligature_caret = gdef_table
            .node()
            .iter_children()
            .find_map(fea_rs::typed::GdefLigatureCaret::cast)
            .unwrap();
        let stmt = LigatureCaretByPosStatement::from(ligature_caret);
        assert_eq!(stmt.glyphs.as_fea(""), "f_f_i");
        assert_eq!(stmt.carets, vec![200, 400]);
        assert_eq!(stmt.as_fea(""), "LigatureCaretByPos f_f_i 200 400;");
    }

    #[test]
    fn test_generate_ligature_caret_by_index() {
        let stmt = LigatureCaretByIndexStatement::new(
            GlyphContainer::GlyphName(GlyphName::new("f_f_i")),
            vec![2, 3],
            0..0,
        );
        assert_eq!(stmt.as_fea(""), "LigatureCaretByIndex f_f_i 2 3;");
    }

    #[test]
    fn test_generate_ligature_caret_by_pos() {
        let stmt = LigatureCaretByPosStatement::new(
            GlyphContainer::GlyphClass(GlyphClass::new(
                vec![
                    GlyphContainer::GlyphName(GlyphName::new("f_f_i")),
                    GlyphContainer::GlyphName(GlyphName::new("f_f_l")),
                ],
                0..0,
            )),
            vec![200, 400],
            0..0,
        );
        assert_eq!(stmt.as_fea(""), "LigatureCaretByPos [f_f_i f_f_l] 200 400;");
    }

    #[test]
    fn test_roundtrip_attach() {
        const FEA: &str = "table GDEF { Attach [a e o] 1 2; } GDEF;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let gdef_table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::GdefTable::cast)
            .unwrap();
        let attach = gdef_table
            .node()
            .iter_children()
            .find_map(fea_rs::typed::GdefAttach::cast)
            .unwrap();
        let stmt = AttachStatement::from(attach);
        assert_eq!(stmt.as_fea(""), "Attach [a e o] 1 2;");
    }

    #[test]
    fn test_generation_attach() {
        let stmt = AttachStatement::new(
            GlyphContainer::GlyphName(GlyphName::new("acutecomb")),
            vec![3, 5, 7],
            0..0,
        );
        assert_eq!(stmt.as_fea(""), "Attach acutecomb 3 5 7;");
    }

    #[test]
    fn test_roundtrip_glyphclassdef() {
        const FEA: &str = "table GDEF { GlyphClassDef [a b c], [f_f_i], [acute grave], ; } GDEF;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let gdef_table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::GdefTable::cast)
            .unwrap();
        let class_def = gdef_table
            .node()
            .iter_children()
            .find_map(fea_rs::typed::GdefClassDef::cast)
            .unwrap();

        let stmt = GlyphClassDefStatement::from(class_def);
        assert!(stmt.base_glyphs.is_some());
        assert!(stmt.ligature_glyphs.is_some());
        assert!(stmt.mark_glyphs.is_some());
        assert!(stmt.component_glyphs.is_none());
        assert_eq!(
            stmt.as_fea(""),
            "GlyphClassDef [a b c], [f_f_i], [acute grave], ;"
        );
    }

    #[test]
    fn test_generation_glyphclassdef() {
        let stmt = GlyphClassDefStatement::new(
            Some(GlyphContainer::GlyphClass(GlyphClass::new(
                vec![GlyphContainer::GlyphName(GlyphName::new("a"))],
                0..0,
            ))),
            None,
            Some(GlyphContainer::GlyphClass(GlyphClass::new(
                vec![GlyphContainer::GlyphName(GlyphName::new("acutecomb"))],
                0..0,
            ))),
            None,
            0..0,
        );
        assert_eq!(stmt.as_fea(""), "GlyphClassDef [a], , [acutecomb], ;");
    }

    #[test]
    fn test_roundtrip_glyphclassdef_named_classes() {
        const FEA: &str =
            "table GDEF { GlyphClassDef @BASE, @LIGATURES, @MARKS, @COMPONENT; } GDEF;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let gdef_table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::GdefTable::cast)
            .unwrap();
        let class_def = gdef_table
            .node()
            .iter_children()
            .find_map(fea_rs::typed::GdefClassDef::cast)
            .unwrap();

        let stmt = GlyphClassDefStatement::from(class_def);
        assert!(stmt.base_glyphs.is_some());
        assert!(stmt.ligature_glyphs.is_some());
        assert!(stmt.mark_glyphs.is_some());
        assert!(stmt.component_glyphs.is_some());
        assert_eq!(
            stmt.as_fea(""),
            "GlyphClassDef @BASE, @LIGATURES, @MARKS, @COMPONENT;"
        );
    }
}
