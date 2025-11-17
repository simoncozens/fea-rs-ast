use std::ops::Range;

use fea_rs::typed::AstNode as _;

use crate::{AsFea, GlyphContainer};

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
}
