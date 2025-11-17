use std::ops::Range;

use fea_rs::{
    typed::{AstNode as _, GlyphOrClass},
    Kind,
};

use crate::{from_anchor, Anchor, AsFea, GlyphContainer, ValueRecord};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SinglePosStatement {
    pub pos: (GlyphContainer, ValueRecord),
    pub prefix: Vec<GlyphContainer>,
    pub suffix: Vec<GlyphContainer>,
    pub force_chain: bool,
    pub location: Range<usize>,
}

impl SinglePosStatement {
    pub fn new(
        prefix: Vec<GlyphContainer>,
        suffix: Vec<GlyphContainer>,
        pos: (GlyphContainer, ValueRecord),
        force_chain: bool,
        location: Range<usize>,
    ) -> Self {
        Self {
            prefix,
            suffix,
            pos,
            force_chain,
            location,
        }
    }
}

impl AsFea for SinglePosStatement {
    fn as_fea(&self, indent: &str) -> String {
        let mut res = String::new();
        res.push_str("pos ");
        if !self.prefix.is_empty() || !self.suffix.is_empty() || self.force_chain {
            if !self.prefix.is_empty() {
                let prefix_str: Vec<String> =
                    self.prefix.iter().map(|g| g.as_fea("") + " ").collect();
                res.push_str(&prefix_str.join(" ").to_string());
            }
            res.push_str(&format!(
                "{}' {}",
                self.pos.0.as_fea(""),
                self.pos.1.as_fea(indent),
            ));
            if !self.suffix.is_empty() {
                let suffix_str: Vec<String> = self.suffix.iter().map(|g| g.as_fea("")).collect();
                res.push_str(&format!(" {}", suffix_str.join(" ")));
            }
        } else {
            res.push_str(&format!(
                "{} {}",
                self.pos.0.as_fea(""),
                self.pos.1.as_fea(indent),
            ));
        }
        res.push(';');
        res
    }
}

impl From<fea_rs::typed::Gpos1> for SinglePosStatement {
    fn from(val: fea_rs::typed::Gpos1) -> Self {
        let target = val.iter().find_map(GlyphOrClass::cast).unwrap();
        let value_record = val
            .iter()
            .find_map(fea_rs::typed::ValueRecord::cast)
            .unwrap();
        Self::new(
            vec![],
            vec![],
            (target.into(), value_record.into()),
            false,
            val.node().range(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PairPosStatement {
    glyphs_1: GlyphContainer,
    glyphs_2: GlyphContainer,
    value_record_1: ValueRecord,
    value_record_2: Option<ValueRecord>,
    enumerated: bool,
    location: Range<usize>,
}

impl PairPosStatement {
    pub fn new(
        glyphs_1: GlyphContainer,
        glyphs_2: GlyphContainer,
        value_record_1: ValueRecord,
        value_record_2: Option<ValueRecord>,
        enumerated: bool,
        location: Range<usize>,
    ) -> Self {
        Self {
            glyphs_1,
            glyphs_2,
            value_record_1,
            value_record_2,
            enumerated,
            location,
        }
    }
}

impl AsFea for PairPosStatement {
    fn as_fea(&self, indent: &str) -> String {
        let mut res = String::new();
        res.push_str("pos ");
        res.push_str(&format!(
            "{} {} ",
            self.glyphs_1.as_fea(""),
            self.glyphs_2.as_fea("")
        ));
        res.push_str(&self.value_record_1.as_fea(indent).to_string());
        if let Some(vr2) = &self.value_record_2 {
            res.push_str(&format!(" {}", vr2.as_fea(indent)));
        }
        res.push(';');
        res
    }
}

impl From<fea_rs::typed::Gpos2> for PairPosStatement {
    fn from(val: fea_rs::typed::Gpos2) -> Self {
        let glyphs_1 = val.iter().find_map(GlyphOrClass::cast).unwrap().into();
        let glyphs_2 = val
            .iter()
            .filter_map(GlyphOrClass::cast)
            .nth(1)
            .unwrap()
            .into();
        let value_record_1 = val
            .iter()
            .find_map(fea_rs::typed::ValueRecord::cast)
            .unwrap();
        let value_record_2 = val
            .iter()
            .filter_map(fea_rs::typed::ValueRecord::cast)
            .nth(1)
            .map(|vr| vr.into());
        let enumerated = val
            .iter()
            .take_while(|t| t.kind() != Kind::PosKw)
            .any(|t| t.kind() == Kind::EnumKw);
        Self::new(
            glyphs_1,
            glyphs_2,
            value_record_1.into(),
            value_record_2,
            enumerated,
            val.node().range(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CursivePosStatement {
    pub location: Range<usize>,
    pub glyphclass: GlyphContainer,
    pub entry: Option<Anchor>,
    pub exit: Option<Anchor>,
}

impl CursivePosStatement {
    pub fn new(
        glyphclass: GlyphContainer,
        entry: Option<Anchor>,
        exit: Option<Anchor>,
        location: Range<usize>,
    ) -> Self {
        Self {
            glyphclass,
            entry,
            exit,
            location,
        }
    }
}

impl AsFea for CursivePosStatement {
    fn as_fea(&self, indent: &str) -> String {
        format!(
            "pos cursive {} {} {};",
            self.glyphclass.as_fea(""),
            self.entry
                .as_ref()
                .map(|e| e.as_fea(indent))
                .unwrap_or_else(|| "<anchor NULL>".to_string()),
            self.exit
                .as_ref()
                .map(|e| e.as_fea(indent))
                .unwrap_or_else(|| "<anchor NULL>".to_string()),
        )
    }
}
impl From<fea_rs::typed::Gpos3> for CursivePosStatement {
    fn from(val: fea_rs::typed::Gpos3) -> Self {
        let glyphclass = val.iter().find_map(GlyphOrClass::cast).unwrap().into();
        let entry = val.iter().find_map(fea_rs::typed::Anchor::cast).unwrap();
        let exit = val
            .iter()
            .filter_map(fea_rs::typed::Anchor::cast)
            .nth(1)
            .unwrap();
        Self::new(
            glyphclass,
            from_anchor(entry),
            from_anchor(exit),
            val.node().range(),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::GlyphName;

    use super::*;

    #[test]
    fn test_generate_gpos1() {
        let gpos1 = SinglePosStatement::new(
            vec![GlyphContainer::GlyphName(GlyphName::new("x"))],
            vec![],
            (
                GlyphContainer::GlyphName(GlyphName::new("A")),
                ValueRecord {
                    x_advance: Some(50),
                    y_advance: None,
                    x_placement: None,
                    y_placement: None,
                    x_placement_device: None,
                    y_placement_device: None,
                    x_advance_device: None,
                    y_advance_device: None,
                    vertical: false,
                    location: 0..0,
                },
            ),
            false,
            0..10,
        );
        let fea_str = gpos1.as_fea("");
        assert_eq!(fea_str, "pos x A' 50;");
    }

    #[test]
    fn test_roundtrip_gpos1() {
        const FEA: &str = "feature foo { pos A 50; } foo;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let gpos1 = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::Gpos1::cast)
            })
            .unwrap();
        let gpos1_stmt: SinglePosStatement = gpos1.into();
        let fea_str_roundtrip = gpos1_stmt.as_fea("");
        assert_eq!(fea_str_roundtrip, "pos A 50;");
    }

    #[test]
    fn test_generate_gpos2() {
        let gpos2 = PairPosStatement::new(
            GlyphContainer::GlyphName(GlyphName::new("A")),
            GlyphContainer::GlyphName(GlyphName::new("B")),
            ValueRecord {
                x_advance: Some(50),
                y_advance: None,
                x_placement: None,
                y_placement: None,
                x_placement_device: None,
                y_placement_device: None,
                x_advance_device: None,
                y_advance_device: None,
                vertical: false,
                location: 0..0,
            },
            Some(ValueRecord {
                x_advance: Some(30),
                y_advance: None,
                x_placement: None,
                y_placement: None,
                x_placement_device: None,
                y_placement_device: None,
                x_advance_device: None,
                y_advance_device: None,
                vertical: false,
                location: 0..0,
            }),
            false,
            0..10,
        );
        let fea_str = gpos2.as_fea("");
        assert_eq!(fea_str, "pos A B 50 30;");
    }

    #[test]
    fn test_generate_gpos3() {
        let gpos3 = CursivePosStatement::new(
            GlyphContainer::GlyphName(GlyphName::new("A")),
            Some(Anchor::new_simple(100, 200, 0..0)),
            Some(Anchor::new_simple(150, 250, 0..0)),
            0..10,
        );
        let fea_str = gpos3.as_fea("");
        assert_eq!(fea_str, "pos cursive A <anchor 100 200> <anchor 150 250>;");

        // Try with some NULL anchors
        let gpos3_null = CursivePosStatement::new(
            GlyphContainer::GlyphName(GlyphName::new("A")),
            None,
            Some(Anchor::new_simple(150, 250, 0..10)),
            0..10,
        );
        let fea_str_null = gpos3_null.as_fea("");
        assert_eq!(
            fea_str_null,
            "pos cursive A <anchor NULL> <anchor 150 250>;"
        );
    }

    #[test]
    fn test_roundtrip_gpos3() {
        const FEA: &str = "feature foo { pos cursive A <anchor 100 200> <anchor 150 250>; } foo;";
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let gpos3 = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::Gpos3::cast)
            })
            .unwrap();
        let gpos3_stmt: CursivePosStatement = gpos3.into();
        let fea_str_roundtrip = gpos3_stmt.as_fea("");
        assert_eq!(
            fea_str_roundtrip,
            "pos cursive A <anchor 100 200> <anchor 150 250>;"
        );
    }
}
