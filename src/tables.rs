use std::ops::Range;

use fea_rs::{typed::AstNode, NodeOrToken};

use crate::{
    stat::StatStatement, AsFea, Comment, FontRevisionStatement, GdefStatement, NameRecord, SHIFT,
};

pub trait FeaTable {
    type Statement: AsFea;
    type FeaRsTable: AstNode;
    const TAG: &'static str;
    fn to_statement(child: &NodeOrToken) -> Option<Self::Statement>;
    fn statements_from_node(node: &fea_rs::Node) -> Vec<Self::Statement> {
        node.iter_children()
            .filter_map(Self::to_statement)
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Table<T: FeaTable> {
    pub statements: Vec<T::Statement>,
}

impl<T: FeaTable> AsFea for Table<T> {
    fn as_fea(&self, indent: &str) -> String {
        let mut res = String::new();
        res.push_str(&format!("{}table {} {{\n", indent, T::TAG));
        for stmt in &self.statements {
            res.push_str(&stmt.as_fea(&(indent.to_string() + SHIFT)));
            res.push('\n');
        }
        res.push_str(&format!("{}}} {};\n", indent, T::TAG));
        res
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Gdef;
impl FeaTable for Gdef {
    type Statement = GdefStatement;
    type FeaRsTable = fea_rs::typed::GdefTable;
    const TAG: &'static str = "GDEF";
    #[allow(clippy::manual_map)]
    fn to_statement(child: &NodeOrToken) -> Option<Self::Statement> {
        if child.kind() == fea_rs::Kind::Comment {
            Some(GdefStatement::Comment(Comment::from(
                child.token_text().unwrap(),
            )))
        } else if let Some(at) = fea_rs::typed::GdefAttach::cast(child) {
            Some(GdefStatement::Attach(at.into()))
        } else if let Some(gcd) = fea_rs::typed::GdefClassDef::cast(child) {
            Some(GdefStatement::GlyphClassDef(gcd.into()))
        } else if let Some(lc) = fea_rs::typed::GdefLigatureCaret::cast(child) {
            // Check if it's by position or by index based on the first keyword
            let is_by_pos = lc
                .iter()
                .next()
                .map(|t| t.kind() == fea_rs::Kind::LigatureCaretByPosKw)
                .unwrap_or(false);
            if is_by_pos {
                Some(GdefStatement::LigatureCaretByPos(lc.into()))
            } else {
                Some(GdefStatement::LigatureCaretByIndex(lc.into()))
            }
        } else {
            None
        }
    }
}

impl From<fea_rs::typed::GdefTable> for Table<Gdef> {
    fn from(val: fea_rs::typed::GdefTable) -> Self {
        Self {
            statements: Gdef::statements_from_node(val.node()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Head;
impl FeaTable for Head {
    type Statement = HeadStatement;
    const TAG: &'static str = "head";
    type FeaRsTable = fea_rs::typed::HeadTable;
    #[allow(clippy::manual_map)]
    fn to_statement(child: &NodeOrToken) -> Option<HeadStatement> {
        if child.kind() == fea_rs::Kind::Comment {
            Some(HeadStatement::Comment(Comment::from(
                child.token_text().unwrap(),
            )))
        } else if let Some(fr) = fea_rs::typed::HeadFontRevision::cast(child) {
            Some(HeadStatement::FontRevision(fr.into()))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HeadStatement {
    Comment(Comment),
    FontRevision(FontRevisionStatement),
}
impl AsFea for HeadStatement {
    fn as_fea(&self, indent: &str) -> String {
        match self {
            HeadStatement::Comment(cmt) => cmt.as_fea(indent),
            HeadStatement::FontRevision(stmt) => stmt.as_fea(indent),
        }
    }
}

impl From<fea_rs::typed::HeadTable> for Table<Head> {
    fn from(val: fea_rs::typed::HeadTable) -> Self {
        Self {
            statements: Head::statements_from_node(val.node()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name;
impl FeaTable for Name {
    type Statement = NameStatement;
    const TAG: &'static str = "name";
    type FeaRsTable = fea_rs::typed::NameTable;
    #[allow(clippy::manual_map)]
    fn to_statement(child: &NodeOrToken) -> Option<NameStatement> {
        if child.kind() == fea_rs::Kind::Comment {
            Some(NameStatement::Comment(Comment::from(
                child.token_text().unwrap(),
            )))
        } else if let Some(fr) = fea_rs::typed::NameRecord::cast(child) {
            Some(NameStatement::NameRecord(fr.into()))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameStatement {
    Comment(Comment),
    NameRecord(NameRecord),
}
impl AsFea for NameStatement {
    fn as_fea(&self, indent: &str) -> String {
        match self {
            NameStatement::Comment(cmt) => cmt.as_fea(indent),
            NameStatement::NameRecord(stmt) => stmt.as_fea(indent),
        }
    }
}

impl From<fea_rs::typed::NameTable> for Table<Name> {
    fn from(val: fea_rs::typed::NameTable) -> Self {
        Self {
            statements: Name::statements_from_node(val.node()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stat;
impl FeaTable for Stat {
    type Statement = StatStatement;
    const TAG: &'static str = "STAT";
    type FeaRsTable = fea_rs::typed::StatTable;
    #[allow(clippy::manual_map)]
    fn to_statement(child: &NodeOrToken) -> Option<StatStatement> {
        if child.kind() == fea_rs::Kind::Comment {
            Some(StatStatement::Comment(Comment::from(
                child.token_text().unwrap(),
            )))
        } else if let Some(da) = fea_rs::typed::StatDesignAxis::cast(child) {
            Some(StatStatement::DesignAxis(da.into()))
        } else if let Some(efn) = fea_rs::typed::StatElidedFallbackName::cast(child) {
            Some(StatStatement::from(efn))
        } else if let Some(efn) = fea_rs::typed::StatAxisValue::cast(child) {
            Some(StatStatement::AxisValue(efn.into()))
        } else {
            None
        }
    }
}

impl From<fea_rs::typed::StatTable> for Table<Stat> {
    fn from(val: fea_rs::typed::StatTable) -> Self {
        Self {
            statements: Stat::statements_from_node(val.node()),
        }
    }
}

// hhea

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HheaFieldType {
    CaretOffset,
    Ascender,
    Descender,
    LineGap,
}
impl AsFea for HheaFieldType {
    fn as_fea(&self, _indent: &str) -> String {
        match self {
            HheaFieldType::CaretOffset => "CaretOffset".to_string(),
            HheaFieldType::Ascender => "Ascender".to_string(),
            HheaFieldType::Descender => "Descender".to_string(),
            HheaFieldType::LineGap => "LineGap".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HheaField {
    field_type: HheaFieldType,
    value: i16,
    location: Range<usize>,
}
impl AsFea for HheaField {
    fn as_fea(&self, indent: &str) -> String {
        format!("{}{} {};", indent, self.field_type.as_fea(""), self.value)
    }
}
impl From<fea_rs::typed::MetricRecord> for HheaField {
    fn from(val: fea_rs::typed::MetricRecord) -> Self {
        let keyword = val
            .node()
            .iter_children()
            .next()
            .and_then(|t| t.as_token())
            .unwrap();
        let metric = val
            .node()
            .iter_children()
            .find_map(fea_rs::typed::Metric::cast)
            .unwrap();
        let value = match metric {
            fea_rs::typed::Metric::Scalar(number) => number.text().parse::<i16>().unwrap(),
            _ => unimplemented!(),
        };
        HheaField {
            field_type: match keyword.kind {
                fea_rs::Kind::CaretOffsetKw => HheaFieldType::CaretOffset,
                fea_rs::Kind::AscenderKw => HheaFieldType::Ascender,
                fea_rs::Kind::DescenderKw => HheaFieldType::Descender,
                fea_rs::Kind::LineGapKw => HheaFieldType::LineGap,
                _ => panic!("Unexpected keyword in HHEA metric record"),
            },
            value,
            location: val.range(),
        }
    }
}

impl From<fea_rs::typed::HheaTable> for Table<Hhea> {
    fn from(val: fea_rs::typed::HheaTable) -> Self {
        Self {
            statements: Hhea::statements_from_node(val.node()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hhea;
impl FeaTable for Hhea {
    type Statement = HheaField;
    const TAG: &'static str = "hhea";
    type FeaRsTable = fea_rs::typed::HheaTable;
    #[allow(clippy::manual_map)]
    fn to_statement(child: &NodeOrToken) -> Option<HheaField> {
        // Damn the comments.
        if let Some(fr) = fea_rs::typed::MetricRecord::cast(child) {
            Some(fr.into())
        } else {
            None
        }
    }
}

// vhea

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VheaFieldType {
    VertTypoAscender,
    VertTypoDescender,
    VertTypoLineGap,
}
impl AsFea for VheaFieldType {
    fn as_fea(&self, _indent: &str) -> String {
        match self {
            VheaFieldType::VertTypoAscender => "VertTypoAscender".to_string(),
            VheaFieldType::VertTypoDescender => "VertTypoDescender".to_string(),
            VheaFieldType::VertTypoLineGap => "VertTypoLineGap".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VheaField {
    field_type: VheaFieldType,
    value: i16,
    location: Range<usize>,
}
impl AsFea for VheaField {
    fn as_fea(&self, indent: &str) -> String {
        format!("{}{} {};", indent, self.field_type.as_fea(""), self.value)
    }
}
impl From<fea_rs::typed::MetricRecord> for VheaField {
    fn from(val: fea_rs::typed::MetricRecord) -> Self {
        let keyword = val
            .node()
            .iter_children()
            .next()
            .and_then(|t| t.as_token())
            .unwrap();
        let metric = val
            .node()
            .iter_children()
            .find_map(fea_rs::typed::Metric::cast)
            .unwrap();
        let value = match metric {
            fea_rs::typed::Metric::Scalar(number) => number.text().parse::<i16>().unwrap(),
            _ => unimplemented!(),
        };
        VheaField {
            field_type: match keyword.kind {
                fea_rs::Kind::VertTypoAscenderKw => VheaFieldType::VertTypoAscender,
                fea_rs::Kind::VertTypoDescenderKw => VheaFieldType::VertTypoDescender,
                fea_rs::Kind::VertTypoLineGapKw => VheaFieldType::VertTypoLineGap,
                _ => panic!("Unexpected keyword in Vhea metric record"),
            },
            value,
            location: val.range(),
        }
    }
}

impl From<fea_rs::typed::VheaTable> for Table<Vhea> {
    fn from(val: fea_rs::typed::VheaTable) -> Self {
        Self {
            statements: Vhea::statements_from_node(val.node()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Vhea;
impl FeaTable for Vhea {
    type Statement = VheaField;
    const TAG: &'static str = "vhea";
    type FeaRsTable = fea_rs::typed::VheaTable;
    #[allow(clippy::manual_map)]
    fn to_statement(child: &NodeOrToken) -> Option<VheaField> {
        // Damn the comments.
        if let Some(fr) = fea_rs::typed::MetricRecord::cast(child) {
            Some(fr.into())
        } else {
            None
        }
    }
}
