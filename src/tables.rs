use std::ops::Range;

use fea_rs::{typed::AstNode, NodeOrToken};

use crate::{
    stat::StatStatement, AsFea, Comment, FontRevisionStatement, GdefStatement, NameRecord, SHIFT,
};

/// A helper for constructing tables which hold statements of a particular type.
pub trait FeaTable {
    /// The type of statement contained in this table.
    type Statement: AsFea;
    /// The corresponding fea-rs typed table.
    type FeaRsTable: AstNode;
    /// The tag of this table.
    const TAG: &'static str;
    /// Convert a child node or token into a statement of this table's type, if possible.
    fn to_statement(child: &NodeOrToken) -> Option<Self::Statement>;
    /// Extract all statements of this table's type from a fea-rs node.
    fn statements_from_node(node: &fea_rs::Node) -> Vec<Self::Statement> {
        node.iter_children()
            .filter_map(Self::to_statement)
            .collect()
    }
}

/// A table in a feature file, parameterized by the type of statements it contains.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Table<T: FeaTable> {
    /// The statements in this table.
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
/// The `GDEF` table
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
/// The `head` table
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
/// A statement in the `head` table
pub enum HeadStatement {
    /// A comment
    Comment(Comment),
    /// a `FontRevision` statement
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
/// The `name` table
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

/// A statement in the `name` table
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameStatement {
    /// A comment
    Comment(Comment),
    /// a `NameRecord` statement
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
/// The `STAT` table
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
/// Fields in the `hhea` table
pub enum HheaField {
    /// A comment
    Comment(Comment),
    /// A `CaretOffset` statement
    CaretOffset(i16),
    /// An `Ascender` statement
    Ascender(i16),
    /// A `Descender` statement
    Descender(i16),
    /// A `LineGap` statement
    LineGap(i16),
}
impl AsFea for HheaField {
    fn as_fea(&self, indent: &str) -> String {
        match self {
            HheaField::Comment(cmt) => cmt.as_fea(indent),
            HheaField::CaretOffset(x) => format!("{}CaretOffset {};", indent, x),
            HheaField::Ascender(x) => format!("{}Ascender {};", indent, x),
            HheaField::Descender(x) => format!("{}Descender {};", indent, x),
            HheaField::LineGap(x) => format!("{}LineGap {};", indent, x),
        }
    }
}

/// A statement in the `hhea` table
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HheaStatement {
    /// The field of this statement
    pub field: HheaField,
    /// The location of this statement in the source
    pub location: Range<usize>,
}
impl AsFea for HheaStatement {
    fn as_fea(&self, indent: &str) -> String {
        self.field.as_fea(indent)
    }
}
impl From<fea_rs::typed::MetricRecord> for HheaStatement {
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
        HheaStatement {
            field: match keyword.kind {
                fea_rs::Kind::CaretOffsetKw => HheaField::CaretOffset(value),
                fea_rs::Kind::AscenderKw => HheaField::Ascender(value),
                fea_rs::Kind::DescenderKw => HheaField::Descender(value),
                fea_rs::Kind::LineGapKw => HheaField::LineGap(value),
                _ => panic!("Unexpected keyword in HHEA metric record"),
            },
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
/// The `hhea` table
pub struct Hhea;
impl FeaTable for Hhea {
    type Statement = HheaStatement;
    const TAG: &'static str = "hhea";
    type FeaRsTable = fea_rs::typed::HheaTable;
    #[allow(clippy::manual_map)]
    fn to_statement(child: &NodeOrToken) -> Option<HheaStatement> {
        if child.kind() == fea_rs::Kind::Comment {
            Some(HheaStatement {
                field: HheaField::Comment(Comment::from(child.token_text().unwrap())),
                location: child.range(),
            })
        } else if let Some(fr) = fea_rs::typed::MetricRecord::cast(child) {
            Some(fr.into())
        } else {
            None
        }
    }
}

// vhea

/// A field in the `vhea` table
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VheaField {
    /// A comment
    Comment(Comment),
    /// A `VertTypoAscender` statement
    VertTypoAscender(i16),
    /// A `VertTypoDescender` statement
    VertTypoDescender(i16),
    /// A `VertTypoLineGap` statement
    VertTypoLineGap(i16),
}
impl AsFea for VheaField {
    fn as_fea(&self, indent: &str) -> String {
        match self {
            VheaField::Comment(cmt) => cmt.as_fea(indent),
            VheaField::VertTypoAscender(x) => format!("{}VertTypoAscender {};", indent, x),
            VheaField::VertTypoDescender(x) => format!("{}VertTypoDescender {};", indent, x),
            VheaField::VertTypoLineGap(x) => format!("{}VertTypoLineGap {};", indent, x),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A statement in the `vhea` table
pub struct VheaStatement {
    field: VheaField,
    location: Range<usize>,
}
impl AsFea for VheaStatement {
    fn as_fea(&self, indent: &str) -> String {
        self.field.as_fea(indent)
    }
}
impl From<fea_rs::typed::MetricRecord> for VheaStatement {
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
        VheaStatement {
            field: match keyword.kind {
                fea_rs::Kind::VertTypoAscenderKw => VheaField::VertTypoAscender(value),
                fea_rs::Kind::VertTypoDescenderKw => VheaField::VertTypoDescender(value),
                fea_rs::Kind::VertTypoLineGapKw => VheaField::VertTypoLineGap(value),
                _ => panic!("Unexpected keyword in Vhea metric record"),
            },
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
/// The `vhea` table
pub struct Vhea;
impl FeaTable for Vhea {
    type Statement = VheaStatement;
    const TAG: &'static str = "vhea";
    type FeaRsTable = fea_rs::typed::VheaTable;
    #[allow(clippy::manual_map)]
    fn to_statement(child: &NodeOrToken) -> Option<VheaStatement> {
        if child.kind() == fea_rs::Kind::Comment {
            Some(VheaStatement {
                field: VheaField::Comment(Comment::from(child.token_text().unwrap())),
                location: child.range(),
            })
        } else if let Some(fr) = fea_rs::typed::MetricRecord::cast(child) {
            Some(fr.into())
        } else {
            None
        }
    }
}
