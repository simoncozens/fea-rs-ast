use fea_rs::{NodeOrToken, typed::AstNode as _};

use crate::{
    AsFea, AttachStatement, Comment, FontRevisionStatement, GlyphClassDefStatement,
    LigatureCaretByIndexStatement, LigatureCaretByPosStatement, SHIFT,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GdefTable {
    pub statements: Vec<GdefStatement>,
}

impl GdefTable {
    pub fn new(statements: Vec<GdefStatement>) -> Self {
        Self { statements }
    }
}

impl AsFea for GdefTable {
    fn as_fea(&self, indent: &str) -> String {
        let mut res = String::new();
        res.push_str(&format!("{}table GDEF {{\n", indent));
        for stmt in &self.statements {
            res.push_str(&stmt.as_fea(&(indent.to_string() + SHIFT)));
            res.push('\n');
        }
        res.push_str(&format!("{}}} GDEF;\n", indent));
        res
    }
}

impl From<fea_rs::typed::GdefTable> for GdefTable {
    fn from(val: fea_rs::typed::GdefTable) -> Self {
        let statements: Vec<GdefStatement> = val
            .node()
            .iter_children()
            .filter_map(to_gdef_statement)
            .collect();
        GdefTable::new(statements)
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

fn to_gdef_statement(child: &NodeOrToken) -> Option<GdefStatement> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HeadTable {
    pub statements: Vec<HeadStatement>,
}

impl HeadTable {
    pub fn new(statements: Vec<HeadStatement>) -> Self {
        Self { statements }
    }
}
impl AsFea for HeadTable {
    fn as_fea(&self, indent: &str) -> String {
        let mut res = String::new();
        res.push_str(&format!("{}table head {{\n", indent));
        for stmt in &self.statements {
            res.push_str(&stmt.as_fea(&(indent.to_string() + SHIFT)));
            res.push('\n');
        }
        res.push_str(&format!("{}}} head;\n", indent));
        res
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
#[allow(clippy::manual_map)]
fn to_head_statement(child: &NodeOrToken) -> Option<HeadStatement> {
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
impl From<fea_rs::typed::HeadTable> for HeadTable {
    fn from(val: fea_rs::typed::HeadTable) -> Self {
        let statements: Vec<HeadStatement> = val
            .node()
            .iter_children()
            .filter_map(to_head_statement)
            .collect();
        HeadTable::new(statements)
    }
}
