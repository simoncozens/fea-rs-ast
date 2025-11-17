use std::ops::Range;
mod contextual;
mod gdef;
mod glyphcontainers;
mod gpos;
mod gsub;
mod miscellenea;
mod values;
pub use contextual::*;
pub use fea_rs;
use fea_rs::{typed::AstNode as _, NodeOrToken};
pub use gdef::*;
pub use glyphcontainers::*;
pub use gpos::*;
pub use gsub::*;
pub use miscellenea::*;
use smol_str::SmolStr;
pub use values::*;

const SHIFT: &str = "    ";

pub trait AsFea {
    fn as_fea(&self, indent: &str) -> String;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    // GSUB statements
    SingleSubst(SingleSubstStatement),
    MultipleSubst(MultipleSubstStatement),
    AlternateSubst(AlternateSubstStatement),
    ReverseChainSubst(ReverseChainSingleSubstStatement),
    ChainedContextSubst(ChainedContextStatement<Subst>),
    ChainedContextPos(ChainedContextStatement<Pos>),
    IgnoreSubst(IgnoreStatement<Subst>),
    // GPOS
    SinglePos(SinglePosStatement),
    PairPos(PairPosStatement),
    CursivePos(CursivePosStatement),
    IgnorePos(IgnoreStatement<Pos>),
    LigatureSubst(LigatureSubstStatement),
    // Miscellenea
    AnchorDefinition(AnchorDefinition),
    Attach(AttachStatement),
    GlyphClassDef(GlyphClassDefStatement),
    LigatureCaretByIndex(LigatureCaretByIndexStatement),
    LigatureCaretByPos(LigatureCaretByPosStatement),
    Comment(Comment),
    FontRevision(FontRevisionStatement),
    FeatureReference(FeatureReferenceStatement),
    GlyphClassDefinition(GlyphClassDefinition),
    Language(LanguageStatement),
    LanguageSystem(LanguageSystemStatement),
    LookupFlag(LookupFlagStatement),
    LookupReference(LookupReferenceStatement),
    Subtable(SubtableStatement),
    Script(ScriptStatement),
}
impl AsFea for Statement {
    fn as_fea(&self, indent: &str) -> String {
        match self {
            // GSUB
            Statement::SingleSubst(ss) => ss.as_fea(indent),
            Statement::MultipleSubst(ms) => ms.as_fea(indent),
            Statement::AlternateSubst(alt) => alt.as_fea(indent),
            Statement::LigatureSubst(ls) => ls.as_fea(indent),
            Statement::ChainedContextSubst(ccs) => ccs.as_fea(indent),
            Statement::IgnoreSubst(is) => is.as_fea(indent),
            Statement::ReverseChainSubst(rss) => rss.as_fea(indent),
            // GPOS
            Statement::SinglePos(sp) => sp.as_fea(indent),
            Statement::PairPos(pp) => pp.as_fea(indent),
            Statement::CursivePos(cp) => cp.as_fea(indent),
            Statement::ChainedContextPos(ccs) => ccs.as_fea(indent),
            Statement::IgnorePos(ip) => ip.as_fea(indent),
            // Miscellenea
            Statement::AnchorDefinition(ad) => ad.as_fea(indent),
            Statement::Attach(at) => at.as_fea(indent),
            Statement::GlyphClassDef(gcd) => gcd.as_fea(indent),
            Statement::LigatureCaretByIndex(lc) => lc.as_fea(indent),
            Statement::LigatureCaretByPos(lc) => lc.as_fea(indent),
            Statement::Comment(c) => c.as_fea(indent),
            Statement::FontRevision(fr) => fr.as_fea(indent),
            Statement::FeatureReference(fr) => fr.as_fea(indent),
            Statement::GlyphClassDefinition(gcd) => gcd.as_fea(indent),
            Statement::Language(ls) => ls.as_fea(indent),
            Statement::LanguageSystem(ls) => ls.as_fea(indent),
            Statement::LookupFlag(lf) => lf.as_fea(indent),
            Statement::LookupReference(lr) => lr.as_fea(indent),
            Statement::Script(sc) => sc.as_fea(indent),
            Statement::Subtable(st) => st.as_fea(indent),
        }
    }
}

fn to_statement(child: &NodeOrToken) -> Option<Statement> {
    if child.kind() == fea_rs::Kind::Comment {
        return Some(Statement::Comment(Comment::from(
            child.token_text().unwrap(),
        )));
    }
    #[allow(clippy::manual_map)]
    // GSUB
    if let Some(gsub1) = fea_rs::typed::Gsub1::cast(child) {
        Some(Statement::SingleSubst(gsub1.into()))
    } else if let Some(gsub2) = fea_rs::typed::Gsub2::cast(child) {
        Some(Statement::MultipleSubst(gsub2.into()))
    } else if let Some(gsub3) = fea_rs::typed::Gsub3::cast(child) {
        Some(Statement::AlternateSubst(gsub3.into()))
    } else if let Some(gsub4) = fea_rs::typed::Gsub4::cast(child) {
        Some(Statement::LigatureSubst(gsub4.into()))
    } else if let Some(gsub6) = fea_rs::typed::Gsub6::cast(child) {
        Some(gsub6.into())
    } else if let Some(rss) = fea_rs::typed::Gsub8::cast(child) {
        Some(Statement::ReverseChainSubst(rss.into()))
    // GPOS
    } else if let Some(gpos1) = fea_rs::typed::Gpos1::cast(child) {
        Some(Statement::SinglePos(gpos1.into()))
    } else if let Some(gpos2) = fea_rs::typed::Gpos2::cast(child) {
        Some(Statement::PairPos(gpos2.into()))
    } else if let Some(gpos3) = fea_rs::typed::Gpos3::cast(child) {
        Some(Statement::CursivePos(gpos3.into()))
    // Miscellenea
    } else if let Some(ad) = fea_rs::typed::AnchorDef::cast(child) {
        Some(Statement::AnchorDefinition(ad.into()))
    } else if let Some(at) = fea_rs::typed::GdefAttach::cast(child) {
        Some(Statement::Attach(at.into()))
    } else if let Some(gcd) = fea_rs::typed::GdefClassDef::cast(child) {
        Some(Statement::GlyphClassDef(gcd.into()))
    } else if let Some(lc) = fea_rs::typed::GdefLigatureCaret::cast(child) {
        // Check if it's by position or by index based on the first keyword
        let is_by_pos = lc
            .iter()
            .next()
            .map(|t| t.kind() == fea_rs::Kind::LigatureCaretByPosKw)
            .unwrap_or(false);
        if is_by_pos {
            Some(Statement::LigatureCaretByPos(lc.into()))
        } else {
            Some(Statement::LigatureCaretByIndex(lc.into()))
        }
    } else if let Some(fr) = fea_rs::typed::FeatureRef::cast(child) {
        Some(Statement::FeatureReference(fr.into()))
    } else if let Some(fr) = fea_rs::typed::HeadFontRevision::cast(child) {
        Some(Statement::FontRevision(fr.into()))
    } else if let Some(gcd) = fea_rs::typed::GlyphClassDef::cast(child) {
        Some(Statement::GlyphClassDefinition(gcd.into()))
    } else if let Some(lang) = fea_rs::typed::Language::cast(child) {
        Some(Statement::Language(lang.into()))
    } else if let Some(langsys) = fea_rs::typed::LanguageSystem::cast(child) {
        Some(Statement::LanguageSystem(langsys.into()))
    } else if let Some(lookupflag) = fea_rs::typed::LookupFlag::cast(child) {
        Some(Statement::LookupFlag(lookupflag.into()))
    } else if let Some(lookupref) = fea_rs::typed::LookupRef::cast(child) {
        Some(Statement::LookupReference(lookupref.into()))
    } else if let Some(script) = fea_rs::typed::Script::cast(child) {
        Some(Statement::Script(script.into()))
    // Doesn't exist in fea_rs AST!
    // } else if let Some(subtable) = fea_rs::typed::Subtable::cast(child) {
    //     Some(Statement::Subtable(SubtableStatement::new()))
    } else {
        None
    }
}

pub struct FeatureBlock {
    pub name: SmolStr,
    pub statements: Vec<Statement>,
    pub use_extension: bool,
    pub pos: Range<usize>,
}

impl FeatureBlock {
    pub fn new(
        name: SmolStr,
        statements: Vec<Statement>,
        use_extension: bool,
        pos: Range<usize>,
    ) -> Self {
        Self {
            name,
            statements,
            use_extension,
            pos,
        }
    }
}

impl AsFea for FeatureBlock {
    fn as_fea(&self, indent: &str) -> String {
        let mut res = String::new();
        res.push_str(&format!("{}feature {} {{\n", indent, self.name));
        let mid_indent = indent.to_string() + SHIFT;
        res.push_str(&format!(
            "{mid_indent}{}\n",
            self.statements
                .iter()
                .map(|s| s.as_fea(&mid_indent))
                .collect::<Vec<_>>()
                .join(&format!("\n{mid_indent}"))
        ));
        res.push_str(&format!("{}}} {};\n", indent, self.name));
        res
    }
}

impl From<fea_rs::typed::Feature> for FeatureBlock {
    fn from(val: fea_rs::typed::Feature) -> Self {
        let statements: Vec<Statement> = val
            .node()
            .iter_children()
            .filter_map(to_statement)
            .collect();
        FeatureBlock {
            name: SmolStr::new(&val.tag().token().text),
            use_extension: val.iter().any(|t| t.kind() == fea_rs::Kind::UseExtensionKw),
            statements,
            pos: val.node().range(),
        }
    }
}

pub struct LookupBlock {
    pub name: SmolStr,
    pub statements: Vec<Statement>,
    pub use_extension: bool,
    pub pos: Range<usize>,
}

impl LookupBlock {
    pub fn new(
        name: SmolStr,
        statements: Vec<Statement>,
        use_extension: bool,
        pos: Range<usize>,
    ) -> Self {
        Self {
            name,
            statements,
            use_extension,
            pos,
        }
    }
}

impl AsFea for LookupBlock {
    fn as_fea(&self, indent: &str) -> String {
        let mut res = String::new();
        res.push_str(&format!("{}lookup {} {{\n", indent, self.name));
        let mid_indent = indent.to_string() + SHIFT;
        res.push_str(&format!(
            "{mid_indent}{}\n",
            self.statements
                .iter()
                .map(|s| s.as_fea(&mid_indent))
                .collect::<Vec<_>>()
                .join(&format!("\n{mid_indent}"))
        ));
        res.push_str(&format!("{}}} {};\n", indent, self.name));
        res
    }
}

impl From<fea_rs::typed::LookupBlock> for LookupBlock {
    fn from(val: fea_rs::typed::LookupBlock) -> Self {
        let statements: Vec<Statement> = val
            .node()
            .iter_children()
            .filter_map(to_statement)
            .collect();
        let label = val
            .iter()
            .find(|t| t.kind() == fea_rs::Kind::Label)
            .unwrap();
        LookupBlock {
            name: SmolStr::from(label.as_token().unwrap().text.as_str()),
            use_extension: val.iter().any(|t| t.kind() == fea_rs::Kind::UseExtensionKw),
            statements,
            pos: val.node().range(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        const FEA: &str = r#"feature smcp {
            sub a by a.smcp;
        } smcp;
        "#;
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let feature_block = parsed.root().iter_children().next().unwrap();

        let Some(feature) = fea_rs::typed::Feature::cast(feature_block) else {
            panic!("Expected Feature, got {:?}", feature_block.kind());
        };
        let feature_block: FeatureBlock = feature.into();
        assert_eq!(feature_block.name.as_str(), "smcp");
        assert_eq!(feature_block.statements.len(), 1);
        assert_eq!(
            feature_block.as_fea(""),
            "feature smcp {\n    sub a by a.smcp;\n} smcp;\n"
        );
    }
}
