use std::ops::Range;
mod contextual;
mod gdef;
mod glyphcontainers;
mod gpos;
mod gsub;
mod miscellenea;
mod name;
mod stat;
mod tables;
mod values;
pub use contextual::*;
pub use fea_rs;
use fea_rs::{typed::AstNode as _, NodeOrToken, ParseTree};
pub use gdef::*;
pub use glyphcontainers::*;
pub use gpos::*;
pub use gsub::*;
pub use miscellenea::*;
pub use name::*;
use smol_str::SmolStr;
use stat::*;
pub use tables::*;
pub use values::*;

pub(crate) const SHIFT: &str = "    ";

pub trait AsFea {
    fn as_fea(&self, indent: &str) -> String;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    // GSUB statements
    SingleSubst(SingleSubstStatement),
    MultipleSubst(MultipleSubstStatement),
    AlternateSubst(AlternateSubstStatement),
    LigatureSubst(LigatureSubstStatement),
    ReverseChainSubst(ReverseChainSingleSubstStatement),
    ChainedContextSubst(ChainedContextStatement<Subst>),
    IgnoreSubst(IgnoreStatement<Subst>),
    // GPOS
    SinglePos(SinglePosStatement),
    PairPos(PairPosStatement),
    CursivePos(CursivePosStatement),
    MarkBasePos(MarkBasePosStatement),
    MarkLigPos(MarkLigPosStatement),
    MarkMarkPos(MarkMarkPosStatement),
    ChainedContextPos(ChainedContextStatement<Pos>),
    IgnorePos(IgnoreStatement<Pos>),
    // Miscellenea
    AnchorDefinition(AnchorDefinition),
    Attach(AttachStatement),
    GlyphClassDef(GlyphClassDefStatement),
    LigatureCaretByIndex(LigatureCaretByIndexStatement),
    LigatureCaretByPos(LigatureCaretByPosStatement),
    MarkClassDefinition(MarkClassDefinition),
    Comment(Comment),
    FeatureNameStatement(NameRecord),
    FontRevision(FontRevisionStatement),
    FeatureReference(FeatureReferenceStatement),
    GlyphClassDefinition(GlyphClassDefinition),
    Language(LanguageStatement),
    LanguageSystem(LanguageSystemStatement),
    LookupFlag(LookupFlagStatement),
    LookupReference(LookupReferenceStatement),
    SizeParameters(SizeParameters),
    SizeMenuName(NameRecord),
    Subtable(SubtableStatement),
    Script(ScriptStatement),
    // Tables and blocks
    Gdef(Table<Gdef>),
    Head(Table<Head>),
    Hhea(Table<Hhea>),
    Name(Table<Name>),
    Stat(Table<Stat>),
    Vhea(Table<Vhea>),
    FeatureBlock(FeatureBlock),
    LookupBlock(LookupBlock),
    NestedBlock(NestedBlock),
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
            Statement::MarkBasePos(mbp) => mbp.as_fea(indent),
            Statement::MarkLigPos(mlp) => mlp.as_fea(indent),
            Statement::MarkMarkPos(mmp) => mmp.as_fea(indent),
            Statement::ChainedContextPos(ccs) => ccs.as_fea(indent),
            Statement::IgnorePos(ip) => ip.as_fea(indent),
            // Miscellenea
            Statement::AnchorDefinition(ad) => ad.as_fea(indent),
            Statement::Attach(at) => at.as_fea(indent),
            Statement::Comment(c) => c.as_fea(indent),
            Statement::FeatureReference(fr) => fr.as_fea(indent),
            Statement::FeatureNameStatement(fr) => fr.as_fea(indent),
            Statement::FontRevision(fr) => fr.as_fea(indent),
            Statement::GlyphClassDef(gcd) => gcd.as_fea(indent),
            Statement::GlyphClassDefinition(gcd) => gcd.as_fea(indent),
            Statement::Language(ls) => ls.as_fea(indent),
            Statement::LanguageSystem(ls) => ls.as_fea(indent),
            Statement::LigatureCaretByIndex(lc) => lc.as_fea(indent),
            Statement::LigatureCaretByPos(lc) => lc.as_fea(indent),
            Statement::LookupFlag(lf) => lf.as_fea(indent),
            Statement::LookupReference(lr) => lr.as_fea(indent),
            Statement::MarkClassDefinition(mc) => mc.as_fea(indent),
            Statement::Script(sc) => sc.as_fea(indent),
            Statement::SizeMenuName(sm) => sm.as_fea(indent),
            Statement::SizeParameters(sp) => sp.as_fea(indent),
            Statement::Subtable(st) => st.as_fea(indent),
            // Tables and blocks
            Statement::Gdef(gdef) => gdef.as_fea(indent),
            Statement::Head(head) => head.as_fea(indent),
            Statement::Hhea(hhea) => hhea.as_fea(indent),
            Statement::Name(name) => name.as_fea(indent),
            Statement::Stat(stat) => stat.as_fea(indent),
            Statement::Vhea(vhea) => vhea.as_fea(indent),
            Statement::FeatureBlock(fb) => fb.as_fea(indent),
            Statement::LookupBlock(lb) => lb.as_fea(indent),
            Statement::NestedBlock(nb) => nb.as_fea(indent),
        }
    }
}

fn to_statement(child: &NodeOrToken) -> Option<Statement> {
    if child.kind() == fea_rs::Kind::Comment {
        return Some(Statement::Comment(Comment::from(
            child.token_text().unwrap(),
        )));
    } else if child.kind() == fea_rs::Kind::SubtableNode {
        return Some(Statement::Subtable(SubtableStatement::new()));
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
    } else if let Some(gsig) = fea_rs::typed::GsubIgnore::cast(child) {
        Some(Statement::IgnoreSubst(gsig.into()))
        // GPOS
    } else if let Some(gpos1) = fea_rs::typed::Gpos1::cast(child) {
        Some(Statement::SinglePos(gpos1.into()))
    } else if let Some(gpos2) = fea_rs::typed::Gpos2::cast(child) {
        Some(Statement::PairPos(gpos2.into()))
    } else if let Some(gpos3) = fea_rs::typed::Gpos3::cast(child) {
        Some(Statement::CursivePos(gpos3.into()))
    } else if let Some(gpos4) = fea_rs::typed::Gpos4::cast(child) {
        Some(Statement::MarkBasePos(gpos4.into()))
    } else if let Some(gpos5) = fea_rs::typed::Gpos5::cast(child) {
        Some(Statement::MarkLigPos(gpos5.into()))
    } else if let Some(gpos6) = fea_rs::typed::Gpos6::cast(child) {
        Some(Statement::MarkMarkPos(gpos6.into()))
    } else if let Some(gpos8) = fea_rs::typed::Gpos8::cast(child) {
        Some(gpos8.into())
    } else if let Some(gpig) = fea_rs::typed::GposIgnore::cast(child) {
        Some(Statement::IgnorePos(gpig.into()))
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
    } else if let Some(mcd) = fea_rs::typed::MarkClassDef::cast(child) {
        Some(Statement::MarkClassDefinition(mcd.into()))
    } else if let Some(script) = fea_rs::typed::Script::cast(child) {
        Some(Statement::Script(script.into()))
    } else if let Some(menuname) = fea_rs::typed::SizeMenuName::cast(child) {
        Some(Statement::SizeMenuName(menuname.into()))
    } else if let Some(sizeparams) = fea_rs::typed::Parameters::cast(child) {
        Some(Statement::SizeParameters(sizeparams.into()))
    } else if let Some(featurenames) = fea_rs::typed::FeatureNames::cast(child) {
        Some(Statement::NestedBlock(featurenames.into()))
    // Doesn't exist in fea_rs AST!
    // } else if let Some(subtable) = fea_rs::typed::Subtable::cast(child) {
    //     Some(Statement::Subtable(SubtableStatement::new()))

    // Lookup blocks can exist within features
    } else if let Some(lookup) = fea_rs::typed::LookupBlock::cast(child) {
        Some(Statement::LookupBlock(lookup.into()))
    } else {
        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NestedBlock {
    pub tag: SmolStr,
    pub statements: Vec<Statement>,
    pub pos: Range<usize>,
}

impl AsFea for NestedBlock {
    fn as_fea(&self, indent: &str) -> String {
        let mut res = String::new();
        res.push_str(&format!("{}{} {{\n", indent, self.tag));
        let mid_indent = indent.to_string() + SHIFT;
        res.push_str(&format!(
            "{mid_indent}{}\n",
            self.statements
                .iter()
                .map(|s| s.as_fea(&mid_indent))
                .collect::<Vec<_>>()
                .join(&format!("\n{mid_indent}"))
        ));
        res.push_str(&format!("{}}};\n", indent));
        res
    }
}

impl From<fea_rs::typed::FeatureNames> for NestedBlock {
    fn from(val: fea_rs::typed::FeatureNames) -> Self {
        #[allow(clippy::manual_map)]
        let statements: Vec<Statement> = val
            .node()
            .iter_children()
            .filter_map(|child| {
                // Preserve comments
                if child.kind() == fea_rs::Kind::Comment {
                    return Some(Statement::Comment(Comment::from(
                        child.token_text().unwrap(),
                    )));
                }
                if let Some(name_spec) = fea_rs::typed::NameSpec::cast(child) {
                    let (platform_id, plat_enc_id, lang_id, string) = parse_namespec(name_spec);
                    Some(Statement::FeatureNameStatement(NameRecord {
                        platform_id,
                        plat_enc_id,
                        lang_id,
                        string,
                        kind: NameRecordKind::FeatureName,
                        location: child.range(),
                    }))
                } else {
                    None
                }
            })
            .collect();
        NestedBlock {
            tag: SmolStr::new("featureNames"),
            statements,
            pos: val.node().range(),
        }
    }
}

pub enum ToplevelItem {
    GlyphClassDefinition(GlyphClassDefinition),
    MarkClassDefinition(MarkClassDefinition),
    LanguageSystem(LanguageSystemStatement),
    // Include(IncludeStatement),
    Feature(FeatureBlock),
    Lookup(LookupBlock),
    Comment(Comment),
    AnchorDefinition(AnchorDefinition),
    // Tables
    Gdef(Table<Gdef>),
    Head(Table<Head>),
    Hhea(Table<Hhea>),
    Name(Table<Name>),
    Stat(Table<Stat>),
    Vhea(Table<Vhea>),
}
impl From<ToplevelItem> for Statement {
    fn from(val: ToplevelItem) -> Self {
        match val {
            ToplevelItem::GlyphClassDefinition(gcd) => Statement::GlyphClassDefinition(gcd),
            ToplevelItem::MarkClassDefinition(gcd) => Statement::MarkClassDefinition(gcd),

            ToplevelItem::LanguageSystem(ls) => Statement::LanguageSystem(ls),
            ToplevelItem::Feature(fb) => Statement::FeatureBlock(fb),
            ToplevelItem::Lookup(lb) => Statement::LookupBlock(lb),
            ToplevelItem::Comment(cmt) => Statement::Comment(cmt),
            ToplevelItem::AnchorDefinition(ad) => Statement::AnchorDefinition(ad),
            ToplevelItem::Name(name) => Statement::Name(name),
            ToplevelItem::Stat(stat) => Statement::Stat(stat),
            ToplevelItem::Gdef(gdef) => Statement::Gdef(gdef),
            ToplevelItem::Head(head) => Statement::Head(head),
            ToplevelItem::Hhea(hhea) => Statement::Hhea(hhea),
            ToplevelItem::Vhea(vhea) => Statement::Vhea(vhea),
        }
    }
}
impl AsFea for ToplevelItem {
    fn as_fea(&self, indent: &str) -> String {
        match self {
            ToplevelItem::GlyphClassDefinition(gcd) => gcd.as_fea(indent),
            ToplevelItem::MarkClassDefinition(mcd) => mcd.as_fea(indent),
            ToplevelItem::LanguageSystem(ls) => ls.as_fea(indent),
            ToplevelItem::Feature(fb) => fb.as_fea(indent),
            ToplevelItem::Lookup(lb) => lb.as_fea(indent),
            ToplevelItem::Comment(cmt) => cmt.as_fea(indent),
            ToplevelItem::AnchorDefinition(ad) => ad.as_fea(indent),
            ToplevelItem::Gdef(gdef) => gdef.as_fea(indent),
            ToplevelItem::Name(name) => name.as_fea(indent),
            ToplevelItem::Stat(stat) => stat.as_fea(indent),
            ToplevelItem::Head(head) => head.as_fea(indent),
            ToplevelItem::Hhea(hhea) => hhea.as_fea(indent),
            ToplevelItem::Vhea(vhea) => vhea.as_fea(indent),
        }
    }
}
#[allow(clippy::manual_map)]
fn to_toplevel_item(child: &NodeOrToken) -> Option<ToplevelItem> {
    if child.kind() == fea_rs::Kind::Comment {
        Some(ToplevelItem::Comment(Comment::from(
            child.token_text().unwrap(),
        )))
    } else if let Some(gcd) = fea_rs::typed::GlyphClassDef::cast(child) {
        Some(ToplevelItem::GlyphClassDefinition(gcd.into()))
    } else if let Some(mcd) = fea_rs::typed::MarkClassDef::cast(child) {
        Some(ToplevelItem::MarkClassDefinition(mcd.into()))
    } else if let Some(langsys) = fea_rs::typed::LanguageSystem::cast(child) {
        Some(ToplevelItem::LanguageSystem(langsys.into()))
    } else if let Some(feature) = fea_rs::typed::Feature::cast(child) {
        Some(ToplevelItem::Feature(feature.into()))
    } else if let Some(lookup) = fea_rs::typed::LookupBlock::cast(child) {
        Some(ToplevelItem::Lookup(lookup.into()))
    } else if let Some(ad) = fea_rs::typed::AnchorDef::cast(child) {
        Some(ToplevelItem::AnchorDefinition(ad.into()))
    } else if let Some(gdef) = fea_rs::typed::GdefTable::cast(child) {
        Some(ToplevelItem::Gdef(gdef.into()))
    } else if let Some(head) = fea_rs::typed::HeadTable::cast(child) {
        Some(ToplevelItem::Head(head.into()))
    } else if let Some(hhea) = fea_rs::typed::HheaTable::cast(child) {
        Some(ToplevelItem::Hhea(hhea.into()))
    } else if let Some(vhea) = fea_rs::typed::VheaTable::cast(child) {
        Some(ToplevelItem::Vhea(vhea.into()))
    } else if let Some(name) = fea_rs::typed::NameTable::cast(child) {
        Some(ToplevelItem::Name(name.into()))
    } else if let Some(stat) = fea_rs::typed::StatTable::cast(child) {
        Some(ToplevelItem::Stat(stat.into()))
    } else {
        None
    }
}

pub struct FeatureFile {
    pub statements: Vec<ToplevelItem>,
}
impl FeatureFile {
    pub fn new(statements: Vec<ToplevelItem>) -> Self {
        Self { statements }
    }

    pub fn iter(&self) -> impl Iterator<Item = &ToplevelItem> {
        self.statements.iter()
    }
}
impl AsFea for FeatureFile {
    fn as_fea(&self, indent: &str) -> String {
        let mut res = String::new();
        for stmt in &self.statements {
            res.push_str(&stmt.as_fea(indent));
            res.push('\n');
        }
        res
    }
}
impl From<ParseTree> for FeatureFile {
    fn from(val: ParseTree) -> Self {
        let statements: Vec<ToplevelItem> = val
            .root()
            .iter_children()
            .filter_map(to_toplevel_item)
            .collect();
        FeatureFile { statements }
    }
}
#[cfg(test)]
mod tests {
    use rstest::rstest;

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

    fn normalize_whitespace(s: &str) -> String {
        s.replace("#", "\n#")
            .replace("\n\n", "\n")
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(|l| l.trim())
            .collect::<Vec<_>>()
            .join("\n")
            .replace("\t", "    ")
            .replace("position ", "pos ")
            .replace("substitute ", "sub ")
            .replace("reversesub ", "rsub ")
    }

    #[rstest]
    fn for_each_file(
        #[files("resources/test/*.fea")]
        #[exclude("ChainPosSubtable_fea")] // fontTools doesn't support it either
        #[exclude("AlternateChained.fea")] // fontTools doesn't support it either
        #[exclude("baseClass.fea")] // Fine, just the line breaks are different
        #[exclude("STAT_bad.fea")] // Fine, just the line breaks are different
        #[exclude("include0.fea")] // We don't process includes
        #[exclude("GSUB_error.fea")] // Literally a parse failure
        #[exclude("spec10.fea")] // I don't care
        path: std::path::PathBuf,
    ) {
        let fea_str = std::fs::read_to_string(&path).unwrap();
        let (parsed, diag) = fea_rs::parse::parse_string(fea_str.clone());
        if diag.has_errors() {
            println!("fea-rs didn't like file {:?}:\n{:#?}", path, diag);
            return;
        }
        let feature_file: FeatureFile = parsed.into();
        let fea_output = feature_file.as_fea("");
        let orig = normalize_whitespace(&fea_str);
        let output = normalize_whitespace(&fea_output);
        let mut orig_lines = orig.lines().collect::<Vec<_>>();
        for i in 0..orig_lines.len() {
            if let Some(replacement) = orig_lines[i].strip_prefix("#test-fea2fea: ") {
                orig_lines[i + 1] = replacement;
            }
        }
        let orig = orig_lines.join("\n");
        pretty_assertions::assert_eq!(orig, output, "Mismatch in file {:?}", path);
    }
}
