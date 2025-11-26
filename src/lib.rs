#![deny(missing_docs)]
//! # fea-rs-ast
//!
//! A Rust port of Python's [`fontTools.feaLib.ast`](https://fonttools.readthedocs.io/en/latest/feaLib/ast.html)
//! library, providing a fontTools-compatible AST (Abstract Syntax Tree) for OpenType Feature Files.
//!
//! This crate builds on top of the [`fea-rs`](https://github.com/googlefonts/fontc/tree/main/fea-rs)
//! parser, providing a higher-level, more ergonomic interface that matches the familiar fontTools API
//! while leveraging Rust's type safety and performance.
//!
//! ## Overview
//!
//! OpenType Feature Files (.fea) define advanced typographic features for fonts using a
//! domain-specific language. This crate provides:
//!
//! - **Parsing**: Load and parse feature files into a structured AST using `fea-rs`.
//! - **Construction**: Programmatically build feature file structures
//! - **Serialization**: Convert AST back to valid feature file syntax via the [`AsFea`] trait
//! - **Transformation**: Modify AST using the visitor pattern
//!
//! ## Architecture
//!
//! The crate provides two main statement enums:
//!
//! - [`Statement`]: All possible statements in a feature file, regardless of context
//! - [`ToplevelItem`]: Only statements valid at the top level of a feature file
//!
//! Both implement the [`AsFea`] trait for serialization back to .fea syntax.
//!
//! ## Examples
//!
//! ### Loading an Existing Feature File
//!
//! Parse a feature file from a string:
//!
//! ```rust
//! use fea_rs_ast::{FeatureFile, AsFea};
//!
//! let fea_code = r#"
//!     languagesystem DFLT dflt;
//!     
//!     feature smcp {
//!         sub a by a.smcp;
//!         sub b by b.smcp;
//!     } smcp;
//! "#;
//!
//! // Simple parsing without glyph name resolution
//! let feature_file = FeatureFile::try_from(fea_code).unwrap();
//!
//! // Or with full resolution support
//! let feature_file = FeatureFile::new_from_fea(
//!     fea_code,
//!     Some(&["a", "a.smcp", "b", "b.smcp"]), // Glyph names
//!     None::<&str>, // Project root for includes
//! ).unwrap();
//!
//! // Serialize back to .fea syntax
//! let output = feature_file.as_fea("");
//! println!("{}", output);
//! ```
//!
//! ### Constructing New Statements
//!
//! Build feature file structures programmatically:
//!
//! ```rust
//! use fea_rs_ast::*;
//!
//! // Create a glyph class definition
//! let lowercase = GlyphClassDefinition::new(
//!     "lowercase".to_string(),
//!     GlyphClass::new(vec![
//!         GlyphContainer::GlyphName(GlyphName::new("a")),
//!         GlyphContainer::GlyphName(GlyphName::new("b")),
//!         GlyphContainer::GlyphName(GlyphName::new("c")),
//!     ], 0..0),
//!     0..0, // location range
//! );
//!
//! // Create a single substitution statement
//! let subst = SingleSubstStatement::new(
//!     vec![GlyphContainer::GlyphName(GlyphName::new("a"))],
//!     vec![GlyphContainer::GlyphName(GlyphName::new("a.smcp"))],
//!     vec![], // prefix
//!     vec![], // suffix
//!     0..0,   // location
//!     false,  // force_chain
//! );
//!
//! // Create a feature block
//! let feature = FeatureBlock::new(
//!     "smcp".into(),
//!     vec![Statement::SingleSubst(subst)],
//!     false, // use_extension
//!     0..0,  // location
//! );
//!
//! // Build the complete feature file
//! let feature_file = FeatureFile::new(vec![
//!     ToplevelItem::GlyphClassDefinition(lowercase),
//!     ToplevelItem::Feature(feature),
//! ]);
//!
//! // Serialize to .fea syntax
//! let output = feature_file.as_fea("");
//! assert!(output.contains("@lowercase = [a b c];"));
//! assert!(output.contains("feature smcp"));
//! assert!(output.contains("sub a by a.smcp;"));
//! ```
//!
//! ### Using the Visitor Pattern
//!
//! Transform AST structures by implementing the [`LayoutVisitor`] trait:
//!
//! ```rust
//! use fea_rs_ast::*;
//!
//! // Create a visitor that renames all features
//! struct FeatureRenamer {
//!     old_name: String,
//!     new_name: String,
//! }
//!
//! impl LayoutVisitor for FeatureRenamer {
//!     fn visit_statement(&mut self, statement: &mut Statement) -> bool {
//!         match statement {
//!             Statement::FeatureBlock(feature) => {
//!                 if feature.name == self.old_name.as_str() {
//!                     feature.name = self.new_name.as_str().into();
//!                 }
//!             }
//!             _ => {}
//!         }
//!         true // Continue visiting
//!     }
//! }
//!
//! // Use the visitor
//! let fea_code = r#"
//!     feature liga {
//!         sub f i by fi;
//!     } liga;
//! "#;
//!
//! let mut feature_file = FeatureFile::try_from(fea_code).unwrap();
//! let mut visitor = FeatureRenamer {
//!     old_name: "liga".to_string(),
//!     new_name: "dlig".to_string(),
//! };
//!
//! visitor.visit(&mut feature_file).unwrap();
//!
//! let output = feature_file.as_fea("");
//! assert!(output.contains("feature dlig"));
//! ```
//!
//! ### More Complex Visitor: Glyph Name Substitution
//!
//! ```rust
//! use fea_rs_ast::*;
//! use std::collections::HashMap;
//!
//! // Visitor that replaces glyph names throughout the AST
//! struct GlyphNameReplacer {
//!     replacements: HashMap<String, String>,
//! }
//!
//! impl LayoutVisitor for GlyphNameReplacer {
//!     fn visit_statement(&mut self, statement: &mut Statement) -> bool {
//!         // Replace glyph names in various statement types
//!         match statement {
//!             Statement::SingleSubst(subst) => {
//!                 for container in &mut subst.glyphs {
//!                     self.replace_in_container(container);
//!                 }
//!                 for container in &mut subst.replacement {
//!                     self.replace_in_container(container);
//!                 }
//!             }
//!             Statement::GlyphClassDefinition(gcd) => {
//!                 for container in &mut gcd.glyphs.glyphs {
//!                     self.replace_in_container(container);
//!                 }
//!             }
//!             _ => {}
//!         }
//!         true
//!     }
//! }
//!
//! impl GlyphNameReplacer {
//!     fn replace_in_container(&self, container: &mut GlyphContainer) {
//!         match container {
//!             GlyphContainer::GlyphName(gn) => {
//!                 if let Some(new_name) = self.replacements.get(gn.name.as_str()) {
//!                     gn.name = new_name.as_str().into();
//!                 }
//!             }
//!             GlyphContainer::GlyphClass(gc) => {
//!                 for glyph_container in &mut gc.glyphs {
//!                     self.replace_in_container(glyph_container);
//!                 }
//!             }
//!             _ => {}
//!         }
//!     }
//! }
//! ```
//!
//! ## Feature Coverage
//!
//! This crate supports most OpenType feature file constructs:
//!
//! - **GSUB**: Single, Multiple, Alternate, Ligature, Contextual, and Reverse Chaining substitutions
//! - **GPOS**: Single, Pair, Cursive, Mark-to-Base, Mark-to-Ligature, and Mark-to-Mark positioning
//! - **Tables**: GDEF, BASE, head, hhea, name, OS/2, STAT, vhea
//! - **Contextual Rules**: Chaining context and ignore statements
//! - **Variable Fonts**: Conditionsets and variation blocks
//! - **Lookups**: Lookup blocks with flags and references
//! - **Features**: Feature blocks with useExtension
//!
//! Features which fea-rs parses which this crate does not currently support:
//!
//! - Glyphs number variables in value records
//! - CID-keyed glyph names
//!
//! ## Compatibility
//!
//! The API closely mirrors fontTools' Python API where practical, making it easier to port
//! existing Python code to Rust. Key differences:
//!
//! - Rust's type system provides compile-time guarantees about statement validity
//! - The [`Statement`] enum distinguishes between all possible statements
//! - The [`ToplevelItem`] enum ensures only valid top-level constructs
//! - Location tracking uses byte ranges (`Range<usize>`) instead of line/column numbers
//!
//! ## Re-exports
//!
//! This crate re-exports the underlying [`fea_rs`] parser for advanced use cases where
//! direct access to the parse tree is needed.

use std::{
    ops::Range,
    path::{Path, PathBuf},
    sync::Arc,
};
mod base;
mod contextual;
mod dummyresolver;
mod error;
mod gdef;
mod glyphcontainers;
mod gpos;
mod gsub;
mod miscellenea;
mod name;
mod os2;
mod stat;
mod tables;
mod values;
mod visitor;
pub use contextual::*;
pub use error::Error;
pub use fea_rs;
use fea_rs::{parse::FileSystemResolver, typed::AstNode as _, GlyphMap, NodeOrToken, ParseTree};
pub use gdef::*;
pub use glyphcontainers::*;
pub use gpos::*;
pub use gsub::*;
pub use miscellenea::*;
pub use name::*;
use smol_str::SmolStr;
pub use tables::*;
pub use values::*;
pub use visitor::LayoutVisitor;

use crate::{base::Base, os2::Os2};

pub(crate) const SHIFT: &str = "    ";

/// Trait for converting AST nodes back to feature file syntax.
pub trait AsFea {
    /// Convert the AST node to feature file syntax with the given indentation.
    fn as_fea(&self, indent: &str) -> String;
}

// All possible statements in a feature file need to go
// here, regardless of context, because we need to be able to
// treat them as a heterogeneous collection when we do visiting etc.
// We split them up by context in later enums.
/// An AST node representing a single statement in a feature file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    // GSUB statements
    /// A single substitution (GSUB type 1) statement: `sub a by b;`
    SingleSubst(SingleSubstStatement),
    /// A multiple substitution (GSUB type 2) statement: `sub a by b c;`
    MultipleSubst(MultipleSubstStatement),
    /// An alternate substitution (GSUB type 3) statement: `sub a from [b c d];`
    AlternateSubst(AlternateSubstStatement),
    /// A ligature substitution (GSUB type 4) statement: `sub a b by c;`
    LigatureSubst(LigatureSubstStatement),
    /// A reverse chaining contextual single substitution (GSUB type 8) statement
    ReverseChainSubst(ReverseChainSingleSubstStatement),
    /// A chaining contextual substitution (GSUB type 6) statement: `sub a' lookup foo b;`
    ChainedContextSubst(ChainedContextStatement<Subst>),
    /// An ignore substitution rule: `ignore sub a b;`
    IgnoreSubst(IgnoreStatement<Subst>),
    // GPOS
    /// A single adjustment positioning (GPOS type 1) statement: `pos a <10 0 20 0>;`
    SinglePos(SinglePosStatement),
    /// A pair adjustment positioning (GPOS type 2) statement: `pos a b <10 0 20 0>;`
    PairPos(PairPosStatement),
    /// A cursive attachment positioning (GPOS type 3) statement
    CursivePos(CursivePosStatement),
    /// A mark-to-base attachment positioning (GPOS type 4) statement
    MarkBasePos(MarkBasePosStatement),
    /// A mark-to-ligature attachment positioning (GPOS type 5) statement
    MarkLigPos(MarkLigPosStatement),
    /// A mark-to-mark attachment positioning (GPOS type 6) statement
    MarkMarkPos(MarkMarkPosStatement),
    /// A chaining contextual positioning (GPOS type 8) statement: `pos a' lookup foo b;`
    ChainedContextPos(ChainedContextStatement<Pos>),
    /// An ignore positioning rule: `ignore pos a b;`
    IgnorePos(IgnoreStatement<Pos>),
    // Miscellenea
    /// An anchor definition: `anchorDef 100 200 contourpoint 5 MyAnchor;`
    AnchorDefinition(AnchorDefinition),
    /// A mark class definition: `markClass a <anchor 100 200> @TOP_MARKS;`
    MarkClassDefinition(MarkClassDefinition),
    /// A comment in the feature file: `# This is a comment`
    Comment(Comment),
    /// A feature name statement within a `featureNames` block
    FeatureNameStatement(NameRecord),
    /// A font revision statement: `FontRevision 1.000;`
    FontRevision(FontRevisionStatement),
    /// A feature reference statement: `feature liga;`
    FeatureReference(FeatureReferenceStatement),
    /// A glyph class definition: `@lowercase = [a b c];`
    GlyphClassDefinition(GlyphClassDefinition),
    /// A language statement: `language dflt;`
    Language(LanguageStatement),
    /// A language system statement: `languagesystem DFLT dflt;`
    LanguageSystem(LanguageSystemStatement),
    /// A lookup flag statement: `lookupflag RightToLeft;`
    LookupFlag(LookupFlagStatement),
    /// A lookup reference statement: `lookup MyLookup;`
    LookupReference(LookupReferenceStatement),
    /// Size feature parameters: `parameters 10.0 0;`
    SizeParameters(SizeParameters),
    /// A size menu name statement: `sizemenuname 3 1 0x409 "Small";`
    SizeMenuName(NameRecord),
    /// A subtable statement: `subtable;`
    Subtable(SubtableStatement),
    /// A script statement: `script latn;`
    Script(ScriptStatement),
    /// A value record definition: `valueRecordDef 10 MyValue;`
    ValueRecordDefinition(ValueRecordDefinition),
    /// A condition set for variable fonts: `conditionset heavy { wght 700 900; } heavy;`
    ConditionSet(ConditionSet),
    /// A variation block for variable fonts: `variation rvrn heavy { ... } rvrn;`
    VariationBlock(VariationBlock),
    // Tables and blocks
    /// A BASE table definition: `table BASE { ... } BASE;`
    Base(Table<Base>),
    /// A GDEF table definition: `table GDEF { ... } GDEF;`
    Gdef(Table<Gdef>),
    /// A head table definition: `table head { ... } head;`
    Head(Table<Head>),
    /// An hhea table definition: `table hhea { ... } hhea;`
    Hhea(Table<Hhea>),
    /// A name table definition: `table name { ... } name;`
    Name(Table<Name>),
    /// An OS/2 table definition: `table OS/2 { ... } OS/2;`
    Os2(Table<Os2>),
    /// A STAT table definition: `table STAT { ... } STAT;`
    Stat(Table<Stat>),
    /// A vhea table definition: `table vhea { ... } vhea;`
    Vhea(Table<Vhea>),
    /// A feature block: `feature liga { ... } liga;`
    FeatureBlock(FeatureBlock),
    /// A lookup block: `lookup MyLookup { ... } MyLookup;`
    LookupBlock(LookupBlock),
    /// A nested block (e.g., `featureNames { ... };`)
    NestedBlock(NestedBlock),
    // GDEF-related statements
    /// A GDEF Attach statement: `Attach a 1 2 3;`
    GdefAttach(AttachStatement),
    /// A GDEF GlyphClassDef statement: `GlyphClassDef [a b], [c d], , [e f];`
    GdefClassDef(GlyphClassDefStatement),
    /// A GDEF LigatureCaretByIndex statement: `LigatureCaret a 1 2;`
    GdefLigatureCaretByIndex(LigatureCaretByIndexStatement),
    /// A GDEF LigatureCaretByPos statement: `LigatureCaretByPos a 100 200;`
    GdefLigatureCaretByPos(LigatureCaretByPosStatement),
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
            Statement::Comment(c) => c.as_fea(indent),
            Statement::FeatureReference(fr) => fr.as_fea(indent),
            Statement::FeatureNameStatement(fr) => fr.as_fea(indent),
            Statement::FontRevision(fr) => fr.as_fea(indent),
            Statement::GlyphClassDefinition(gcd) => gcd.as_fea(indent),
            Statement::Language(ls) => ls.as_fea(indent),
            Statement::LanguageSystem(ls) => ls.as_fea(indent),
            Statement::LookupFlag(lf) => lf.as_fea(indent),
            Statement::LookupReference(lr) => lr.as_fea(indent),
            Statement::MarkClassDefinition(mc) => mc.as_fea(indent),
            Statement::Script(sc) => sc.as_fea(indent),
            Statement::SizeMenuName(sm) => sm.as_fea(indent),
            Statement::SizeParameters(sp) => sp.as_fea(indent),
            Statement::Subtable(st) => st.as_fea(indent),
            Statement::ValueRecordDefinition(vrd) => vrd.as_fea(indent),
            Statement::ConditionSet(cs) => cs.as_fea(indent),
            Statement::VariationBlock(vb) => vb.as_fea(indent),
            // GDEF-related statements
            Statement::GdefAttach(at) => at.as_fea(indent),
            Statement::GdefClassDef(gcd) => gcd.as_fea(indent),
            Statement::GdefLigatureCaretByIndex(lc) => lc.as_fea(indent),
            Statement::GdefLigatureCaretByPos(lc) => lc.as_fea(indent),
            // Tables and blocks
            Statement::Base(base) => base.as_fea(indent),
            Statement::Gdef(gdef) => gdef.as_fea(indent),
            Statement::Head(head) => head.as_fea(indent),
            Statement::Hhea(hhea) => hhea.as_fea(indent),
            Statement::Name(name) => name.as_fea(indent),
            Statement::Os2(os2) => os2.as_fea(indent),
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
        Some(Statement::GdefAttach(at.into()))
    } else if let Some(gcd) = fea_rs::typed::GdefClassDef::cast(child) {
        Some(Statement::GdefClassDef(gcd.into()))
    } else if let Some(lc) = fea_rs::typed::GdefLigatureCaret::cast(child) {
        // Check if it's by position or by index based on the first keyword
        let is_by_pos = lc
            .iter()
            .next()
            .map(|t| t.kind() == fea_rs::Kind::LigatureCaretByPosKw)
            .unwrap_or(false);
        if is_by_pos {
            Some(Statement::GdefLigatureCaretByPos(lc.into()))
        } else {
            Some(Statement::GdefLigatureCaretByIndex(lc.into()))
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
    } else if let Some(vrd) = fea_rs::typed::ValueRecordDef::cast(child) {
        Some(Statement::ValueRecordDefinition(vrd.into()))
    } else if let Some(cs) = fea_rs::typed::ConditionSet::cast(child) {
        Some(Statement::ConditionSet(cs.into()))
    } else if let Some(fv) = fea_rs::typed::FeatureVariation::cast(child) {
        Some(Statement::VariationBlock(fv.into()))
    // Lookup blocks can exist within features
    } else if let Some(lookup) = fea_rs::typed::LookupBlock::cast(child) {
        Some(Statement::LookupBlock(lookup.into()))
    } else {
        None
    }
}

/// A named feature block. (`feature foo { ... } foo;`)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FeatureBlock {
    /// The name of the feature (also called the tag)
    pub name: SmolStr,
    /// The statements in the feature block
    pub statements: Vec<Statement>,
    /// Whether the feature uses `useExtension`
    pub use_extension: bool,
    /// The position of the feature block in the source
    pub pos: Range<usize>,
}

impl FeatureBlock {
    /// Creates a new FeatureBlock.
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
            "{}\n",
            self.statements
                .iter()
                .map(|s| s.as_fea(&mid_indent))
                .collect::<Vec<_>>()
                .join(&format!("\n{mid_indent}"))
        ));
        res.push_str(&format!("{}}} {};", indent, self.name));
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

/// A named lookup block. (`lookup foo { ... } foo;`)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LookupBlock {
    /// The name of the lookup
    pub name: SmolStr,
    /// The statements in the lookup block
    pub statements: Vec<Statement>,
    /// Whether the lookup should be placed in a separate extension subtable
    pub use_extension: bool,
    /// The position of the lookup block in the source
    pub pos: Range<usize>,
}

impl LookupBlock {
    /// Creates a new LookupBlock.
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
        res.push_str(&format!("{}}} {};", indent, self.name));
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

/// A nested block containing statements (e.g., `featureNames { ... };`)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NestedBlock {
    /// The tag identifying the block type
    pub tag: SmolStr,
    /// The statements contained in the block
    pub statements: Vec<Statement>,
    /// The position of the block in the source
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

/// Statements that can appear at the top level of a feature file.
///
/// This is a subset of [`Statement`] containing only constructs that are
/// valid at the top level, excluding statements that can only appear within
/// features, lookups, or table definitions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ToplevelItem {
    /// A glyph class definition: `@lowercase = [a b c];`
    GlyphClassDefinition(GlyphClassDefinition),
    /// A mark class definition: `markClass a <anchor 100 200> @TOP_MARKS;`
    MarkClassDefinition(MarkClassDefinition),
    /// A language system statement: `languagesystem DFLT dflt;`
    LanguageSystem(LanguageSystemStatement),
    // Include(IncludeStatement),
    /// A feature block: `feature liga { ... } liga;`
    Feature(FeatureBlock),
    /// A lookup block: `lookup MyLookup { ... } MyLookup;`
    Lookup(LookupBlock),
    /// A comment in the feature file: `# This is a comment`
    Comment(Comment),
    /// An anchor definition: `anchorDef 100 200 contourpoint 5 MyAnchor;`
    AnchorDefinition(AnchorDefinition),
    /// A value record definition: `valueRecordDef 10 MyValue;`
    ValueRecordDefinition(ValueRecordDefinition),
    /// A condition set for variable fonts: `conditionset heavy { wght 700 900; } heavy;`
    ConditionSet(ConditionSet),
    /// A variation block for variable fonts: `variation rvrn heavy { ... } rvrn;`
    VariationBlock(VariationBlock),
    // Tables
    /// A BASE table definition: `table BASE { ... } BASE;`
    Base(Table<Base>),
    /// A GDEF table definition: `table GDEF { ... } GDEF;`
    Gdef(Table<Gdef>),
    /// A head table definition: `table head { ... } head;`
    Head(Table<Head>),
    /// An hhea table definition: `table hhea { ... } hhea;`
    Hhea(Table<Hhea>),
    /// A name table definition: `table name { ... } name;`
    Name(Table<Name>),
    /// An OS/2 table definition: `table OS/2 { ... } OS/2;`
    Os2(Table<Os2>),
    /// A STAT table definition: `table STAT { ... } STAT;`
    Stat(Table<Stat>),
    /// A vhea table definition: `table vhea { ... } vhea;`
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
            ToplevelItem::ValueRecordDefinition(vrd) => Statement::ValueRecordDefinition(vrd),
            ToplevelItem::ConditionSet(cs) => Statement::ConditionSet(cs),
            ToplevelItem::VariationBlock(vb) => Statement::VariationBlock(vb),
            ToplevelItem::Base(base) => Statement::Base(base),
            ToplevelItem::Gdef(gdef) => Statement::Gdef(gdef),
            ToplevelItem::Head(head) => Statement::Head(head),
            ToplevelItem::Hhea(hhea) => Statement::Hhea(hhea),
            ToplevelItem::Name(name) => Statement::Name(name),
            ToplevelItem::Os2(os2) => Statement::Os2(os2),
            ToplevelItem::Stat(stat) => Statement::Stat(stat),
            ToplevelItem::Vhea(vhea) => Statement::Vhea(vhea),
        }
    }
}
impl TryFrom<Statement> for ToplevelItem {
    type Error = crate::Error;

    fn try_from(value: Statement) -> Result<Self, Self::Error> {
        match value {
            Statement::GlyphClassDefinition(gcd) => Ok(ToplevelItem::GlyphClassDefinition(gcd)),
            Statement::MarkClassDefinition(mcd) => Ok(ToplevelItem::MarkClassDefinition(mcd)),
            Statement::LanguageSystem(ls) => Ok(ToplevelItem::LanguageSystem(ls)),
            Statement::FeatureBlock(fb) => Ok(ToplevelItem::Feature(fb)),
            Statement::LookupBlock(lb) => Ok(ToplevelItem::Lookup(lb)),
            Statement::Comment(cmt) => Ok(ToplevelItem::Comment(cmt)),
            Statement::AnchorDefinition(ad) => Ok(ToplevelItem::AnchorDefinition(ad)),
            Statement::ValueRecordDefinition(vrd) => Ok(ToplevelItem::ValueRecordDefinition(vrd)),
            Statement::ConditionSet(cs) => Ok(ToplevelItem::ConditionSet(cs)),
            Statement::VariationBlock(vb) => Ok(ToplevelItem::VariationBlock(vb)),
            Statement::Base(base) => Ok(ToplevelItem::Base(base)),
            Statement::Gdef(gdef) => Ok(ToplevelItem::Gdef(gdef)),
            Statement::Head(head) => Ok(ToplevelItem::Head(head)),
            Statement::Hhea(hhea) => Ok(ToplevelItem::Hhea(hhea)),
            Statement::Name(name) => Ok(ToplevelItem::Name(name)),
            Statement::Os2(os2) => Ok(ToplevelItem::Os2(os2)),
            Statement::Stat(stat) => Ok(ToplevelItem::Stat(stat)),
            Statement::Vhea(vhea) => Ok(ToplevelItem::Vhea(vhea)),
            _ => Err(crate::Error::CannotConvert),
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
            ToplevelItem::ValueRecordDefinition(vrd) => vrd.as_fea(indent),
            ToplevelItem::ConditionSet(cs) => cs.as_fea(indent),
            ToplevelItem::VariationBlock(vb) => vb.as_fea(indent),
            ToplevelItem::Base(base) => base.as_fea(indent),
            ToplevelItem::Gdef(gdef) => gdef.as_fea(indent),
            ToplevelItem::Head(head) => head.as_fea(indent),
            ToplevelItem::Hhea(hhea) => hhea.as_fea(indent),
            ToplevelItem::Name(name) => name.as_fea(indent),
            ToplevelItem::Os2(os2) => os2.as_fea(indent),
            ToplevelItem::Stat(stat) => stat.as_fea(indent),
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
    } else if let Some(vrd) = fea_rs::typed::ValueRecordDef::cast(child) {
        Some(ToplevelItem::ValueRecordDefinition(vrd.into()))
    } else if let Some(cs) = fea_rs::typed::ConditionSet::cast(child) {
        Some(ToplevelItem::ConditionSet(cs.into()))
    } else if let Some(fv) = fea_rs::typed::FeatureVariation::cast(child) {
        Some(ToplevelItem::VariationBlock(fv.into()))
    } else if let Some(base) = fea_rs::typed::BaseTable::cast(child) {
        Some(ToplevelItem::Base(base.into()))
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
    } else if let Some(os2) = fea_rs::typed::Os2Table::cast(child) {
        Some(ToplevelItem::Os2(os2.into()))
    } else if let Some(stat) = fea_rs::typed::StatTable::cast(child) {
        Some(ToplevelItem::Stat(stat.into()))
    } else {
        None
    }
}

/// A complete OpenType Feature File.
///
/// This is the root structure representing a parsed .fea file, containing
/// a sequence of top-level statements such as glyph class definitions,
/// feature blocks, lookup blocks, and table definitions.
///
/// # Examples
///
/// ```
/// use fea_rs_ast::FeatureFile;
///
/// let fea_code = "languagesystem DFLT dflt;";
/// let feature_file = FeatureFile::try_from(fea_code).unwrap();
/// ```
pub struct FeatureFile {
    /// The top-level statements in the feature file
    pub statements: Vec<ToplevelItem>,
}
impl FeatureFile {
    /// Creates a new `FeatureFile` from a list of top-level statements.
    pub fn new(statements: Vec<ToplevelItem>) -> Self {
        Self { statements }
    }

    /// Returns an iterator over the top-level statements in the file.
    pub fn iter(&self) -> impl Iterator<Item = &ToplevelItem> {
        self.statements.iter()
    }

    /// Parses a feature file from a string with optional glyph name resolution.
    ///
    /// # Arguments
    ///
    /// * `features` - The feature file source code as a string
    /// * `glyph_names` - Optional list of glyph names for validation and range expansion
    /// * `project_root` - Optional project root directory for resolving `include` statements
    ///
    /// # Examples
    ///
    /// ```
    /// use fea_rs_ast::FeatureFile;
    ///
    /// let fea_code = "languagesystem DFLT dflt;";
    /// let feature_file = FeatureFile::new_from_fea(
    ///     fea_code,
    ///     None::<&[&str]>,
    ///     None::<&str>,
    /// ).unwrap();
    /// ```
    pub fn new_from_fea(
        features: &str,
        glyph_names: Option<&[&str]>,
        project_root: Option<impl Into<PathBuf>>,
    ) -> Result<Self, crate::Error> {
        let glyph_map = glyph_names.map(|gn| GlyphMap::from_iter(gn.iter().cloned()));
        let resolver: Box<dyn fea_rs::parse::SourceResolver> =
            if let Some(project_root) = project_root {
                let path = project_root.into();
                Box::new(FileSystemResolver::new(path))
            } else {
                Box::new(dummyresolver::DummyResolver)
            };
        let features_text: Arc<str> = Arc::from(features);
        let (parse_tree, mut diagnostics) = fea_rs::parse::parse_root(
            "get_parse_tree".into(),
            glyph_map.as_ref(),
            Box::new(move |s: &Path| {
                if s == Path::new("get_parse_tree") {
                    Ok(features_text.clone())
                } else {
                    let path = resolver.resolve_raw_path(s.as_ref(), None);
                    let canonical = resolver.canonicalize(&path)?;
                    resolver.get_contents(&canonical)
                }
            }),
        )?;
        diagnostics.split_off_warnings();
        if diagnostics.has_errors() {
            return Err(crate::Error::FeatureParsing(diagnostics));
        }
        Ok(parse_tree.into())
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

/// Turn a string into a FeatureFile
///
/// Only suitable for simple cases and testing; does not resolve glyph name
/// ranges or includes.
impl TryFrom<&str> for FeatureFile {
    type Error = fea_rs::DiagnosticSet;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let (parsed, diag) = fea_rs::parse::parse_string(value);
        if diag.has_errors() {
            Err(diag)
        } else {
            Ok(parsed.into())
        }
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
            normalize_whitespace(&feature_block.as_fea("")),
            normalize_whitespace("feature smcp {\n    sub a by a.smcp;\n} smcp;\n")
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
            panic!("fea-rs didn't like file {:?}:\n{:#?}", path, diag);
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
