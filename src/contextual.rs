use std::{fmt::Display, ops::Range};

use fea_rs::{
    Kind,
    typed::{AstNode as _, GlyphOrClass, GposIgnore, GsubIgnore},
};
use smol_str::SmolStr;

use crate::{
    AsFea, GlyphContainer, LigatureSubstStatement, MultipleSubstStatement, SingleSubstStatement,
    Statement,
};

pub(crate) fn backtrack(val: &fea_rs::Node) -> Vec<GlyphContainer> {
    fea_rs::Node::iter_children(val)
        .find(|c| c.kind() == Kind::BacktrackSequence)
        .unwrap()
        .as_node()
        .unwrap()
        .iter_children()
        .filter_map(GlyphOrClass::cast)
        .map(|goc| goc.into())
        .collect()
}
pub(crate) fn lookahead(val: &fea_rs::Node) -> Vec<GlyphContainer> {
    fea_rs::Node::iter_children(val)
        .find(|c| c.kind() == Kind::LookaheadSequence)
        .unwrap()
        .as_node()
        .unwrap()
        .iter_children()
        .take_while(|c| c.kind() != Kind::InlineSubNode)
        .filter_map(GlyphOrClass::cast)
        .map(|goc| goc.into())
        .collect()
}

pub(crate) fn context_glyphs(val: &fea_rs::Node) -> Vec<GlyphContainer> {
    let glyphnodes = fea_rs::Node::iter_children(val)
        .find(|c| c.kind() == Kind::ContextSequence)
        .unwrap()
        .as_node()
        .unwrap()
        .iter_children()
        .filter(|c| c.kind() == Kind::ContextGlyphNode)
        .collect::<Vec<_>>();
    glyphnodes
        .iter()
        .flat_map(|gn| {
            gn.as_node()
                .unwrap()
                .iter_children()
                .filter_map(GlyphOrClass::cast)
        })
        .map(|goc| goc.into())
        .collect()
}

/// A chained contextual substitution statement, either GSUB or GPOS.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChainedContextStatement<T: SubOrPos> {
    /// The location of the statement in the source FEA.
    pub location: Range<usize>,
    /// The prefix (backtrack) glyphs
    pub prefix: Vec<GlyphContainer>,
    /// The suffix (lookahead) glyphs
    pub suffix: Vec<GlyphContainer>,
    /// Input glyphs
    pub glyphs: Vec<GlyphContainer>,
    /// Lookups to apply at each glyph position
    pub lookups: Vec<Vec<SmolStr>>,
    sub_or_pos: T,
}

impl<T: SubOrPos> ChainedContextStatement<T> {
    /// Create a new chained contextual statement.
    ///
    /// ``prefix``, ``glyphs``, and ``suffix`` should be lists of
    /// `glyph-containing objects`_ .
    ///
    /// ``lookups`` should be a list of elements representing what lookups
    /// to apply at each glyph position. Each element should be a
    /// :class:`LookupBlock` to apply a single chaining lookup at the given
    /// position, a list of :class:`LookupBlock`\ s to apply multiple
    /// lookups, or ``None`` to apply no lookup. The length of the outer
    /// list should equal the length of ``glyphs``; the inner lists can be
    /// of variable length
    pub fn new(
        glyphs: Vec<GlyphContainer>,
        prefix: Vec<GlyphContainer>,
        suffix: Vec<GlyphContainer>,
        lookups: Vec<Vec<SmolStr>>,
        location: Range<usize>,
        sub_or_pos: T,
    ) -> Self {
        Self {
            prefix,
            suffix,
            glyphs,
            lookups,
            location,
            sub_or_pos,
        }
    }
}

impl<T: SubOrPos> PotentiallyContextualStatement for ChainedContextStatement<T> {
    fn is_contextual(&self) -> bool {
        true
    }
    fn prefix(&self) -> &[GlyphContainer] {
        &self.prefix
    }
    fn suffix(&self) -> &[GlyphContainer] {
        &self.suffix
    }
    fn force_chain(&self) -> bool {
        true
    }
    fn format_begin(&self, _indent: &str) -> String {
        format!("{} ", self.sub_or_pos)
    }
    fn format_contextual_parts(&self, indent: &str) -> Vec<String> {
        let mut parts = Vec::new();
        for (i, g) in self.glyphs.iter().enumerate() {
            let mut s = format!("{}'", g.as_fea(indent));
            if !self.lookups[i].is_empty() {
                for lu in &self.lookups[i] {
                    s.push_str(&format!(" lookup {}", lu));
                }
            }
            parts.push(s);
        }
        parts
    }
    fn format_noncontextual_parts(&self, _indent: &str) -> Vec<String> {
        unreachable!()
    }
}

impl From<fea_rs::typed::Gsub6> for Statement {
    fn from(val: fea_rs::typed::Gsub6) -> Self {
        // There are four possible forms here:
        // If it has an InlineSubNode child, it's a contextual form of one of
        // the other gsub types. If not, and if there is a LookupRefNode, we return it as a normal ChainedContextualSubstStatement.
        // To distinguish, we count the context and target glyphs.
        // We need to find a LookupRefNode within the ContextGlyphNode within ContextSequence
        let prefix = backtrack(val.node());
        let suffix = lookahead(val.node());
        let context_glyph_nodes = fea_rs::Node::iter_children(val.node())
            .find(|c| c.kind() == Kind::ContextSequence)
            .unwrap()
            .as_node()
            .unwrap()
            .iter_children()
            .filter(|c| c.kind() == Kind::ContextGlyphNode)
            .collect::<Vec<_>>(); // Safe?

        if let Some((context_glyphs, lookups)) = check_for_simple_contextual(&context_glyph_nodes) {
            return Statement::ChainedContextSubst(ChainedContextStatement::new(
                context_glyphs,
                prefix,
                suffix,
                lookups,
                val.node().range(),
                Subst,
            ));
        }
        // I'm assuming there's an InlineSubNode here, let's find it.
        let Some(inline_sub) = val
            .node()
            .iter_children()
            .find_map(fea_rs::typed::InlineSubRule::cast)
        else {
            panic!(
                "No LookRefNode or InlineSubNode found in Gsub6, can't get here, fea-rs has failed me: {}",
                val.node()
                    .iter_tokens()
                    .map(|t| t.text.clone())
                    .collect::<Vec<_>>()
                    .join("")
            );
        };
        let target_glyphs = inline_sub
            .node()
            .iter_children()
            .filter_map(GlyphOrClass::cast)
            .map(|goc| goc.into())
            .collect::<Vec<_>>();
        let mut context_glyphs = context_glyphs(val.node());
        if target_glyphs.len() > 1 {
            return Statement::MultipleSubst(MultipleSubstStatement {
                prefix,
                suffix,
                glyph: context_glyphs.remove(0),
                replacement: target_glyphs,
                location: val.node().range(),
                force_chain: true,
            });
        }
        if context_glyphs.len() == 1 && target_glyphs.len() == 1 {
            return Statement::SingleSubst(SingleSubstStatement {
                prefix,
                suffix,
                glyphs: context_glyphs,
                replacement: target_glyphs,
                location: val.node().range(),
                force_chain: true,
            });
        }
        if context_glyphs.len() > 1 && target_glyphs.len() == 1 {
            // It's a LigatureSubst, we don't support contextual AlternateSubst
            return Statement::LigatureSubst(LigatureSubstStatement {
                prefix,
                suffix,
                glyphs: context_glyphs,
                replacement: target_glyphs[0].clone(),
                location: val.node().range(),
                force_chain: true,
            });
        }
        panic!("Don't know what this GSUB6 is supposed to be!")
    }
}

fn check_for_simple_contextual(
    context_glyph_nodes: &[&fea_rs::NodeOrToken],
) -> Option<(Vec<GlyphContainer>, Vec<Vec<SmolStr>>)> {
    // Do we see any LookupRefNode children within the context glyph nodes?
    if context_glyph_nodes.iter().any(|cgn| {
        cgn.as_node()
            .unwrap()
            .iter_children()
            .any(|child| child.kind() == Kind::LookupRefNode)
    }) {
        // Within context_glyph_node we want to see a sequence of GlyphOrClass nodes,
        // each of which may have zero or more LookupRefNode children.
        let mut context_glyphs = Vec::new();
        let mut lookups = Vec::new();
        for context_glyph_node in context_glyph_nodes.iter() {
            let glyph_node = context_glyph_node.as_node().unwrap();
            for node in glyph_node.iter_children() {
                if let Some(goc) = GlyphOrClass::cast(node) {
                    context_glyphs.push(goc.into());
                    lookups.push(vec![]);
                } else if let Some(lookup_ref) = fea_rs::typed::LookupRef::cast(node)
                    && let Some(last) = lookups.last_mut()
                {
                    last.push(SmolStr::new(
                        &lookup_ref
                            .node()
                            .iter_tokens()
                            .find(|t| t.kind == Kind::Ident)
                            .unwrap()
                            .text,
                    ));
                }
            }
        }
        return Some((context_glyphs, lookups));
    }
    None
}

impl From<fea_rs::typed::Gpos8> for Statement {
    fn from(val: fea_rs::typed::Gpos8) -> Self {
        let prefix = backtrack(val.node());
        let suffix = lookahead(val.node());
        let context_glyph_nodes = fea_rs::Node::iter_children(val.node())
            .find(|c| c.kind() == Kind::ContextSequence)
            .unwrap()
            .as_node()
            .unwrap()
            .iter_children()
            .filter(|c| c.kind() == Kind::ContextGlyphNode)
            .collect::<Vec<_>>(); // Safe?
        if let Some((context_glyphs, lookups)) = check_for_simple_contextual(&context_glyph_nodes) {
            return Statement::ChainedContextPos(ChainedContextStatement::new(
                context_glyphs,
                prefix,
                suffix,
                lookups,
                val.node().range(),
                Pos,
            ));
        }
        // Each ContextGlyphNode should have a single GlyphOrClass child and a ValueRecord child
        let mut context_glyphs = Vec::new();
        for context_glyph_node in context_glyph_nodes.iter() {
            let glyph_node = context_glyph_node.as_node().unwrap();
            let glyph = glyph_node.iter_children().find_map(GlyphOrClass::cast);
            let value_record_node = glyph_node
                .iter_children()
                .find_map(fea_rs::typed::ValueRecord::cast);
            if let Some(goc) = glyph {
                context_glyphs.push((goc.into(), value_record_node.map(|vr| vr.into())));
            }
        }
        Statement::SinglePos(crate::gpos::SinglePosStatement::new(
            prefix,
            suffix,
            context_glyphs,
            true,
            val.node().range(),
        ))
    }
}

/// Type marker that the statement is a positioning statement
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pos;

/// Type marker that the statement is a substitution statement
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Subst;
/// A trait implemented by both Pos and Subst to allow generic handling
pub trait SubOrPos: Display {}
impl SubOrPos for Pos {}
impl SubOrPos for Subst {}
impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "pos")
    }
}
impl Display for Subst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "sub")
    }
}

/// Either a GPOS or GSUB ignore statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IgnoreStatement<T: SubOrPos> {
    /// The location of the statement in the source FEA.
    pub location: Range<usize>,
    /// The chain contexts: a list of (prefix, glyphs, suffix) tuples.
    pub chain_contexts: Vec<(
        Vec<GlyphContainer>,
        Vec<GlyphContainer>,
        Vec<GlyphContainer>,
    )>,
    sub_or_pos: T,
}

impl<T: SubOrPos> IgnoreStatement<T> {
    /// Creates a new IgnoreStatement.
    pub fn new(
        chain_contexts: Vec<(
            Vec<GlyphContainer>,
            Vec<GlyphContainer>,
            Vec<GlyphContainer>,
        )>,
        location: Range<usize>,
        sub_or_pos: T,
    ) -> Self {
        Self {
            chain_contexts,
            location,
            sub_or_pos,
        }
    }
}

impl<T: SubOrPos> AsFea for IgnoreStatement<T> {
    fn as_fea(&self, indent: &str) -> String {
        let mut res = String::new();
        res.push_str(&format!("ignore {} ", self.sub_or_pos));
        let contexts_str: Vec<String> = self
            .chain_contexts
            .iter()
            .map(|(prefix, glyphs, suffix)| {
                let mut s = String::new();
                if !prefix.is_empty() {
                    let prefix_str: Vec<String> = prefix.iter().map(|g| g.as_fea(indent)).collect();
                    s.push_str(&prefix_str.join(" ").to_string());
                    s.push(' ');
                }
                let glyphs_str: Vec<String> =
                    glyphs.iter().map(|g| g.as_fea(indent) + "'").collect();
                s.push_str(&glyphs_str.join(" "));
                if !suffix.is_empty() {
                    s.push(' ');
                    let suffix_str: Vec<String> = suffix.iter().map(|g| g.as_fea(indent)).collect();
                    s.push_str(&suffix_str.join(" "));
                }
                s
            })
            .collect();
        res.push_str(&contexts_str.join(", "));
        res.push(';');
        res
    }
}

impl From<GsubIgnore> for IgnoreStatement<Subst> {
    fn from(val: GsubIgnore) -> Self {
        let mut chain_contexts = Vec::new();
        for context in val.node().iter_children() {
            if let Some(chain_context) = fea_rs::typed::IgnoreRule::cast(context) {
                let prefix = backtrack(chain_context.node());
                let suffix = lookahead(chain_context.node());
                let glyphs = context_glyphs(chain_context.node());
                chain_contexts.push((prefix, glyphs, suffix));
            }
        }
        IgnoreStatement {
            chain_contexts,
            location: val.node().range(),
            sub_or_pos: Subst,
        }
    }
}

impl From<GposIgnore> for IgnoreStatement<Pos> {
    fn from(val: GposIgnore) -> Self {
        let mut chain_contexts = Vec::new();
        for context in val.node().iter_children() {
            if let Some(chain_context) = fea_rs::typed::IgnoreRule::cast(context) {
                let prefix = backtrack(chain_context.node());
                let suffix = lookahead(chain_context.node());
                let glyphs = context_glyphs(chain_context.node());
                chain_contexts.push((prefix, glyphs, suffix));
            }
        }
        IgnoreStatement {
            chain_contexts,
            location: val.node().range(),
            sub_or_pos: Pos,
        }
    }
}

// There's a parallelism in a few statement types that might have a
// prefix or a suffix, and the string joining for the FEA generation
// is always a bit fiddly.
// Put it here once and get it right for all.
pub(crate) trait PotentiallyContextualStatement {
    fn prefix(&self) -> &[GlyphContainer];
    fn suffix(&self) -> &[GlyphContainer];
    fn force_chain(&self) -> bool;
    fn format_begin(&self, indent: &str) -> String;
    fn format_contextual_parts(&self, indent: &str) -> Vec<String>;
    fn format_noncontextual_parts(&self, indent: &str) -> Vec<String>;
    fn format_end(&self, _indent: &str) -> String {
        "".to_string()
    }

    fn is_contextual(&self) -> bool {
        !self.prefix().is_empty() || !self.suffix().is_empty() || self.force_chain()
    }
}

impl<T: PotentiallyContextualStatement> AsFea for T {
    fn as_fea(&self, indent: &str) -> String {
        let mut res = String::new();
        res.push_str(&self.format_begin(indent));
        if self.is_contextual() {
            if !self.prefix().is_empty() {
                let prefix_str: Vec<String> = self.prefix().iter().map(|g| g.as_fea("")).collect();
                res.push_str(&prefix_str.join(" ").to_string());
                res.push(' ');
            }
            res.push_str(&self.format_contextual_parts(indent).join(" "));
            if !self.suffix().is_empty() {
                let suffix_str: Vec<String> = self.suffix().iter().map(|g| g.as_fea("")).collect();
                res.push_str(&format!(" {}", suffix_str.join(" ")));
            }
        } else {
            res.push_str(self.format_noncontextual_parts(indent).join(" ").as_str());
        }
        res.push_str(&self.format_end(indent));
        res.push(';');
        res
    }
}

#[cfg(test)]
mod tests {
    use crate::{GlyphClass, GlyphName};

    use super::*;

    #[test]
    fn test_generate_chain_subst() {
        let chain_subst = ChainedContextStatement::new(
            vec![
                GlyphContainer::GlyphName(GlyphName::new("x")),
                GlyphContainer::GlyphName(GlyphName::new("y")),
                GlyphContainer::GlyphName(GlyphName::new("z")),
            ],
            vec![GlyphContainer::GlyphClass(GlyphClass::new(
                vec![
                    GlyphContainer::GlyphName(GlyphName::new("a.smcp")),
                    GlyphContainer::GlyphName(GlyphName::new("b.smcp")),
                ],
                0..0,
            ))],
            vec![],
            vec![
                vec![SmolStr::new("lookup1")],
                vec![],
                vec![SmolStr::new("lookup2"), SmolStr::new("lookup3")],
            ],
            0..0,
            Subst,
        );
        assert_eq!(
            chain_subst.as_fea(""),
            "sub [a.smcp b.smcp] x' lookup lookup1 y' z' lookup lookup2 lookup lookup3;"
        );
    }

    #[test]
    fn chain_context_subst_from_gsub6() {
        let fea =
            r#"feature foo { sub x [a b] c' lookup test d' e' lookup bar lookup quux f; } foo;"#;
        let (parsed, _) = fea_rs::parse::parse_string(fea);
        let gsub6 = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::Gsub6::cast)
            })
            .unwrap();
        let statement = Statement::from(gsub6);
        let Statement::ChainedContextSubst(chain_subst) = statement else {
            panic!("Expected ChainedContextSubstStatement, got {:?}", statement);
        };
        assert_eq!(chain_subst.prefix.len(), 2);
        assert_eq!(chain_subst.suffix.len(), 1);
        assert_eq!(chain_subst.glyphs.len(), 3);
        assert_eq!(
            chain_subst.lookups,
            vec![
                vec![SmolStr::new("test")],
                vec![],
                vec![SmolStr::new("bar"), SmolStr::new("quux")]
            ]
        );
    }

    #[test]
    fn chain_context_subst_round_trip() {
        let fea =
            r#"feature foo { sub [a b] x' lookup test y' z' lookup bar lookup quux f; } foo;"#;
        let (parsed, _) = fea_rs::parse::parse_string(fea);
        let gsub6 = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::Gsub6::cast)
            })
            .unwrap();
        let Statement::ChainedContextSubst(chain_subst) = Statement::from(gsub6) else {
            panic!("Expected ChainedContextSubstStatement");
        };
        let fea_generated = chain_subst.as_fea("");
        assert_eq!(
            fea_generated,
            "sub [a b] x' lookup test y' z' lookup bar lookup quux f;"
        );
    }

    #[test]
    fn generate_ignore_subst() {
        let ignore_subst = IgnoreStatement::new(
            vec![
                (
                    vec![GlyphContainer::GlyphName(GlyphName::new("a"))],
                    vec![GlyphContainer::GlyphName(GlyphName::new("x"))],
                    vec![GlyphContainer::GlyphName(GlyphName::new("b"))],
                ),
                (
                    vec![],
                    vec![GlyphContainer::GlyphName(GlyphName::new("y"))],
                    vec![],
                ),
            ],
            0..0,
            Subst,
        );
        assert_eq!(ignore_subst.as_fea(""), "ignore sub a x' b, y';");
    }

    #[test]
    fn test_roundtrip_ignore_subst() {
        let fea = "feature foo { ignore sub a x' b, y'; } foo;";
        let (parsed, _) = fea_rs::parse::parse_string(fea);
        let gsub_ignore = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::GsubIgnore::cast)
            })
            .unwrap();
        let ignore_subst = IgnoreStatement::<Subst>::from(gsub_ignore);
        assert_eq!(ignore_subst.as_fea(""), "ignore sub a x' b, y';");
    }

    #[test]
    fn test_roundtrip_gsub1_contextual() {
        let fea = "feature smcp { sub x a' by a.smcp; } smcp;";
        let (parsed, _) = fea_rs::parse::parse_string(fea);
        let gsub6 = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::Gsub6::cast)
            })
            .unwrap();
        let single_subst = Statement::from(gsub6);
        assert!(
            matches!(
                single_subst,
                Statement::SingleSubst(SingleSubstStatement { .. })
            ),
            "Expected SingleSubstStatement, got {:?}",
            single_subst
        );
        assert_eq!(single_subst.as_fea(""), "sub x a' by a.smcp;");
    }

    #[test]
    fn test_roundtrip_gsub2_contextual() {
        let fea = "feature smcp { sub x a' by a b; } smcp;";
        let (parsed, _) = fea_rs::parse::parse_string(fea);
        let gsub6 = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::Gsub6::cast)
            })
            .unwrap();
        let mult_subst = Statement::from(gsub6);
        assert!(
            matches!(
                mult_subst,
                Statement::MultipleSubst(MultipleSubstStatement { .. })
            ),
            "Expected MultipleSubstStatement, got {:?}",
            mult_subst
        );
        assert_eq!(mult_subst.as_fea(""), "sub x a' by a b;");
    }

    #[test]
    fn test_roundtrip_gsub4_contextual() {
        let fea = "feature smcp { sub x a' b' by a; } smcp;";
        let (parsed, _) = fea_rs::parse::parse_string(fea);
        let gsub6 = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::Gsub6::cast)
            })
            .unwrap();
        let liga_subst = Statement::from(gsub6);
        assert!(
            matches!(
                liga_subst,
                Statement::LigatureSubst(LigatureSubstStatement { .. })
            ),
            "Expected LigatureSubstStatement, got {:?}",
            liga_subst
        );
        assert_eq!(
            liga_subst.as_fea(""),
            "sub x a' b' by a;",
            "{:#?}",
            liga_subst
        );
    }
}
