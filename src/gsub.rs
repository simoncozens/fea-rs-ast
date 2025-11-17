use fea_rs::{
    typed::{AstNode as _, GlyphOrClass},
    Kind,
};

use crate::contextual::{backtrack, context_glyphs, lookahead};
use crate::{AsFea, GlyphContainer};
use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SingleSubstStatement {
    pub location: Range<usize>,
    pub prefix: Vec<GlyphContainer>,
    pub suffix: Vec<GlyphContainer>,
    pub glyphs: Vec<GlyphContainer>,
    pub replacement: Vec<GlyphContainer>,
    pub force_chain: bool,
}

impl SingleSubstStatement {
    /// Create a new single substitution statement.
    ///
    /// Note the unusual argument order: `prefix` and suffix come `after`
    /// the replacement `glyphs`. `prefix`, `suffix`, `glyphs` and
    /// `replacement` should be lists of `glyph-containing objects`_. `glyphs` and
    /// `replacement` should be one-item lists.
    pub fn new(
        glyphs: Vec<GlyphContainer>,
        replacement: Vec<GlyphContainer>,
        prefix: Vec<GlyphContainer>,
        suffix: Vec<GlyphContainer>,
        location: Range<usize>,
        force_chain: bool,
    ) -> Self {
        Self {
            prefix,
            suffix,
            glyphs,
            replacement,
            location,
            force_chain,
        }
    }
}
impl AsFea for SingleSubstStatement {
    fn as_fea(&self, _indent: &str) -> String {
        let mut res = String::new();
        res.push_str("sub ");
        if !self.prefix.is_empty() || !self.suffix.is_empty() || self.force_chain {
            if !self.prefix.is_empty() {
                let prefix_str: Vec<String> =
                    self.prefix.iter().map(|g| g.as_fea("") + " ").collect();
                res.push_str(&prefix_str.join(" ").to_string());
            }
            let glyphs_str: Vec<String> = self
                .glyphs
                .iter()
                .map(|g| format!("{}'", g.as_fea("")))
                .collect();
            res.push_str(&glyphs_str.join(" "));
            if !self.suffix.is_empty() {
                let suffix_str: Vec<String> = self.suffix.iter().map(|g| g.as_fea("")).collect();
                res.push_str(&format!(" {}", suffix_str.join(" ")));
            }
        } else {
            let glyphs_str: Vec<String> = self.glyphs.iter().map(|g| g.as_fea("")).collect();
            res.push_str(&glyphs_str.join(" "));
        }
        let replacement_str: Vec<String> = self.replacement.iter().map(|g| g.as_fea("")).collect();
        res.push_str(&format!(" by {};", replacement_str.join(" ")));
        res
    }
}

impl From<fea_rs::typed::Gsub1> for SingleSubstStatement {
    fn from(val: fea_rs::typed::Gsub1) -> Self {
        let target = val
            .node()
            .iter_children()
            .find_map(GlyphOrClass::cast)
            .unwrap();
        let replacement = val
            .node()
            .iter_children()
            .skip_while(|t| t.kind() != Kind::ByKw)
            .find_map(GlyphOrClass::cast)
            .unwrap();
        SingleSubstStatement {
            prefix: vec![],
            suffix: vec![],
            glyphs: vec![target.into()],
            replacement: vec![replacement.into()],
            location: val.node().range(),
            force_chain: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultipleSubstStatement {
    pub location: Range<usize>,
    pub prefix: Vec<GlyphContainer>,
    pub suffix: Vec<GlyphContainer>,
    pub glyph: GlyphContainer,
    pub replacement: Vec<GlyphContainer>,
    pub force_chain: bool,
}

impl MultipleSubstStatement {
    /// Create a new multiple substitution statement.
    pub fn new(
        glyph: GlyphContainer,
        replacement: Vec<GlyphContainer>,
        prefix: Vec<GlyphContainer>,
        suffix: Vec<GlyphContainer>,
        location: Range<usize>,
        force_chain: bool,
    ) -> Self {
        Self {
            prefix,
            suffix,
            glyph,
            replacement,
            location,
            force_chain,
        }
    }
}
impl AsFea for MultipleSubstStatement {
    fn as_fea(&self, _indent: &str) -> String {
        let mut res = String::new();
        res.push_str("sub ");
        if !self.prefix.is_empty() || !self.suffix.is_empty() || self.force_chain {
            if !self.prefix.is_empty() {
                let prefix_str: Vec<String> =
                    self.prefix.iter().map(|g| g.as_fea("") + " ").collect();
                res.push_str(&prefix_str.join(" ").to_string());
            }
            res.push_str(&format!("{}'", self.glyph.as_fea("")));
            if !self.suffix.is_empty() {
                res.push(' ');
                let suffix_str: Vec<String> = self.suffix.iter().map(|g| g.as_fea("")).collect();
                res.push_str(&suffix_str.join(" ").to_string());
            }
        } else {
            res.push_str(&self.glyph.as_fea(""));
        }
        let replacement_str: Vec<String> = self.replacement.iter().map(|g| g.as_fea("")).collect();
        res.push_str(&format!(" by {};", replacement_str.join(" ")));
        res
    }
}

impl From<fea_rs::typed::Gsub2> for MultipleSubstStatement {
    fn from(val: fea_rs::typed::Gsub2) -> Self {
        let target = val
            .node()
            .iter_children()
            .find_map(GlyphOrClass::cast)
            .unwrap();
        let replacement = val
            .node()
            .iter_children()
            .skip_while(|t| t.kind() != Kind::ByKw)
            .skip(1)
            .filter_map(GlyphOrClass::cast);
        MultipleSubstStatement {
            prefix: vec![],
            suffix: vec![],
            glyph: target.into(),
            replacement: replacement.map(|x| x.into()).collect(),
            location: val.node().range(),
            force_chain: false,
        }
    }
}

impl TryFrom<fea_rs::typed::Gsub6> for MultipleSubstStatement {
    type Error = ();

    fn try_from(val: fea_rs::typed::Gsub6) -> Result<Self, ()> {
        // Two conditions: We need to see an InlineSubRule, and it must have
        // more than one child.
        if !val.node().iter_children().any(|c| {
            if let Some(inline) = fea_rs::typed::InlineSubRule::cast(c) {
                inline
                    .node()
                    .iter_children()
                    .filter(|k| GlyphOrClass::cast(k).is_some())
                    .count()
                    > 1
            } else {
                false
            }
        }) {
            return Err(());
        }
        let prefix = backtrack(val.node());
        let context = context_glyphs(val.node());
        let suffix = lookahead(val.node());
        let targets = inline_sub_targets(val.node());

        Ok(MultipleSubstStatement {
            prefix,
            suffix,
            glyph: context.into_iter().next().unwrap(),
            replacement: targets,
            location: val.node().range(),
            force_chain: true,
        })
    }
}

fn inline_sub_targets(val: &fea_rs::Node) -> Vec<GlyphContainer> {
    let inline_sub = val
        .iter_children()
        .find_map(fea_rs::typed::InlineSubRule::cast)
        .unwrap();
    inline_sub
        .node()
        .iter_children()
        .filter_map(GlyphOrClass::cast)
        .map(|goc| goc.into())
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlternateSubstStatement {
    pub location: Range<usize>,
    pub prefix: Vec<GlyphContainer>,
    pub suffix: Vec<GlyphContainer>,
    pub glyph: GlyphContainer,
    pub replacement: GlyphContainer,
    pub force_chain: bool,
}

impl AlternateSubstStatement {
    /// Create a new Alternate substitution statement.
    pub fn new(
        glyph: GlyphContainer,
        replacement: GlyphContainer,
        prefix: Vec<GlyphContainer>,
        suffix: Vec<GlyphContainer>,
        location: Range<usize>,
        force_chain: bool,
    ) -> Self {
        Self {
            prefix,
            suffix,
            glyph,
            replacement,
            location,
            force_chain,
        }
    }
}
impl AsFea for AlternateSubstStatement {
    fn as_fea(&self, _indent: &str) -> String {
        let mut res = String::new();
        res.push_str("sub ");
        if !self.prefix.is_empty() || !self.suffix.is_empty() || self.force_chain {
            if !self.prefix.is_empty() {
                let prefix_str: Vec<String> =
                    self.prefix.iter().map(|g| g.as_fea("") + " ").collect();
                res.push_str(&prefix_str.join(" ").to_string());
            }
            res.push_str(&format!("{}'", self.glyph.as_fea("")));
            if !self.suffix.is_empty() {
                let suffix_str: Vec<String> = self.suffix.iter().map(|g| g.as_fea("")).collect();
                res.push_str(&suffix_str.join(" ").to_string());
            }
        } else {
            res.push_str(&self.glyph.as_fea(""));
        }
        let replacement_str: String = self.replacement.as_fea("");
        res.push_str(&format!(" from {};", replacement_str));
        res
    }
}

impl From<fea_rs::typed::Gsub3> for AlternateSubstStatement {
    fn from(val: fea_rs::typed::Gsub3) -> Self {
        let target = val
            .node()
            .iter_children()
            .find_map(GlyphOrClass::cast)
            .unwrap();
        let replacement = val
            .node()
            .iter_children()
            .skip_while(|t| t.kind() != Kind::FromKw)
            .find_map(fea_rs::typed::GlyphClass::cast)
            .unwrap();
        AlternateSubstStatement {
            prefix: vec![],
            suffix: vec![],
            glyph: target.into(),
            replacement: replacement.into(),
            location: val.node().range(),
            force_chain: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LigatureSubstStatement {
    pub location: Range<usize>,
    pub prefix: Vec<GlyphContainer>,
    pub suffix: Vec<GlyphContainer>,
    pub glyphs: Vec<GlyphContainer>,
    pub replacement: GlyphContainer,
    pub force_chain: bool,
}

impl LigatureSubstStatement {
    /// Create a new ligature substitution statement.
    pub fn new(
        glyphs: Vec<GlyphContainer>,
        replacement: GlyphContainer,
        prefix: Vec<GlyphContainer>,
        suffix: Vec<GlyphContainer>,
        location: Range<usize>,
        force_chain: bool,
    ) -> Self {
        Self {
            prefix,
            suffix,
            glyphs,
            replacement,
            location,
            force_chain,
        }
    }
}

impl AsFea for LigatureSubstStatement {
    fn as_fea(&self, _indent: &str) -> String {
        let mut res = String::new();
        res.push_str("sub ");
        if !self.prefix.is_empty() || !self.suffix.is_empty() || self.force_chain {
            if !self.prefix.is_empty() {
                let prefix_str: Vec<String> =
                    self.prefix.iter().map(|g| g.as_fea("") + " ").collect();
                res.push_str(&prefix_str.join(" ").to_string());
            }
            res.push_str(
                &self
                    .glyphs
                    .iter()
                    .map(|g| g.as_fea("") + "'")
                    .collect::<Vec<_>>()
                    .join(" "),
            );
            if !self.suffix.is_empty() {
                let suffix_str: Vec<String> = self.suffix.iter().map(|g| g.as_fea("")).collect();
                res.push_str(&suffix_str.join(" ").to_string());
            }
        } else {
            res.push_str(
                &self
                    .glyphs
                    .iter()
                    .map(|g| g.as_fea(""))
                    .collect::<Vec<_>>()
                    .join(" "),
            );
        }
        let replacement_str: String = self.replacement.as_fea("");
        res.push_str(&format!(" by {};", replacement_str));
        res
    }
}

impl From<fea_rs::typed::Gsub4> for LigatureSubstStatement {
    fn from(val: fea_rs::typed::Gsub4) -> Self {
        let target = val
            .node()
            .iter_children()
            .take_while(|t| t.kind() != Kind::ByKw)
            .filter_map(GlyphOrClass::cast)
            .collect::<Vec<_>>();
        let replacement = val
            .node()
            .iter_children()
            .skip_while(|t| t.kind() != Kind::ByKw)
            .find_map(GlyphOrClass::cast)
            .unwrap();
        LigatureSubstStatement {
            prefix: vec![],
            suffix: vec![],
            glyphs: target.into_iter().map(|g| g.into()).collect(),
            replacement: replacement.into(),
            location: val.node().range(),
            force_chain: false,
        }
    }
}

/// A reverse chaining substitution statement
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReverseChainSingleSubstStatement {
    pub location: Range<usize>,
    pub prefix: Vec<GlyphContainer>,
    pub suffix: Vec<GlyphContainer>,
    pub glyphs: Vec<GlyphContainer>,
    pub replacements: Vec<GlyphContainer>,
}

impl ReverseChainSingleSubstStatement {
    pub fn new(
        glyphs: Vec<GlyphContainer>,
        replacements: Vec<GlyphContainer>,
        prefix: Vec<GlyphContainer>,
        suffix: Vec<GlyphContainer>,
        location: Range<usize>,
    ) -> Self {
        Self {
            prefix,
            suffix,
            glyphs,
            replacements,
            location,
        }
    }
}

impl AsFea for ReverseChainSingleSubstStatement {
    fn as_fea(&self, _indent: &str) -> String {
        let mut res = String::new();
        res.push_str("rsub ");
        if !self.prefix.is_empty() || !self.suffix.is_empty() {
            if !self.prefix.is_empty() {
                let prefix_str: Vec<String> =
                    self.prefix.iter().map(|g| g.as_fea("") + " ").collect();
                res.push_str(&format!("{} ", prefix_str.join(" ")));
            }
            let glyphs_str: Vec<String> = self
                .glyphs
                .iter()
                .map(|g| format!("{}'", g.as_fea("")))
                .collect();
            res.push_str(&glyphs_str.join(" "));
            if !self.suffix.is_empty() {
                let suffix_str: Vec<String> = self.suffix.iter().map(|g| g.as_fea("")).collect();
                res.push_str(&format!(" {}", suffix_str.join(" ")));
            }
        } else {
            let glyphs_str: Vec<String> = self.glyphs.iter().map(|g| g.as_fea("")).collect();
            res.push_str(&glyphs_str.join(" "));
        }
        let replacement_str: Vec<String> = self.replacements.iter().map(|g| g.as_fea("")).collect();
        res.push_str(&format!(" by {};", replacement_str.join(" ")));
        res
    }
}

impl From<fea_rs::typed::Gsub8> for ReverseChainSingleSubstStatement {
    fn from(val: fea_rs::typed::Gsub8) -> Self {
        let prefix = backtrack(val.node());
        let context = context_glyphs(val.node());
        let suffix = lookahead(val.node());
        let targets = inline_sub_targets(val.node());
        // println!("Prefix = {:?}", prefix);
        // println!("Suffix = {:?}", suffix);
        // println!("Context = {:?}", context);
        // println!("Targets = {:?}", targets);

        ReverseChainSingleSubstStatement {
            prefix,
            suffix,
            glyphs: context, // Should only be one glyph here really but python uses an array
            replacements: targets,
            location: val.node().range(),
        }
    }
}

#[cfg(test)]
mod tests {
    use smol_str::SmolStr;

    use crate::{GlyphClass, GlyphName};

    use super::*;

    #[test]
    fn test_generate_gsub1() {
        let gsub1 = SingleSubstStatement::new(
            vec![GlyphContainer::GlyphName(GlyphName::new("x"))],
            vec![GlyphContainer::GlyphName(GlyphName::new("a"))],
            vec![GlyphContainer::GlyphClass(GlyphClass::new(
                vec![
                    GlyphContainer::GlyphName(GlyphName::new("a.smcp")),
                    GlyphContainer::GlyphName(GlyphName::new("b.smcp")),
                ],
                0..0,
            ))],
            vec![],
            0..0,
            false,
        );
        assert_eq!(gsub1.as_fea(""), "sub [a.smcp b.smcp] x' by a;");
    }

    #[test]
    fn test_roundtrip_gsub1() {
        let fea = "feature smcp { sub a by a.smcp; } smcp;";
        let (parsed, _) = fea_rs::parse::parse_string(fea);
        let gsub1 = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::Gsub1::cast)
            })
            .unwrap();
        let single_subst = SingleSubstStatement::from(gsub1);
        assert_eq!(single_subst.as_fea(""), "sub a by a.smcp;");
    }

    #[test]
    fn test_generate_gsub2() {
        let gsub2 = MultipleSubstStatement::new(
            GlyphContainer::GlyphName(GlyphName::new("x")),
            vec![
                GlyphContainer::GlyphName(GlyphName::new("a")),
                GlyphContainer::GlyphName(GlyphName::new("b")),
            ],
            vec![],
            vec![GlyphContainer::GlyphClass(GlyphClass::new(
                vec![
                    GlyphContainer::GlyphName(GlyphName::new("c.smcp")),
                    GlyphContainer::GlyphName(GlyphName::new("d.smcp")),
                ],
                0..0,
            ))],
            0..0,
            false,
        );
        assert_eq!(gsub2.as_fea(""), "sub x' [c.smcp d.smcp] by a b;");
    }

    #[test]
    fn test_roundtrip_gsub2() {
        let fea = "feature liga { sub x by a b; } liga;";
        let (parsed, _) = fea_rs::parse::parse_string(fea);
        let gsub2 = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::Gsub2::cast)
            })
            .unwrap();
        let multiple_subst = MultipleSubstStatement::from(gsub2);
        assert_eq!(multiple_subst.as_fea(""), "sub x by a b;");
    }

    #[test]
    fn test_generate_gsub3() {
        let gsub3 = AlternateSubstStatement::new(
            GlyphContainer::GlyphName(GlyphName::new("x")),
            GlyphContainer::GlyphClassName(SmolStr::new("@a_alternates")),
            vec![],
            vec![],
            0..0,
            false,
        );
        assert_eq!(gsub3.as_fea(""), "sub x from @a_alternates;");
    }

    #[test]
    fn test_roundtrip_gsub3() {
        let fea = "feature calt { sub x from @x_alternates; } calt;";
        let (parsed, _) = fea_rs::parse::parse_string(fea);
        let gsub3 = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::Gsub3::cast)
            })
            .unwrap();
        let alternate_subst = AlternateSubstStatement::from(gsub3);
        assert_eq!(alternate_subst.as_fea(""), "sub x from @x_alternates;");
    }

    #[test]
    fn test_generate_gsub4() {
        let gsub4 = LigatureSubstStatement::new(
            vec![
                GlyphContainer::GlyphName(GlyphName::new("f")),
                GlyphContainer::GlyphName(GlyphName::new("i")),
            ],
            GlyphContainer::GlyphName(GlyphName::new("fi")),
            vec![],
            vec![],
            0..0,
            false,
        );
        assert_eq!(gsub4.as_fea(""), "sub f i by fi;");
    }

    #[test]
    fn test_roundtrip_gsub4() {
        let fea = "feature lig { sub f i by fi; } lig;";
        let (parsed, _) = fea_rs::parse::parse_string(fea);
        let gsub4 = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::Gsub4::cast)
            })
            .unwrap();
        let ligature_subst = LigatureSubstStatement::from(gsub4);
        assert_eq!(ligature_subst.as_fea(""), "sub f i by fi;");
    }

    #[test]
    fn test_multiple_subst_from_gsub6() {
        let fea = r#"feature foo { sub [a b c] d' e f g h i j by k l m n o p; } foo;"#;
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
        let multiple_subst = MultipleSubstStatement::try_from(gsub6).unwrap();
        assert_eq!(multiple_subst.prefix.len(), 1);
        assert_eq!(multiple_subst.suffix.len(), 6);
        assert_eq!(multiple_subst.glyph.as_fea(""), "d");
        assert_eq!(
            multiple_subst
                .replacement
                .iter()
                .map(|g| g.as_fea(""))
                .collect::<Vec<_>>(),
            vec!["k", "l", "m", "n", "o", "p"]
        );
    }

    #[test]
    fn test_roundtrip_gsub8_contextual() {
        let fea = r#"feature foo { rsub x y a' z w by b; } foo;"#;
        let (parsed, _) = fea_rs::parse::parse_string(fea);
        let gsub8 = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::Feature::cast)
            .and_then(|feature| {
                feature
                    .node()
                    .iter_children()
                    .find_map(fea_rs::typed::Gsub8::cast)
            })
            .unwrap();
        let stmt = ReverseChainSingleSubstStatement::from(gsub8);
        assert_eq!(stmt.glyphs.len(), 1);
        assert_eq!(stmt.glyphs[0].as_fea(""), "a");
        assert_eq!(stmt.replacements.len(), 1);
        assert_eq!(stmt.replacements[0].as_fea(""), "b");
        assert_eq!(stmt.prefix.len(), 2);
        assert_eq!(stmt.suffix.len(), 2);
        assert_eq!(stmt.as_fea(""), "rsub x  y  a' z w by b;");
    }

    #[test]
    fn test_generation_gsub8() {
        let stmt = ReverseChainSingleSubstStatement::new(
            vec![GlyphContainer::GlyphName(GlyphName::new("x"))],
            vec![GlyphContainer::GlyphName(GlyphName::new("y"))],
            vec![],
            vec![],
            0..0,
        );
        assert_eq!(stmt.as_fea(""), "rsub x by y;");
    }
}
