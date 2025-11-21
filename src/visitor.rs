use crate::{CannotConvertError, FeatureFile, Statement};

// Use constants as documentation
pub(crate) const STOP: bool = false;
pub(crate) const CONTINUE: bool = true;

#[allow(unused_variables)]
pub trait LayoutVisitor {
    fn depth_first(&self) -> bool {
        true
    }
    fn visit_statement(&mut self, statement: &mut Statement) -> bool;
    fn visit(&mut self, root: &mut FeatureFile) -> Result<(), CannotConvertError> {
        for toplevel_item in root.statements.iter_mut() {
            let mut statement: Statement = toplevel_item.clone().into();
            self._visit_impl(&mut statement)?;
            *toplevel_item = statement.try_into()?;
        }
        Ok(())
    }
    #[allow(clippy::nonminimal_bool)]
    fn _visit_impl(&mut self, statement: &mut Statement) -> Result<bool, CannotConvertError> {
        // Pre-order visit: visit the node and return if we're told to stop
        if !self.depth_first() && self.visit_statement(statement) == STOP {
            return Ok(STOP);
        }
        // Now visit any items with children
        match statement {
            Statement::Gdef(table) => {
                for item in table.statements.iter_mut() {
                    let mut statement = item.clone().into();
                    if self._visit_impl(&mut statement)? == STOP {
                        return Ok(STOP);
                    }
                    *item = statement.try_into()?;
                }
            }
            // Statement::Head(table) => todo!(),
            // Statement::Hhea(table) => todo!(),
            // Statement::Name(table) => todo!(),
            // Statement::Stat(table) => todo!(),
            // Statement::Vhea(table) => todo!(),
            Statement::FeatureBlock(feature_block) => {
                for item in feature_block.statements.iter_mut() {
                    if self._visit_impl(item)? == STOP {
                        return Ok(STOP);
                    }
                }
            }
            Statement::LookupBlock(lookup_block) => {
                for item in lookup_block.statements.iter_mut() {
                    if self._visit_impl(item)? == STOP {
                        return Ok(STOP);
                    }
                }
            }
            Statement::NestedBlock(nested_block) => {
                for item in nested_block.statements.iter_mut() {
                    if self._visit_impl(item)? == STOP {
                        return Ok(STOP);
                    }
                }
            }
            _ => { /* no children to visit */ }
        };

        // Post-order visit: now we've visited the children, visit the node
        if self.depth_first() && !self.visit_statement(statement) {
            return Ok(STOP);
        }
        Ok(CONTINUE)
    }
}

#[cfg(test)]
mod tests {
    use crate::AsFea;

    use super::*;

    struct TestVisitor;
    impl LayoutVisitor for TestVisitor {
        fn visit_statement(&mut self, statement: &mut Statement) -> bool {
            match statement {
                Statement::FeatureBlock(feature_block) => {
                    feature_block.name = "test".into();
                }
                Statement::PairPos(pair_pos) => {
                    pair_pos.glyphs_1 = crate::GlyphContainer::new_class(&["x", "y"]);
                }
                _ => {}
            }
            true
        }
    }
    use pretty_assertions::assert_eq;

    #[test]
    fn test_visitor() {
        let fea_source = r#"
feature liga {
    lookup MyLookup {
        pos A B 100;
    } MyLookup;
} liga;"#;
        let mut feature: FeatureFile = FeatureFile::try_from(fea_source).unwrap();
        let mut visitor = TestVisitor;
        visitor.visit(&mut feature).unwrap();
        let output = feature.as_fea("");
        let expected = r#"feature test {
    lookup MyLookup {
        pos [x y] B 100;
    } MyLookup;
} test;
"#;
        assert_eq!(output, expected);
    }
}
