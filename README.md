# fea-rs-ast

A Rust port of Python's [`fontTools.feaLib.ast`](https://fonttools.readthedocs.io/en/latest/feaLib/ast.html)
library, providing a fontTools-compatible AST (Abstract Syntax Tree) for OpenType Feature Files.

This crate builds on top of the [`fea-rs`](https://github.com/googlefonts/fontc/tree/main/fea-rs)
parser, providing a higher-level, more ergonomic interface that matches the familiar fontTools API
while leveraging Rust's type safety and performance.

## Overview

OpenType Feature Files (.fea) define advanced typographic features for fonts using a
domain-specific language. This crate provides:

- **Parsing**: Load and parse feature files into a structured AST using `fea-rs`.
- **Construction**: Programmatically build feature file structures
- **Serialization**: Convert AST back to valid feature file syntax via the [`AsFea`] trait
- **Transformation**: Modify AST using the visitor pattern

## Architecture

The crate provides two main statement enums:

- [`Statement`]: All possible statements in a feature file, regardless of context
- [`ToplevelItem`]: Only statements valid at the top level of a feature file

Both implement the [`AsFea`] trait for serialization back to .fea syntax.

## Examples

### Loading an Existing Feature File

Parse a feature file from a string:

```rust
use fea_rs_ast::{FeatureFile, AsFea};

let fea_code = r#"
    languagesystem DFLT dflt;
    
    feature smcp {
        sub a by a.smcp;
        sub b by b.smcp;
    } smcp;
"#;

// Simple parsing without glyph name resolution
let feature_file = FeatureFile::try_from(fea_code).unwrap();

// Or with full resolution support
let feature_file = FeatureFile::new_from_fea(
    fea_code,
    Some(&["a", "a.smcp", "b", "b.smcp"]), // Glyph names
    None::<&str>, // Project root for includes
).unwrap();

// Serialize back to .fea syntax
let output = feature_file.as_fea("");
println!("{}", output);
```

### Constructing New Statements

Build feature file structures programmatically:

```rust
use fea_rs_ast::*;

// Create a glyph class definition
let lowercase = GlyphClassDefinition::new(
    "lowercase".to_string(),
    GlyphClass::new(vec![
        GlyphContainer::GlyphName(GlyphName::new("a")),
        GlyphContainer::GlyphName(GlyphName::new("b")),
        GlyphContainer::GlyphName(GlyphName::new("c")),
    ], 0..0),
    0..0, // location range
);

// Create a single substitution statement
let subst = SingleSubstStatement::new(
    vec![GlyphContainer::GlyphName(GlyphName::new("a"))],
    vec![GlyphContainer::GlyphName(GlyphName::new("a.smcp"))],
    vec![], // prefix
    vec![], // suffix
    0..0,   // location
    false,  // force_chain
);

// Create a feature block
let feature = FeatureBlock::new(
    "smcp".into(),
    vec![Statement::SingleSubst(subst)],
    false, // use_extension
    0..0,  // location
);

// Build the complete feature file
let feature_file = FeatureFile::new(vec![
    ToplevelItem::GlyphClassDefinition(lowercase),
    ToplevelItem::Feature(feature),
]);

// Serialize to .fea syntax
let output = feature_file.as_fea("");
assert!(output.contains("@lowercase = [a b c];"));
assert!(output.contains("feature smcp"));
assert!(output.contains("sub a by a.smcp;"));
```

### Using the Visitor Pattern

Transform AST structures by implementing the [`LayoutVisitor`] trait:

```rust
use fea_rs_ast::*;

// Create a visitor that renames all features
struct FeatureRenamer {
    old_name: String,
    new_name: String,
}

impl LayoutVisitor for FeatureRenamer {
    fn visit_statement(&mut self, statement: &mut Statement) -> bool {
        match statement {
            Statement::FeatureBlock(feature) => {
                if feature.name == self.old_name.as_str() {
                    feature.name = self.new_name.as_str().into();
                }
            }
            _ => {}
        }
        true // Continue visiting
    }
}

// Use the visitor
let fea_code = r#"
    feature liga {
        sub f i by fi;
    } liga;
"#;

let mut feature_file = FeatureFile::try_from(fea_code).unwrap();
let mut visitor = FeatureRenamer {
    old_name: "liga".to_string(),
    new_name: "dlig".to_string(),
};

visitor.visit(&mut feature_file).unwrap();

let output = feature_file.as_fea("");
assert!(output.contains("feature dlig"));
```

### More Complex Visitor: Glyph Name Substitution

```rust
use fea_rs_ast::*;
use std::collections::HashMap;

// Visitor that replaces glyph names throughout the AST
struct GlyphNameReplacer {
    replacements: HashMap<String, String>,
}

impl LayoutVisitor for GlyphNameReplacer {
    fn visit_statement(&mut self, statement: &mut Statement) -> bool {
        // Replace glyph names in various statement types
        match statement {
            Statement::SingleSubst(subst) => {
                for container in &mut subst.glyphs {
                    self.replace_in_container(container);
                }
                for container in &mut subst.replacement {
                    self.replace_in_container(container);
                }
            }
            Statement::GlyphClassDefinition(gcd) => {
                for container in &mut gcd.glyphs.glyphs {
                    self.replace_in_container(container);
                }
            }
            _ => {}
        }
        true
    }
}

impl GlyphNameReplacer {
    fn replace_in_container(&self, container: &mut GlyphContainer) {
        match container {
            GlyphContainer::GlyphName(gn) => {
                if let Some(new_name) = self.replacements.get(gn.name.as_str()) {
                    gn.name = new_name.as_str().into();
                }
            }
            GlyphContainer::GlyphClass(gc) => {
                for glyph_container in &mut gc.glyphs {
                    self.replace_in_container(glyph_container);
                }
            }
            _ => {}
        }
    }
}
```

## Feature Coverage

This crate supports most OpenType feature file constructs:

- **GSUB**: Single, Multiple, Alternate, Ligature, Contextual, and Reverse Chaining substitutions
- **GPOS**: Single, Pair, Cursive, Mark-to-Base, Mark-to-Ligature, and Mark-to-Mark positioning
- **Tables**: GDEF, BASE, head, hhea, name, OS/2, STAT, vhea
- **Contextual Rules**: Chaining context and ignore statements
- **Variable Fonts**: Conditionsets and variation blocks
- **Lookups**: Lookup blocks with flags and references
- **Features**: Feature blocks with useExtension

Features which fea-rs parses which this crate does not currently support:

- Glyphs number variables in value records
- CID-keyed glyph names

## Compatibility

The API closely mirrors fontTools' Python API where practical, making it easier to port
existing Python code to Rust. Key differences:

- Rust's type system provides compile-time guarantees about statement validity
- The [`Statement`] enum distinguishes between all possible statements
- The [`ToplevelItem`] enum ensures only valid top-level constructs
- Location tracking uses byte ranges (`Range<usize>`) instead of line/column numbers

## Re-exports

This crate re-exports the underlying [`fea_rs`] parser for advanced use cases where
direct access to the parse tree is needed.

## License

This project is licensed under the MIT License or Apache-2.0 License, at your option.