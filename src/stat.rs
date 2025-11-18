use std::ops::Range;

use fea_rs::typed::AstNode as _;
use smol_str::SmolStr;

use crate::{parse_namespec, AsFea, NameRecord, NameRecordKind, SHIFT};

/// A STAT table Design Axis
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StatDesignAxisStatement {
    pub tag: SmolStr,
    pub axis_order: usize,
    pub names: Vec<NameRecord>,
    pub location: Range<usize>,
}

impl AsFea for StatDesignAxisStatement {
    fn as_fea(&self, indent: &str) -> String {
        let indent = indent.to_string() + SHIFT;
        let mut fea = format!("DesignAxis {} {} {{\n", self.tag, self.axis_order);
        for name in &self.names {
            fea.push_str(&format!("\n{}{}", indent, name.as_fea(&indent)));
        }
        fea.push_str("\n};\n");
        fea
    }
}

impl From<fea_rs::typed::StatDesignAxis> for StatDesignAxisStatement {
    fn from(design_axis: fea_rs::typed::StatDesignAxis) -> Self {
        let tag = design_axis
            .node()
            .iter_children()
            .find_map(fea_rs::typed::Tag::cast)
            .unwrap()
            .text()
            .as_str()
            .into();
        let axis_order = design_axis
            .node()
            .iter_children()
            .take_while(|t| t.kind() != fea_rs::Kind::LBrace)
            .find_map(fea_rs::typed::Number::cast)
            .unwrap()
            .text()
            .parse()
            .unwrap();
        let names = design_axis
            .node()
            .iter_children()
            .skip_while(|t| t.kind() != fea_rs::Kind::LBrace)
            .filter_map(fea_rs::typed::NameSpec::cast)
            .map(|name_spec| {
                let location = name_spec.node().range();
                let (platform_id, plat_enc_id, lang_id, string) = parse_namespec(name_spec);
                NameRecord::new(
                    platform_id,
                    plat_enc_id,
                    lang_id,
                    string,
                    NameRecordKind::StatName,
                    location,
                )
            })
            .collect();
        Self {
            tag,
            axis_order,
            names,
            location: design_axis.node().range(),
        }
    }
}

/// STAT table ElidedFallbackNameID
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElidedFallbackNameId {
    /// an int pointing to an existing name table name ID
    pub value: u16,
    pub location: Range<usize>,
}

impl AsFea for ElidedFallbackNameId {
    fn as_fea(&self, _indent: &str) -> String {
        format!("ElidedFallbackNameID {};\n", self.value)
    }
}

/// STAT table ElidedFallbackName
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElidedFallbackName {
    pub names: Vec<NameRecord>,
    pub location: Range<usize>,
}

impl AsFea for ElidedFallbackName {
    fn as_fea(&self, indent: &str) -> String {
        let mut res = "ElidedFallbackName {\n".to_string();
        let indent = indent.to_string() + SHIFT;
        for name in &self.names {
            res.push_str(&format!("\n{}{}", indent, name.as_fea(&indent)));
        }
        res.push_str("\n};\n");
        res
    }
}

impl From<fea_rs::typed::StatElidedFallbackName> for StatStatement {
    fn from(val: fea_rs::typed::StatElidedFallbackName) -> Self {
        // Is there an ID?
        if let Some(id) = val
            .iter()
            .take_while(|t| t.kind() != fea_rs::Kind::NameKw)
            .find_map(fea_rs::typed::Number::cast)
        {
            let value = id.text().parse().unwrap();
            return StatStatement::ElidedFallbackNameId(ElidedFallbackNameId {
                value,
                location: val.node().range(),
            });
        }

        let names = val
            .node()
            .iter_children()
            .filter_map(fea_rs::typed::NameSpec::cast)
            .map(|name_spec| {
                let location = name_spec.node().range();
                let (platform_id, plat_enc_id, lang_id, string) = parse_namespec(name_spec);
                NameRecord::new(
                    platform_id,
                    plat_enc_id,
                    lang_id,
                    string,
                    NameRecordKind::StatName,
                    location,
                )
            })
            .collect();
        StatStatement::ElidedFallbackName(ElidedFallbackName {
            names,
            location: val.node().range(),
        })
    }
}

/// A STAT table axis value record.
///
/// Example:
/// ```fea
/// AxisValue {
///     location wdth 80 80 89;
///     name "Condensed";
///     flag ElidableAxisValueName;
/// };
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct STATAxisValueStatement {
    /// Name records for this axis value
    pub names: Vec<NameRecord>,
    /// Location specifications
    pub locations: Vec<AxisValueLocationStatement>,
    /// Flags (bitfield: 0x01 = OlderSiblingFontAttribute, 0x02 = ElidableAxisValueName)
    pub flags: u16,
    /// Location in the source FEA file
    pub location: Range<usize>,
}
impl Eq for STATAxisValueStatement {}

impl From<fea_rs::typed::StatAxisValue> for STATAxisValueStatement {
    fn from(val: fea_rs::typed::StatAxisValue) -> Self {
        let mut names = Vec::new();
        let mut locations = Vec::new();
        let mut flags = 0u16;

        // Manually iterate through children to extract statements
        for item in val
            .iter()
            .skip(2)
            .filter_map(fea_rs::typed::StatAxisValueItem::cast)
        {
            match item {
                fea_rs::typed::StatAxisValueItem::NameRecord(name_spec) => {
                    let location = name_spec.node().range();
                    let (platform_id, plat_enc_id, lang_id, string) = parse_namespec(name_spec);
                    names.push(NameRecord::new(
                        platform_id,
                        plat_enc_id,
                        lang_id,
                        string,
                        NameRecordKind::StatName,
                        location,
                    ));
                }
                fea_rs::typed::StatAxisValueItem::Location(loc) => {
                    locations.push(AxisValueLocationStatement::from(loc));
                }
                fea_rs::typed::StatAxisValueItem::Flag(flag) => {
                    // Manually accumulate flag bits
                    flags = flag
                        .iter()
                        .skip(1)
                        .take_while(|t| t.kind() != fea_rs::Kind::Semi)
                        .filter_map(|t| match t.kind() {
                            fea_rs::Kind::OlderSiblingFontAttributeKw => Some(0x01),
                            fea_rs::Kind::ElidableAxisValueNameKw => Some(0x02),
                            _ => None,
                        })
                        .sum();
                }
            }
        }

        Self {
            names,
            locations,
            flags,
            location: val.node().range(),
        }
    }
}

impl AsFea for STATAxisValueStatement {
    fn as_fea(&self, _indent: &str) -> String {
        let mut res = String::from("AxisValue {\n");

        for location in &self.locations {
            res.push_str(&location.as_fea(""));
        }

        for name in &self.names {
            res.push_str(&name.as_fea(""));
            res.push('\n');
        }

        if self.flags != 0 {
            let mut flag_strings = Vec::new();
            if self.flags & 0x01 != 0 {
                flag_strings.push("OlderSiblingFontAttribute");
            }
            if self.flags & 0x02 != 0 {
                flag_strings.push("ElidableAxisValueName");
            }
            res.push_str(&format!("flag {};\n", flag_strings.join(" ")));
        }

        res.push_str("};");
        res
    }
}

/// A STAT table axis value location specification.
///
/// This can represent three different formats:
/// - Single value: `location wdth 100;`
/// - Linked value: `location wdth 100 150;`
/// - Min/max range: `location opsz 8 5 9;` (nominal, min, max)
#[derive(Debug, Clone, PartialEq)]
pub struct AxisValueLocationStatement {
    /// The axis tag (e.g., "wdth", "wght", "opsz")
    pub tag: SmolStr,
    /// Location values (1-3 floats depending on format)
    pub values: Vec<f32>,
    /// Location in the source FEA file
    pub location: Range<usize>,
}

impl From<fea_rs::typed::StatAxisLocation> for AxisValueLocationStatement {
    fn from(val: fea_rs::typed::StatAxisLocation) -> Self {
        // Extract tag
        let tag = val
            .iter()
            .find_map(fea_rs::typed::Tag::cast)
            .unwrap()
            .text()
            .as_str()
            .into();

        // Helper to parse FloatLike into f32
        let parse_float = |fl: fea_rs::typed::FloatLike| -> f32 {
            match fl {
                fea_rs::typed::FloatLike::Float(f) => f.text().parse().unwrap(),
                fea_rs::typed::FloatLike::Number(n) => n.text().parse::<i16>().unwrap() as f32,
            }
        };

        // Extract all FloatLike values
        let values: Vec<f32> = val
            .iter()
            .filter_map(fea_rs::typed::FloatLike::cast)
            .map(parse_float)
            .collect();

        Self {
            tag,
            values,
            location: val.node().range(),
        }
    }
}
impl AsFea for AxisValueLocationStatement {
    fn as_fea(&self, _indent: &str) -> String {
        format!(
            "location {} {};\n",
            self.tag,
            self.values
                .iter()
                .map(|v| {
                    // Format floats nicely - remove trailing zeros and decimal point if integer
                    let s = format!("{}", v);
                    if s.contains('.') {
                        s.trim_end_matches('0').trim_end_matches('.').to_string()
                    } else {
                        s
                    }
                })
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatStatement {
    Comment(crate::Comment),
    DesignAxis(StatDesignAxisStatement),
    ElidedFallbackName(ElidedFallbackName),
    ElidedFallbackNameId(ElidedFallbackNameId),
    AxisValue(STATAxisValueStatement),
}

impl AsFea for StatStatement {
    fn as_fea(&self, indent: &str) -> String {
        match self {
            StatStatement::Comment(cmt) => cmt.as_fea(indent),
            StatStatement::DesignAxis(stmt) => stmt.as_fea(indent),
            StatStatement::ElidedFallbackName(stmt) => stmt.as_fea(indent),
            StatStatement::ElidedFallbackNameId(stmt) => stmt.as_fea(indent),
            StatStatement::AxisValue(stmt) => stmt.as_fea(indent),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_roundtrip_stat_axis_value_simple() {
        const FEA: &str = r#"table STAT {
   AxisValue {
      location wdth 80 80 89;
      name "Condensed";
   };
} STAT;"#;

        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let stat_table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::StatTable::cast)
            .unwrap();

        let axis_value = stat_table
            .iter()
            .filter_map(fea_rs::typed::StatTableItem::cast)
            .find_map(|s| match s {
                fea_rs::typed::StatTableItem::AxisValue(av) => Some(av),
                _ => None,
            })
            .unwrap();

        let stmt = STATAxisValueStatement::from(axis_value);

        assert_eq!(stmt.locations.len(), 1);
        assert_eq!(stmt.locations[0].tag.as_str(), "wdth");
        assert_eq!(stmt.locations[0].values, vec![80.0, 80.0, 89.0]);
        assert_eq!(stmt.names.len(), 1);
        assert_eq!(stmt.names[0].string, "Condensed");
        assert_eq!(stmt.flags, 0);

        let output = stmt.as_fea("");
        assert!(output.contains("location wdth 80 80 89;"));
        assert!(output.contains("name \"Condensed\";"));
    }

    #[test]
    fn test_roundtrip_stat_axis_value_with_flags() {
        const FEA: &str = r#"table STAT {
   AxisValue {
      location opsz 11 9 12;
      name "Text";
      flag OlderSiblingFontAttribute ElidableAxisValueName;
   };
} STAT;"#;

        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let stat_table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::StatTable::cast)
            .unwrap();

        let axis_value = stat_table
            .iter()
            .filter_map(fea_rs::typed::StatTableItem::cast)
            .find_map(|s| match s {
                fea_rs::typed::StatTableItem::AxisValue(av) => Some(av),
                _ => None,
            })
            .unwrap();

        let stmt = STATAxisValueStatement::from(axis_value);

        assert_eq!(stmt.locations.len(), 1);
        assert_eq!(stmt.locations[0].tag.as_str(), "opsz");
        assert_eq!(stmt.locations[0].values, vec![11.0, 9.0, 12.0]);
        assert_eq!(stmt.names.len(), 1);
        assert_eq!(stmt.flags, 0x03); // Both flags set

        let output = stmt.as_fea("");
        assert!(output.contains("location opsz 11 9 12;"));
        assert!(output.contains("flag OlderSiblingFontAttribute ElidableAxisValueName;"));
    }

    #[test]
    fn test_roundtrip_stat_axis_value_single_location() {
        const FEA: &str = r#"table STAT {
   AxisValue {
      location wght 400;
      name "Regular";
   };
} STAT;"#;

        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let stat_table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::StatTable::cast)
            .unwrap();

        let axis_value = stat_table
            .iter()
            .filter_map(fea_rs::typed::StatTableItem::cast)
            .find_map(|s| match s {
                fea_rs::typed::StatTableItem::AxisValue(av) => Some(av),
                _ => None,
            })
            .unwrap();

        let stmt = STATAxisValueStatement::from(axis_value);

        assert_eq!(stmt.locations.len(), 1);
        assert_eq!(stmt.locations[0].tag.as_str(), "wght");
        assert_eq!(stmt.locations[0].values, vec![400.0]);

        let output = stmt.as_fea("");
        assert!(output.contains("location wght 400;"));
    }

    #[test]
    fn test_axis_value_location_float_formatting() {
        const FEA: &str = r#"table STAT {
   AxisValue {
      location opsz 16.7 12 24;
      name "Subhead";
   };
} STAT;"#;

        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let stat_table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::StatTable::cast)
            .unwrap();

        let axis_value = stat_table
            .iter()
            .filter_map(fea_rs::typed::StatTableItem::cast)
            .find_map(|s| match s {
                fea_rs::typed::StatTableItem::AxisValue(av) => Some(av),
                _ => None,
            })
            .unwrap();

        let stmt = STATAxisValueStatement::from(axis_value);

        assert_eq!(stmt.locations[0].values, vec![16.7, 12.0, 24.0]);

        let output = stmt.as_fea("");
        assert!(output.contains("location opsz 16.7 12 24;"));
    }

    #[test]
    fn test_generate_stat_axis_value() {
        let stmt = STATAxisValueStatement {
            names: vec![NameRecord::new(
                3,
                1,
                0x0409,
                "Bold".to_string(),
                NameRecordKind::StatName,
                0..0,
            )],
            locations: vec![AxisValueLocationStatement {
                tag: "wght".into(),
                values: vec![700.0],
                location: 0..0,
            }],
            flags: 0,
            location: 0..0,
        };

        let output = stmt.as_fea("");
        assert!(output.contains("AxisValue {"));
        assert!(output.contains("location wght 700;"));
        assert!(output.contains("name \"Bold\";"));
        assert!(output.contains("};"));
    }
}
