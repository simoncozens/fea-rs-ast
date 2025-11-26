use std::ops::Range;

use fea_rs::{
    typed::{AstNode as _, Number, Os2TableItem},
    NodeOrToken,
};
use smol_str::SmolStr;

use crate::{AsFea, Comment, FeaTable, Table};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Os2Field {
    Comment(Comment),
    FSType(u8),
    Panose(Vec<u8>),
    UnicodeRange(Vec<u16>),
    CodePageRange(Vec<u16>),
    TypoAscender(i16),
    TypoDescender(i16),
    TypoLineGap(i16),
    WinAscent(u16),
    WinDescent(u16),
    XHeight(i16),
    CapHeight(i16),
    WeightClass(u16),
    WidthClass(u16),

    Vendor(SmolStr),
}
impl AsFea for Os2Field {
    fn as_fea(&self, indent: &str) -> String {
        match self {
            Os2Field::Comment(cmt) => cmt.as_fea(indent),
            Os2Field::FSType(x) => format!("{}FSType {};", indent, x),
            Os2Field::WeightClass(x) => format!("{}WeightClass {};", indent, x),
            Os2Field::WidthClass(x) => format!("{}WidthClass {};", indent, x),
            Os2Field::Panose(vals) => format!(
                "{}Panose {};",
                indent,
                vals.iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Os2Field::Vendor(x) => format!("{}Vendor \"{}\";", indent, x),
            Os2Field::TypoAscender(x) => format!("{}TypoAscender {};", indent, x),
            Os2Field::TypoDescender(x) => format!("{}TypoDescender {};", indent, x),
            Os2Field::TypoLineGap(x) => format!("{}TypoLineGap {};", indent, x),
            Os2Field::UnicodeRange(items) => format!(
                "{}UnicodeRange {};",
                indent,
                items
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Os2Field::CodePageRange(items) => format!(
                "{}CodePageRange {};",
                indent,
                items
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Os2Field::WinAscent(x) => format!("{}winAscent {};", indent, x),
            Os2Field::WinDescent(x) => format!("{}winDescent {};", indent, x),
            Os2Field::XHeight(x) => format!("{}XHeight {};", indent, x),
            Os2Field::CapHeight(x) => format!("{}CapHeight {};", indent, x),
        }
    }
}

impl From<Os2TableItem> for Os2Statement {
    fn from(val: Os2TableItem) -> Self {
        match val {
            Os2TableItem::Number(number_record) => {
                let keyword = number_record
                    .iter()
                    .next()
                    .and_then(|t| t.as_token())
                    .unwrap();
                let number = number_record
                    .iter()
                    .find_map(Number::cast)
                    .unwrap()
                    .text()
                    .parse::<i64>()
                    .unwrap();
                let field = match keyword.as_str() {
                    "FSType" => Os2Field::FSType(number as u8),
                    "WeightClass" => Os2Field::WeightClass(number as u16),
                    "WidthClass" => Os2Field::WidthClass(number as u16),
                    _ => panic!("Unknown OS/2 number field: {}", keyword.as_str()),
                };
                Os2Statement {
                    field,
                    location: number_record.range(),
                }
            }
            Os2TableItem::NumberList(os2_number_list) => {
                let keyword = os2_number_list
                    .iter()
                    .next()
                    .and_then(|t| t.as_token())
                    .unwrap();
                let numbers: Vec<u16> = os2_number_list
                    .iter()
                    .filter_map(Number::cast)
                    .map(|n| n.text().parse::<u16>().unwrap())
                    .collect();
                let field = match keyword.as_str() {
                    "Panose" => Os2Field::Panose(numbers.iter().map(|&n| n as u8).collect()),
                    "UnicodeRange" => Os2Field::UnicodeRange(numbers),
                    "CodePageRange" => Os2Field::CodePageRange(numbers),
                    _ => panic!("Unknown OS/2 number list field: {}", keyword.as_str()),
                };
                Os2Statement {
                    field,
                    location: os2_number_list.range(),
                }
            }
            Os2TableItem::Metric(metric_record) => {
                let keyword = metric_record
                    .iter()
                    .next()
                    .and_then(|t| t.as_token())
                    .unwrap();
                let metric = metric_record
                    .iter()
                    .find_map(fea_rs::typed::Metric::cast)
                    .unwrap();

                let value = match metric {
                    fea_rs::typed::Metric::Scalar(number) => number.text().parse::<i16>().unwrap(),
                    _ => unimplemented!(),
                };
                let field = match keyword.as_str() {
                    "TypoAscender" => Os2Field::TypoAscender(value),
                    "TypoDescender" => Os2Field::TypoDescender(value),
                    "TypoLineGap" => Os2Field::TypoLineGap(value),
                    "XHeight" => Os2Field::XHeight(value),
                    "CapHeight" => Os2Field::CapHeight(value),
                    "winAscent" => Os2Field::WinAscent(value as u16),
                    "winDescent" => Os2Field::WinDescent(value as u16),
                    _ => panic!("Unknown OS/2 metric field: {}", keyword.as_str()),
                };
                Os2Statement {
                    field,
                    location: metric_record.range(),
                }
            }
            Os2TableItem::Vendor(vendor_record) => {
                let vendor = vendor_record
                    .iter()
                    .find(|t| t.kind() == fea_rs::Kind::String)
                    .and_then(|t| t.as_token())
                    .unwrap();
                Os2Statement {
                    field: Os2Field::Vendor(vendor.text.trim_matches('"').to_string().into()),
                    location: vendor_record.range(),
                }
            }
            Os2TableItem::FamilyClass(_os2_family_class) => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Os2Statement {
    field: Os2Field,
    location: Range<usize>,
}
impl AsFea for Os2Statement {
    fn as_fea(&self, indent: &str) -> String {
        self.field.as_fea(indent)
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Os2;
impl FeaTable for Os2 {
    type Statement = Os2Statement;
    const TAG: &'static str = "OS/2";
    type FeaRsTable = fea_rs::typed::Os2Table;
    #[allow(clippy::manual_map)]
    fn to_statement(child: &NodeOrToken) -> Option<Os2Statement> {
        if child.kind() == fea_rs::Kind::Comment {
            Some(Os2Statement {
                field: Os2Field::Comment(Comment::from(child.token_text().unwrap())),
                location: child.range(),
            })
        } else if let Some(fr) = fea_rs::typed::Os2TableItem::cast(child) {
            Some(fr.into())
        } else {
            None
        }
    }
}

impl From<fea_rs::typed::Os2Table> for Table<Os2> {
    fn from(val: fea_rs::typed::Os2Table) -> Self {
        Self {
            statements: Os2::statements_from_node(val.node()),
        }
    }
}
