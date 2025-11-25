// Unfortunately Python parses this completely differently to fea_rs
// We're going to do a halfway thing with more structure and typing
// on the axis, but still keeping each axis as the main "statement".

use crate::{AsFea, FeaTable, Table};
use fea_rs::{
    Kind, NodeOrToken,
    typed::{AstNode as _, ScriptRecord},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MinMax {
    pub script: String,
    pub language: String,
    pub min: i16,
    pub max: i16,
}

impl From<fea_rs::typed::BaseMinMax> for MinMax {
    fn from(value: fea_rs::typed::BaseMinMax) -> Self {
        let mut iter = value.iter();
        let script = iter
            .find(|t| t.kind() == Kind::Tag)
            .unwrap()
            .token_text()
            .unwrap_or_default()
            .to_string();
        let language = iter
            .find(|t| t.kind() == Kind::Tag)
            .unwrap()
            .token_text()
            .unwrap_or_default()
            .to_string();
        let min = iter
            .find(|t| t.kind() == Kind::Number)
            .and_then(|t| t.token_text())
            .and_then(|s| s.parse::<i16>().ok())
            .unwrap_or_default();
        let max = iter
            .find(|t| t.kind() == Kind::Number)
            .and_then(|t| t.token_text())
            .and_then(|s| s.parse::<i16>().ok())
            .unwrap_or_default();
        Self {
            script,
            language,
            min,
            max,
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaseScript {
    pub script_tag: String,
    pub default_baseline_tag: String,
    pub coordinates: Vec<i16>,
}

impl From<fea_rs::typed::ScriptRecord> for BaseScript {
    fn from(value: fea_rs::typed::ScriptRecord) -> Self {
        // Script and tag are the first two tokens
        let script_tag = value
            .iter()
            .find(|t| t.kind() == Kind::Tag)
            .unwrap()
            .token_text()
            .unwrap()
            .to_string();
        let default_baseline_tag = value
            .iter()
            .filter(|t| t.kind() == Kind::Tag)
            .nth(1)
            .unwrap()
            .token_text()
            .unwrap()
            .to_string();
        let coordinates = value
            .iter()
            .filter(|t| t.kind() == Kind::Number)
            .map(|t| {
                t.token_text()
                    .unwrap_or_default()
                    .parse::<i16>()
                    .unwrap_or_default()
            })
            .collect();
        Self {
            script_tag,
            default_baseline_tag,
            coordinates,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaseAxis {
    pub bases: Vec<String>,
    pub scripts: Vec<BaseScript>,
    pub vertical: bool,
    pub minmax: Vec<MinMax>,
}

impl BaseAxis {
    fn new() -> Self {
        Self {
            bases: Vec::new(),
            scripts: Vec::new(),
            vertical: false,
            minmax: Vec::new(),
        }
    }
}

impl AsFea for BaseAxis {
    fn as_fea(&self, indent: &str) -> String {
        let direction = if self.vertical { "Vert" } else { "Horiz" };
        let scripts: Vec<String> = self
            .scripts
            .iter()
            .map(|a| {
                format!(
                    "{:4} {} {}",
                    a.script_tag,
                    a.default_baseline_tag,
                    a.coordinates
                        .iter()
                        .map(|c| c.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            })
            .collect();
        let minmaxes: Vec<String> = self
            .minmax
            .iter()
            .map(|a| {
                format!(
                    "\n{}{}Axis.MinMax {:4} {:4} {}, {};",
                    indent, direction, a.script, a.language, a.min, a.max
                )
            })
            .collect();
        format!(
            "{}Axis.BaseTagList {};\n{}{}Axis.BaseScriptList {};{}\n",
            direction,
            self.bases.join(" "),
            indent,
            direction,
            scripts.join(", "),
            minmaxes.join("")
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Base;
impl FeaTable for Base {
    type Statement = BaseAxis;
    const TAG: &'static str = "BASE";
    type FeaRsTable = fea_rs::typed::BaseTable;
    fn statements_from_node(node: &fea_rs::Node) -> Vec<Self::Statement> {
        let mut axes = Vec::new();
        for child in node.iter_children() {
            if let Some(taglist) = fea_rs::typed::BaseTagList::cast(child) {
                let mut axis = BaseAxis::new();
                axis.vertical = match taglist.iter().next().map(|t| t.kind()) {
                    Some(Kind::HorizAxisBaseTagListKw) => false,
                    Some(Kind::VertAxisBaseTagListKw) => true,
                    other => panic!("unexpected token in BaseTagList {other:?}"),
                };
                axis.bases = taglist
                    .iter()
                    .skip(1)
                    .take_while(|t| t.kind() != Kind::Semi)
                    .filter(|t| t.kind() == Kind::Tag)
                    .map(|t| t.token_text().unwrap_or_default().to_string())
                    .collect();
                axes.push(axis);
            } else if let Some(scriptlist) = fea_rs::typed::BaseScriptList::cast(child) {
                let script_records = scriptlist
                    .iter()
                    .skip(1)
                    .take_while(|t| t.kind() != Kind::Semi)
                    .filter_map(ScriptRecord::cast)
                    .map(BaseScript::from)
                    .collect::<Vec<_>>();
                if let Some(last_axis) = axes.last_mut() {
                    last_axis.scripts = script_records;
                } else {
                    panic!("BaseScriptList found before BaseTagList");
                }
            } else if let Some(minmax_stmt) = fea_rs::typed::BaseMinMax::cast(child) {
                let minmax = MinMax::from(minmax_stmt);

                if let Some(last_axis) = axes.last_mut() {
                    last_axis.minmax.push(minmax);
                } else {
                    panic!("BaseAxisMinMax found before BaseTagList");
                }
            }
        }
        axes
    }
    fn to_statement(_child: &NodeOrToken) -> Option<Self::Statement> {
        None
    }
}

impl From<fea_rs::typed::BaseTable> for Table<Base> {
    fn from(val: fea_rs::typed::BaseTable) -> Self {
        Self {
            statements: Base::statements_from_node(val.node()),
        }
    }
}
