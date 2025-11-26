use std::ops::Range;

use fea_rs::{
    typed::{AstNode as _, LocationSpec, LocationValue, Number},
    Kind,
};
use indexmap::IndexMap;
use smol_str::SmolStr;

use crate::AsFea;

/// A metric, which is potentially variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Metric {
    /// A simple scalar metric
    Scalar(i16),
    /// A variable metric with different values across the design space
    Variable(Vec<(IndexMap<SmolStr, i16>, i16)>),
}
impl From<i16> for Metric {
    fn from(val: i16) -> Self {
        Metric::Scalar(val)
    }
}
impl std::hash::Hash for Metric {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Metric::Scalar(val) => {
                state.write_i16(*val);
            }
            Metric::Variable(variations) => {
                for (location, value) in variations {
                    for (tag, coord) in location {
                        state.write(tag.as_bytes());
                        state.write_i16(*coord);
                    }
                    state.write_i16(*value);
                }
            }
        }
    }
}
impl From<fea_rs::typed::Metric> for Metric {
    fn from(val: fea_rs::typed::Metric) -> Self {
        match val {
            fea_rs::typed::Metric::Scalar(number) => {
                Metric::Scalar(number.token().text.parse::<i16>().unwrap())
            }
            fea_rs::typed::Metric::Variable(variable_metric) => {
                let location_values = variable_metric.iter().filter_map(LocationValue::cast);
                let mut variations = Vec::new();
                for location_value in location_values {
                    let location = location_value.iter().find_map(LocationSpec::cast).unwrap();
                    let value = location_value.iter().find_map(Number::cast).unwrap();
                    variations.push((
                        from_locationspec(&location),
                        value.token().text.parse::<i16>().unwrap(),
                    ));
                }
                Metric::Variable(variations)
            }
            fea_rs::typed::Metric::GlyphsAppNumber(_glyphs_app_number) => todo!(),
        }
    }
}
fn from_locationspec(val: &fea_rs::typed::LocationSpec) -> IndexMap<SmolStr, i16> {
    let mut map = IndexMap::new();
    for item in val.iter().filter_map(fea_rs::typed::LocationSpecItem::cast) {
        let axis_tag = item.iter().find_map(fea_rs::typed::Tag::cast).unwrap();
        let axislocation = item
            .iter()
            .skip(2)
            .find_map(fea_rs::typed::AxisLocation::cast)
            .unwrap();
        let raw = axislocation.iter().next().unwrap();
        let value = Number::cast(raw)
            .map(|num| num.text().parse::<f64>().unwrap())
            .unwrap();
        map.insert(axis_tag.token().as_str().into(), value as i16);
    }
    map
}

impl AsFea for Metric {
    fn as_fea(&self, _indent: &str) -> String {
        match self {
            Metric::Scalar(val) => format!("{}", val),
            Metric::Variable(variations) => {
                let mut res = String::from("(");
                for (i, (location, value)) in variations.iter().enumerate() {
                    if i > 0 {
                        res.push_str(" ");
                    }
                    let loc_str = location
                        .iter()
                        .map(|(tag, coord)| format!("{}={}", tag, coord))
                        .collect::<Vec<_>>()
                        .join(",");
                    res.push_str(&format!("{}:{}", loc_str, value));
                }
                res.push(')');
                res
            }
        }
    }
}

type DeviceTable = Vec<(u8, i8)>;
/// A `ValueRecord` element, used inside a `pos` rule to change a glyph's position
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueRecord {
    /// The horizontal placement adjustment
    pub x_placement: Option<Metric>,
    /// The vertical placement adjustment
    pub y_placement: Option<Metric>,
    /// The horizontal advance adjustment
    pub x_advance: Option<Metric>,
    /// The vertical advance adjustment
    pub y_advance: Option<Metric>,
    /// The horizontal placement device table
    pub x_placement_device: Option<DeviceTable>,
    /// The vertical placement device table
    pub y_placement_device: Option<DeviceTable>,
    /// The horizontal advance device table
    pub x_advance_device: Option<DeviceTable>,
    /// The vertical advance device table
    pub y_advance_device: Option<DeviceTable>,
    /// Whether this is a vertical value record
    pub vertical: bool,
    /// The location of the value record in the source FEA.
    pub location: Range<usize>,
}

fn device_to_string(device: &Option<DeviceTable>) -> String {
    match device {
        Some(d) => format!(
            "<device {}>",
            d.iter()
                .map(|(ppem, delta)| format!("{} {}", ppem, delta))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        None => "<device NULL>".to_string(),
    }
}

impl ValueRecord {
    /// Creates a new ValueRecord.
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        x_placement: Option<Metric>,
        y_placement: Option<Metric>,
        x_advance: Option<Metric>,
        y_advance: Option<Metric>,
        x_placement_device: Option<DeviceTable>,
        y_placement_device: Option<DeviceTable>,
        x_advance_device: Option<DeviceTable>,
        y_advance_device: Option<DeviceTable>,
        vertical: bool,
        location: Range<usize>,
    ) -> Self {
        Self {
            x_placement,
            y_placement,
            x_advance,
            y_advance,
            x_placement_device,
            y_placement_device,
            x_advance_device,
            y_advance_device,
            vertical,
            location,
        }
    }
    /// Returns true if any of the fields are Some.
    pub fn is_some(&self) -> bool {
        self.x_placement.is_some()
            || self.y_placement.is_some()
            || self.x_advance.is_some()
            || self.y_advance.is_some()
            || self.x_placement_device.is_some()
            || self.y_placement_device.is_some()
            || self.x_advance_device.is_some()
            || self.y_advance_device.is_some()
    }

    /// Creates a new ValueRecord in format A.
    pub fn new_format_a(advance: i16, vertical: bool, location: Range<usize>) -> Self {
        if vertical {
            Self::new(
                None,
                None,
                None,
                Some(advance.into()),
                None,
                None,
                None,
                None,
                vertical,
                location,
            )
        } else {
            Self::new(
                None,
                None,
                Some(advance.into()),
                None,
                None,
                None,
                None,
                None,
                vertical,
                location,
            )
        }
    }

    /// Creates a new ValueRecord in format B.
    pub fn new_format_b(
        x_placement: i16,
        y_placement: i16,
        x_advance: i16,
        y_advance: i16,
        vertical: bool,
        location: Range<usize>,
    ) -> Self {
        Self::new(
            Some(x_placement.into()),
            Some(y_placement.into()),
            Some(x_advance.into()),
            Some(y_advance.into()),
            None,
            None,
            None,
            None,
            vertical,
            location,
        )
    }
}

impl AsFea for ValueRecord {
    fn as_fea(&self, _indent: &str) -> String {
        if !self.is_some() {
            return "<NULL>".to_string();
        }
        let (x, y) = (&self.x_placement, &self.y_placement);
        let (x_advance, y_advance) = (&self.x_advance, &self.y_advance);
        let (x_pla_device, y_pla_device) = (&self.x_placement_device, &self.y_placement_device);
        let (x_adv_device, y_adv_device) = (&self.x_advance_device, &self.y_advance_device);
        let vertical = self.vertical;
        // Try format A, if possible.
        if x.is_none() && y.is_none() {
            if x_advance.is_none() && vertical {
                return y_advance.as_ref().unwrap().as_fea("");
            } else if y_advance.is_none() && !vertical {
                return x_advance.as_ref().unwrap().as_fea("");
            }
        }
        // Make any remaining None value 0 to avoid generating invalid records.
        let zero: Metric = 0.into();
        let x = x.as_ref().unwrap_or(&zero);
        let y = y.as_ref().unwrap_or(&zero);
        let x_advance = x_advance.as_ref().unwrap_or(&zero);
        let y_advance = y_advance.as_ref().unwrap_or(&zero);
        // Try format B, if possible.
        if x_pla_device.is_none()
            && y_pla_device.is_none()
            && x_adv_device.is_none()
            && y_adv_device.is_none()
        {
            return format!(
                "<{} {} {} {}>",
                x.as_fea(""),
                y.as_fea(""),
                x_advance.as_fea(""),
                y_advance.as_fea("")
            );
        }

        // Last resort is format C.
        format!(
            "<{} {} {} {} {} {} {} {}>",
            x.as_fea(""),
            y.as_fea(""),
            x_advance.as_fea(""),
            y_advance.as_fea(""),
            device_to_string(x_pla_device),
            device_to_string(y_pla_device),
            device_to_string(x_adv_device),
            device_to_string(y_adv_device),
        )
    }
}

impl From<fea_rs::typed::ValueRecord> for ValueRecord {
    fn from(val: fea_rs::typed::ValueRecord) -> Self {
        // Is it null?
        if val.iter().take(3).any(|t| t.kind() == Kind::NullKw) {
            return Self::new(
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                false,
                val.node().range(),
            );
        }
        // Count the integer values
        let mut numbers = val
            .iter()
            .filter_map(fea_rs::typed::Metric::cast)
            .collect::<Vec<_>>();
        if numbers.len() == 1 {
            // Format A, just an advance
            return Self {
                x_placement: None,
                y_placement: None,
                x_advance: Some(numbers.remove(0).into()),
                y_advance: None,
                x_placement_device: None,
                y_placement_device: None,
                x_advance_device: None,
                y_advance_device: None,
                vertical: false,
                location: val.node().range(),
            };
        } else if numbers.len() == 4 {
            // Format B or C - ignore format C for now
            let x_placement: Metric = numbers.remove(0).into();
            let y_placement: Metric = numbers.remove(0).into();
            let x_advance: Metric = numbers.remove(0).into();
            let y_advance: Metric = numbers.remove(0).into();
            let devices = val
                .iter()
                .filter_map(fea_rs::typed::Device::cast)
                .flat_map(from_device)
                .collect::<Vec<_>>();
            if devices.is_empty() {
                return Self {
                    x_placement: Some(x_placement),
                    y_placement: Some(y_placement),
                    x_advance: Some(x_advance),
                    y_advance: Some(y_advance),
                    vertical: false,
                    location: val.node().range(),
                    x_placement_device: None,
                    y_placement_device: None,
                    x_advance_device: None,
                    y_advance_device: None,
                };
            }
            let x_pla_device = devices.first().cloned();
            let y_pla_device = devices.get(1).cloned();
            let x_adv_device = devices.get(2).cloned();
            let y_adv_device = devices.get(3).cloned();
            return Self::new(
                Some(x_placement),
                Some(y_placement),
                Some(x_advance),
                Some(y_advance),
                x_pla_device,
                y_pla_device,
                x_adv_device,
                y_adv_device,
                false,
                val.node().range(),
            );
        }
        panic!("Invalid ValueRecord format");
    }
}

fn from_device(device: fea_rs::typed::Device) -> Option<DeviceTable> {
    let mut table = Vec::new();
    let numbers = device
        .iter()
        .filter_map(fea_rs::typed::Number::cast)
        .collect::<Vec<_>>();
    for chunk in numbers.chunks(2) {
        if chunk.len() == 2 {
            let ppem = chunk[0].token().as_str().parse::<u8>().unwrap();
            let delta = chunk[1].token().as_str().parse::<i8>().unwrap();
            table.push((ppem, delta));
        }
    }

    if table.is_empty() {
        None
    } else {
        Some(table)
    }
}

/// An `Anchor` element, used inside a `pos` rule.
///
/// If a `name` is given, this will be used in preference to the coordinates.
/// Other values should be integer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Anchor {
    /// The horizontal coordinate of the anchor
    pub x: Metric,
    /// The vertical coordinate of the anchor
    pub y: Metric,
    /// The optional name of the anchor
    pub name: Option<SmolStr>,
    /// The optional contour point index
    pub contourpoint: Option<u16>,
    /// The optional horizontal device table
    pub x_device_table: Option<DeviceTable>,
    /// The optional vertical device table
    pub y_device_table: Option<DeviceTable>,
    /// The location of the anchor in the source FEA.
    pub location: Range<usize>,
}

impl Anchor {
    /// Creates a new Anchor.
    pub fn new(
        x: Metric,
        y: Metric,
        name: Option<SmolStr>,
        contourpoint: Option<u16>,
        x_device_table: Option<DeviceTable>,
        y_device_table: Option<DeviceTable>,
        location: Range<usize>,
    ) -> Self {
        Self {
            x,
            y,
            name,
            contourpoint,
            x_device_table,
            y_device_table,
            location,
        }
    }

    /// Creates a new simple Anchor with x and y coordinates.
    pub fn new_simple(x: i16, y: i16, location: Range<usize>) -> Self {
        Self {
            x: x.into(),
            y: y.into(),
            name: None,
            contourpoint: None,
            x_device_table: None,
            y_device_table: None,
            location,
        }
    }

    /// Creates a new named Anchor.
    pub fn new_named(name: SmolStr, location: Range<usize>) -> Self {
        Self {
            x: 0.into(),
            y: 0.into(),
            name: Some(name),
            contourpoint: None,
            x_device_table: None,
            y_device_table: None,
            location,
        }
    }
}

impl AsFea for Anchor {
    fn as_fea(&self, _indent: &str) -> String {
        if let Some(name) = &self.name {
            return format!("<anchor {}>", name);
        }
        let mut res = format!("<anchor {} {}", self.x.as_fea(""), self.y.as_fea(""));
        if let Some(contourpoint) = &self.contourpoint {
            res.push_str(&format!(" contourpoint {}", contourpoint));
        }
        if self.x_device_table.is_some() || self.y_device_table.is_some() {
            res.push(' ');
            res.push_str(&device_to_string(&self.x_device_table));
            res.push(' ');
            res.push_str(&device_to_string(&self.y_device_table));
        }
        res.push('>');
        res
    }
}

pub(crate) fn from_anchor(val: fea_rs::typed::Anchor) -> Option<Anchor> {
    // Is it null?
    if val.iter().any(|t| t.kind() == Kind::NullKw) {
        return None;
    }
    // Is it named?
    if let Some(name) = val.iter().find(|t| t.kind() == Kind::Ident) {
        return Some(Anchor::new_named(
            name.token_text().unwrap().into(),
            val.node().range(),
        ));
    }
    // Otherwise, extract coordinates
    let mut metrics: Vec<Metric> = val
        .iter()
        .filter_map(fea_rs::typed::Metric::cast)
        .map(|m| m.into())
        .collect::<Vec<_>>();
    let x = metrics.remove(0);
    let y = metrics.remove(0);
    let contourpoint = val
        .iter()
        .skip_while(|x| x.kind() != Kind::ContourpointKw)
        .find_map(fea_rs::typed::Number::cast)
        .map(|n| n.token().as_str().parse::<u16>().unwrap());
    // Extract device tables if any
    let mut devices = val
        .iter()
        .filter_map(fea_rs::typed::Device::cast)
        .flat_map(from_device);
    let x_device_table = devices.next();
    let y_device_table = devices.next();
    Some(Anchor {
        x,
        y,
        name: None,
        contourpoint,
        x_device_table,
        y_device_table,
        location: val.node().range(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valuerecord_asfea() {
        let val = ValueRecord::new(
            Some(1.into()),
            Some(2.into()),
            Some(3.into()),
            Some(4.into()),
            None,
            None,
            None,
            None,
            false,
            0..0,
        );
        assert_eq!(val.as_fea(""), "<1 2 3 4>");

        let format_a = ValueRecord::new_format_a(500, false, 0..0);
        assert_eq!(format_a.as_fea(""), "500");
    }
}
