use std::ops::Range;

use fea_rs::{
    typed::{AstNode as _, Metric},
    Kind,
};
use smol_str::SmolStr;

use crate::AsFea;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueRecord {
    pub x_placement: Option<i16>,
    pub y_placement: Option<i16>,
    pub x_advance: Option<i16>,
    pub y_advance: Option<i16>,
    pub x_placement_device: Option<SmolStr>, // will change
    pub y_placement_device: Option<SmolStr>,
    pub x_advance_device: Option<SmolStr>,
    pub y_advance_device: Option<SmolStr>,
    pub vertical: bool,
    pub location: Range<usize>,
}

fn device_to_string(device: &Option<SmolStr>) -> String {
    match device {
        Some(d) => d.to_string(),
        None => "0".to_string(),
    }
}

impl ValueRecord {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        x_placement: Option<i16>,
        y_placement: Option<i16>,
        x_advance: Option<i16>,
        y_advance: Option<i16>,
        x_placement_device: Option<SmolStr>,
        y_placement_device: Option<SmolStr>,
        x_advance_device: Option<SmolStr>,
        y_advance_device: Option<SmolStr>,
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

    pub fn new_format_a(advance: i16, vertical: bool, location: Range<usize>) -> Self {
        if vertical {
            Self::new(
                None,
                None,
                None,
                Some(advance),
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
                Some(advance),
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

    pub fn new_format_b(
        x_placement: i16,
        y_placement: i16,
        x_advance: i16,
        y_advance: i16,
        vertical: bool,
        location: Range<usize>,
    ) -> Self {
        Self::new(
            Some(x_placement),
            Some(y_placement),
            Some(x_advance),
            Some(y_advance),
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
        let (x, y) = (self.x_placement, self.y_placement);
        let (x_advance, y_advance) = (self.x_advance, self.y_advance);
        let (x_pla_device, y_pla_device) = (&self.x_placement_device, &self.y_placement_device);
        let (x_adv_device, y_adv_device) = (&self.x_advance_device, &self.y_advance_device);
        let vertical = self.vertical;
        // Try format A, if possible.
        if x.is_none() && y.is_none() {
            if x_advance.is_none() && vertical {
                return format!("{}", y_advance.unwrap());
            } else if y_advance.is_none() && !vertical {
                return format!("{}", x_advance.unwrap());
            }
        }
        // Make any remaining None value 0 to avoid generating invalid records.
        let x = x.unwrap_or(0);
        let y = y.unwrap_or(0);
        let x_advance = x_advance.unwrap_or(0);
        let y_advance = y_advance.unwrap_or(0);
        // Try format B, if possible.
        if x_pla_device.is_none()
            && y_pla_device.is_none()
            && x_adv_device.is_none()
            && y_adv_device.is_none()
        {
            return format!("<{} {} {} {}>", x, y, x_advance, y_advance);
        }

        // Last resort is format C.
        format!(
            "<{} {} {} {} {} {} {} {}>",
            x,
            y,
            x_advance,
            y_advance,
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
        let numbers = val
            .iter()
            .filter(|t| t.kind() == Kind::Number)
            .collect::<Vec<_>>();
        if numbers.len() == 1 {
            // Format A, just an advance
            let advance = numbers[0].token_text().unwrap().parse::<i16>().unwrap();
            return Self::new_format_a(advance, false, val.node().range());
        } else if numbers.len() == 4 {
            // Format B or C - ignore format C for now
            let x_placement = numbers[0].token_text().unwrap().parse::<i16>().unwrap();
            let y_placement = numbers[1].token_text().unwrap().parse::<i16>().unwrap();
            let x_advance = numbers[2].token_text().unwrap().parse::<i16>().unwrap();
            let y_advance = numbers[3].token_text().unwrap().parse::<i16>().unwrap();
            return Self::new_format_b(
                x_placement,
                y_placement,
                x_advance,
                y_advance,
                false,
                val.node().range(),
            );
        }
        panic!("Invalid ValueRecord format");
    }
}

/// An `Anchor` element, used inside a `pos` rule.
///
/// If a `name` is given, this will be used in preference to the coordinates.
/// Other values should be integer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Anchor {
    pub x: i16,
    pub y: i16,
    pub name: Option<SmolStr>,
    pub contourpoint: Option<u16>,
    pub x_device_table: Option<SmolStr>,
    pub y_device_table: Option<SmolStr>,
    pub location: Range<usize>,
}

impl Anchor {
    pub fn new(
        x: i16,
        y: i16,
        name: Option<SmolStr>,
        contourpoint: Option<u16>,
        x_device_table: Option<SmolStr>,
        y_device_table: Option<SmolStr>,
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

    pub fn new_simple(x: i16, y: i16, location: Range<usize>) -> Self {
        Self {
            x,
            y,
            name: None,
            contourpoint: None,
            x_device_table: None,
            y_device_table: None,
            location,
        }
    }

    pub fn new_named(name: SmolStr, location: Range<usize>) -> Self {
        Self {
            x: 0,
            y: 0,
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
        let mut res = format!("<anchor {} {}", self.x, self.y);
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
    let metrics = val
        .iter()
        .filter_map(fea_rs::typed::Metric::cast)
        .collect::<Vec<_>>();
    let x = if let Metric::Scalar(x) = &metrics[0] {
        x.token().text.parse::<i16>().unwrap()
    } else {
        panic!("Invalid Anchor format");
    };
    let y = if let Metric::Scalar(y) = &metrics[1] {
        y.token().text.parse::<i16>().unwrap()
    } else {
        panic!("Invalid Anchor format");
    };
    let contourpoint = val
        .iter()
        .skip_while(|x| x.kind() != Kind::ContourpointKw)
        .find_map(fea_rs::typed::Number::cast)
        .map(|n| n.token().as_str().parse::<u16>().unwrap());
    Some(Anchor {
        x,
        y,
        name: None,
        contourpoint,
        x_device_table: None,
        y_device_table: None,
        location: val.node().range(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valuerecord_asfea() {
        let val = ValueRecord::new(
            Some(1),
            Some(2),
            Some(3),
            Some(4),
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
