use std::ops::Range;

use fea_rs::typed::AstNode;

use crate::AsFea;
use read_fonts::tables::name::MacRomanMapping;

/// Kind of name record.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameRecordKind {
    /// A standard name record with a specific name ID
    Name(u16),
    /// The name of the feature to be displayed in a UI
    FeatureName,
    /// The feature size menu name
    FeatureSizeMenuName,
    /// A name to be used in the STAT table
    StatName,
}

/// A name record statement in a name table.
///
/// Example: `nameid 9 "Joachim M\00fcller-Lanc\00e9";`
///
/// The string field contains the raw string with escape sequences already processed.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameRecord {
    /// Platform ID
    pub platform_id: u16,
    /// Platform encoding ID
    pub plat_enc_id: u16,
    /// Language ID
    pub lang_id: u16,
    /// The name string (with escape sequences processed)
    pub string: String,
    /// Kind of the name record
    pub kind: NameRecordKind,
    /// Location in the source FEA file
    #[cfg_attr(feature = "serde", serde(default = "crate::default_range", skip_serializing_if = "crate::is_default_range"))]
    pub location: Range<usize>,
}

impl NameRecord {
    /// Creates a new NameRecord.
    pub fn new(
        platform_id: u16,
        plat_enc_id: u16,
        lang_id: u16,
        string: String,
        kind: NameRecordKind,
        location: Range<usize>,
    ) -> Self {
        Self {
            platform_id,
            plat_enc_id,
            lang_id,
            string,
            kind,
            location,
        }
    }

    fn escape_string(&self) -> String {
        // Windows escapes are UTF-16 code units, and Macintosh ones are Mac Roman bytes
        let is_mac = self.platform_id == 1;
        let mut escaped = String::with_capacity(self.string.len());
        for c in self.string.chars() {
            if (' '..='~').contains(&c) && c != '"' && c != '\\' {
                escaped.push(c);
            } else if !is_mac {
                for unit in c.encode_utf16(&mut [0; 2]) {
                    escaped.push_str(&format!(r"\{:04x}", unit));
                }
            } else if let Some(code) = MacRomanMapping.encode(c) {
                escaped.push_str(&format!(r"\{:02x}", code));
            } else {
                // Preserve characters that Mac Roman cannot encode.
                escaped.push(c);
            }
        }
        escaped
    }
}

impl AsFea for NameRecord {
    fn as_fea(&self, _indent: &str) -> String {
        // Escape the string for FEA output
        let escaped = self.escape_string();

        let plat = if self.platform_id == 3 && self.plat_enc_id == 1 && self.lang_id == 1033 {
            ""
        } else if self.platform_id == 1 && self.plat_enc_id == 0 && self.lang_id == 0 {
            "1 "
        } else {
            &format!(
                "{} {} {} ",
                self.platform_id, self.plat_enc_id, self.lang_id
            )
        };
        match self.kind {
            NameRecordKind::Name(id) => format!("nameid {} {}\"{}\";", id, plat, escaped),

            NameRecordKind::FeatureName => {
                format!("name {}\"{}\";", plat, escaped)
            }
            NameRecordKind::FeatureSizeMenuName => {
                format!("sizemenuname {}\"{}\";", plat, escaped)
            }
            NameRecordKind::StatName => format!("name {}\"{}\";", plat, escaped),
        }
    }
}

impl From<fea_rs::typed::NameRecord> for NameRecord {
    fn from(val: fea_rs::typed::NameRecord) -> Self {
        // Parse name_id (can be decimal, octal, or hex)
        let name_id_node = val.iter().find_map(fea_rs::typed::DecOctHex::cast).unwrap();
        let name_id = parse_dec_oct_hex(&name_id_node);

        // Get the NameSpec which contains platform info and string
        let name_spec = val.iter().find_map(fea_rs::typed::NameSpec::cast).unwrap();

        let (platform_id, plat_enc_id, lang_id, string) = parse_namespec(name_spec);

        Self::new(
            platform_id,
            plat_enc_id,
            lang_id,
            string,
            NameRecordKind::Name(name_id),
            val.range(),
        )
    }
}

pub(crate) fn parse_namespec(name_spec: fea_rs::typed::NameSpec) -> (u16, u16, u16, String) {
    // Parse platform/encoding/language IDs if present
    // They appear as three consecutive DecOctHex values after the name_id
    let platform_ids: Vec<u16> = name_spec
        .iter()
        .filter_map(fea_rs::typed::DecOctHex::cast)
        .map(|n| parse_dec_oct_hex(&n))
        .collect();
    let (platform_id, plat_enc_id, lang_id) = if platform_ids.len() == 3 {
        (platform_ids[0], platform_ids[1], platform_ids[2])
    } else if platform_ids.is_empty() || platform_ids[0] == 3 {
        (3, 1, 0x409)
    } else {
        (1, 0, 0)
    };
    // Get the string (with escape sequences that need to be unescaped)
    let string = name_spec
        .iter()
        .find(|t| t.kind() == fea_rs::Kind::String)
        .and_then(|t| t.as_token())
        .map(|tok| {
            let s = tok.text.as_str();
            // Remove surrounding quotes and unescape
            unescape_string(platform_id, &s[1..s.len() - 1])
        })
        .unwrap();
    (platform_id, plat_enc_id, lang_id, string)
}

/// Parse a DecOctHex value into u16
fn parse_dec_oct_hex(node: &fea_rs::typed::DecOctHex) -> u16 {
    use fea_rs::typed::DecOctHex;
    match node {
        DecOctHex::Decimal(num) => num.text().parse().unwrap(),
        DecOctHex::Octal(num) => u16::from_str_radix(num.text(), 8).unwrap(),
        DecOctHex::Hex(num) => {
            u16::from_str_radix(num.text().trim_start_matches("0x"), 16).unwrap()
        }
    }
}

/// Decode Windows UTF-16 escapes (`\XXXX`) or Macintosh Mac Roman escapes (`\XX`).
fn unescape_string(platform_id: u16, s: &str) -> String {
    let is_mac = platform_id == 1;
    let escape_digits = if is_mac { 2 } else { 4 };
    let mut units: Vec<u16> = Vec::with_capacity(s.len());
    let mut rest = s;
    while let Some(pos) = rest.find('\\') {
        units.extend(rest[..pos].encode_utf16());
        rest = &rest[pos + 1..];
        let code = rest
            .get(..escape_digits)
            .filter(|hex| hex.bytes().all(|b| b.is_ascii_hexdigit()))
            .and_then(|hex| u16::from_str_radix(hex, 16).ok());
        match code {
            Some(code) if is_mac => {
                let c = MacRomanMapping.decode(code as u8);
                units.extend(c.encode_utf16(&mut [0; 2]).iter());
            }
            Some(code) => units.push(code),
            None => {
                // Preserve the backslash when the following text is not a valid escape.
                units.push('\\' as u16);
                continue;
            }
        }
        rest = &rest[escape_digits..];
    }
    units.extend(rest.encode_utf16());
    String::from_utf16_lossy(&units)
}

impl From<fea_rs::typed::SizeMenuName> for NameRecord {
    fn from(val: fea_rs::typed::SizeMenuName) -> Self {
        // Get the NameSpec which contains platform info and string
        let name_spec = val.iter().find_map(fea_rs::typed::NameSpec::cast).unwrap();

        let (platform_id, plat_enc_id, lang_id, string) = parse_namespec(name_spec);

        Self::new(
            platform_id,
            plat_enc_id,
            lang_id,
            string,
            NameRecordKind::FeatureSizeMenuName,
            val.range(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_roundtrip_namerecord_simple() {
        const FEA: &str = r#"table name { nameid 9 "John Smith"; } name;"#;
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::NameTable::cast)
            .unwrap();
        let name_rec = table
            .node()
            .iter_children()
            .find_map(fea_rs::typed::NameRecord::cast)
            .unwrap();
        let stmt = NameRecord::from(name_rec);
        assert_eq!(stmt.kind, NameRecordKind::Name(9));
        assert_eq!(stmt.platform_id, 3);
        assert_eq!(stmt.string, "John Smith");
        assert_eq!(stmt.as_fea(""), r#"nameid 9 "John Smith";"#);
    }

    #[test]
    fn test_roundtrip_namerecord_with_platform() {
        const FEA: &str = r#"table name { nameid 1 3 1 0x409 "Arial"; } name;"#;
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::NameTable::cast)
            .unwrap();
        let name_rec = table
            .node()
            .iter_children()
            .find_map(fea_rs::typed::NameRecord::cast)
            .unwrap();
        let stmt = NameRecord::from(name_rec);
        assert_eq!(stmt.kind, NameRecordKind::Name(1));
        assert_eq!(stmt.platform_id, 3);
        assert_eq!(stmt.plat_enc_id, 1);
        assert_eq!(stmt.lang_id, 0x409);
        assert_eq!(stmt.string, "Arial");
        assert_eq!(stmt.as_fea(""), r#"nameid 1 "Arial";"#);
    }

    #[test]
    fn test_roundtrip_namerecord_with_escapes() {
        const FEA: &str = r#"table name { nameid 9 "Joachim M\00fcller-Lanc\00e9"; } name;"#;
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::NameTable::cast)
            .unwrap();
        let name_rec = table
            .node()
            .iter_children()
            .find_map(fea_rs::typed::NameRecord::cast)
            .unwrap();
        let stmt = NameRecord::from(name_rec);
        assert_eq!(stmt.kind, NameRecordKind::Name(9));
        // Parser unescapes the string, so we get actual Unicode characters
        assert_eq!(stmt.string, "Joachim Müller-Lancé");
        // When we serialize, we re-escape non-ASCII characters
        assert_eq!(
            stmt.as_fea(""),
            r#"nameid 9 "Joachim M\00fcller-Lanc\00e9";"#
        );
    }

    #[test]
    fn test_roundtrip_namerecord_with_mac_escapes() {
        const FEA: &str = r#"table name { nameid 9 1 "Joachim M\9fller-Lanc\8e \22\5c"; } name;"#;
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::NameTable::cast)
            .unwrap();
        let name_rec = table
            .node()
            .iter_children()
            .find_map(fea_rs::typed::NameRecord::cast)
            .unwrap();
        let stmt = NameRecord::from(name_rec);
        assert_eq!(stmt.string, "Joachim Müller-Lancé \"\\");
        assert_eq!(
            stmt.as_fea(""),
            r#"nameid 9 1 "Joachim M\9fller-Lanc\8e \22\5c";"#
        );
    }

    #[test]
    fn test_roundtrip_namerecord_with_windows_escapes() {
        const FEA: &str = r#"table name { nameid 9 "\0022\005c\000a \d83d\de00"; } name;"#;
        let (parsed, _) = fea_rs::parse::parse_string(FEA);
        let table = parsed
            .root()
            .iter_children()
            .find_map(fea_rs::typed::NameTable::cast)
            .unwrap();
        let name_rec = table
            .node()
            .iter_children()
            .find_map(fea_rs::typed::NameRecord::cast)
            .unwrap();
        let stmt = NameRecord::from(name_rec);
        assert_eq!(stmt.string, "\"\\\n 😀");
        assert_eq!(stmt.as_fea(""), r#"nameid 9 "\0022\005c\000a \d83d\de00";"#);
    }

    #[test]
    fn test_generate_namerecord() {
        let stmt = NameRecord::new(
            3,
            1,
            0x409,
            "Test Designer".to_string(),
            NameRecordKind::Name(9),
            0..0,
        );
        assert_eq!(stmt.as_fea(""), r#"nameid 9 "Test Designer";"#);
    }
}
