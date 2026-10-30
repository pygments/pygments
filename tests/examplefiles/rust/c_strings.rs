fn main() {
    let ordinary = c"hello";
    let escaped = c"\xC3\xA6";
    let unicode = c"\u{00E6}";
    let raw = cr"C:\path";
    let quoted = cr#""hello""#;
    let hashed = cr##""hello"#"##;
}
