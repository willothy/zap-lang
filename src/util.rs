pub trait TrimDupSpaces {
    fn trim_dup_spaces(&self) -> String;
}

impl TrimDupSpaces for String {
    fn trim_dup_spaces(&self) -> String {
        let mut result = String::with_capacity(self.len());
        self.split_whitespace().for_each(|s| {
            if !result.is_empty() {
                result.push(' ');
            }
            result.push_str(s);
        });
        result
    }
}
