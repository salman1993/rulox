use std::fs;

#[derive(Debug)]
pub struct Source {
    pub filepath: String,
    pub content: String,
}

impl Source {
    pub fn new(filepath: &str, content: &str) -> Source {
        Source {
            filepath: filepath.to_string(),
            content: content.to_string(),
        }
    }

    pub fn from_content(content: &str) -> Source {
        Source::new("N/A", content)
    }
}

pub fn read_source(filepath: &str) -> Result<Source, String> {
    let content = fs::read_to_string(filepath);

    match content {
        Ok(s) => Ok(Source {
            filepath: String::from(filepath),
            content: s,
        }),
        Err(e) => Err(format!("Couldn't read {filepath} : {e}")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_read_success() {
        let filepath = "/tmp/test.lox";
        let loxfile = "var x = 2;\nprint 1 != 2;";
        fs::write(filepath, loxfile).expect("Unable to write file");
        let source = read_source(filepath).unwrap();
        assert_eq!(source.filepath, filepath);
        assert_eq!(source.content, loxfile);
    }

    #[test]
    fn test_file_does_not_exist() {
        let result = read_source("/tmp/doesnotexist.lox");
        assert!(result.is_err());
    }
}
