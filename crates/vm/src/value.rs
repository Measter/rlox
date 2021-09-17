#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
}

impl Value {
    pub fn display(&self) {
        match self {
            Value::Number(num) => print!("{}", num),
        }
    }
}
