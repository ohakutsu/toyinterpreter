#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i32),
    Boolean(bool),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(n) => write!(f, "{}", n),
            Object::Boolean(b) => write!(f, "{}", b),
        }
    }
}
