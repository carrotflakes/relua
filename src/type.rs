#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    String,
    Bool,
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Table,
    Function(Vec<Type>, Box<Type>),
    Nil,
    Unknown,
    Union(Vec<Type>),
}

impl Type {
    pub fn include(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Number, Type::Number) => true,
            (Type::Number, _) => false,
            (Type::String, Type::String) => true,
            (Type::String, _) => false,
            (Type::Bool, Type::Bool) => true,
            (Type::Bool, _) => false,
            (Type::Array(l), Type::Array(r)) => l.include(r),
            (Type::Array(_), _) => false,
            (Type::Tuple(l), Type::Tuple(r)) => l.iter().zip(r.iter()).all(|(l, r)| l.include(r)),
            (Type::Tuple(_), _) => false,
            (Type::Table, Type::Table) => true,
            (Type::Table, _) => false,
            (Type::Function(l_ps, l_ret), Type::Function(r_ps, r_ret)) => {
                l_ps.len() == r_ps.len()
                    && l_ps.iter().zip(r_ps.iter()).all(|(l, r)| l.include(r))
                    && l_ret.include(r_ret)
            }
            (Type::Function(_, _), _) => false,
            (Type::Nil, Type::Nil) => true,
            (Type::Nil, _) => false,
            (Type::Unknown, _) => true,
            (l @ Type::Union(_), Type::Union(r)) => r.iter().all(|r| l.include(r)),
            (Type::Union(l), r) => l.iter().any(|t| t.include(r)),
        }
    }
}
