#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
    Const(ConstData),
    Union(Vec<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConstData {
    Number(F64),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct F64(f64);

impl Type {
    pub fn include(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Const(l), Type::Const(r)) => l == r,
            (Type::Const(_), _) => false,
            (l, Type::Const(cd)) => l.include(&cd.r#type()),
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
            (Type::Table, Type::Array(_)) => true,
            (Type::Table, Type::Tuple(_)) => true,
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

    pub fn normalize(&self) -> Type {
        match self {
            Type::Union(types) => {
                let mut types = types
                    .iter()
                    .flat_map(|t| match t.normalize() {
                        Type::Union(ts) => ts.iter().cloned().collect::<Vec<_>>(),
                        t => vec![t.clone()],
                    })
                    .collect::<Vec<_>>();
                types.sort();
                types.dedup();
                if types.len() == 1 {
                    types.pop().unwrap()
                } else {
                    Type::Union(types)
                }
            }
            Type::Array(t) => Type::Array(Box::new(t.normalize())),
            Type::Tuple(ts) => Type::Tuple(ts.iter().map(|t| t.normalize()).collect()),
            Type::Function(ps, r) => Type::Function(
                ps.iter().map(|t| t.normalize()).collect(),
                Box::new(r.normalize()),
            ),
            t => t.clone(),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number => write!(f, "num"),
            Type::String => write!(f, "str"),
            Type::Bool => write!(f, "bool"),
            Type::Array(t) => write!(f, "[{}]", t),
            Type::Tuple(ts) => {
                write!(f, "(")?;
                for (i, t) in ts.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Table => write!(f, "table"),
            Type::Function(ps, r) => {
                write!(f, "(")?;
                for (i, t) in ps.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ") -> {}", r)
            }
            Type::Nil => write!(f, "()"),
            Type::Unknown => write!(f, "unknown"),
            Type::Union(ts) => {
                write!(f, "(")?;
                for (i, t) in ts.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Const(const_data) => match const_data {
                ConstData::Number(n) => write!(f, "{}", n.0),
                ConstData::String(s) => write!(f, "{:?}", s),
                ConstData::Bool(b) => write!(f, "{}", b),
            },
        }
    }
}

impl ConstData {
    pub fn r#type(&self) -> Type {
        match self {
            ConstData::Number(_) => Type::Number,
            ConstData::String(_) => Type::String,
            ConstData::Bool(_) => Type::Bool,
        }
    }

    pub fn try_from_f64(n: f64) -> Option<Self> {
        if n.is_nan() {
            None
        } else {
            Some(ConstData::Number(F64(n)))
        }
    }
}

impl Eq for F64 {}

impl Ord for F64 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[test]
fn test() {
    let t = Type::Union(vec![
        Type::Number,
        Type::Union(vec![Type::String, Type::Number, Type::String, Type::Number]),
    ]);
    assert_eq!(t.normalize(), Type::Union(vec![Type::Number, Type::String]));
}
