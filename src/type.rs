#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Number,
    String,
    Bool,
    Table(TypeTable),
    Function(Vec<Type>, Box<Type>),
    Nil,
    Unknown,
    Const(ConstData),
    Union(Vec<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeTable {
    pub consts: Vec<(ConstData, Type)>,
    pub number: Option<Box<Type>>,
    pub string: Option<Box<Type>>,
    pub bool: Option<Box<Type>>,
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
            (Type::Table(l), Type::Table(r)) => l.include(r),
            (Type::Table(_), _) => false,
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
            Type::Table(t) => Type::Table(t.normalize()),
            Type::Function(ps, r) => Type::Function(
                ps.iter().map(|t| t.normalize()).collect(),
                Box::new(r.normalize()),
            ),
            t => t.clone(),
        }
    }

    pub fn from_types(types: Vec<Type>) -> Option<Type> {
        let mut types = types
            .into_iter()
            .flat_map(|t| match t {
                Type::Union(ts) => ts.into_iter().collect::<Vec<_>>(),
                t => vec![t],
            })
            .collect::<Vec<_>>();
        types.sort();
        types.dedup();

        if types.is_empty() {
            None
        } else if types.len() == 1 {
            Some(types.pop().unwrap())
        } else {
            Some(Type::Union(types))
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number => write!(f, "num"),
            Type::String => write!(f, "str"),
            Type::Bool => write!(f, "bool"),
            Type::Table(t) => write!(f, "{}", t),
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

impl TypeTable {
    pub fn any() -> Self {
        TypeTable {
            consts: vec![],
            number: Some(Box::new(Type::Unknown)),
            string: Some(Box::new(Type::Unknown)),
            bool: Some(Box::new(Type::Unknown)),
        }
    }

    pub fn include(&self, other: &TypeTable) -> bool {
        other
            .number
            .as_ref()
            .map(|o| self.number.as_ref().map(|s| s.include(&o)).unwrap_or(false))
            .unwrap_or(true)
            && other
                .string
                .as_ref()
                .map(|o| self.string.as_ref().map(|s| s.include(&o)).unwrap_or(false))
                .unwrap_or(true)
            && other
                .bool
                .as_ref()
                .map(|o| self.bool.as_ref().map(|s| s.include(&o)).unwrap_or(false))
                .unwrap_or(true)
            && self.consts.iter().all(|(cd, t)| {
                other
                    .consts
                    .iter()
                    .any(|(other_cd, other_t)| other_cd == cd && other_t.include(t))
            })
            && other.consts.iter().all(|(cd, t)| {
                self.consts
                    .iter()
                    .any(|(self_cd, self_t)| self_cd == cd && self_t.include(t))
                    || match cd {
                        ConstData::Number(_) => {
                            self.number.as_ref().map(|s| s.include(t)).unwrap_or(false)
                        }
                        ConstData::String(_) => {
                            self.string.as_ref().map(|s| s.include(t)).unwrap_or(false)
                        }
                        ConstData::Bool(_) => {
                            self.bool.as_ref().map(|s| s.include(t)).unwrap_or(false)
                        }
                    }
            })
    }

    pub fn normalize(&self) -> Self {
        TypeTable {
            consts: self
                .consts
                .iter()
                .map(|(cd, t)| (cd.clone(), t.normalize()))
                .collect(),
            number: self.number.as_ref().map(|t| Box::new(t.normalize())),
            string: self.string.as_ref().map(|t| Box::new(t.normalize())),
            bool: self.bool.as_ref().map(|t| Box::new(t.normalize())),
        }
    }
}

impl std::fmt::Display for TypeTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (cd, t)) in self.consts.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            match cd {
                ConstData::Number(n) => write!(f, "{}", n.0),
                ConstData::String(s) => write!(f, "{:?}", s),
                ConstData::Bool(b) => write!(f, "{}", b),
            }?;
            write!(f, ": {}", t)?;
        }
        if let Some(t) = &self.number {
            write!(f, ", [num]: {}", t)?;
        }
        if let Some(t) = &self.string {
            write!(f, ", [str]: {}", t)?;
        }
        if let Some(t) = &self.bool {
            write!(f, ", [bool]: {}", t)?;
        }
        write!(f, "}}")
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
