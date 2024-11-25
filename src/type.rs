#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    /// Any aka Top
    Any,
    Unknown,

    Union(Vec<Type>),

    Number,
    String,
    Bool,
    Nil,
    Table(TypeTable),
    Function(Vec<Type>, Box<Type>),

    Const(ConstData),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeTable {
    pub consts: Vec<(ConstData, Type)>,
    pub number: Option<Box<Type>>,
    pub string: Option<Box<Type>>,
    pub bool: Option<Box<Type>>,
    pub table: Option<Box<Type>>,
    pub function: Option<Box<Type>>,
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
            (l @ Type::Union(_), Type::Union(r)) => r.iter().all(|r| l.include(r)),
            (Type::Union(l), r) => l.iter().any(|t| t.include(r)),

            (Type::Any, _) => true,
            (_, Type::Any) => true,
            (Type::Unknown, _) => true,

            (Type::Const(l), Type::Const(r)) => l == r,
            (Type::Const(_), _) => false,
            (l, Type::Const(cd)) => l.include(&cd.r#type()),

            (Type::Number, Type::Number) => true,
            (Type::Number, _) => false,
            (Type::String, Type::String) => true,
            (Type::String, _) => false,
            (Type::Bool, Type::Bool) => true,
            (Type::Bool, _) => false,
            (Type::Nil, Type::Nil) => true,
            (Type::Nil, _) => false,
            (Type::Table(l), Type::Table(r)) => l.include(r),
            (Type::Table(_), _) => false,
            (Type::Function(l_ps, l_ret), Type::Function(r_ps, r_ret)) => {
                l_ps.iter()
                    .zip(r_ps.iter().chain(vec![Type::Nil].iter().cycle()))
                    .all(|(l, r)| l.include(r))
                    && l_ret.include(r_ret)
            }
            (Type::Function(_, _), _) => false,
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

                for i in 0..types.len() {
                    for j in ((i + 1)..types.len()).rev() {
                        if types[i].include(&types[j]) {
                            types.remove(j);
                        }
                    }
                }

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

    pub fn from_types(mut types: Vec<Type>) -> Option<Type> {
        if types.is_empty() {
            None
        } else if types.len() == 1 {
            Some(types.pop().unwrap())
        } else {
            Some(Type::Union(types).normalize())
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
            Type::Any => write!(f, "any"),
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
            table: Some(Box::new(Type::Unknown)),
            function: Some(Box::new(Type::Unknown)),
        }
    }

    pub fn include(&self, other: &TypeTable) -> bool {
        match (&self.number, &other.number) {
            (_, None) => {}
            (Some(l), Some(r)) if l.include(r) => {}
            _ => return false,
        }

        match (&self.string, &other.string) {
            (_, None) => {}
            (Some(l), Some(r)) if l.include(r) => {}
            _ => return false,
        }

        match (&self.bool, &other.bool) {
            (_, None) => {}
            (Some(l), Some(r)) if l.include(r) => {}
            _ => return false,
        }

        match (&self.table, &other.table) {
            (_, None) => {}
            (Some(l), Some(r)) if l.include(r) => {}
            _ => return false,
        }

        match (&self.function, &other.function) {
            (_, None) => {}
            (Some(l), Some(r)) if l.include(r) => {}
            _ => return false,
        }

        if !self.consts.iter().all(|(cd, t)| {
            other
                .consts
                .iter()
                .any(|(other_cd, other_t)| other_cd == cd && t.include(other_t))
        }) {
            return false;
        }

        if !other.consts.iter().all(|(cd, t)| {
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
                    ConstData::Bool(_) => self.bool.as_ref().map(|s| s.include(t)).unwrap_or(false),
                }
        }) {
            return false;
        }

        true
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
            table: self.table.as_ref().map(|t| Box::new(t.normalize())),
            function: self.function.as_ref().map(|t| Box::new(t.normalize())),
        }
    }
}

impl std::fmt::Display for TypeTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let TypeTable {
            consts,
            number,
            string,
            bool,
            table,
            function,
        } = self;

        write!(f, "{{")?;
        for (i, (cd, t)) in consts.iter().enumerate() {
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
        if let Some(t) = number {
            write!(f, ", [num]: {}", t)?;
        }
        if let Some(t) = string {
            write!(f, ", [str]: {}", t)?;
        }
        if let Some(t) = bool {
            write!(f, ", [bool]: {}", t)?;
        }
        if let Some(t) = table {
            write!(f, ", [table]: {}", t)?;
        }
        if let Some(t) = function {
            write!(f, ", [fn]: {}", t)?;
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

    let t = Type::Union(vec![
        Type::Number,
        Type::Union(vec![Type::String, Type::Number, Type::String, Type::Number]),
        Type::Any,
    ]);
    assert_eq!(t.normalize(), Type::Any);

    let t = Type::Union(vec![Type::Unknown, Type::String, Type::Any]);
    assert_eq!(t.normalize(), Type::Any);
}
