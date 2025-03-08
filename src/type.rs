#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Error,

    Any,
    Unknown,

    Union(Vec<Type>),

    Number,
    String,
    Bool,
    Nil,
    Table(TypeTable),
    Function(Vec<Type>, Vec<Type>),

    Const(ConstData),

    Variable(Variable),
    Generic(Vec<Variable>, Box<Type>),
}

// NOTE: We can assume that that Option<Box<Type>> is an unknown type.
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

#[derive(Debug, Clone)]
pub struct Variable {
    name: String,
    #[cfg(feature = "extra_span")]
    span: crate::ast::Span,
}

impl Type {
    pub fn never() -> Self {
        Type::Union(vec![])
    }

    pub fn is_never(&self) -> bool {
        matches!(self, Type::Union(vec) if vec.is_empty())
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Type::Error)
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Type::Union(types) => types.iter().all(|t| t.is_concrete()),
            Type::Any | Type::Unknown => true,
            Type::Number | Type::String | Type::Bool | Type::Nil => true,
            Type::Table(t) => {
                t.consts.iter().all(|(_, t)| t.is_concrete())
                    && t.number.as_ref().map(|t| t.is_concrete()).unwrap_or(true)
                    && t.string.as_ref().map(|t| t.is_concrete()).unwrap_or(true)
                    && t.bool.as_ref().map(|t| t.is_concrete()).unwrap_or(true)
                    && t.table.as_ref().map(|t| t.is_concrete()).unwrap_or(true)
                    && t.function.as_ref().map(|t| t.is_concrete()).unwrap_or(true)
            }
            Type::Function(ps, r) => {
                ps.iter().all(|t| t.is_concrete()) && r.iter().all(|t| t.is_concrete())
            }
            Type::Const(_) => true,
            Type::Variable(_) => true, // !?
            Type::Generic(_, _) => false,
            Type::Error => false,
        }
    }

    pub fn include(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Error, _) | (_, Type::Error) => true,

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
                    && l_ret
                        .iter()
                        .zip(r_ret.iter().chain(vec![Type::Nil].iter().cycle()))
                        .all(|(l, r)| l.include(r))
            }
            (Type::Function(_, _), _) => false,
            (Type::Variable(l), Type::Variable(r)) => l == r, // Is this correct?
            (Type::Variable(_), _) => panic!("Variable type should be resolved: {}", self),
            // (Type::Variable(_), _) => true,
            (Type::Generic(_, _), _) => panic!("Generic type should be resolved: {}", self),
        }
    }

    pub fn intersect(&self, other: &Type) -> Type {
        match (self, other) {
            (Type::Error, _) | (_, Type::Error) => Type::Error,

            (Type::Union(l), Type::Union(r)) => {
                let mut types = vec![];
                for l in l {
                    for r in r {
                        types.push(l.intersect(r));
                    }
                }
                Type::Union(types).normalize()
            }
            (Type::Union(l), r) => {
                let mut types = vec![];
                for l in l {
                    types.push(l.intersect(r));
                }
                Type::Union(types).normalize()
            }
            (l, Type::Union(r)) => {
                let mut types = vec![];
                for r in r {
                    types.push(l.intersect(r));
                }
                Type::Union(types).normalize()
            }

            (Type::Any, _) => other.clone(),
            (_, Type::Any) => self.clone(),
            (Type::Unknown, _) => other.clone(),
            (_, Type::Unknown) => self.clone(),

            (Type::Const(l), Type::Const(r)) if l == r => self.clone(),
            (Type::Const(_), _) => Type::never(),
            (Type::Number, Type::Number) => Type::Number,
            (Type::Number, _) => Type::never(),
            (Type::String, Type::String) => Type::String,
            (Type::String, _) => Type::never(),
            (Type::Bool, Type::Bool) => Type::Bool,
            (Type::Bool, _) => Type::never(),
            (Type::Nil, Type::Nil) => Type::Nil,
            (Type::Nil, _) => Type::never(),
            (Type::Table(l), Type::Table(r)) => Type::Table(l.intersect(r)),
            (Type::Table(_), _) => Type::never(),
            (Type::Function(l_ps, l_ret), Type::Function(r_ps, r_ret)) => Type::Function(
                l_ps.iter()
                    .zip(r_ps.iter())
                    .map(|(l, r)| l.intersect(r))
                    .collect(),
                l_ret
                    .iter()
                    .zip(r_ret.iter())
                    .map(|(l, r)| l.intersect(r))
                    .collect(),
            ),
            (Type::Function(_, _), _) => Type::never(),
            (Type::Variable(l), Type::Variable(r)) if l == r => self.clone(),
            (Type::Variable(_), _) => panic!("Variable type should be resolved: {}", self),
            (Type::Generic(_, _), _) => panic!("Generic type should be resolved: {}", self),
        }
    }

    pub fn normalize(&self) -> Type {
        // TODO: Handle Error
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
                r.iter().map(|t| t.normalize()).collect(),
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

    /// Resolve all variables in the type
    pub fn resolve(&self, map: &dyn Fn(&str) -> Option<Type>) -> Result<Type, String> {
        match self {
            Type::Variable(var) => {
                map(var.name()).ok_or_else(|| format!("Variable {} not found", var.name()))
            }
            Type::Union(types) => {
                let mut types = types
                    .iter()
                    .map(|t| t.resolve(map))
                    .collect::<Result<Vec<_>, _>>()?;
                types.sort();
                Ok(Type::Union(types).normalize())
            }
            Type::Table(TypeTable {
                consts,
                number,
                string,
                bool,
                table,
                function,
            }) => Ok(Type::Table(TypeTable {
                consts: consts
                    .iter()
                    .map(|(cd, t)| Ok((cd.clone(), t.resolve(map)?)))
                    .collect::<Result<_, String>>()?,
                number: number
                    .as_ref()
                    .map(|t| t.resolve(map))
                    .transpose()?
                    .map(Box::new),
                string: string
                    .as_ref()
                    .map(|t| t.resolve(map))
                    .transpose()?
                    .map(Box::new),
                bool: bool
                    .as_ref()
                    .map(|t| t.resolve(map))
                    .transpose()?
                    .map(Box::new),
                table: table
                    .as_ref()
                    .map(|t| t.resolve(map))
                    .transpose()?
                    .map(Box::new),
                function: function
                    .as_ref()
                    .map(|t| t.resolve(map))
                    .transpose()?
                    .map(Box::new),
            })),
            Type::Function(ps, r) => Ok(Type::Function(
                ps.iter()
                    .map(|t| t.resolve(map))
                    .collect::<Result<_, _>>()?,
                r.iter().map(|t| t.resolve(map)).collect::<Result<_, _>>()?,
            )),
            Type::Generic(params, t) => {
                let map = |name: &str| {
                    if let Some(t) = params.iter().find(|p| p.name() == name) {
                        Some(Type::Variable(t.clone()))
                    } else {
                        map(name)
                    }
                };
                Ok(t.resolve(&map)?)
            }
            t => Ok(t.clone()),
        }
    }

    pub fn indexing(&self, path: &[ConstData]) -> Type {
        if path.is_empty() {
            return self.clone();
        }

        match self {
            Type::Any => Type::Any,
            Type::Unknown => Type::Unknown,
            Type::Union(vec) => {
                let mut types = vec![];
                for t in vec {
                    types.push(t.indexing(path));
                }
                Type::Union(types).normalize()
            }
            Type::Number => Type::never(),
            Type::String => Type::never(),
            Type::Bool => Type::never(),
            Type::Nil => Type::never(),
            Type::Table(type_table) => {
                for (cd, t) in &type_table.consts {
                    if cd == &path[0] {
                        return t.indexing(&path[1..]);
                    }
                }

                // Can I return nil? or should I return unknown?
                return match path[0] {
                    ConstData::Number(_) => type_table
                        .number
                        .as_ref()
                        .map(|t| t.indexing(&path[1..]))
                        .unwrap_or(Type::Nil),
                    ConstData::String(_) => type_table
                        .string
                        .as_ref()
                        .map(|t| t.indexing(&path[1..]))
                        .unwrap_or(Type::Nil),
                    ConstData::Bool(_) => type_table
                        .bool
                        .as_ref()
                        .map(|t| t.indexing(&path[1..]))
                        .unwrap_or(Type::Nil),
                };
            }
            Type::Function(_, _) => Type::never(),
            Type::Const(_) => Type::never(),
            Type::Variable(_) => panic!("Variable type should be resolved: {}", self),
            Type::Generic(_, _) => panic!("Generic type should be resolved: {}", self),
            Type::Error => Type::Error,
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
                write!(f, ") -> (")?;
                for (i, t) in r.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Nil => write!(f, "()"),
            Type::Unknown => write!(f, "unknown"),
            Type::Any => write!(f, "any"),
            Type::Union(ts) => {
                if ts.is_empty() {
                    return write!(f, "never");
                }
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
            Type::Variable(var) => write!(f, "{}", var.name()),
            Type::Generic(params, t) => {
                write!(f, "<")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p.name())?;
                }
                write!(f, "> {}", t)
            }
            Type::Error => write!(f, "error"),
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

    pub fn intersect(&self, other: &Self) -> Self {
        TypeTable {
            consts: self
                .consts
                .iter()
                .filter_map(|(cd, t)| {
                    other
                        .consts
                        .iter()
                        .find(|(other_cd, _)| other_cd == cd)
                        .map(|(_, t)| t.clone())
                        .or_else(|| match cd {
                            ConstData::Number(_) => other.number.clone().map(|t| (*t).to_owned()),
                            ConstData::String(_) => other.string.clone().map(|t| (*t).to_owned()),
                            ConstData::Bool(_) => other.bool.clone().map(|t| (*t).to_owned()),
                        })
                        .map(|other_t| (cd.clone(), t.intersect(&other_t)))
                })
                .chain(other.consts.iter().filter_map(|(cd, t)| {
                    if self.consts.iter().any(|(self_cd, _)| self_cd == cd) {
                        return None;
                    }
                    self.consts
                        .iter()
                        .find(|(self_cd, _)| self_cd == cd)
                        .map(|(_, t)| t.clone())
                        .or_else(|| match cd {
                            ConstData::Number(_) => self.number.clone().map(|t| (*t).to_owned()),
                            ConstData::String(_) => self.string.clone().map(|t| (*t).to_owned()),
                            ConstData::Bool(_) => self.bool.clone().map(|t| (*t).to_owned()),
                        })
                        .map(|self_t| (cd.clone(), t.intersect(&self_t)))
                }))
                .collect(),
            number: match (&self.number, &other.number) {
                (Some(l), Some(r)) => Some(Box::new(l.intersect(r))),
                _ => None,
            },
            string: match (&self.string, &other.string) {
                (Some(l), Some(r)) => Some(Box::new(l.intersect(r))),
                _ => None,
            },
            bool: match (&self.bool, &other.bool) {
                (Some(l), Some(r)) => Some(Box::new(l.intersect(r))),
                _ => None,
            },
            table: match (&self.table, &other.table) {
                (Some(l), Some(r)) => Some(Box::new(l.intersect(r))),
                _ => None,
            },
            function: match (&self.function, &other.function) {
                (Some(l), Some(r)) => Some(Box::new(l.intersect(r))),
                _ => None,
            },
        }
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
        let mut comma = false;
        for (cd, t) in consts.iter() {
            if comma {
                write!(f, ", ")?;
            } else {
                comma = true;
            }
            match cd {
                ConstData::Number(n) => write!(f, "{}", n.0),
                ConstData::String(s) => write!(f, "{:?}", s),
                ConstData::Bool(b) => write!(f, "{}", b),
            }?;
            write!(f, ": {}", t)?;
        }
        for (name, t) in &[
            ("num", number),
            ("str", string),
            ("bool", bool),
            ("table", table),
            ("fn", function),
        ] {
            if let Some(t) = t {
                if comma {
                    write!(f, ", ")?;
                } else {
                    comma = true;
                }
                write!(f, "[{}]: {}", name, t)?;
            }
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

impl Variable {
    pub fn from_spanned_str(spanned: crate::ast::Spanned<String>) -> Self {
        Variable {
            name: spanned.node,
            #[cfg(feature = "extra_span")]
            span: spanned.span,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    #[cfg(feature = "extra_span")]
    pub fn span(&self) -> &crate::ast::Span {
        &self.span
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Variable {}

impl PartialOrd for Variable {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.name.cmp(&other.name))
    }
}

impl Ord for Variable {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
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
