pub mod python;

use std::{collections::HashMap, fmt::Display, sync::Arc};

use once_cell::sync::Lazy;
use thiserror::Error;

// Data types
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ArithOp {
    Plus,
    Minus,
    Mul,
    Div,
    FloorDiv,
    Pow,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum Type {
    Int,
    Float,
    Bool,
    Numeric,
}

pub type OperatorT = Arc<dyn Fn(&[Value]) -> Result<Value, EvaluationError> + Send + Sync>;

pub enum Operator {
    ArithOp(ArithOp),
    Named(String),
}

pub enum ExpressionTree {
    Value(Value),
    Constant(String),
    Apply(Operator, Vec<ExpressionTree>),
}

#[derive(Error, Debug)]
pub enum EvaluationError {
    #[error("Type error: Expected {expected}, got {got}")]
    TypeError { expected: Type, got: Type },
    #[error("There's no registered constant with the name {0:?}")]
    NoSuchConstant(String),
    #[error("There's no registered function with the name {0:?}")]
    NoSuchFunction(String),
    #[error("Arity error: Expected {min}..={max}, got {got}")]
    ArityError { min: usize, max: usize, got: usize },
}

pub struct LookupContext {
    functions: HashMap<String, OperatorT>,
    constants: HashMap<String, Value>,
}

// Implementations
impl Value {
    fn get_type(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::Float(_) => Type::Float,
            Self::Bool(_) => Type::Bool,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => f.write_str("int"),
            Self::Float => f.write_str("float"),
            Self::Numeric => f.write_str("int or float"),
            Self::Bool => f.write_str("bool"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(n) => n.fmt(f),
            Self::Float(n) => n.fmt(f),
            Self::Bool(n) => n.fmt(f),
        }
    }
}

impl Operator {
    fn resolve(self, lookup_context: &LookupContext) -> Result<OperatorT, EvaluationError> {
        match self {
            Self::ArithOp(ArithOp::Plus) => Ok(Arc::new(globals::add)),
            Self::ArithOp(ArithOp::Minus) => Ok(Arc::new(globals::sub)),
            Self::ArithOp(ArithOp::Mul) => Ok(Arc::new(globals::mul)),
            Self::ArithOp(ArithOp::Div) => Ok(Arc::new(globals::div)),
            Self::ArithOp(ArithOp::FloorDiv) => Ok(Arc::new(globals::floor_div)),
            Self::ArithOp(ArithOp::Pow) => Ok(Arc::new(globals::pow)),
            Self::Named(name) => lookup_context
                .functions
                .get(&name)
                .ok_or(EvaluationError::NoSuchFunction(name))
                .cloned(),
        }
    }
}

// Globals
pub mod globals {
    use crate::calc::Type;

    use super::{EvaluationError, Value};

    macro_rules! impl_arith_op {
        ($name:ident, $op:tt, $empty:expr) => {
            pub fn $name(v: &[Value]) -> Result<Value, EvaluationError> {
                if v.is_empty() {
                    return Ok($empty);
                }

                fn binary(a: Value, b: Value) -> Result<Value, EvaluationError> {
                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a $op b)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 $op b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a $op b as f64)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a $op b)),
                        (Value::Bool(_), _) => Err(EvaluationError::TypeError { expected: Type::Numeric, got: Type::Bool }),
                        (_, Value::Bool(_)) => Err(EvaluationError::TypeError { expected: Type::Numeric, got: Type::Bool }),
                    }
                }

                let mut result = v[0];
                for &arg in &v[1..] {
                    result = binary(result, arg)?;
                }
                Ok(result)
            }
        };
    }

    impl_arith_op!(add, +, Value::Int(0));
    impl_arith_op!(sub, -, Value::Int(0));
    impl_arith_op!(mul, *, Value::Int(1));

    pub fn div(v: &[Value]) -> Result<Value, EvaluationError> {
        if v.is_empty() {
            return Ok(Value::Int(1));
        }

        let mut result = match v[0] {
            Value::Int(n) => n as f64,
            Value::Float(n) => n,
            Value::Bool(_) => {
                return Err(EvaluationError::TypeError {
                    expected: Type::Numeric,
                    got: Type::Bool,
                })
            }
        };

        for &arg in &v[1..] {
            result = result
                / match arg {
                    Value::Int(n) => n as f64,
                    Value::Float(n) => n,
                    Value::Bool(_) => {
                        return Err(EvaluationError::TypeError {
                            expected: Type::Numeric,
                            got: Type::Bool,
                        })
                    }
                };
        }

        Ok(Value::Float(result))
    }

    pub fn floor_div(v: &[Value]) -> Result<Value, EvaluationError> {
        if v.is_empty() {
            return Ok(Value::Int(1));
        }

        let mut result = match v[0] {
            Value::Int(n) => n,
            other => {
                return Err(EvaluationError::TypeError {
                    expected: Type::Int,
                    got: other.get_type(),
                })
            }
        };

        for &arg in &v[1..] {
            result = result
                / match arg {
                    Value::Int(n) => n,
                    other => {
                        return Err(EvaluationError::TypeError {
                            expected: Type::Int,
                            got: other.get_type(),
                        })
                    }
                };
        }

        Ok(Value::Int(result))
    }

    pub fn negate(v: &[Value]) -> Result<Value, EvaluationError> {
        Ok(match v {
            [Value::Int(n)] => Value::Int(-n),
            [Value::Float(n)] => Value::Float(-n),
            [x] => return Err(EvaluationError::TypeError { expected: Type::Numeric, got: x.get_type() }),
            _ => return Err(EvaluationError::ArityError { min: 1, max: 1, got: v.len() }),
        })
    }

    pub fn pow(_v: &[Value]) -> Result<Value, EvaluationError> {
        todo!()
    }

    pub fn exp(_v: &[Value]) -> Result<Value, EvaluationError> {
        todo!()
    }

    pub fn log(_v: &[Value]) -> Result<Value, EvaluationError> {
        todo!()
    }

    pub fn log2(_v: &[Value]) -> Result<Value, EvaluationError> {
        todo!()
    }
}

pub static DEFAULT_LOOKUP_CONTEXT: Lazy<LookupContext> = Lazy::new(|| {
    let functions = HashMap::from([
        ("add".to_string(), Arc::new(globals::add) as OperatorT),
        ("sum".to_string(), Arc::new(globals::add)),
        ("sub".to_string(), Arc::new(globals::sub)),
        ("mul".to_string(), Arc::new(globals::mul)),
        ("div".to_string(), Arc::new(globals::div)),
        ("floor_div".to_string(), Arc::new(globals::floor_div)),
        ("negate".to_string(), Arc::new(globals::negate)),
        ("pow".to_string(), Arc::new(globals::pow)),
        ("exp".to_string(), Arc::new(globals::exp)),
        ("log".to_string(), Arc::new(globals::log)),
        ("log2".to_string(), Arc::new(globals::log2)),
    ]);
    let constants = HashMap::from([("pi".to_string(), Value::Float(3.14159265358979))]);

    LookupContext {
        functions,
        constants,
    }
});
// Functions
pub fn evaluate(
    tree: ExpressionTree,
    lookup_context: &LookupContext,
) -> Result<Value, EvaluationError> {
    match tree {
        ExpressionTree::Value(v) => Ok(v),
        ExpressionTree::Constant(name) => lookup_context
            .constants
            .get(&name)
            .ok_or(EvaluationError::NoSuchConstant(name))
            .copied(),
        ExpressionTree::Apply(f, args) => f.resolve(lookup_context)?(
            &args
                .into_iter()
                .map(|x| evaluate(x, lookup_context))
                .collect::<Result<Vec<_>, _>>()?,
        ),
    }
}

pub fn parse_python(expr: &str) -> ExpressionTree {
    python::parser::parse(python::lexer::lex(expr).unwrap())
}
