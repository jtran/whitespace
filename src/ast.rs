use std::cell::Cell;

use fnv::FnvHashMap;
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use serde::{self, ser::SerializeMap};
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use wasm_bindgen::prelude::wasm_bindgen;

use crate::environment::{SlotIndex, VarLoc};
use crate::source_loc::*;

#[derive(Debug)]
pub struct ResolvedCode {
    pub statements: Vec<Stmt>,
}

#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    serde(tag = "type", content = "args")
)]
#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Break(SourceLoc),
    Class(ClassDefinition),
    Continue(SourceLoc),
    Expression(Expr),
    Fun(NamedFunctionDefinition),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    Return(Expr, SourceLoc),
    Var(String, Cell<SlotIndex>, Expr, SourceLoc),
    While(Expr, Box<Stmt>),
    WhileIncrement(Expr, Box<Stmt>, Box<Expr>),
}

#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    serde(tag = "type", content = "args")
)]
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign(String, Cell<VarLoc>, Box<Expr>, SourceLoc),
    Call(Box<Expr>, Vec<Expr>, SourceLoc),
    Binary(Box<Expr>, BinaryOperator, Box<Expr>, SourceLoc),
    Function(Box<FunctionDefinition>),
    Get(Box<Expr>, String, SourceLoc),
    GetIndex(Box<Expr>, Box<Expr>, SourceLoc),
    Grouping(Box<Expr>),
    LiteralArray(Vec<Expr>),
    LiteralBool(bool),
    LiteralMap(Map<Expr>),
    LiteralNumber(f64),
    LiteralNil,
    LiteralString(String),
    Logical(Box<Expr>, LogicalOperator, Box<Expr>),
    Set(Box<Expr>, String, Box<Expr>, SourceLoc),
    SetIndex(Box<Expr>, Box<Expr>, Box<Expr>, SourceLoc),
    Super(Cell<VarLoc>, String, SourceLoc),
    Variable(String, Cell<VarLoc>, SourceLoc),
    Unary(UnaryOperator, Box<Expr>, SourceLoc),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Map<V> {
    map: FnvHashMap<String, V>,
}

#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[derive(Clone, Debug, PartialEq)]
pub struct ClassDefinition {
    pub name: String,
    pub superclass: Option<Box<Expr>>,
    pub methods: Vec<NamedFunctionDefinition>,
    pub source_loc: SourceLoc,
}

#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[derive(Clone, Debug, PartialEq)]
pub struct NamedFunctionDefinition {
    pub name: String,
    pub fun_def: FunctionDefinition,
}

#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    pub parameters: Vec<Parameter>,
    pub body: Vec<Stmt>,
    pub fun_type: FunctionType,
    pub source_loc: SourceLoc,
}

#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub source_loc: SourceLoc,
}

#[cfg_attr(all(target_arch = "wasm32", target_os = "unknown"), wasm_bindgen)]
#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FunctionType {
    PlainFunction,
    Method,
    Initializer,
    ClassMethod,
}

#[cfg_attr(all(target_arch = "wasm32", target_os = "unknown"), wasm_bindgen)]
#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Not,
}

#[cfg_attr(all(target_arch = "wasm32", target_os = "unknown"), wasm_bindgen)]
#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,

    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[cfg_attr(all(target_arch = "wasm32", target_os = "unknown"), wasm_bindgen)]
#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
}

impl ResolvedCode {
    pub fn new(statements: Vec<Stmt>) -> ResolvedCode {
        ResolvedCode { statements }
    }
}

impl AsRef<Stmt> for Stmt {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl ClassDefinition {
    pub fn has_superclass(&self) -> bool {
        self.superclass.is_some()
    }
}

impl NamedFunctionDefinition {
    pub fn new(
        name: String,
        fun_def: FunctionDefinition,
    ) -> NamedFunctionDefinition {
        NamedFunctionDefinition { name, fun_def }
    }
}

impl FunctionDefinition {
    pub fn new(
        parameters: Vec<Parameter>,
        body: Vec<Stmt>,
        fun_type: FunctionType,
        source_loc: SourceLoc,
    ) -> FunctionDefinition {
        FunctionDefinition {
            parameters,
            body,
            fun_type,
            source_loc,
        }
    }
}

impl Parameter {
    pub fn new(name: String, source_loc: SourceLoc) -> Parameter {
        Parameter { name, source_loc }
    }
}

impl<V> Map<V> {
    pub fn new(map: FnvHashMap<String, V>) -> Self {
        Map { map }
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, String, V> {
        self.map.iter()
    }

    pub fn iter_mut(
        &mut self,
    ) -> std::collections::hash_map::IterMut<'_, String, V> {
        self.map.iter_mut()
    }

    pub fn insert(&mut self, key: String, value: V) -> Option<V> {
        self.map.insert(key, value)
    }

    pub fn entry(
        &mut self,
        key: String,
    ) -> std::collections::hash_map::Entry<'_, String, V> {
        self.map.entry(key)
    }
}

impl<V> Default for Map<V> {
    fn default() -> Self {
        Self {
            map: Default::default(),
        }
    }
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
impl<V> serde::Serialize for Map<V>
where
    V: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.map.len()))?;
        for (k, v) in &self.map {
            map.serialize_entry(k, v)?;
        }
        map.end()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;

    #[test]
    fn test_size_of_stmt() {
        assert_eq!(mem::size_of::<Stmt>(), 96);
    }

    #[test]
    fn test_size_of_class_definition() {
        assert_eq!(mem::size_of::<ClassDefinition>(), 64);
    }

    #[test]
    fn test_size_of_function_definition() {
        assert_eq!(mem::size_of::<FunctionDefinition>(), 64);
    }

    #[test]
    fn test_size_of_expr() {
        assert_eq!(mem::size_of::<Expr>(), 56);
    }
}
