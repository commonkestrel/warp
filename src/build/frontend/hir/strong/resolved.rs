use std::collections::HashMap;

use async_std::path::PathBuf;
use slotmap::SlotMap;

use crate::{
    build::{
        ascii::AsciiStr,
        frontend::hir::{weak::unresolved::{self, Item, ItemId, UnresolvedDb}, Type},
        syntax::ast::{BinaryOp, Path, UnaryOp},
    },
    span::Spanned,
};

#[derive(Debug, Clone)]
pub struct Typed<T: Clone> {
    inner: T,
    ty: MaybeType,
}

pub struct Database {
    items: SlotMap<ItemId, Item>
}

impl Database {
    pub fn resolve_package(unresolved: UnresolvedDb, libs: HashMap<PathBuf, UnresolvedDb>, items: SlotMap<ItemId, unresolved::Item>) -> Option<Database> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum MaybeType {
    Explicit(Spanned<Type>),
    Inferred(Type),
    WeakInteger,
    WeakPointer(Box<MaybeType>),
    WeakArray(Box<MaybeType>),
    WeakTuple(Vec<MaybeType>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Void,
    Immediate(i128),
    Boolean(bool),
    Str(AsciiStr),
    Reference(Path),
    Call(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    Tuple(Vec<Spanned<Expr>>),
    Array(Vec<Spanned<Expr>>),
    BinaryOp(Box<BinOp>),
    UnaryOp(Spanned<UnaryOp>, Box<Spanned<Expr>>),
    As(Box<Spanned<Expr>>, Spanned<Type>),
    Sizeof(Spanned<Type>),
}

#[derive(Debug, Clone)]
pub struct BinOp {
    lhs: Typed<Expr>,
    op: BinaryOp,
    rhs: Typed<Expr>,
}
