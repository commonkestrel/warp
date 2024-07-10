use std::collections::HashMap;

use async_std::path::PathBuf;
use slotmap::SlotMap;

use crate::{
    build::{
        ascii::AsciiStr,
        frontend::hir::{
            weak::unresolved::{self, ItemId, UnresolvedDb},
            Type, Visible,
        },
        syntax::{
            ast::{BinaryOp, Mutability, Path, UnaryOp},
            parse::Visibility,
            token::{Ident, LitString},
        },
    },
    diagnostic::Reporter,
    error,
    span::Spanned,
    spanned_error,
};

#[derive(Debug, Clone)]
pub struct Typed<T: Clone> {
    inner: T,
    ty: MaybeType,
}

pub struct Database {
    items: HashMap<Ident, ItemId>,
}

impl Database {
    pub async fn resolve(
        self_id: ItemId,
        root: ItemId,
        superspace: Option<ItemId>,
        libs: &HashMap<PathBuf, ItemId>,
        items: &mut SlotMap<ItemId, unresolved::Item>,
        resolved_items: &mut &SlotMap<ItemId, Item>,
        reporter: &Reporter,
    ) -> Option<Database> {
        let item_ptr = items as *mut SlotMap<ItemId, unresolved::Item>;
        let udb = match items.get_mut(self_id) {
            Some(item) => match item {
                unresolved::Item::Subspace(udb) | unresolved::Item::Library(udb) => udb,
                _ => {
                    reporter
                        .report(
                            error!("attempted to resolve an item that is not a space or package")
                                .as_bug(),
                        )
                        .await;
                    return None;
                }
            },
            None => {
                reporter
                    .report(error!("no item found for referenced space").as_bug())
                    .await;
                return None;
            }
        };

        // Resolve imports and add them to the subspace's items
        for (ident, (ident_span, path)) in udb.imports.iter() {
            match unresolved::resolve_path(
                &udb.items,
                &udb.libs,
                path.inner().clone(),
                root,
                superspace,
                // SAFETY: We can guarentee that `udb` will never be modified in a way that affects `resolve_path`
                unsafe { &*item_ptr },
                libs,
                reporter,
            )
            .await
            {
                Some(item) => {
                    if let Some(_) = udb.items.insert(
                        *ident,
                        // Since imports cannot be public, it's ok to just add them with a Private visibility
                        Visible::new(ident_span.clone(), Visibility::Private, item),
                    ) {
                        reporter
                            .report(spanned_error!(ident_span.clone(), "duplicate identifier"))
                            .await;
                    }
                }
                None => continue,
            }
        }

        for id in items.iter().filter_map(|item| match item.1 {
            unresolved::Item::Library(_) => Some(item.0),
            _ => None,
        }) {
            if let Some(db) = Box::pin(Database::resolve(
                id,
                id,
                None,
                libs,
                // SAFETY: We can guarentee that `items` will never be modified in a way that affects `iter`
                unsafe { &mut *item_ptr },
                resolved_items,
                reporter,
            )).await {

            }
        }
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum Item {

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
pub enum Statement {
    Expr(Expr),
    Block(Vec<Spanned<Statement>>),
    If {
        condition: Spanned<Expr>,
        content: Box<Spanned<Statement>>,
        else_block: Option<Box<Spanned<Statement>>>,
    },
    For(Box<ForLoop>),
    While(Box<WhileLoop>),
    Break,
    Continue,
    Return(Option<Spanned<Expr>>),
    Asm(LitString),
    Var {
        mutability: Mutability,
        ident: Spanned<Ident>,
        ty: MaybeType,
        assignment: Spanned<Expr>,
    },
}

#[derive(Debug, Clone)]
pub struct ForLoop {
    init: Spanned<Statement>,
    check: Spanned<Expr>,
    post: Spanned<Statement>,
    content: Spanned<Statement>,
}

#[derive(Debug, Clone)]
pub struct WhileLoop {
    check: Spanned<Expr>,
    contents: Spanned<Statement>,
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
