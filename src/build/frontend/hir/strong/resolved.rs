use std::collections::HashMap;

use async_std::path::PathBuf;
use slotmap::SlotMap;

use crate::{
    build::{
        ascii::AsciiStr,
        frontend::hir::{
            weak::unresolved::{self, Item, ItemId, UnresolvedDb},
            Type, Visible,
        },
        syntax::{
            ast::{BinaryOp, Path, UnaryOp},
            parse::Visibility,
            token::Ident,
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
        let udb = match items.get_mut(self_id) {
            Some(item) => match item {
                Item::Subspace(udb) | Item::Library(udb) => udb,
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

        for (ident, (ident_span, path)) in udb.imports.iter() {
            match unresolved::resolve_path(
                &udb.items,
                &udb.libs,
                path.inner().clone(),
                root,
                superspace,
                resolved_items,
                libs,
                reporter,
            )
            .await
            {
                Some(item) => {
                    if let Some(_) = udb.items.insert(
                        *ident,
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
