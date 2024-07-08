use std::sync::Arc;

use async_std::sync::RwLock;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: Arc<RwLock<Vec<String>>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: Arc::new(RwLock::new(Vec::new())),
        }
    }

    pub async fn find_or_insert(&self, symbol: &str) -> SymbolRef {
        match self.find(symbol).await {
            Some(pos) => pos,
            None => {
                let mut symbols = self.symbols.write().await;

                symbols.push(symbol.to_owned());
                let pos = symbols.len() - 1;

                SymbolRef::new(pos)
            }
        }
    }

    pub async fn find(&self, symbol: &str) -> Option<SymbolRef> {
        let symbols = self.symbols.read().await;

        symbols.iter().position(|s| s == symbol).map(SymbolRef::new)
    }

    pub async fn get(&self, id: SymbolRef) -> String {
        let symbols = self.symbols.read().await;
        symbols[id.inner()].clone()
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct SymbolRef(usize);

impl SymbolRef {
    fn new(id: usize) -> Self {
        SymbolRef(id)
    }

    pub fn inner(&self) -> usize {
        self.0
    }
}
