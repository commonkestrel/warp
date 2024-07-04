#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    symbols: Vec<String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
        }
    }

    pub fn find_or_insert(&mut self, symbol: &str) -> SymbolRef {
        match self.find(symbol) {
            Some(pos) => pos,
            None => {
                self.symbols.push(symbol.to_owned());
                let pos = self.symbols.len() - 1;

                SymbolRef::new(pos)
            }
        }
    }

    pub fn find(&self, symbol: &str) -> Option<SymbolRef> {
        self.symbols
            .iter()
            .position(|s| s == symbol)
            .map(SymbolRef::new)
    }

    pub fn get<'a>(&'a self, id: SymbolRef) -> Option<&'a str> {
        self.symbols.get(id.inner()).map(|s| s.as_str())
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
