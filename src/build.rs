mod frontend {
    pub mod uncaught_error;
}

mod syntax {
    pub mod ast;
    pub mod lex;
    pub mod parse;
    pub mod token;
}

mod ascii;
mod symbol_table;
