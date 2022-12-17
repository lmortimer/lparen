module LParen.Interpreter.Library.Quote

open LParen.Interpreter.Common

// Execute the special form quote.
// eg.
// >> a
// Error: could not locate symbol a
// >> (quote a)
// a
let quote (atom: Atom) = atom