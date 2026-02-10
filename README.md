# Dawn
 
A toy compiler for a multi-paradigm programming language with C-style syntax, written in OCaml entirely from scratch - no LLVM, no external codegen libraries.

## Architecture

The compiler implements a full pipeline from source to native x86-64:

**Frontend:**
- Lexing and parsing via ocamllex + menhir
- High-level AST representation

**Middle-end (Sea of Nodes IR):**
- Lowering to a Sea of Nodes style intermediate representation
- SSA form with merged data flow and control flow graphs
- Data nodes "float" within the CFG and are later scheduled into a concrete order based on dependencies

**Backend:**
- Global instruction scheduling 
- Graph-coloring register allocation (Chaitin-Briggs style)
- Native x86-64 assembly generation

## Current Status

Early stage development. The language design is actively evolving and not finalized. 

## Future Plans For The Language

Things I want to implement in the future:
- [ ] Algebraic types with possibility of recursive types
- [ ] Pattern matching
- [ ] Higher-order functions 
- [ ] Rust style traits

and more to be seen...
