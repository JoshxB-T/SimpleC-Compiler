# SimpleC-Compiler
## Compiler Features
- [x] Lexical Analysis
- [x] Syntax Analysis
- [x] Semantic Analysis
- [ ] Intermediate Code Generation
- [ ] Code Optimizer
- [ ] Target Code Generation

## BNF ("Backus-Naur Form") Definition of the SimpleC Syntax
```plaintext
<simpleC>   -> void main () { <stmts> } $

<stmts>     -> <stmt> <morestmts>
<morestmts> -> <stmt> <morestmts>
             | EMPTY

<stmt>      -> <empty>
             | <vardecl>
             | <input>
             | <output>
             | <assignment>
             | ifstmt

<empty>     -> ;
```
