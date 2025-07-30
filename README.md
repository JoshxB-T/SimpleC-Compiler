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
<simpleC>      -> void main () { <stmts> } $

<stmts>        -> <stmt> <morestmts>
<morestmts>    -> <stmt> <morestmts>
                | EMPTY

<stmt>         -> <empty>
                | <vardecl>
                | <input>
                | <output>
                | <assignment>
                | ifstmt

<empty>        -> ;
<vardecl>      -> int identifier;
                | int identifier = int_literal;
<intput>       -> cin >> identifier;
<output>       -> cout << <output-value>;
<output-value> -> <expr-value>
                | endl

<assignment>   -> identifier = <expr> ;
<ifstmt>       -> if ( <condition> ) <then-part> <else-part>
<condition>    -> <expr>
<then-part>    -> <stmt>
<else-part>    -> else <stmt>
                | EMPTY

<expr>         -> <expr-value> <expr-op> <expr-value>
                | <expr-value>

<expr-value>   -> identifier
                | int_literal
                | str_literal
                | true
                | false
                
<expr-op>      -> +
                | -
                | *
                | /
                | ^
                | <
                | <=
                | >
                | >=
                | ==
                | !=
```
