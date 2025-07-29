# SimpleC-Compiler
## BNF ("Backus-Naur Form") Definition of the SimpleC Syntax
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
