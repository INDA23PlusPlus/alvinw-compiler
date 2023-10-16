# alvinw-compiler
## Language Specification
Minimum goal:
```
let first = 0;
let second = 1;
loop {
    let n = first + second;
    set first = second;
    set second = n;
    print(n);
}
```
+ the other mandatory stuff.

If I have time I would like to do more things!

## BNF
```
<block> ::= E | <statement> | <statement> <block>
<statement_or_expr> ::= <statement> | <expr>

<expr> ::= <term> | <expr> "+" <term> | <expr> "-" <term>
<term> ::= <factor> | <term> "*" <factor> | <term> "/" <factor>
<factor> ::= <integer> | "(" <expr> ")"
<integer> ::= <digit> | <integer> <digit>
<digit> ::= [0-9]

<identifier> ::= <letter> | <identifier> <letter>
<letter> ::= [a-z]

<statement> ::= <variable_decl> | <if_statement> | <loop>
<variable_decl> ::= "let " <identifier> "=" <expr> ";"
<if_statement> ::= "if (" <expr> ") {" <block> "}"
<loop> ::= "loop {" <block> "}"

```