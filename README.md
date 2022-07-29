# MPL - Mathematical Proof Language
OK, I made another language 

MPL is an interactive theorem prover. It allows for expressing mathematical assertions, mechanically checks proofs of these assertions, helps find formal proofs, and extracts a certified program from the constructive proof of its formal specification. It implements a dependently typed functional programming language. Although only algebraic manipulation has been implemented so far, it works within the theory of the calculus of inductive constructions, a derivative of the calculus of constructions. 

#Currently:
```
<expression> ::= <operator-0>
<operator-0> ::= <operator-1> ((`+` | `-`) <operator-0>)*
<operator-1> ::= <operator-2> ((`*` | `/`) <operator-1>)*
<operator-2> ::= <primary> (`^` <operator-2>)*
<primary> ::= (`(` <expression> `)`) | <application-chain> | <symbol> | <variable>
<application-chain> ::= (<symbol> | <variable>) (<fun-args>)+
<symbol> ::= [a-z0-9][_a-zA-Z0-9]*
<variable> ::= [_A-Z][_a-zA-Z0-9]*
<fun-args> ::= `(` (<expression>),* `)`
```
