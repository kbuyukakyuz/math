use std::collections::HashMap;
use std::fmt;
mod lexer;
use lexer::*;
#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Sym(String),    
    Var(String),
    Fun(Box<Expr>, Vec<Expr>),
}
enum Error{
    UnexpectedToken(TokenKind, Token),
    RuleAlreadyExists(String, Loc, Loc),
    RuleDoesNotExist(String, Loc),
    AlreadyShaping(Loc),
    NoShapingInPlace(Loc),
}
impl Expr {
    fn parse_peekable(lexer: &mut std::iter::Peekable<impl Iterator<Item = Token>>) -> Self {
        if let Some(name) = lexer.next() {
            match name.kind {
                TokenKind::Sym => {
                    if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::OpenParen) {
                        let mut args = Vec::new();
                        if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::CloseParen) {
                            return Expr::Fun(Box::new(Expr::Sym(name.text)), args);
                        }
                        args.push(Self::parse_peekable(lexer));
                        while let Some(_) = lexer.next_if(|t| t.kind == TokenKind::Comma) {
                            args.push(Self::parse_peekable(lexer))
                        }
                        if lexer.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                            todo!();
                        }
                        Expr::Fun(Box::new(name.text), args)
                    } else {
                        Expr::Sym(name.text)
                    }
                }
                _ => todo!(),
            }
        } else {
            panic!("Expected expression");
        }
    }

    fn parse(lexer: &mut Peekable<impl Iterator <Item = Token>>) -> Result<Self, Error>{
        use TokenKind::*;
        let name = lexer.next().expect("Exhausted");
        match name.kind(){
            Sym => {
                if let Some(_) = lexer.next_if(|t| t.kind == OpenParen){
                    let mut args = Vec::new();
                    if let Some(_) = lexer.next_if(|t| t.kind == CloseParen){
                        return Ok(Expr::Fun(Box::new(if name.text.chars().next()
                    .filter(|x| x.is_uppercase().is_some()))))
                    }else{
                        Expr::Sym(name.text)
                    }
                }
            }
        }
    }
}
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Sym(name) | Expr::Var(name) => write!(f, "{}", name),
            Expr::Fun(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            },
        }
    }
}
#[derive(Debug)]
struct Rule {
    head: Expr,
    body: Expr,
}
fn subs_bindings(bindings: &Bindings, expr: &Expr) -> Expr {
    use Expr::*;
    match expr {
        Var(name) => {
            if let Some(value) = bindings.get(name) {
                value.clone()
            } else {
                expr.clone()
            }
        }
        Fun(head, args) => {
            let new_head = subs_bindings(bindings, head);
            let mut new_args = Vec::new();
            for arg in args {
                new_args.push(subs_bindings(bindings, &arg))
            }
            Fun(Box::new(new_head), new_args)
        }
    }
}
impl Rule {
    fn apply_all(&self, expr: Expr) -> Expr {
        use Expr::*;
        if let Some(bindings) = pattern_match(&self.head, &expr) {
            subs_bindings(&bindings, &self.body)
        } else {
            match expr {
                Sym(_) | Var(_)=> expr.clone(),
                Fun(name, args) => {
                    let mut new_args = Vec::new();
                    for arg in args {
                        new_args.push(self.apply_all(arg))
                    }
                    Fun(name.clone(), new_args)
                }
            }
        }
    }
}
impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {}", self.head, self.body)
    }
}
type Bindings = HashMap<String, Expr>;
fn pattern_match(pattern: &Expr, value: &Expr) -> Option<Bindings> {
    let mut bindings = HashMap::new();
    fn pattern_match_impl(pattern: &Expr, value: &Expr, bindings: &mut Bindings) -> bool {
        use Expr::*;
        match (pattern, value) {
            (Sym(name), _) => {
                if let Some(bound_value) = bindings.get(name) {
                    bound_value == value
                } else {
                    bindings.insert(name.clone(), value.clone());
                    true
                }
            }
            (Fun(name1, args1), Fun(name2, args2)) => {
                if name1 == name2 && args1.len() == args2.len() {
                    for i in 0..args1.len() {
                        if !pattern_match_impl(&args1[i], &args2[i], bindings) {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }
    if pattern_match_impl(pattern, value, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}
macro_rules! sym {
    ($name: ident) => {
        Expr::Sym(stringify!($name).to_string())
    };
}
macro_rules! fun{
    ($name:ident) => {
        Expr::Fun(stringify!($name).to_string(), vec![])
    };
    ($name:ident, $($args:expr), *) =>{
        Expr::Fun(stringify!($name).to_string(), vec![$($args),*])
    }
}
macro_rules! expr{
    ($name: ident) =>{
        Expr::Sym(stringify!($name).to_string())
    };
    ($name: ident($($args:tt)*)) =>{
        Expr::Fun(Box::new(stringify!($name).to_string()), fun_args![$($args)*])
    };

}
macro_rules! fun_args{
    () => { vec![] };
    ($name: ident) => {vec![expr!($name)]};
    ($name:ident, $($rest: tt)*) => {
        {
        let mut t = vec![expr!($name)];
        t.append(&mut fun_args!($($rest)*));
        t
        }
    };
    ($name: ident($($args:tt)*)) => {
        vec![expr!($name($($args)*))]
    };
    ($name: ident($($args:tt)*), $($rest: tt)*) => {
        {
        let mut t = vec![expr!($name($($args)*))];
        t.append(&mut fun_args!($($rest)*));
        t
    }
    };
}
#[derive(Debug, PartialEq)]
enum TokenKind {
    Sym,
    OpenParen,
    CloseParen,
    Comma,
    Equals,
}
#[derive(Debug, PartialEq)]
struct Token {
    kind: TokenKind,
    text: String,
}
#[derive(Debug)]
struct Lexer<Chars: Iterator<Item = char>> {
    chars: std::iter::Peekable<Chars>,
}
impl<Chars: Iterator<Item = char>> Lexer<Chars> {
    fn from_iter(chars: Chars) -> Self {
        Self {
            chars: chars.peekable(),
        }
    }
}
impl<Chars: Iterator<Item = char>> Iterator for Lexer<Chars> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        while let Some(_) = self.chars.next_if(|x| x.is_whitespace()) {}
        if let Some(x) = self.chars.next() {
            let mut text = String::new();
            text.push(x);
            match x {
                '(' => Some(Token {
                    kind: TokenKind::OpenParen,
                    text,
                }),
                ' ' => None,
                ')' => Some(Token {
                    kind: TokenKind::CloseParen,
                    text,
                }),
                ',' => Some(Token {
                    kind: TokenKind::Comma,
                    text,
                }),
                '=' => Some(Token {
                    kind: TokenKind::Equals,
                    text,
                }),
                _ => {
                    if !x.is_alphanumeric() {
                        panic!("Unexpected token");
                    }
                    while let Some(x) = self.chars.next_if(|x| x.is_alphanumeric()) {
                        text.push(x)
                    }
                    Some(Token {
                        kind: TokenKind::Sym,
                        text,
                    })
                }
            }
        } else {
            None
        }
    }
}

fn main() {
    let swap = Rule {
        head: expr!(swap(pair(a, b))),
        body: expr!(pair(b, a)),
    };
}