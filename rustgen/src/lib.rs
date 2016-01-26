// Some machinery for emitting Rust code
extern crate itertools;

use std::boxed::Box;
use std::io;
use std::fmt::{self, Display};
use std::iter::FromIterator;
use std::convert::From;
use std::ops::{Deref, Add};

use itertools::Itertools;

fn from<FT: From<T>, T>(t: T) -> FT { From::from(t) }

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Tokens(Vec<Token>);

impl Tokens {
    pub fn nil() -> Tokens { Tokens(vec![]) }
    pub fn new(v: Vec<Token>) -> Tokens { Tokens(v) }
    pub fn iter(&self) -> std::slice::Iter<Token> { self.0.iter() }
}

impl Tokenize for Tokens {
    fn tokens(&self) -> Tokens { self.clone() }
}

impl<'a> Tokenize for &'a Tokens {
    fn tokens(&self) -> Tokens { (**self).clone() }
}

impl<'a, T: Tokenize> From<&'a T> for Tokens {
    fn from(t: &'a T) -> Self { t.tokens() }
}

impl From<Vec<Tokens>> for Tokens {
    fn from(v: Vec<Tokens>) -> Tokens { v.into_iter().flatten().collect() }
}

impl<T: Tokenize> FromIterator<T> for Tokens {
    fn from_iter<IT>(iterator: IT) -> Self
        where IT: IntoIterator<Item=T>
    {
        Tokens(iterator.into_iter().flat_map(|t| t.tokens()).collect())
    }
}

impl IntoIterator for Tokens {
    type Item = Token;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter { self.0.into_iter() }
}

impl Deref for Tokens {
    type Target = Vec<Token>;
    fn deref(&self) -> &Self::Target { &self.0 }
}

impl<'a, T: Tokenize> Add<&'a T> for Tokens {
    type Output = Tokens;
    fn add(self, rhs: &'a T) -> Self::Output { from(vec![self, rhs.tokens()]) }
}

pub trait Tokenize {
    fn tokens(&self) -> Tokens;
}

// XXX would be nice to define this generically for all iterators
impl<T: Tokenize> Tokenize for Vec<T> {
    fn tokens(&self) -> Tokens {
        Tokens(self.iter().flat_map(|t| t.tokens()).collect())
    }
}

impl<T: Tokenize> Tokenize for Box<T> {
    fn tokens(&self) -> Tokens { (**self).tokens() }
}

impl<T: Tokenize> Tokenize for Option<T> {
    fn tokens(&self) -> Tokens {
        self.as_ref().map_or(Tokens::nil(), |t| t.tokens())
    }
}

fn option<P, O>(pfx: &P, opt: &Option<O>) -> Tokens
    where P: Tokenize, O: Tokenize
{
    opt.as_ref().map_or(Tokens::nil(), |t| Tokens::from(pfx) + t)
}

pub fn to_string<T: Tokenize>(toks: T) -> String {
    use std::fmt::Write;

    let mut s = String::new();
    for t in toks.tokens() {
        write!(&mut s, "{}", t).unwrap()
    }
    s
}

fn flatten<IIT, IT, T>(iit: IIT) -> IT
    where IIT: IntoIterator<Item=IT>, IT: IntoIterator<Item=T> + FromIterator<T>
{
    iit.into_iter().flatten().collect()
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum UnOp {
    Negate,                     // '-'
    Deref,                      // '*'
    Not,                        // '!'
}

impl Display for UnOp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use UnOp::*;

        let s = match *self {
            Negate => "-",
            Deref => "*",
            Not => "!",
        };
        write!(fmt, "{} ", s)
    }
}

impl Tokenize for UnOp {
    fn tokens(&self) -> Tokens { from(&Token::UnOp(*self)) }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum BinOp {
    // arith_op
    Add,                        // '+'
    Sub,                        // '-'
    Mult,                       // '*'
    Div,                        // '/'
    Mod,                        // '%'

    // bitwise_op
    And,                        // '&'
    Or,                         // '|'
    Xor,                        // '^'
    Lshift,                     // '<<'
    Rshift,                     // '>>'

    // lazy_bool_op
    Andand,                     // '&&'
    Oror,                       // '||'

    // comp_op
    Eq,                         // '=='
    Ne,                         // '!='
    Lt,                         // '<'
    Gt,                         // '>'
    Le,                         // '<='
    Ge,                         // '>='
}

impl Display for BinOp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use BinOp::*;

        let s = match *self {
             Add => "+",
             Sub => "-",
             Mult => "*",
             Div => "/",
             Mod => "%",

             And => "&",
             Or => "|",
             Xor => "^",
             Lshift => "<<",
             Rshift => ">>",

             Andand => "&&",
             Oror => "||",

             Eq => "==",
             Ne => "!=",
             Lt => "<",
             Gt => ">",
             Le => "<=",
             Ge => ">=",
        };
        write!(fmt, "{} ", s)
    }
}

impl Tokenize for BinOp {
    fn tokens(&self) -> Tokens { from(&Token::BinOp(*self)) }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Symbol {
    Colon,                      // ':'
    Dot,                        // '.'
    Dotdot,                     // '..'
    Dotdotdot,                  // '...'
    Doublecolon,                // '::'
    Arrow,                      // '->'
    DoubleArrow,                // '=>'
    Hash,                       // '#'
    Lbrack,                     // '['
    Rbrack,                     // ']'
    Lparen,                     // '('
    Rparen,                     // ')'
    Lbrace,                     // '{'
    Rbrace,                     // '}'
    Ltbrack,                    // '<'
    Gtbrack,                    // '>'
    Comma,                      // ','
    Semi,                       // ';'
    Wildcard,                   // '_'
    At,                         // '@'
    Bar,                        // '|'
    Ref,                        // '&'
    Plus,                       // '+'

    Assign,                     // '='
    AssignOp(BinOp),            // #'='
}

impl Display for Symbol {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use Symbol::*;
        use BinOp::*;

        let s = match *self {
            Colon => ":",
            Dot => ".",
            Dotdot => "..",
            Dotdotdot => "...",
            Doublecolon => "::",
            Arrow => "->",
            DoubleArrow => "=>",
            Hash => "#",
            Lbrack => "[",
            Rbrack => "]",
            Lparen => "(",
            Rparen => ")",
            Lbrace => "{",
            Rbrace => "}",
            Ltbrack => "<",
            Gtbrack => ">",
            Comma => ",",
            Semi => ";",
            Assign => "=",
            Wildcard => "_",
            At => "@",
            Bar => "|",
            Ref => "&",
            Plus => "+",

            AssignOp(Add) => "+=",
            AssignOp(Sub) => "-=",
            AssignOp(Mult) => "*=",
            AssignOp(Div) => "/=",
            AssignOp(Mod) => "%=",

            AssignOp(And) => "&=",
            AssignOp(Or) => "|=",
            AssignOp(Xor) => "^=",

            AssignOp(op) => panic!("bad assignment op {:?}", op),
        };
        write!(fmt, "{} ", s)
    }
}

impl Tokenize for Symbol {
    fn tokens(&self) -> Tokens { from(&Token::Symbol(*self)) }
}

// Subset of used keywords
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Keyword {
    Type,                       // "type"
    Struct,                     // "struct"
    Enum,                       // "enum"
    Pub,                        // "pub"
    Mod,                        // "mod"
    Fn,                         // "fn"
    As,                         // "as"
    Let,                        // "let"
    If,                         // "if"
    Else,                       // "else"
    For,                        // "for"
    In,                         // "in"
    Match,                      // "match"
    Loop,                       // "loop"
    Break,                      // "break"
    Continue,                   // "continue"
    Mut,                        // "mut"
    Ref,                        // "ref"
    Move,                       // "move"

    U64,                        // "u64"
    I64,                        // "i64"
    U32,                        // "u32"
    I32,                        // "i32"
    U16,                        // "u16"
    I16,                        // "i16"
    U8,                         // "u8"
    I8,                         // "i8"

    F32,                        // "f32"
    F64,                        // "f64"
}

impl Display for Keyword {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use Keyword::*;

        let s = match *self {
            Type => "type",
            Struct => "struct",
            Enum => "enum",
            Pub => "pub",
            Mod => "mod",
            Fn => "fn",
            As => "as",
            Let => "let",
            For => "for",
            In => "in",
            If => "if",
            Else => "else",
            Loop => "loop",
            Match => "match",
            Break => "break",
            Continue => "continue",
            Mut => "mut",
            Ref => "ref",
            Move => "move",

            // Primitive types
            U64 => "u64",
            I64 => "i64",
            U32 => "u32",
            I32 => "i32",
            U16 => "u16",
            I16 => "i16",
            U8 => "u8",
            I8 => "i8",
            F32 => "f32",
            F64 => "f64",
        };
        write!(fmt, "{} ", s)
    }
}

impl Tokenize for Keyword {
    fn tokens(&self) -> Tokens { from(&Token::Keyword(*self)) }
}

// Literal values
#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Literal {
    Bool(bool),
    Enum(String),
    String(String),
    I64(i64),
    U64(u64),
    I32(i32),
    U32(u32),
    I16(i16),
    U16(u16),
    I8(i8),
    U8(u8),
    Unit,
}

impl Display for Literal {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use Literal::*;

        match self {
            &Bool(true) => write!(fmt, "true "),
            &Bool(false) => write!(fmt, "false "),

            &String(ref s) => write!(fmt, "\"{}\" ", s), // XXX quoting
            &Enum(ref s) => write!(fmt, "{} ", s),

            &I64(n) => write!(fmt, "{}_i64 ", n),
            &U64(n) => write!(fmt, "{}_u64 ", n),

            &I32(n) => write!(fmt, "{}_i32 ", n),
            &U32(n) => write!(fmt, "{}_u32 ", n),

            &I16(n) => write!(fmt, "{}_i16 ", n),
            &U16(n) => write!(fmt, "{}_u16 ", n),

            &I8(n) => write!(fmt, "{}_i8 ", n),
            &U8(n) => write!(fmt, "{}_u8 ", n),

            &Unit => write!(fmt, "() "),
        }
    }
}

impl Tokenize for Literal {
    fn tokens(&self) -> Tokens { from(&Token::Literal(self.clone())) }
}

// Identifiers
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Ident(String);

impl Ident {
    fn new(s: &str) -> Ident { Ident(s.to_string()) }
}

impl<'a> From<&'a str> for Ident {
    fn from(s: &str) -> Ident { Ident::new(s) }
}

impl Tokenize for Ident {
    fn tokens(&self) -> Tokens { from(&Token::Ident(self.clone())) }
}

impl Display for Ident {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{} ", self.0)
    }
}

// Raw token
#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Token {
    Keyword(Keyword),
    UnOp(UnOp),
    BinOp(BinOp),
    Ident(Ident),
    Literal(Literal),
    Symbol(Symbol),
}

impl Display for Token {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;

        match self {
            &Keyword(ref kw) => kw.fmt(fmt),
            &UnOp(ref op) => op.fmt(fmt),
            &BinOp(ref op) => op.fmt(fmt),
            &Ident(ref id) => id.fmt(fmt),
            &Literal(ref lit) => lit.fmt(fmt),
            &Symbol(ref sym) => sym.fmt(fmt),
        }
    }
}

pub fn tokenstr<W: io::Write>(w: &mut W, toks: Vec<Token>) -> io::Result<()> {
    for t in toks {
        let _ = try!(write!(w, "{}", t));
    }
    Ok(())
}    

impl Tokenize for Token {
    fn tokens(&self) -> Tokens { Tokens(vec![self.clone()]) }
}

fn wrap(left: Symbol, toks: Tokens, right: Symbol) -> Tokens {
    left.tokens() + &toks + &right.tokens()
}

fn parens(toks: Tokens) -> Tokens { wrap(Symbol::Lparen, toks, Symbol::Rparen) }
fn braces(toks: Tokens) -> Tokens { wrap(Symbol::Lbrace, toks, Symbol::Rbrace) }

// Given a sequence of token sequences, return a vector of
// tokens with each subsequence separated by the given separator
// symbol.
fn sep<IT>(sep: Symbol, toks: IT) -> Tokens
    where IT: IntoIterator<Item=Tokens>,
{
    let sep = Token::Symbol(sep);

    toks.into_iter()
        .map(|t| { t.into_iter().flat_map(|t| t.tokens()).collect() })
        .intersperse(vec![sep])
        .flatten()
        .collect()
}

fn commas<IT>(toks: IT) -> Tokens
    where IT: IntoIterator<Item=Tokens>
{
    sep(Symbol::Comma, toks)
}

fn binop<L, OP, R>(l: &L, op: &OP, r: &R) -> Tokens
    where L: Tokenize, OP: Tokenize, R: Tokenize
{
    l.tokens() + op + r
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Vis {
    Public,                     // "pub"
    Private,                    // (nothing)
}

impl Tokenize for Vis {
    fn tokens(&self) -> Tokens {
        use Vis::*;

        match *self {
            Public => from(&Keyword::Pub),
            Private => Tokens::nil(),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Path(Vec<Ident>);

impl Path {
    pub fn new(id: Ident) -> Path { Path(vec![id]) }
    pub fn newpath(path: Vec<Ident>) -> Path { Path(path) }
}

impl Tokenize for Path {
    fn tokens(&self) -> Tokens {
        sep(Symbol::Doublecolon, self.0.iter().map(|id| id.tokens()))
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Expr {
    // Groupings
    Paren(Box<Expr>),
    Block(Block),

    // Primitives
    Literal(Literal),
    Ident(Ident),
    Unit,

    Tuple(Vec<Expr>),
    Struct(Path, Vec<(Ident, Expr)>, Option<Box<Expr>>),
    TupleStruct(Path, Vec<Expr>),
    Field(Box<Expr>, Ident),
    Index(Box<Expr>, Box<Expr>),
    Range(Option<Box<Expr>>, Option<Box<Expr>>),

    Cast(Box<Expr>, Path),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    AssignOp(Box<Expr>, BinOp, Box<Expr>),

    Match(Box<Expr>, Vec<(MatchPattern, Expr)>),
    If(Box<Expr>, Box<Block>, Option<Box<Block>>),
    IfLet(Box<Pattern>, Box<Expr>, Box<Block>, Option<Box<Block>>),

    For(Box<Pattern>, Box<Expr>, Box<Block>),
    Loop(Box<Block>),

    Closure(bool, Vec<Pattern>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
}

impl Tokenize for Expr {
    fn tokens(&self) -> Tokens {
        use Expr::*;

        match self {
            &Paren(ref expr) => parens(from(expr)),
            &Block(ref block) => block.tokens(),

            &Literal(ref lit) => lit.tokens(),
            &Ident(ref id) => id.tokens(),
            &Unit => ::Literal::Unit.tokens(),

            &Tuple(ref exprs) => parens(exprs.iter().map(|e| Tokens::from(e) + &Symbol::Comma).collect()),

            &Struct(ref id, ref fields, ref rest) => {
                let fields: Vec<Tokens> = fields.iter()
                    .map(|&(ref fid, ref val)| Tokens::from(fid) + &Symbol::Colon + val)
                    .collect();
                let rest = option(&Symbol::Dotdot, rest);

                Tokens::from(id) + &braces(commas(flatten(vec![fields, vec![rest]])))
            },

            &TupleStruct(ref id, ref fields) => {
                let fields = fields.iter()
                    .map(|ref val| val.tokens() + &Symbol::Comma)
                    .collect();

                Tokens::from(id) + &parens(fields)
            },

            &Match(ref expr, ref arms) => {
                let arms =
                    arms.iter().map(|&(ref patterns, ref expr)|
                                    Tokens::from(patterns) + &Symbol::DoubleArrow + expr + &Symbol::Comma)
                    .collect();
                Tokens::from(&Keyword::Match) + expr + &braces(arms)
            },

            &If(ref expr, ref tblock, ref eblock) => Tokens::from(&Keyword::If) + expr + tblock + &option(&Keyword::Else, eblock),
            &IfLet(ref pattern, ref expr, ref tblock, ref eblock) =>
                Tokens::from(&Keyword::If) + &Keyword::Let + pattern + &Symbol::Assign + expr + tblock + &option(&Keyword::Else, eblock),
            &For(ref pattern, ref expr, ref block) =>
                Tokens::from(&Keyword::For) + pattern + &Keyword::In + expr + block,
            &Loop(ref block) => Tokens::from(&Keyword::Loop) + block,
            
            &Cast(ref expr, ref path) => binop(&**expr, &Keyword::As, path),

            &Field(ref expr, ref id) => Tokens::from(expr) + &Symbol::Dot + id,
            &Index(ref arry, ref idx) => Tokens::from(arry) + &Symbol::Lbrack + idx + &Symbol::Rbrack,
            &Range(ref start, ref end) => Tokens::from(start) + &Symbol::Dotdot + end,

            &BinOp(ref op, ref l, ref r) => parens(binop(&**l, op, &**r)),
            &UnOp(ref op, ref expr) => Tokens::from(op) + expr,

            &Assign(ref l, ref r) => binop(&**l, &Symbol::Assign, &**r),
            &AssignOp(ref l, ref op, ref r) => binop(&**l, &Symbol::AssignOp(*op), &**r),

            &Closure(mov, ref args, ref body) =>
                (if mov { Tokens::from(&Keyword::Move) } else { Tokens::nil() }) +
                &Symbol::Bar + &commas(args.iter().map(Tokens::from)) +
                body,

            &Call(ref func, ref args) => Tokens::from(func) + &parens(commas(args.iter().map(Tokens::from))),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Block(Vec<Statement>);

impl Tokenize for Block {
    fn tokens(&self) -> Tokens {
        braces(sep(Symbol::Semi, self.0.iter().map(|s| s.tokens())))
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Statement {
    Decl(Decl),
    Expr(Expr),
}

impl Tokenize for Statement {
    fn tokens(&self) -> Tokens {
        use Statement::*;

        match self {
            &Decl(ref decl) => decl.tokens(),
            &Expr(ref expr) => expr.tokens(),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Decl {
    Item(Item),
    Let(Pattern, Option<Type>, Option<Expr>),
}

impl Tokenize for Decl {
    fn tokens(&self) -> Tokens {
        use Decl::*;

        match self {
            &Item(ref item) => from(item),
            &Let(ref pattern, ref ty, ref init) =>
                Tokens::from(&Keyword::Let) + pattern +
                &option(&Symbol::Colon, &ty.as_ref().map(|t| t.reference())) +
                &option(&Symbol::Assign, init),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Item {
    vis: Vis,
    item: Items,
}

impl Tokenize for Item {
    fn tokens(&self) -> Tokens {
        Tokens::from(&self.vis) + &self.item
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Pattern {
    Bind(Ident, Box<Pattern>),          // var @ pattern
    Range(Expr, Expr),                  // expr ... expr
    Any,                                // _
    All,                                // ..
    Val(Ident),                         // value match
    Ref(bool, Ident),                   // [mut] ref match
    Deref(Box<Pattern>),                // &pattern
    Tuple(Vec<Pattern>),                // (patterh, pattern...)
    Struct(Ident, Vec<(Ident, Pattern)>, bool),// Name { field: pattern, ... }
}

impl Tokenize for Pattern {
    fn tokens(&self) -> Tokens {
        use Pattern::*;

        match self {
            &Bind(ref id, ref pattern) => binop(id, &Symbol::At, pattern),
            &Range(ref start, ref end) => binop(start, &Symbol::Dotdotdot, end),
            &Any => from(&Symbol::Wildcard),
            &All => from(&Symbol::Dotdotdot),
            &Val(ref id) => from(id),
            &Ref(ismut, ref id) =>
                Tokens::from(&Keyword::Ref) +
                &if ismut { from(&Keyword::Mut) } else { Tokens::nil() } +
                id,
            &Deref(ref pattern) => Tokens::from(&Symbol::Ref) + pattern,
            &Tuple(ref patterns) => parens(patterns.iter().map(|t| Tokens::from(t) + &Symbol::Comma).collect()),
            &Struct(ref id, ref fields, rest) => {
                let fields: Vec<Tokens> =
                    fields.iter()
                    .map(|&(ref id, ref pattern)|
                         Tokens::from(id) + &Symbol::Colon + pattern)
                    .collect();
                Tokens::from(id) + &braces(commas(fields) +
                                           &if rest { Tokens::from(&Symbol::Comma) + &Symbol::Dotdot } else { Tokens::nil() })
            },
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct MatchPattern {
    patterns: Vec<Pattern>,     // pattern | pattern...
    pred: Option<Expr>,         // additional predicate "if pred =>"
}

impl Tokenize for MatchPattern {
    fn tokens(&self) -> Tokens {
        sep(Symbol::Bar, self.patterns.iter().map(|t| t.tokens())) + 
            &match self.pred {
                None => Tokens::nil(),
                Some(ref expr) => Tokens::from(&Keyword::If) + expr,
            }
    }
}            

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Type {
    // Primitive
    Unit,
    I8, U8,
    I16, U16,
    I32, U32,
    I64, U64,
    F32,
    F64,

    Type(Ident, Box<Type>),                // type alias

    Tuple(Vec<Type>),
    Struct(Ident, Vec<(Vis, Ident, Type)>),
    TupleStruct(Ident, Vec<(Vis, Type)>),
    Enum(Ident, Vec<EnumField>),
    Param(TypeParam),    // type parameter with constraints
}

impl Type {
    pub fn reference(&self) -> Tokens {
        use Type::*;

        match self {
            &Unit => from(&Literal::Unit),

            &I8 => from(&Keyword::I8),
            &U8 => from(&Keyword::U8),
            &I16 => from(&Keyword::I16),
            &U16 => from(&Keyword::U16),
            &I32 => from(&Keyword::I32),
            &U32 => from(&Keyword::U32),
            &I64 => from(&Keyword::I64),
            &U64 => from(&Keyword::U64),
            &F32 => from(&Keyword::F32),
            &F64 => from(&Keyword::F64),

            &Tuple(ref tup) => parens(commas(tup.iter().map(|t| t.reference()))),

            &Param(ref param) => param.reference(),

            &Type(ref id, _) |
            &Struct(ref id, _) |
            &TupleStruct(ref id, _) |
            &Enum(ref id, _)  => Tokens::from(id),
        }
    }

    pub fn define(&self) -> Tokens {
        use Type::*;

        match self {
            &Struct(ref id, ref fields) => {
                let fields: Vec<_> =
                    fields.iter()
                    .flat_map(|&(ref vis, ref id, ref ty)|
                              vec![from(vis),
                                   from(vec![from(id), from(&Symbol::Colon)]),
                                   ty.reference(),
                                   from(&Symbol::Comma)])
                    .collect();

                from(vec![from(&Keyword::Struct), from(id), braces(from(fields))])
            },

            &TupleStruct(ref id, ref fields) => {
                let fields: Vec<_> =
                    fields.iter()
                    .map(|&(ref vis, ref ty)| from(vec![from(vis), from(ty.reference())]))
                    .collect();

                from(vec![from(&Keyword::Struct), from(id), parens(commas(fields)), from(&Symbol::Semi)])
            },

            &Enum(ref id, ref fields) => {
                let fields: Vec<_> = fields.iter().map(|f| from(f)).collect();

                from(vec![from(&Keyword::Enum), from(id), braces(commas(fields))])
            },

            &Param(ref param) => param.define(),

            &Type(ref name, ref ty) => Tokens::from(&Keyword::Type) + name + &Symbol::Assign + &ty.reference(),
            
            &Unit | &I8 | &U8 | &I16 | &U16 | &I32 | &U32 | &I64 | &U64 | &F32 | &F64 |
            &Tuple(_) =>
                panic!("Intrinsic type {:?} not definable", self),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct TypeParam(Ident, Vec<Type>);

impl TypeParam {
    fn reference(&self) -> Tokens { Tokens::from(&self.0) }
    fn define(&self) -> Tokens {
        Tokens::from(&self.0) + &Symbol::Colon + &sep(Symbol::Plus, self.1.iter().map(|t| t.reference()))
    }
}

impl Tokenize for Vec<TypeParam> {
    fn tokens(&self) -> Tokens {
        wrap(Symbol::Ltbrack, commas(self.iter().map(|t| t.define())), Symbol::Gtbrack)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum EnumField {
    Unit(Ident),
    Tuple(Ident, Vec<Type>),
    Struct(Ident, Vec<(Ident, Type)>),
    Const(Ident, Expr),
}

impl Tokenize for EnumField {
    fn tokens(&self) -> Tokens {
        use EnumField::*;

        match self {
            &Unit(ref id) => from(id),

            &Tuple(ref id, ref ty) => {
                let fields: Vec<_> = ty.iter().map(|t| t.reference()).collect();
                Tokens::from(id) + &parens(commas(fields))
            },

            &Struct(ref id, ref fields) => {
                let fields: Vec<_> = fields.iter()
                    .map(|&(ref fid, ref fty)|
                         Tokens::from(fid) + &Symbol::Colon + &fty.reference())
                    .collect();

                Tokens::from(id) + &braces(commas(fields))
            },

            &Const(ref id, ref expr) =>
                binop(&Token::Ident(id.clone()), &Symbol::Assign, expr),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Items {
    Mod(Ident, Option<Vec<Item>>),
    Fn(Ident, Option<Vec<TypeParam>>, Vec<Pattern>, Option<Type>, Block),
    Type(Type),
}

impl Tokenize for Items {
    fn tokens(&self) -> Tokens {
        use Items::*;
        
        match self {
            &Mod(ref name, ref defs) => Tokens::from(&Keyword::Mod) + name +
                &match defs {
                    &None => Tokens::from(&Symbol::Semi),
                    &Some(ref defs) => braces(defs.iter().map(Tokens::from).collect()),
                },
            &Type(ref ty) => ty.define(),
            &Fn(ref name, ref typeparam, ref args, ref ret, ref body) =>
                Tokens::from(&Keyword::Fn) + name +
                typeparam +
                &parens(commas(args.iter().map(Tokens::from))) +
                &option(&Symbol::Arrow, &ret.as_ref().map(|t| t.reference())) +
                body,
        }
        
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use super::{from, sep};
    use std::boxed::Box;

    #[test]
    fn test_sep() {
        assert_eq!(sep(Symbol::Comma, vec![from(vec![from(&Keyword::Struct), from(&Ident::new("foo"))]),
                                           from(vec![from(&Keyword::U8)])]),
                   from(vec![from(&Keyword::Struct), from(&Ident::new("foo")),
                             from(&Symbol::Comma),
                             from(&Keyword::U8)]));
    }
    
    #[test]
    fn simple() {
        assert_eq!(Tokens::new(vec![Token::Keyword(Keyword::Struct)]), from(&Keyword::Struct));
        assert_eq!(Tokens::new(vec![Token::Symbol(Symbol::Lparen),
                                    Token::Literal(Literal::I32(1)), Token::BinOp(BinOp::Add), Token::Literal(Literal::I32(2)),
                                    Token::Symbol(Symbol::Rparen)]),
                   from(&Expr::BinOp(BinOp::Add, Box::new(Expr::Literal(Literal::I32(1))), Box::new(Expr::Literal(Literal::I32(2))))));
    }

    #[test]
    fn stringify() {
        use std::fmt::Write;
        
        let expr = Expr::BinOp(BinOp::Add, Box::new(Expr::Literal(Literal::I32(1))), Box::new(Expr::Literal(Literal::I32(2))));
        let mut s = String::new();

        for t in expr.tokens() {
            let _ = write!(&mut s, "{}", t);
        }
        
        assert_eq!(s, "( 1_i32 + 2_i32 ) ");
    }

    #[test]
    fn type_simple() {
        assert_eq!(Type::U8.reference(), from(&Keyword::U8));
        assert_eq!(to_string(Type::U8.reference()), "u8 ");
    }

    #[test]
    fn type_struct() {
        assert_eq!(to_string(Type::Struct(Ident::new("foo"), vec![(Vis::Public, Ident::new("bar"), Type::U8)]).define()),
                   "struct foo { pub bar : u8 , } ");
        assert_eq!(to_string(Type::Struct(Ident::new("foo"), vec![(Vis::Private, Ident::new("bar"), Type::U8)]).define()),
                   "struct foo { bar : u8 , } ");
        assert_eq!(to_string(Type::Struct(Ident::new("foo"), vec![(Vis::Public, Ident::new("bar"), Type::U8)]).reference()),
                   "foo ");

        assert_eq!(to_string(Type::TupleStruct(Ident::new("foo"), vec![(Vis::Private, Type::U8)]).define()),
                   "struct foo ( u8 ) ; ");
    }

    #[test]
    fn type_enum() {
        assert_eq!(to_string(Type::Enum(Ident::new("foo"),
                                        vec![EnumField::Unit(Ident::new("Bar")),
                                             EnumField::Tuple(Ident::new("Boff"), vec![Type::U8, Type::F32]),
                                             EnumField::Struct(Ident::new("Bonk"), vec![(Ident::new("blop"), Type::I32),
                                                                                        (Ident::new("flop"), Type::Unit)]),
                                             EnumField::Const(Ident::new("Thing"), Expr::Literal(Literal::Bool(false)))]).define()),
                   "enum foo { Bar , Boff ( u8 , f32 ) , Bonk { blop : i32 , flop : () } , Thing = false } ");
    }

    #[test]
    fn pattern() {
        use Pattern::*;

        let patterns = vec![
            (Bind(Ident::new("foo"), Box::new(Any)), "foo @ _ "),
            (Range(Expr::Unit, Expr::Literal(Literal::I32(123))), "() ... 123_i32 "),
            (Struct(Ident::new("Thing"),
                    vec![(Ident::new("foo"), Any),
                         (Ident::new("bar"), Val(Ident::new("bar"))),
                         (Ident::new("biff"), Ref(false, Ident::new("biff")))],
                    false), "Thing { foo : _ , bar : bar , biff : ref biff } "),
            (Struct(Ident::new("Thing"),
                    vec![(Ident::new("foo"), Any),
                         (Ident::new("bar"), Val(Ident::new("bar"))),
                         (Ident::new("biff"), Ref(false, Ident::new("biff")))],
                    true), "Thing { foo : _ , bar : bar , biff : ref biff , .. } "),
            (Ref(true, Ident::new("foo")), "ref mut foo "),
            (Deref(Box::new(Val(Ident::new("foo")))), "& foo "),
            (Tuple(vec![Val(Ident::new("foo"))]), "( foo , ) "),
            (Tuple(vec![Val(Ident::new("foo")), Any]), "( foo , _ , ) "),
            ];

        for &(ref p, ref s) in &patterns {
            assert_eq!(to_string(p.tokens()), *s)
        }
    }

    #[test]
    fn test_match() {
        use Pattern::*;

        let m = Expr::Match(Box::new(Expr::Ident(Ident::new("foo"))),
                            vec![(MatchPattern { patterns: vec![Ref(false, Ident::new("a")),
                                                                Struct(Ident::new("Foo"),
                                                                       vec![(Ident::new("foo"), Ref(false, Ident::new("a")))], true)],
                                                 pred: None },
                                  Expr::Ident(Ident::new("foo")))]);
        assert_eq!(to_string(m.tokens()), "match foo { ref a | Foo { foo : ref a , .. } => foo , } ");
    }

    #[test]
    fn test_for() {
        use Pattern::*;

        let f = Expr::For(Box::new(Tuple(vec![Ref(false, Ident::new("p")), Ref(false, Ident::new("s"))])),
                          Box::new(Expr::Ident(Ident::new("thing"))),
                          Box::new(Block(vec![])));

        assert_eq!(to_string(f.tokens()), "for ( ref p , ref s , ) in thing { } ");
    }
}
