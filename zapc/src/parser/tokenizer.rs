use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric1, char, hex_digit1, multispace0, oct_digit1};
use nom::combinator::{map, opt, recognize};
use nom::multi::{fold_many0, many0_count, many1};
use nom::sequence::{delimited, pair, tuple};
use nom::{
    branch::alt,
    bytes::complete::{escaped, escaped_transform, is_not, take_until},
    character::complete::{anychar, digit1, multispace1, none_of, one_of},
    combinator::{value, verify},
    sequence::{preceded, separated_pair, terminated},
};

use nom_locate::LocatedSpan;

type Span<'a> = LocatedSpan<&'a str, &'a str>;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: TokenLoc,
}

impl Token {
    pub fn same_kind(&self, other: &TokenKind) -> bool {
        match (&self.kind, &other) {
            (TokenKind::Identifier(_), TokenKind::Identifier(_)) => true,
            (TokenKind::Keyword(_), TokenKind::Keyword(_)) => true,
            (TokenKind::Literal(_), TokenKind::Literal(_)) => true,
            (TokenKind::Operator(_), TokenKind::Operator(_)) => true,
            (TokenKind::Symbol(_), TokenKind::Symbol(_)) => true,
            (TokenKind::Comment, TokenKind::Comment) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenLoc {
    pub file: String,
    pub line: usize,
    pub col: usize,
}

impl Into<(String, usize, usize)> for TokenLoc {
    fn into(self) -> (String, usize, usize) {
        (self.file, self.line, self.col)
    }
}

impl<'a> From<&Span<'a>> for TokenLoc {
    fn from(s: &Span<'a>) -> Self {
        TokenLoc {
            file: s.extra.to_string(),
            line: s.location_line() as usize,
            col: s.get_column(),
        }
    }
}

impl<'a> From<TokenLoc> for Token {
    fn from(t: TokenLoc) -> Self {
        Token {
            kind: TokenKind::Comment,
            loc: t,
        }
    }
}

impl<'a> From<(Keyword, TokenLoc)> for Token {
    fn from(k: (Keyword, TokenLoc)) -> Self {
        Token {
            kind: TokenKind::Keyword(k.0),
            loc: k.1,
        }
    }
}

impl<'a> From<(Literal, TokenLoc)> for Token {
    fn from(l: (Literal, TokenLoc)) -> Self {
        Token {
            kind: TokenKind::Literal(l.0),
            loc: l.1,
        }
    }
}

impl<'a> From<(Operator, TokenLoc)> for Token {
    fn from(o: (Operator, TokenLoc)) -> Self {
        Token {
            kind: TokenKind::Operator(o.0),
            loc: o.1,
        }
    }
}

impl<'a> From<(Symbol, TokenLoc)> for Token {
    fn from(s: (Symbol, TokenLoc)) -> Self {
        Token {
            kind: TokenKind::Symbol(s.0),
            loc: s.1,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Identifier(String),
    Keyword(Keyword),
    Literal(Literal),
    Operator(Operator),
    Symbol(Symbol),
    Comment,
}

#[derive(Debug, Clone)]
pub enum Symbol {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Semicolon,
    Comma,
    Colon,
    DoubleColon,
    Dot,
    ArrowR,
    ArrowL,
    Question,
    Hash,
    Dollar,
    At,
    Tilde,
    Caret,
    Percent,
    FatArrow,
    Ellipsis,
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Function,
    Extern,
    Struct,
    Return,
    If,
    Else,
    For,
    While,
    Break,
    Continue,
    Loop,
    Import,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum Operator {
    Assignment(AssignmentOp),
    Binary(BinaryOp),
    Unary(UnaryOp),
}

#[derive(Debug, Clone)]
pub enum AssignmentOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    ShlAssign,
    ShrAssign,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogAnd,
    LogOr,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    LogNot,
    Not,
    Neg,
    // Ref (&) and Deref(*) are unary, but are used for mul and logical and so their meaning is not known at tokenization time
}

fn keyword<'a>(input: Span<'a>) -> nom::IResult<Span<'a>, Token> {
    map(
        alt((
            map(tag("fn"), |s| (Keyword::Function, TokenLoc::from(&s))),
            map(tag("extern"), |s| (Keyword::Extern, TokenLoc::from(&s))),
            map(tag("struct"), |s| (Keyword::Struct, TokenLoc::from(&s))),
            map(tag("return"), |s| (Keyword::Return, TokenLoc::from(&s))),
            map(tag("if"), |s| (Keyword::If, TokenLoc::from(&s))),
            map(tag("else"), |s| (Keyword::Else, TokenLoc::from(&s))),
            map(tag("for"), |s| (Keyword::For, TokenLoc::from(&s))),
            map(tag("loop"), |s| (Keyword::Loop, TokenLoc::from(&s))),
            map(tag("while"), |s| (Keyword::While, TokenLoc::from(&s))),
            map(tag("break"), |s| (Keyword::Break, TokenLoc::from(&s))),
            map(tag("continue"), |s| (Keyword::Continue, TokenLoc::from(&s))),
            map(tag("import"), |s| (Keyword::Import, TokenLoc::from(&s))),
        )),
        |k| Token {
            kind: TokenKind::Keyword(k.0),
            loc: k.1,
        },
    )(input)
}

fn operator<'a>(input: Span<'a>) -> nom::IResult<Span<'a>, Token> {
    alt((
        map(
            alt((
                map(tag("+"), |s| {
                    (Operator::Binary(BinaryOp::Add), TokenLoc::from(&s))
                }),
                map(tag("-"), |s| {
                    (Operator::Binary(BinaryOp::Sub), TokenLoc::from(&s))
                }),
                map(tag("*"), |s| {
                    (Operator::Binary(BinaryOp::Mul), TokenLoc::from(&s))
                }),
                map(tag("/"), |s| {
                    (Operator::Binary(BinaryOp::Div), TokenLoc::from(&s))
                }),
                map(tag("%"), |s| {
                    (Operator::Binary(BinaryOp::Mod), TokenLoc::from(&s))
                }),
                map(tag("&&"), |s| {
                    (Operator::Binary(BinaryOp::LogAnd), TokenLoc::from(&s))
                }),
                map(tag("||"), |s| {
                    (Operator::Binary(BinaryOp::LogOr), TokenLoc::from(&s))
                }),
                map(tag("&"), |s| {
                    (Operator::Binary(BinaryOp::And), TokenLoc::from(&s))
                }),
                map(tag("|"), |s| {
                    (Operator::Binary(BinaryOp::Or), TokenLoc::from(&s))
                }),
                map(tag("^"), |s| {
                    (Operator::Binary(BinaryOp::Xor), TokenLoc::from(&s))
                }),
                map(tag("<<"), |s| {
                    (Operator::Binary(BinaryOp::Shl), TokenLoc::from(&s))
                }),
                map(tag(">>"), |s| {
                    (Operator::Binary(BinaryOp::Shr), TokenLoc::from(&s))
                }),
                map(tag("=="), |s| {
                    (Operator::Binary(BinaryOp::Eq), TokenLoc::from(&s))
                }),
                map(tag("!="), |s| {
                    (Operator::Binary(BinaryOp::Ne), TokenLoc::from(&s))
                }),
                map(tag(">="), |s| {
                    (Operator::Binary(BinaryOp::Ge), TokenLoc::from(&s))
                }),
                map(tag("<="), |s| {
                    (Operator::Binary(BinaryOp::Le), TokenLoc::from(&s))
                }),
                map(tag("<"), |s| {
                    (Operator::Binary(BinaryOp::Lt), TokenLoc::from(&s))
                }),
                map(tag(">"), |s| {
                    (Operator::Binary(BinaryOp::Gt), TokenLoc::from(&s))
                }),
            )),
            |op| Token::from(op),
        ),
        map(
            alt((
                map(tag("~"), |s| {
                    (Operator::Unary(UnaryOp::Not), TokenLoc::from(&s))
                }),
                map(tag("!"), |s| {
                    (Operator::Unary(UnaryOp::LogNot), TokenLoc::from(&s))
                }),
            )),
            |op| Token::from(op),
        ),
        map(
            alt((
                map(tag("+="), |s| {
                    (
                        Operator::Assignment(AssignmentOp::AddAssign),
                        TokenLoc::from(&s),
                    )
                }),
                map(tag("-="), |s| {
                    (
                        Operator::Assignment(AssignmentOp::SubAssign),
                        TokenLoc::from(&s),
                    )
                }),
                map(tag("*="), |s| {
                    (
                        Operator::Assignment(AssignmentOp::MulAssign),
                        TokenLoc::from(&s),
                    )
                }),
                map(tag("/="), |s| {
                    (
                        Operator::Assignment(AssignmentOp::DivAssign),
                        TokenLoc::from(&s),
                    )
                }),
                map(tag("%="), |s| {
                    (
                        Operator::Assignment(AssignmentOp::ModAssign),
                        TokenLoc::from(&s),
                    )
                }),
                map(tag("&="), |s| {
                    (
                        Operator::Assignment(AssignmentOp::AndAssign),
                        TokenLoc::from(&s),
                    )
                }),
                map(tag("|="), |s| {
                    (
                        Operator::Assignment(AssignmentOp::OrAssign),
                        TokenLoc::from(&s),
                    )
                }),
                map(tag("^="), |s| {
                    (
                        Operator::Assignment(AssignmentOp::XorAssign),
                        TokenLoc::from(&s),
                    )
                }),
                map(tag("<<="), |s| {
                    (
                        Operator::Assignment(AssignmentOp::ShlAssign),
                        TokenLoc::from(&s),
                    )
                }),
                map(tag(">>="), |s| {
                    (
                        Operator::Assignment(AssignmentOp::ShrAssign),
                        TokenLoc::from(&s),
                    )
                }),
                map(tag("="), |s| {
                    (
                        Operator::Assignment(AssignmentOp::Assign),
                        TokenLoc::from(&s),
                    )
                }),
            )),
            |op| Token::from(op),
        ),
    ))(input)
}

fn symbol<'a>(input: Span<'a>) -> nom::IResult<Span<'a>, Token> {
    map(
        alt((
            alt((
                map(tag("("), |s| (Symbol::OpenParen, TokenLoc::from(&s))),
                map(tag(")"), |s| (Symbol::CloseParen, TokenLoc::from(&s))),
                map(tag("{"), |s| (Symbol::OpenBrace, TokenLoc::from(&s))),
                map(tag("}"), |s| (Symbol::CloseBrace, TokenLoc::from(&s))),
                map(tag("["), |s| (Symbol::OpenBracket, TokenLoc::from(&s))),
                map(tag("]"), |s| (Symbol::CloseBracket, TokenLoc::from(&s))),
                map(tag(";"), |s| (Symbol::Semicolon, TokenLoc::from(&s))),
                map(tag(":"), |s| (Symbol::Colon, TokenLoc::from(&s))),
                map(tag("::"), |s| (Symbol::DoubleColon, TokenLoc::from(&s))),
                map(tag("."), |s| (Symbol::Dot, TokenLoc::from(&s))),
                map(tag("->"), |s| (Symbol::ArrowR, TokenLoc::from(&s))),
            )),
            alt((
                map(tag("<-"), |s| (Symbol::ArrowL, TokenLoc::from(&s))),
                map(tag("?"), |s| (Symbol::Question, TokenLoc::from(&s))),
                map(tag("#"), |s| (Symbol::Hash, TokenLoc::from(&s))),
                map(tag("$"), |s| (Symbol::Dollar, TokenLoc::from(&s))),
                map(tag("@"), |s| (Symbol::At, TokenLoc::from(&s))),
                map(tag("~"), |s| (Symbol::Tilde, TokenLoc::from(&s))),
                map(tag("^"), |s| (Symbol::Caret, TokenLoc::from(&s))),
                map(tag("%"), |s| (Symbol::Percent, TokenLoc::from(&s))),
                map(tag(","), |s| (Symbol::Comma, TokenLoc::from(&s))),
                map(tag("..."), |s| (Symbol::Ellipsis, TokenLoc::from(&s))),
                map(tag("=>"), |s| (Symbol::FatArrow, TokenLoc::from(&s))),
            )),
        )),
        |res| Token::from(res),
    )(input)
}

fn literal<'a>(input: Span<'a>) -> nom::IResult<Span<'a>, Token> {
    alt((integer, boolean, float, string, character))(input)
}

fn character<'a>(input: Span<'a>) -> nom::IResult<Span<'a>, Token> {
    map(
        map(
            delimited(
                tag("'"),
                escaped(none_of("'"), '\\', one_of(r#"'nrt\0"#)),
                tag("'"),
            ),
            |s: Span<'a>| {
                (
                    Literal::Char(
                        snailquote::unescape(s.fragment())
                            .unwrap()
                            .chars()
                            .next()
                            .unwrap(),
                    ),
                    TokenLoc::from(&s),
                )
            },
        ),
        |res| Token::from(res),
    )(input)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum StringFragment<'a> {
    Literal(Span<'a>),
    EscapedChar(char),
    EscapedWS,
    ByteVal(u8),
}

fn string<'a>(input: Span<'a>) -> nom::IResult<Span<'a>, Token> {
    let maybe_location = TokenLoc::from(&input);

    let build_string = fold_many0(
        alt((
            map(
                preceded(
                    char('\\'),
                    alt((
                        value('\n', char('n')),
                        value('\r', char('r')),
                        value('\t', char('t')),
                        value('\u{08}', char('b')),
                        value('\u{0C}', char('f')),
                        value('\\', char('\\')),
                        value('/', char('/')),
                        value('"', char('"')),
                    )),
                ),
                StringFragment::EscapedChar,
            ),
            map(
                preceded(
                    char('\\'),
                    alt((
                        map(preceded(tag("0x"), hex_digit1), |s: Span<'a>| {
                            u8::from_str_radix(s.fragment(), 16).unwrap()
                        }),
                        map(preceded(tag("0o"), oct_digit1), |s: Span<'a>| {
                            u8::from_str_radix(s.fragment(), 8).unwrap()
                        }),
                        map(preceded(tag("0b"), many1(one_of("01"))), |s| {
                            u8::from_str_radix(s.iter().map(|x| *x).collect::<String>().as_str(), 2)
                                .unwrap()
                        }),
                        map(digit1, |s: Span<'a>| {
                            u8::from_str_radix(s.fragment(), 10).unwrap()
                        }),
                    )),
                ),
                StringFragment::ByteVal,
            ),
            map(
                verify(is_not("\"\\"), |s: &Span<'a>| !s.fragment().is_empty()),
                StringFragment::Literal,
            ),
            value(StringFragment::EscapedWS, preceded(char('\\'), multispace1)),
        )),
        String::new,
        |mut string, fragment| {
            match fragment {
                StringFragment::Literal(s) => string.push_str(s.fragment()),
                StringFragment::ByteVal(b) => string.push(b as char),
                StringFragment::EscapedChar(c) => string.push(c),
                StringFragment::EscapedWS => {}
            }
            string
        },
    );

    map(delimited(char('"'), build_string, char('"')), move |s| {
        Token::from((Literal::String(s), maybe_location.clone()))
    })(input)
}

fn float<'a>(input: Span<'a>) -> nom::IResult<Span<'a>, Token> {
    map(
        alt((
            map(
                terminated(nom::character::complete::digit1, tag("f")),
                |s: Span<'a>| {
                    (
                        Literal::Float(s.fragment().parse().unwrap()),
                        TokenLoc::from(&s),
                    )
                },
            ),
            map(
                terminated(nom::character::complete::digit1, tag("f32")),
                |s: Span<'a>| {
                    (
                        Literal::Float(s.fragment().parse().unwrap()),
                        TokenLoc::from(&s),
                    )
                },
            ),
            map(
                terminated(nom::character::complete::digit1, tag("f64")),
                |s: Span<'a>| {
                    (
                        Literal::Float(s.fragment().parse().unwrap()),
                        TokenLoc::from(&s),
                    )
                },
            ),
            map(
                terminated(nom::character::complete::digit1, tag(".")),
                |s: Span<'a>| {
                    (
                        Literal::Float(s.fragment().parse().unwrap()),
                        TokenLoc::from(&s),
                    )
                },
            ),
            map(
                separated_pair(
                    nom::character::complete::digit1,
                    tag("."),
                    nom::character::complete::digit1,
                ),
                |(num, dec): (Span<'a>, Span<'a>)| {
                    (
                        Literal::Float(
                            format!("{}.{}", num.fragment(), dec.fragment())
                                .parse()
                                .unwrap(),
                        ),
                        TokenLoc::from(&num),
                    )
                },
            ),
        )),
        |res| Token::from(res),
    )(input)
}

fn boolean<'a>(input: Span<'a>) -> nom::IResult<Span<'a>, Token> {
    map(
        alt((
            map(tag("true"), |s| (Literal::Bool(true), TokenLoc::from(&s))),
            map(tag("false"), |s| (Literal::Bool(false), TokenLoc::from(&s))),
        )),
        |res| Token::from(res),
    )(input)
}

fn integer<'a>(input: Span<'a>) -> nom::IResult<Span<'a>, Token> {
    map(
        alt((
            map(
                tuple((opt(tag("-")), preceded(tag("0x"), hex_digit1))),
                |(neg, s): (Option<Span<'a>>, Span<'a>)| {
                    let fragment = s.fragment();
                    let n = i64::from_str_radix(fragment, 16).unwrap();
                    if neg.is_some() {
                        (Literal::Integer(-n), TokenLoc::from(&neg.unwrap()))
                    } else {
                        (Literal::Integer(n), TokenLoc::from(&s))
                    }
                },
            ),
            map(
                tuple((opt(tag("-")), preceded(tag("0o"), oct_digit1))),
                |(neg, s): (Option<Span<'a>>, Span<'a>)| {
                    let fragment = s.fragment();
                    let n = i64::from_str_radix(fragment, 8).unwrap();
                    if neg.is_some() {
                        (Literal::Integer(-n), TokenLoc::from(&neg.unwrap()))
                    } else {
                        (Literal::Integer(n), TokenLoc::from(&s))
                    }
                },
            ),
            map(
                tuple((opt(tag("-")), digit1)),
                |(neg, s): (Option<Span<'a>>, Span<'a>)| {
                    let fragment = s.fragment();
                    let n = i64::from_str_radix(fragment, 10).unwrap();
                    if neg.is_some() {
                        (Literal::Integer(-n), TokenLoc::from(&neg.unwrap()))
                    } else {
                        (Literal::Integer(n), TokenLoc::from(&s))
                    }
                },
            ),
        )),
        |res| Token::from(res),
    )(input)
}

fn comment<'a>(input: Span<'a>) -> nom::IResult<Span<'a>, Token> {
    map(preceded(tag("//"), take_until("\n")), |s| {
        Token::from(TokenLoc::from(&s))
    })(input)
}

fn identifier<'a>(input: Span<'a>) -> nom::IResult<Span<'a>, Token> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        |s: Span<'a>| Token {
            kind: TokenKind::Identifier(s.fragment().to_string()),
            loc: TokenLoc::from(&s),
        },
    )(input)
}

fn token<'a>(input: Span<'a>) -> nom::IResult<Span<'a>, Token> {
    terminated(
        preceded(
            multispace0,
            alt((comment, keyword, symbol, operator, literal, identifier)),
        ),
        multispace0,
    )(input)
}

pub fn tokenize<'a>(input: &'a str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut input = Span::from(input);
    while !input.is_empty() {
        let res = token(input);
        match res {
            Ok((i, t)) => {
                tokens.push(t);
                input = i;
            }
            Err(e) => {
                println!("Error: {:?}", e);
                break;
            }
        }
    }
    tokens
}

#[test]
fn literals() {
    let input = r#""hello\" world""#;
    let res = literal(Span::from(input)).unwrap();
    let TokenKind::Literal(Literal::String(s)) = res.1.kind else {
            panic!("Expected string literal");
        };
    assert_eq!(s, "hello\" world");

    let input = r#""hello\" world\0""#;
    let res = literal(Span::from(input)).unwrap();
    let TokenKind::Literal(Literal::String(s)) = res.1.kind else {
            panic!("Expected string literal");
        };
    assert_eq!(s, "hello\" world\0");
}
