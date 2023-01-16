use std::path::PathBuf;

use super::tokenizer::{Token, TokenKind};
use anyhow::{Context, Result};
use backend::ast::*;

pub fn parse<'a>(tokens: &'a [Token], name: String, path: PathBuf) -> Result<backend::ast::Unit> {
    let mut parser = Parser::new(tokens);
    let program = parser.parse_unit(name, path)?;
    Ok(program)
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse_unit(&mut self, name: String, path: PathBuf) -> Result<Unit> {
        let mut unit = Unit::new_empty(name, path);
        while !self.is_at_end() {
            //let item = self.parse_item()?;
        }
        Ok(unit)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn lookahead(&self) -> &Token {
        &self.tokens[self.current + 1]
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn expect(&mut self, token: &TokenKind) -> anyhow::Result<()> {
        if self.peek().same_kind(token) {
            self.current += 1;
            Ok(())
        } else {
            Err(ParseError::Generic).context(format!("Expected token kind {:?}", token))
        }
    }

    fn parse_item(&mut self) -> Result<Item> {
        let item = match &self.peek().kind {
            TokenKind::Keyword(kw) => match kw {
                super::tokenizer::Keyword::Function => Item::Function(self.parse_fn_def()?),
                super::tokenizer::Keyword::Extern => {
                    self.advance();
                    if matches!(
                        self.peek().kind,
                        TokenKind::Keyword(super::tokenizer::Keyword::Function)
                    ) {
                        Item::Function(self.parse_fn_decl()?)
                    } else {
                        return Err(ParseError::Generic)
                            .context("Expected function after extern keyword");
                    }
                }
                super::tokenizer::Keyword::Import => Item::Import(self.parse_import()?),
                super::tokenizer::Keyword::Struct => Item::Struct(self.parse_struct_def()?),
                igl => {
                    return Err(ParseError::Generic)
                        .context(format!("Unexpected keyword {:?}", igl))
                }
            },
            TokenKind::Literal(_) => todo!(),
            TokenKind::Operator(_) => todo!(),
            TokenKind::Symbol(_) => todo!(),
            TokenKind::Identifier(_) => unreachable!("Identifier should be handled by parse_item"),
            TokenKind::Comment => unreachable!(),
        };
        Ok(item)
    }

    fn parse_struct_def(&self) -> Result<Struct> {
        todo!()
    }

    fn parse_import(&self) -> Result<Import> {
        todo!()
    }

    fn parse_fn_decl(&self) -> Result<Function> {
        todo!()
    }

    fn parse_fn_def(&self) -> Result<Function> {
        todo!()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Parse error")]
    Generic,
}
