use std::path::PathBuf;

use super::tokenizer::{BinaryOp, Keyword, Operator, Symbol, Token, TokenKind};
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

    fn peek_next(&self) -> &Token {
        &self.tokens[self.current + 1]
    }

    

    fn advance(&mut self) -> Token {
        let tok = self.peek().clone();
        self.current += 1;
        tok
    }

    fn consume(&mut self, token: &TokenKind) -> anyhow::Result<NodeLocTriple> {
        if !self.peek().same_kind(token) {
            return Err(ParseError::Generic).context(format!("Expected token kind {:?}", token));
        }
        self.advance();
        let peek = self.peek();
        Ok((peek.loc.file.clone(), peek.loc.line, peek.loc.col))
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
            igl => return Err(ParseError::Generic).context(format!("Unexpected token {:?}", igl)),
        };
        Ok(item)
    }

    fn parse_struct_def(&mut self) -> Result<Struct> {
        let loc = self.consume(&TokenKind::Keyword(Keyword::Struct))?;
        let name = self.parse_identifier()?;
        self.consume(&TokenKind::Symbol(Symbol::OpenBrace))?;

        let mut fields = Vec::new();
        while self
            .peek()
            .same_kind(&TokenKind::Symbol(Symbol::CloseBrace))
        {
            let field = self.parse_struct_def_field()?;
            fields.push(field);
        }

        self.consume(&TokenKind::Symbol(Symbol::CloseBrace))?;
        Ok(Struct { name, fields, loc })
    }

    fn parse_struct_def_field(&mut self) -> Result<(String, TypeName)> {
        let name = self.parse_identifier()?;
        self.consume(&TokenKind::Symbol(Symbol::Colon))?;
        let ty = self.parse_type_name()?;
        Ok((name, ty))
    }

    fn parse_type_name(&mut self) -> Result<TypeName> {
        let mut ptr_depth = 0;
        let name = self.parse_identifier()?;
        while self
            .peek()
            .same_kind(&TokenKind::Operator(Operator::Binary(BinaryOp::Mul)))
        {
            ptr_depth += 1;
            self.advance();
        }
        Ok(TypeName { name, ptr_depth })
    }

    fn parse_identifier(&mut self) -> Result<String> {
        let TokenKind::Identifier(name) = &self.peek().kind else {
            return Err(ParseError::Generic).context("Expected identifier")
        };
        let name = name.clone();
        self.advance();
        Ok(name)
    }

    fn parse_import(&mut self) -> Result<Import> {
        let loc = self.consume(&TokenKind::Keyword(Keyword::Import))?;
        let name = self.parse_identifier()?;
        self.consume(&TokenKind::Symbol(Symbol::Semicolon))?;

        Ok(Import {
            name: name.clone(),
            loc,
            path: name + ".zap",
        })
    }

    fn parse_fn_sig(
        &mut self,
    ) -> Result<(
        String,
        TypeName,
        Vec<(String, TypeName)>,
        (String, usize, usize),
    )> {
        let loc = self.consume(&TokenKind::Keyword(Keyword::Function))?;
        let name = self.parse_identifier()?;
        self.consume(&TokenKind::Symbol(Symbol::OpenParen))?;
        let mut args = Vec::new();
        while !self
            .peek()
            .same_kind(&TokenKind::Symbol(Symbol::CloseParen))
        {
            let arg_name = self.parse_identifier()?;
            self.consume(&TokenKind::Symbol(Symbol::Colon))?;
            let arg_ty = self.parse_type_name()?;
            args.push((arg_name, arg_ty));
        }
        self.consume(&TokenKind::Symbol(Symbol::CloseParen))?;
        if self.peek().same_kind(&TokenKind::Symbol(Symbol::Colon)) {
            self.consume(&TokenKind::Symbol(Symbol::Colon))?;
            let ret_ty = self.parse_type_name()?;
            Ok((name, ret_ty, args, loc))
        } else {
            Ok((name, TypeName::new("void"), args, loc))
        }
    }

    fn parse_fn_decl(&mut self) -> Result<Function> {
        let (name, return_type, params, loc) = self.parse_fn_sig()?;
        self.consume(&TokenKind::Symbol(Symbol::Semicolon))?;
        Ok(Function::Extern {
            name,
            return_type,
            params,
            loc,
        })
    }

    fn parse_fn_def(&mut self) -> Result<Function> {
        let (name, return_type, params, loc) = self.parse_fn_sig()?;
        let body = self.parse_block()?;
        todo!()
    }

    fn parse_block(&mut self) -> Result<Vec<Statement>> {
        let loc = self.consume(&TokenKind::Symbol(Symbol::OpenBrace))?;
        let mut block = Vec::new();
        while !self
            .peek()
            .same_kind(&TokenKind::Symbol(Symbol::CloseBrace))
        {
            let stmt = self.parse_stmt()?;
            block.push(stmt);
        }
        self.consume(&TokenKind::Symbol(Symbol::CloseBrace))?;
        Ok(block)
    }

    fn parse_stmt(&mut self) -> Result<Statement> {
        let peek = ;
        match &peek.kind {
            TokenKind::Keyword(kw) => match kw {
                Keyword::Return => {
                    self.advance();
                    if let TokenKind::Symbol(Symbol::Semicolon) = self.peek().kind {
                        self.advance();
                        Ok(Statement::Return {
                            value: None,
                            loc: peek.loc.into(),
                        })
                    } else {
                        let value = self.parse_expr()?;
                        self.consume(&TokenKind::Symbol(Symbol::Semicolon))?;
                        Ok(Statement::Return {
                            value: Some(value),
                            loc: peek.loc.into(),
                        })
                    }
                }
                Keyword::If => todo!(),
                Keyword::For => todo!(),
                Keyword::While => todo!(),
                Keyword::Break => todo!(),
                Keyword::Continue => todo!(),
                Keyword::Loop => todo!(),
                _ => unreachable!("illegal keyword in statement"),
            },
            _ => Ok(Statement::Expression {
                expr: self.parse_expr()?,
                loc: peek.loc.into(),
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expression> {
        todo!()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Parse error")]
    Generic,
}
