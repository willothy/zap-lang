use crate::{
    asm,
    codegen::{
        builder::{InsertLoc, InsertPoint, SegmentKind},
        gen::{BlockScope, Register, Variable, VariableLocation},
        Generator,
    },
    ops::AssignmentOperator,
    tir::{self, Expression, Statement},
    ty::Type,
    util::TrimDupSpaces,
};

impl<'gen> Generator<'gen> {
    /// Generates assembly for a statement
    pub fn statement(&mut self, stmt: tir::Statement, scope: &mut BlockScope) {
        match stmt {
            tir::Statement::Return(v) => self.return_stmt(v, scope),
            tir::Statement::Expression(expr) => {
                let loc = self.expression(&expr, None, scope).0;
                if let VariableLocation::Register(reg) = loc {
                    self.free_register(reg);
                }
            }
            tir::Statement::VariableDeclaration {
                name,
                ty,
                initializer,
            } => self.var_decl(name, ty, initializer, scope),
            tir::Statement::VariableAssignment { name, op, value } => {
                self.var_assign(name, op, value, scope)
            }
            tir::Statement::If {
                condition,
                then_body,
                else_body,
            } => self.if_stmt(condition, then_body, else_body, scope),
            tir::Statement::Result(_) => todo!(),
            tir::Statement::While { .. } => todo!(),
            tir::Statement::For { .. } => todo!(),
            tir::Statement::Break => todo!(),
            tir::Statement::Continue => todo!(),
        }
    }

    /// Generates assembly for a return statement.
    fn return_stmt(&mut self, expr: Option<Expression>, scope: &mut BlockScope) {
        if let Some(v) = expr {
            let (loc, size) = self.expression(&v, None, scope);
            if !matches!(loc, VariableLocation::Register(Register::Rax)) {
                self.builder
                    .insert(asm!("mov", Register::Rax.sized(size), loc.to_asm(size)));
            }
        }

        // Create a label to return to once the size of all locals is known
        let id = self.next_block_id(Some("tmp_return_block"));
        self.builder.set_insert_point(InsertPoint {
            segment: SegmentKind::Text,
            loc: InsertLoc::SegmentEnd,
        });
        self.builder.insert_label(&id);
        self.builder
            .insert_many(vec![asm!("pop", "rbp"), asm!("ret")]);
    }

    /// Generates assembly for a variable declaration.
    fn var_decl(
        &mut self,
        name: String,
        ty: Type,
        initializer: Option<Expression>,
        scope: &mut BlockScope,
    ) {
        let size = ty.size();
        let var = Variable {
            is_arg: false,
            name: name.clone(),
            ty,
            location: {
                if let Some(init) = initializer {
                    self.expression(&init, None, scope).0
                } else {
                    if size <= 8 {
                        // TODO: Should I allocate locals in registers if possible?
                        /* match self.get_register() {
                            Some(reg) => VariableLocation::Register(reg),
                            None => self.stack_alloc(size, scope),
                        } */
                        self.stack_alloc(size, scope)
                    } else {
                        self.stack_alloc(size, scope)
                    }
                }
            },
        };
        scope.locals.push((name, var));
    }

    /// Generates assembly for a variable assignment.
    fn var_assign(
        &mut self,
        name: String,
        _op: AssignmentOperator,
        value: Expression,
        scope: &mut BlockScope,
    ) {
        let (value, size) = self.expression(&value, None, scope);
        let var = scope
            .locals
            .iter()
            .find(|(n, _)| if *n == name { true } else { false });
        let var = if let Some((_, var)) = var {
            var
        } else {
            &scope.globals.iter().find(|(n, _)| *n == name).unwrap().1
        };
        match value {
            VariableLocation::Register(val_reg) => {
                match var.location {
                    VariableLocation::Register(var_reg) => {
                        self.builder
                            .insert(asm!("mov", var_reg.sized(size), val_reg.sized(size)));
                    }
                    VariableLocation::Stack(_) => {
                        self.builder.insert(asm!(
                            "mov",
                            var.location.to_asm(size),
                            val_reg.sized(size)
                        ));
                    }
                }
                self.free_register(val_reg);
            }
            VariableLocation::Stack(_) => match var.location {
                VariableLocation::Register(var_reg) => {
                    self.builder
                        .insert(asm!("mov", var_reg.sized(size), value.to_asm(size)));
                }
                VariableLocation::Stack(_) => {
                    let tmp_reg = self.get_register().unwrap();
                    self.builder.insert_many(vec![
                        asm!("mov", tmp_reg.sized(size), value.to_asm(size)),
                        asm!("mov", var.location.to_asm(size), tmp_reg.sized(size)),
                    ]);
                    self.free_register(tmp_reg);
                }
            },
        }
    }

    /// Generates assembly for an if statement.
    fn if_stmt(
        &mut self,
        condition: Expression,
        then_body: Vec<Statement>,
        else_body: Vec<Statement>,
        scope: &mut BlockScope,
    ) {
        let else_block = self.next_block_id(Some("else"));
        let end_block = self.next_block_id(Some("if_end"));

        let (cond, size) = self.expression(&condition, None, scope);
        match cond {
            VariableLocation::Register(reg) => {
                self.builder
                    .insert_many(vec![asm!("test", reg, reg), asm!("jz", else_block)]);
                self.free_register(reg);
            }
            VariableLocation::Stack(_) => {
                let reg = self.get_register().unwrap();
                self.builder.insert_many(vec![
                    asm!("mov", reg.sized(size), cond.to_asm(size)),
                    asm!("test", reg, reg),
                    asm!("jz", else_block),
                ]);
                self.free_register(reg);
            }
        };

        let mut then_scope = scope.make_inner();
        for statement in then_body {
            self.statement(statement, &mut then_scope);
        }
        self.builder.insert(asm!("jmp", end_block));
        self.cleanup(&mut then_scope);

        self.builder.insert_label(&else_block);
        self.builder.set_insert_point(InsertPoint {
            segment: SegmentKind::Text,
            loc: InsertLoc::LabelEnd(else_block),
        });

        let mut else_scope = scope.make_inner();
        for statement in else_body {
            self.statement(statement, &mut else_scope);
        }
        self.builder.insert(asm!("jmp", end_block));
        self.cleanup(&mut else_scope);

        self.builder.insert_label(&end_block);
        self.builder.set_insert_point(InsertPoint {
            segment: SegmentKind::Text,
            loc: InsertLoc::LabelEnd(end_block),
        });
    }
}
