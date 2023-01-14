//! Code generation from TIR.

use std::fmt::Display;

use crate::{
    asm,
    ops::BinaryOperator,
    tir::{self, Expression, Literal, Unit},
    ty::Type,
};

use super::builder::{Builder, InsertLoc, InsertPoint, SegmentKind};

/// References a name in local or global scope.
/// Keeps track of name, type, and location (register, stack w/ offset, etc.)
pub struct Variable {
    pub name: String,
    pub ty: Type,
    pub location: VariableLocation,
}

/// Represents the location of a variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableLocation {
    /// The register (for function parameters)
    Register(Register),
    /// The offset from the base pointer.
    Stack(usize),
}

impl VariableLocation {
    pub fn to_asm(&self, size: usize) -> String {
        match self {
            VariableLocation::Register(reg) => reg.sized(size),
            VariableLocation::Stack(offset) => format!("{} [rbp - {}]", asm_size(size), offset),
        }
    }
}

/// Represents a register.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

/// Converts a numeric size to nasm keyword (e.g. 4 -> dword, 8 -> qword)
pub fn asm_size(size: usize) -> String {
    match size {
        1 => "byte".to_string(),
        2 => "word".to_string(),
        4 => "dword".to_string(),
        8 => "qword".to_string(),
        _ => todo!("Implement size {}", size),
    }
}

impl Register {
    /// Returns the register name for the given size.
    pub fn sized(&self, size: usize) -> String {
        match self {
            Register::Rax => match size {
                1 => "al",
                2 => "ax",
                4 => "eax",
                8 => "rax",
                _ => unreachable!(),
            },
            Register::Rbx => match size {
                1 => "bl",
                2 => "bx",
                4 => "ebx",
                8 => "rbx",
                _ => unreachable!(),
            },
            Register::Rcx => match size {
                1 => "cl",
                2 => "cx",
                4 => "ecx",
                8 => "rcx",
                _ => unreachable!(),
            },
            Register::Rdx => match size {
                1 => "dl",
                2 => "dx",
                4 => "edx",
                8 => "rdx",
                _ => unreachable!(),
            },
            Register::Rsi => match size {
                1 => "sil",
                2 => "si",
                4 => "esi",
                8 => "rsi",
                _ => unreachable!(),
            },
            Register::Rdi => match size {
                1 => "dil",
                2 => "di",
                4 => "edi",
                8 => "rdi",
                _ => unreachable!(),
            },
            Register::Rbp => match size {
                1 => "bpl",
                2 => "bp",
                4 => "ebp",
                8 => "rbp",
                _ => unreachable!(),
            },
            Register::Rsp => match size {
                1 => "spl",
                2 => "sp",
                4 => "esp",
                8 => "rsp",
                _ => unreachable!(),
            },
            Register::R8 => match size {
                1 => "r8b",
                2 => "r8w",
                4 => "r8d",
                8 => "r8",
                _ => unreachable!(),
            },
            Register::R9 => match size {
                1 => "r9b",
                2 => "r9w",
                4 => "r9d",
                8 => "r9",
                _ => unreachable!(),
            },
            Register::R10 => match size {
                1 => "r10b",
                2 => "r10w",
                4 => "r10d",
                8 => "r10",
                _ => unreachable!(),
            },
            Register::R11 => match size {
                1 => "r11b",
                2 => "r11w",
                4 => "r11d",
                8 => "r11",
                _ => unreachable!(),
            },
            Register::R12 => match size {
                1 => "r12b",
                2 => "r12w",
                4 => "r12d",
                8 => "r12",
                _ => unreachable!(),
            },
            Register::R13 => match size {
                1 => "r13b",
                2 => "r13w",
                4 => "r13d",
                8 => "r13",
                _ => unreachable!(),
            },
            Register::R14 => match size {
                1 => "r14b",
                2 => "r14w",
                4 => "r14d",
                8 => "r14",
                _ => unreachable!(),
            },
            Register::R15 => match size {
                1 => "r15b",
                2 => "r15w",
                4 => "r15d",
                8 => "r15",
                _ => unreachable!(),
            },
        }
        .to_owned()
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Rax => write!(f, "rax"),
            Register::Rbx => write!(f, "rbx"),
            Register::Rcx => write!(f, "rcx"),
            Register::Rdx => write!(f, "rdx"),
            Register::Rsi => write!(f, "rsi"),
            Register::Rdi => write!(f, "rdi"),
            Register::Rbp => write!(f, "rbp"),
            Register::Rsp => write!(f, "rsp"),
            Register::R8 => write!(f, "r8"),
            Register::R9 => write!(f, "r9"),
            Register::R10 => write!(f, "r10"),
            Register::R11 => write!(f, "r11"),
            Register::R12 => write!(f, "r12"),
            Register::R13 => write!(f, "r13"),
            Register::R14 => write!(f, "r14"),
            Register::R15 => write!(f, "r15"),
        }
    }
}

/// Keeps track of variables within a scope
pub struct BlockScope<'gen> {
    pub locals: Vec<(String, Variable)>,
    pub globals: Vec<(String, &'gen Variable)>,
}

impl<'gen> BlockScope<'gen> {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            globals: Vec::new(),
        }
    }

    pub fn from_parent(parent: &'gen BlockScope<'gen>) -> Self {
        Self {
            locals: Vec::new(),
            globals: {
                let mut globals = Vec::new();
                for (name, var) in parent.locals.iter() {
                    globals.push((name.clone(), var));
                }
                for (name, var) in parent.globals.iter() {
                    globals.push((name.clone(), var));
                }
                globals
            },
        }
    }

    pub fn cleanup(&mut self, gen: &mut Generator<'gen>) {
        self.locals.iter().for_each(|(_, var)| {
            if let VariableLocation::Register(reg) = var.location {
                gen.free_register(reg);
            }
        });
    }
}

pub struct Generator<'gen> {
    pub builder: Builder,
    pub unit: Unit<'gen>,
    pub registers: Vec<(Register, bool)>,
    pub block_counter: usize,
}

impl<'gen> Generator<'gen> {
    pub fn new(unit: Unit<'gen>) -> Self {
        Self {
            builder: Builder::new(unit.path.clone()),
            unit,
            registers: vec![
                (Register::Rax, false),
                (Register::Rbx, false),
                (Register::Rcx, false),
                (Register::Rdx, false),
                (Register::Rsi, false),
                (Register::Rdi, false),
                (Register::Rbp, true), // Never use RBP as a register for regular variables
                (Register::Rsp, true), // Don't use RSP either
                (Register::R8, false),
                (Register::R9, false),
                (Register::R10, false),
                (Register::R11, false),
                (Register::R12, false),
                (Register::R13, false),
                (Register::R14, false),
                (Register::R15, false),
            ],
            block_counter: 0,
        }
    }

    /// Generates the assembly for the unit
    pub fn generate(&mut self) {
        self.builder.set_insert_point(InsertPoint {
            segment: SegmentKind::Text,
            loc: InsertLoc::SegmentEnd,
        });

        self.functions();
    }

    /// Generates the assembly for all functions in the unit
    pub fn functions(&mut self) {
        let functions: Vec<tir::Function> = self.unit.functions.drain(..).collect();
        for function in functions {
            use tir::Function::*;
            match function {
                Definition {
                    name,
                    return_type: _,
                    params,
                    body,
                } => {
                    self.builder.set_insert_point(InsertPoint {
                        segment: SegmentKind::Text,
                        loc: InsertLoc::SegmentEnd,
                    });
                    self.builder.insert_label(&name);
                    self.builder.set_insert_point(InsertPoint {
                        segment: SegmentKind::Text,
                        loc: InsertLoc::LabelEnd(name.clone()),
                    });

                    // Function params passed in registers rdi, rsi, rdx, rcx, r8, r9 and the rest on the stack
                    let mut gen_params = Vec::new();
                    for (idx, (name, ty)) in params.iter().enumerate() {
                        let param = Variable {
                            name: name.clone(),
                            ty: ty.clone(),
                            location: {
                                use Register::*;
                                match idx {
                                    0 => VariableLocation::Register(Rdi),
                                    1 => VariableLocation::Register(Rsi),
                                    2 => VariableLocation::Register(Rdx),
                                    3 => VariableLocation::Register(Rcx),
                                    4 => VariableLocation::Register(R8),
                                    5 => VariableLocation::Register(R9),
                                    _ => VariableLocation::Stack((idx - 6) * 8),
                                }
                            },
                        };
                        gen_params.push((param.name.clone(), param));
                    }

                    let mut block_scope = BlockScope {
                        locals: gen_params,
                        globals: Vec::new(),
                    };

                    self.builder.insert_many(vec![
                        asm!("push", "rbp"),
                        asm!("mov", "rbp", "rsp"),
                        //asm!("sub", "rsp", 0),
                    ]);

                    for statement in body {
                        self.statement(statement, &mut block_scope);
                    }
                }
                Extern {
                    name,
                    return_type: _,
                    params: _,
                } => {
                    self.builder.set_insert_point(InsertPoint {
                        segment: SegmentKind::Text,
                        loc: InsertLoc::SegmentStart,
                    });
                    self.builder.insert(format!("extern {}\n", name));
                }
            }
        }
    }

    /// Gets the next id/name for a new block
    fn next_block_id(&mut self, name: Option<&str>) -> String {
        let block = self.block_counter;
        self.block_counter += 1;
        let block_name = if let Some(name) = name {
            format!("{}_{}", name, block)
        } else {
            format!("block_{}", block)
        };
        block_name
    }

    /// Gets an available register and marks it as in-use
    fn get_register(&mut self) -> Option<Register> {
        for (reg, taken) in self.registers.iter_mut() {
            if !*taken {
                *taken = true;
                return Some(*reg);
            }
        }
        None
    }

    /// Marks a register as free
    fn free_register(&mut self, reg: Register) {
        self.registers
            .iter_mut()
            .find(|(r, _)| *r == reg)
            .unwrap()
            .1 = true;
    }

    /// Allocates space on the stack
    fn stack_alloc(&mut self, size: usize, scope: &mut BlockScope) -> VariableLocation {
        let offset: usize = scope.locals.iter().map(|(_, v)| v.ty.size()).sum();

        VariableLocation::Stack(offset + size)
    }

    /// Generates the assembly for an expression
    pub fn expression(
        &mut self,
        expr: &tir::Expression,
        scope: &mut BlockScope,
    ) -> (VariableLocation, usize) {
        match expr {
            Expression::Literal { value, ty } => match self.get_register() {
                Some(reg) => {
                    match value {
                        tir::Literal::Int(i) => {
                            self.builder.insert(asm!("mov", reg.sized(ty.size()), *i));
                        }
                        tir::Literal::Bool(b) => {
                            self.builder
                                .insert(asm!("mov", reg.sized(ty.size()), *b as u64));
                        }
                        tir::Literal::Char(c) => {
                            self.builder
                                .insert(asm!("mov", reg.sized(ty.size()), *c as u64));
                        }
                        other => todo!("Implement codegen for literal {:?}", other),
                    }
                    (VariableLocation::Register(reg), ty.size())
                }
                None => {
                    let alloc = self.stack_alloc(ty.size(), scope);
                    match value {
                        tir::Literal::Int(i) => {
                            self.builder.insert(asm!(
                                "mov",
                                format!("{}", alloc.to_asm(ty.size())),
                                *i
                            ));
                        }
                        tir::Literal::Bool(b) => {
                            self.builder.insert(asm!(
                                "mov",
                                format!("{}", alloc.to_asm(ty.size())),
                                *b as u64
                            ));
                        }
                        tir::Literal::Char(c) => {
                            self.builder.insert(asm!(
                                "mov",
                                format!("{}", alloc.to_asm(ty.size())),
                                *c as u64
                            ));
                        }
                        other => todo!("Implement codegen for literal {:?}", other),
                    }
                    (alloc, ty.size())
                }
            },
            Expression::Variable { name, ty } => {
                let var = scope.locals.iter().find(|(n, _)| n == name);
                if let Some((_, var)) = var {
                    (var.location, var.ty.size())
                } else {
                    let var = scope.globals.iter().find(|(n, _)| n == name).unwrap().1;
                    (var.location, var.ty.size())
                }
            }
            Expression::Binary { lhs, rhs, op, ty } => {
                let (lhs, lhs_size) = self.expression(lhs, scope);
                let (rhs, rhs_size) = self.expression(rhs, scope);
                let alloc = match self.get_register() {
                    Some(reg) => VariableLocation::Register(reg),
                    None => self.stack_alloc(ty.size(), scope),
                };

                match op {
                    BinaryOperator::Add => {
                        self.builder.insert_many(vec![
                            asm!("mov", alloc.to_asm(ty.size()), lhs.to_asm(lhs_size)),
                            asm!("add", alloc.to_asm(ty.size()), rhs.to_asm(rhs_size)),
                        ]);
                    }
                    BinaryOperator::Lt => {
                        self.builder.insert_many(vec![
                            asm!("mov", alloc.to_asm(ty.size()), 1),
                            asm!("cmp", lhs.to_asm(lhs_size), rhs.to_asm(rhs_size)),
                            asm!("mov", lhs.to_asm(lhs_size), 0),
                            asm!("cmovl", alloc.to_asm(lhs_size), lhs.to_asm(lhs_size)),
                        ]);
                    }
                    _ => todo!("Implement binop {:?}", op),
                }
                (alloc, ty.size())
            }
            other => todo!("Implement codegen for expr {:?}", other),
        }
    }

    /// Generates assembly for a statement
    pub fn statement(&mut self, stmt: tir::Statement, scope: &mut BlockScope) {
        match stmt {
            tir::Statement::Return(v) => {
                if let Some(v) = v {
                    let (loc, size) = self.expression(&v, scope);
                    self.builder.insert(asm!(
                        "mov",
                        Register::Rax.sized(size),
                        format!("{}", loc.to_asm(size))
                    ));
                }

                //let locals_size: usize = scope.locals.iter().map(|(_, v)| v.ty.size()).sum();

                self.builder.insert_many(vec![
                    //asm!("add", "rsp", locals_size),
                    asm!("pop", "rbp"),
                    asm!("ret"),
                ]);
            }
            tir::Statement::Expression(_) => todo!(),
            tir::Statement::VariableDeclaration {
                name,
                ty,
                initializer,
            } => {
                let size = ty.size();
                let var = Variable {
                    name: name.clone(),
                    ty,
                    location: {
                        if let Some(init) = initializer {
                            self.expression(&init, scope).0
                        } else {
                            if false
                            /* size <= 8 */
                            {
                                match self.get_register() {
                                    Some(reg) => VariableLocation::Register(reg),
                                    None => self.stack_alloc(size, scope),
                                }
                            } else {
                                self.stack_alloc(size, scope)
                            }
                        }
                    },
                };
                scope.locals.push((name, var));
            }
            tir::Statement::VariableAssignment { name, op, value } => {
                let (value, size) = self.expression(&value, scope);
                let var = scope
                    .locals
                    .iter()
                    .find(|(n, v)| if *n == name { true } else { false });
                let var = if let Some((_, var)) = var {
                    var
                } else {
                    scope.globals.iter().find(|(n, _)| *n == name).unwrap().1
                };
                match value {
                    VariableLocation::Register(val_reg) => {
                        match var.location {
                            VariableLocation::Register(var_reg) => {
                                self.builder.insert(asm!(
                                    "mov",
                                    var_reg.sized(size),
                                    val_reg.sized(size)
                                ));
                            }
                            VariableLocation::Stack(offset) => {
                                self.builder.insert(asm!(
                                    "mov",
                                    format!("{} [rbp - {}]", asm_size(size), offset),
                                    val_reg.sized(size)
                                ));
                            }
                        }
                        self.free_register(val_reg);
                    }
                    VariableLocation::Stack(val_offset) => match var.location {
                        VariableLocation::Register(var_reg) => {
                            self.builder.insert(asm!(
                                "mov",
                                var_reg.sized(size),
                                format!("{} [rbp - {}]", asm_size(size), val_offset)
                            ));
                        }
                        VariableLocation::Stack(var_offset) => {
                            let tmp_reg = self.get_register().unwrap();
                            self.builder.insert_many(vec![
                                asm!("push", tmp_reg),
                                asm!(
                                    "mov",
                                    tmp_reg.sized(size),
                                    format!("{} [rbp - {}]", asm_size(size), val_offset)
                                ),
                                asm!(
                                    "mov",
                                    format!("{} [rbp - {}]", asm_size(size), var_offset),
                                    tmp_reg.sized(size)
                                ),
                                asm!("pop", tmp_reg),
                            ]);
                            self.free_register(tmp_reg);
                        }
                    },
                }
            }
            tir::Statement::If {
                condition,
                then_body,
                else_body,
            } => {
                let else_block = self.next_block_id(Some("else"));
                let end_block = self.next_block_id(Some("if_end"));

                let (cond, size) = self.expression(&condition, scope);
                match cond {
                    VariableLocation::Register(reg) => {
                        self.builder
                            .insert_many(vec![asm!("test", reg, reg), asm!("jz", else_block)]);
                        self.free_register(reg);
                    }
                    VariableLocation::Stack(offset) => {
                        let reg = self.get_register().unwrap();
                        self.builder.insert_many(vec![
                            asm!(
                                "mov",
                                reg.sized(size),
                                format!("{} [rbp - {}]", asm_size(size), offset)
                            ),
                            asm!("test", reg, reg),
                            asm!("jz", else_block),
                        ]);
                        self.free_register(reg);
                    }
                };

                let mut then_scope = BlockScope {
                    locals: Vec::new(),
                    globals: {
                        let mut globals = Vec::new();
                        for (name, var) in scope.locals.iter() {
                            globals.push((name.clone(), var));
                        }
                        for (name, var) in scope.globals.iter() {
                            globals.push((name.clone(), var));
                        }
                        globals
                    },
                };
                for statement in then_body {
                    self.statement(statement, &mut then_scope);
                }
                self.builder.insert(asm!("jmp", end_block));

                self.builder.insert_label(&else_block);
                self.builder.set_insert_point(InsertPoint {
                    segment: SegmentKind::Text,
                    loc: InsertLoc::LabelEnd(else_block),
                });

                let mut else_scope = BlockScope {
                    locals: Vec::new(),
                    globals: {
                        let mut globals = Vec::new();
                        for (name, var) in scope.locals.iter() {
                            globals.push((name.clone(), var));
                        }
                        for (name, var) in scope.globals.iter() {
                            globals.push((name.clone(), var));
                        }
                        globals
                    },
                };
                for statement in else_body {
                    self.statement(statement, &mut else_scope);
                }
                self.builder.insert(asm!("jmp", end_block));

                self.builder.insert_label(&end_block);
                self.builder.set_insert_point(InsertPoint {
                    segment: SegmentKind::Text,
                    loc: InsertLoc::LabelEnd(end_block),
                });
            }
            tir::Statement::Result(_) => todo!(),
            tir::Statement::While { condition, body } => todo!(),
            tir::Statement::For {
                initializer,
                condition,
                increment,
                body,
            } => todo!(),
            tir::Statement::Break => todo!(),
            tir::Statement::Continue => todo!(),
            tir::Statement::InlineAssembly(_) => todo!(),
        }
    }
}
