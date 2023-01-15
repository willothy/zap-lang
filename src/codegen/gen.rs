//! Code generation from TIR.

use std::fmt::Display;

use crate::{tir::Unit, ty::Type};

use super::builder::{Builder, InsertLoc, InsertPoint, SegmentKind};

/// References a name in local or global scope.
/// Keeps track of name, type, and location (register, stack w/ offset, etc.)
#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub ty: Type,
    pub location: VariableLocation,
    pub is_arg: bool,
}

/// Represents the location of a variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableLocation {
    /// The register (for function parameters)
    Register(Register),
    /// The offset from the base pointer.
    Stack(isize),
}

impl VariableLocation {
    pub fn to_asm(&self, size: usize) -> String {
        match self {
            VariableLocation::Register(reg) => reg.sized(size),
            VariableLocation::Stack(offset) => {
                let offset = *offset;
                if offset > 0 {
                    format!("{} [rbp - {}]", asm_size(size), offset)
                } else {
                    format!("{} [rbp + {}]", asm_size(size), -offset)
                }
            }
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
    /// Returns the register formatted as a deref
    /// (e.g. Rax -> [rax])
    pub fn deref(&self) -> String {
        format!("[{}]", self.sized(8))
    }

    /// Returns the register formatted as a deref with an offset
    /// (e.g. Rax -> [rax + n])
    pub fn deref_offset(&self, offset: isize) -> String {
        if offset > 0 {
            format!("[{} + {}]", self.sized(8), offset)
        } else {
            format!("[{} - {}]", self.sized(8), -offset)
        }
    }

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
pub struct BlockScope {
    pub locals: Vec<(String, Variable)>,
    pub globals: Vec<(String, Variable)>,
    pub stack_args_size: usize,
}

impl BlockScope {
    pub fn make_inner(&self) -> BlockScope {
        Self {
            locals: Vec::new(),
            globals: {
                let mut globals = Vec::new();
                for (name, var) in self.locals.iter() {
                    globals.push((name.clone(), var.clone()));
                }
                for (name, var) in self.globals.iter() {
                    globals.push((name.clone(), var.clone()));
                }
                globals
            },
            stack_args_size: self.stack_args_size,
        }
    }

    pub fn from_params(params: Vec<(String, Variable)>) -> Self {
        Self {
            locals: params,
            globals: Vec::new(),
            stack_args_size: 0,
        }
    }

    fn locals_size(&self) -> usize {
        self.locals
            .iter()
            .map(|(_, var)| if var.is_arg { 0 } else { var.ty.size() })
            .sum()
    }

    pub fn fn_locals_size(&self) -> usize {
        self.locals_size() + self.stack_args_size
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

    /// Gets the next id/name for a new block
    pub(super) fn next_block_id(&mut self, name: Option<&str>) -> String {
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
    pub(super) fn get_register(&mut self) -> Option<Register> {
        for (reg, taken) in self.registers.iter_mut() {
            if !*taken {
                *taken = true;
                return Some(*reg);
            }
        }
        None
    }

    /// Marks a register as free
    pub(super) fn free_register(&mut self, reg: Register) {
        self.registers
            .iter_mut()
            .find(|(r, _)| *r == reg)
            .unwrap()
            .1 = true;
    }

    pub(super) fn request_register(&mut self, reg: Register) -> Option<Register> {
        let Some((reg, taken)) = self
            .registers
            .iter_mut()
            .find(|(r, taken)| *r == reg && *taken == false) else {
                return None;
            };
        *taken = true;
        Some(*reg)
    }

    /// Cleans up registers from a block scope
    pub fn cleanup(&mut self, scope: &mut BlockScope) {
        scope.locals.iter().for_each(|(_, var)| {
            if let VariableLocation::Register(reg) = var.location {
                self.free_register(reg);
            }
        });
    }

    /// Allocates space on the stack
    pub(super) fn stack_alloc(&mut self, size: usize, scope: &mut BlockScope) -> VariableLocation {
        let offset: usize = scope.fn_locals_size();

        VariableLocation::Stack((offset + size) as isize)
    }

    /// Aligns a size to 8 bytes
    pub(super) fn align_8(size: usize) -> usize {
        if size % 8 == 0 {
            size
        } else {
            size + (8 - (size % 8))
        }
    }
}
