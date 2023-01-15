use crate::{
    asm,
    codegen::{
        builder::{InsertLoc, InsertPoint, SegmentKind},
        gen::{BlockScope, Register, VariableLocation},
        Generator,
    },
    ops::{BinaryOperator, UnaryOperator},
    tir::{Expression, Literal},
    ty::Type,
    util::TrimDupSpaces,
};

impl<'gen> Generator<'gen> {
    /// Helper for allocating the output location for an expression
    fn alloc_output(
        &mut self,
        place_in: Option<VariableLocation>,
        ty: &Type,
        scope: &mut BlockScope,
    ) -> VariableLocation {
        place_in.unwrap_or_else(|| {
            let size = ty.size();
            if size > 8 {
                self.stack_alloc(size, scope)
            } else {
                match self.get_register() {
                    Some(reg) => VariableLocation::Register(reg),
                    None => self.stack_alloc(size, scope),
                }
            }
        })
    }

    /// Generates the assembly for an expression
    pub fn expression(
        &mut self,
        expr: &Expression,
        place_in: Option<VariableLocation>,
        scope: &mut BlockScope,
    ) -> (VariableLocation, usize) {
        match expr {
            Expression::Literal { value, ty } => self.literal(value, ty, place_in, scope),
            Expression::Variable { name, .. } => self.variable(name, place_in, scope),
            Expression::Binary { lhs, rhs, op, ty } => {
                self.binary(lhs, rhs, op, ty, place_in, scope)
            }
            Expression::Call { name, args, ty } => self.call(name, args, ty, scope),
            Expression::Unary { expr, op, ty } => self.unary(expr, op, ty, place_in, scope),
            Expression::InlineAssembly { code } => self.inline_asm(code, place_in),
            Expression::AsExpr { expr, ty } => todo!(),
            #[allow(unreachable_patterns)]
            other => todo!("Implement codegen for expr {:?}", other),
        }
    }

    fn inline_asm(
        &mut self,
        code: &Vec<String>,
        place_in: Option<VariableLocation>,
    ) -> (VariableLocation, usize) {
        let mut new_asm = String::new();
        for line in code {
            let line = line.trim().replace('\t', " ");
            if let Some(first_space) = line.find(' ') {
                let op = line[..first_space].to_string();
                let args: Vec<String> = line[first_space + 1..]
                    .split(',')
                    .map(|s| s.to_owned().trim_dup_spaces().trim().to_owned())
                    .collect();
                match args.len() {
                    1 => {
                        new_asm.push_str(&asm!(op, args[0]));
                    }
                    2 => {
                        new_asm.push_str(&asm!(op, args[0], args[1]));
                    }
                    n => panic!("Invalid number of arguments for inline assembly: {}", n),
                }
            } else {
                new_asm.push_str(&asm!(line));
            }
        }

        self.builder.insert(new_asm);

        if let Some(place_in) = place_in {
            (place_in, 8)
        } else {
            self.request_register(Register::Rax);
            (VariableLocation::Register(Register::Rax), 8)
        }
    }

    fn unary(
        &mut self,
        expr: &Box<Expression>,
        op: &UnaryOperator,
        ty: &Type,
        place_in: Option<VariableLocation>,
        scope: &mut BlockScope,
    ) -> (VariableLocation, usize) {
        let loc = self.alloc_output(place_in, ty, scope);
        match op {
            UnaryOperator::Neg => todo!(),
            UnaryOperator::BitNot => todo!(),
            UnaryOperator::Not => todo!(),
            UnaryOperator::Deref => {
                self.builder.insert("; Deref\n".to_owned());
                let (expr_loc, size) = self.expression(expr, None, scope);
                match expr_loc {
                    VariableLocation::Register(reg) => {
                        self.builder
                            .insert(asm!("mov", loc.to_asm(size), reg.deref()));
                    }
                    VariableLocation::Stack(_) => {
                        let reg = self.get_register().unwrap();
                        self.builder.insert_many(vec![
                            asm!("mov", reg.sized(size), expr_loc.to_asm(size)),
                            asm!("mov", loc.to_asm(size), reg.sized(size)),
                        ]);
                        self.free_register(reg);
                    }
                }
                (loc, size)
            }
            UnaryOperator::Ref => {
                self.builder.insert("; Ref\n".to_owned());
                let expr_stack_alloc = self.stack_alloc(ty.size(), scope);
                let (expr_loc, size) = self.expression(expr, Some(expr_stack_alloc), scope);
                match expr_loc {
                    VariableLocation::Stack(offset) => match loc {
                        VariableLocation::Register(reg) => {
                            self.builder
                                .insert(asm!("lea", reg.sized(8), expr_loc.to_asm(8)));
                        }
                        VariableLocation::Stack(_) => {
                            let reg = self.get_register().unwrap();
                            self.builder.insert_many(vec![
                                asm!("lea", reg.sized(8), format!("[rbp - {}]", offset)),
                                asm!("mov", loc.to_asm(8), reg.sized(8)),
                            ]);
                            self.free_register(reg);
                        }
                    },
                    _ => panic!(),
                }
                (loc, size)
            }
        }
    }

    fn to_byte_string(&self, s: &str) -> String {
        let mut bytes = String::new();
        let mut curr_chars = String::new();
        for (idx, c) in s.chars().enumerate() {
            if c.is_alphanumeric() {
                curr_chars.push(c);
            } else {
                if !curr_chars.is_empty() {
                    bytes.push_str(&format!("\"{}\", ", curr_chars));
                    curr_chars.clear();
                }

                bytes.push_str(&(c as u8).to_string());
                if idx != s.len() - 1 {
                    bytes.push_str(", ");
                }
            }
        }
        if !curr_chars.is_empty() {
            bytes.push_str(&format!("\"{}\"", curr_chars));
        }
        bytes
    }

    fn literal(
        &mut self,
        value: &Literal,
        ty: &Type,
        place_in: Option<VariableLocation>,
        scope: &mut BlockScope,
    ) -> (VariableLocation, usize) {
        let loc = self.alloc_output(place_in, ty, scope);
        match loc {
            VariableLocation::Register(reg) => {
                match value {
                    Literal::Int(i) => {
                        self.builder.insert(asm!("mov", reg.sized(ty.size()), *i));
                    }
                    Literal::Bool(b) => {
                        self.builder
                            .insert(asm!("mov", reg.sized(ty.size()), *b as u64));
                    }
                    Literal::Char(c) => {
                        self.builder
                            .insert(asm!("mov", reg.sized(ty.size()), *c as u64));
                    }
                    Literal::Str(s) => {
                        let id = self.next_block_id(Some("const_str"));
                        self.builder
                            .insert_const_string(&id, &self.to_byte_string(s), true);
                        self.builder.insert(asm!("mov", reg.sized(8), &id));
                    }
                    Literal::Float(_) => todo!(),
                    #[allow(unreachable_patterns)]
                    other => todo!("Implement codegen for literal {:?}", other),
                }
                (VariableLocation::Register(reg), ty.size())
            }
            VariableLocation::Stack(_) => {
                let alloc = loc;
                match value {
                    Literal::Int(i) => {
                        self.builder.insert(asm!(
                            "mov",
                            format!("{}", alloc.to_asm(ty.size())),
                            *i
                        ));
                    }
                    Literal::Bool(b) => {
                        self.builder.insert(asm!(
                            "mov",
                            format!("{}", alloc.to_asm(ty.size())),
                            *b as u64
                        ));
                    }
                    Literal::Char(c) => {
                        self.builder.insert(asm!(
                            "mov",
                            format!("{}", alloc.to_asm(ty.size())),
                            *c as u64
                        ));
                    }
                    Literal::Str(s) => {
                        let id = self.next_block_id(Some("const_str"));
                        self.builder
                            .insert_const_string(&id, &self.to_byte_string(s), true);
                        self.builder.insert(asm!("mov", alloc.to_asm(8), &id));
                    }
                    other => todo!("Implement codegen for literal {:?}", other),
                }
                (alloc, ty.size())
            }
        }
    }

    fn variable(
        &mut self,
        name: &String,
        copy_to: Option<VariableLocation>,
        scope: &mut BlockScope,
    ) -> (VariableLocation, usize) {
        let var = scope.locals.iter().find(|(n, _)| n == name);
        let mut res = if let Some((_, var)) = var {
            (var.location, var.ty.size())
        } else {
            let var = &scope.globals.iter().find(|(n, _)| n == name).unwrap().1;
            (var.location, var.ty.size())
        };
        match copy_to {
            Some(VariableLocation::Register(reg)) => {
                self.builder
                    .insert(asm!("mov", reg.sized(res.1), res.0.to_asm(res.1)));
                res.0 = copy_to.unwrap();
            }
            Some(VariableLocation::Stack(_)) => {
                self.builder.insert(asm!(
                    "mov",
                    copy_to.unwrap().to_asm(res.1),
                    res.0.to_asm(res.1)
                ));
                res.0 = copy_to.unwrap();
            }
            None => {}
        }
        res
    }

    fn binary(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        op: &BinaryOperator,
        ty: &Type,
        place_in: Option<VariableLocation>,
        scope: &mut BlockScope,
    ) -> (VariableLocation, usize) {
        let alloc = self.alloc_output(place_in, ty, scope);
        let (lhs, lhs_size) = self.expression(lhs, None, scope);
        let (rhs, rhs_size) = self.expression(rhs, None, scope);

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
                    asm!("cmp", rhs.to_asm(lhs_size), lhs.to_asm(rhs_size)),
                    asm!("mov", rhs.to_asm(lhs_size), 0),
                    asm!("cmovl", alloc.to_asm(lhs_size), rhs.to_asm(lhs_size)),
                ]);
            }
            _ => todo!("Implement binop {:?}", op),
        }
        (alloc, ty.size())
    }

    fn call(
        &mut self,
        name: &str,
        args: &[Expression],
        ty: &Type,
        scope: &mut BlockScope,
    ) -> (VariableLocation, usize) {
        let mut arg_locs = Vec::new();
        let reg_order = [
            Register::Rdi,
            Register::Rsi,
            Register::Rdx,
            Register::Rcx,
            Register::R8,
            Register::R9,
        ];
        for (num, reg) in reg_order.iter().enumerate() {
            if num >= args.len() {
                break;
            }
            let reg = self
                .request_register(*reg)
                .expect("Register is required to be free");
            let (arg, _) =
                self.expression(&args[num], Some(VariableLocation::Register(reg)), scope);
            arg_locs.push(arg);
        }
        if args.len() > 6 {
            for arg in args.iter().skip(6) {
                let alloc = self.stack_alloc(Self::align_8(arg.ty().size()), scope);
                let (arg, stack_arg_size) = self.expression(arg, Some(alloc), scope);

                arg_locs.push(arg);
                scope.stack_args_size += Self::align_8(stack_arg_size);
            }
        }

        self.builder.insert(asm!("call", name));
        let output = self
            .request_register(Register::Rax)
            .expect("Rax is required to be free for function calls");
        (VariableLocation::Register(output), ty.size())
    }
}
