use crate::{
    asm,
    codegen::{
        builder::{InsertLoc, InsertPoint, SegmentKind},
        gen::{BlockScope, Register, Variable, VariableLocation},
        Generator,
    },
    tir,
    ty::Type,
};

impl<'gen> Generator<'gen> {
    /// Generates the assembly for all functions in the unit
    pub fn functions(&mut self) {
        let functions: Vec<tir::Function> = self.unit.functions.drain(..).collect();
        for function in functions {
            use tir::Function::*;
            match function {
                Definition {
                    name,
                    return_type,
                    params,
                    body,
                } => self.function_def(&name, &return_type, params, body),
                Extern {
                    name,
                    return_type,
                    params,
                } => self.extern_fn(&name, &return_type, &params),
            }
        }
    }

    /// Generates the assembly for an extern fn
    pub fn extern_fn(&mut self, name: &String, _return_type: &Type, _params: &Vec<(String, Type)>) {
        self.builder.set_insert_point(InsertPoint {
            segment: SegmentKind::Text,
            loc: InsertLoc::SegmentStart,
        });
        self.builder.insert(format!("extern {}\n", name));
    }

    /// Generates the assembly for a function def
    pub fn function_def(
        &mut self,
        name: &String,
        _return_type: &Type,
        params: Vec<(String, Type)>,
        body: Vec<tir::Statement>,
    ) {
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
                is_arg: true,
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
                        _ => VariableLocation::Stack((-(((idx - 6) * 8) as isize)) - 16),
                    }
                },
            };
            gen_params.push((param.name.clone(), param));
        }

        let mut block_scope = BlockScope::from_params(gen_params);

        self.builder.insert_many(vec![
            // Push the base pointer
            asm!("push", "rbp"),
            // Set the base pointer to the stack pointer
            asm!("mov", "rbp", "rsp"),
        ]);

        self.builder.set_insert_point(InsertPoint {
            segment: SegmentKind::Text,
            loc: InsertLoc::SegmentEnd,
        });

        // Temp label to insert the stack size
        self.builder.insert_label("tmp_for_stack_size");

        for statement in body {
            self.statement(statement, &mut block_scope);
        }

        let insert = self.builder.get_insert_point();
        let locals_size: usize = block_scope.fn_locals_size();
        self.builder.set_insert_point(InsertPoint {
            segment: SegmentKind::Text,
            loc: InsertLoc::LabelStart("tmp_for_stack_size".into()),
        });

        self.builder.insert(asm!("sub", "rsp", locals_size));
        self.builder.delete_label("tmp_for_stack_size");

        let mut ret_blocks = self.builder.blocks_mut(|b| {
            if let Some(label) = &b.label {
                label.name.starts_with("tmp_return_block")
            } else {
                false
            }
        });

        let mut ret_block;
        loop {
            if let Some(next) = ret_blocks.pop() {
                ret_block = next;
            } else {
                break;
            }

            ret_block.code.insert(0, asm!("add", "rsp", locals_size));
            ret_block.label = None;
        }

        self.builder.set_insert_point(insert);
        self.cleanup(&mut block_scope);
    }
}
