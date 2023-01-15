//! Asm builder

use std::{fs, path::PathBuf, ptr::null};

/// Macro helper to format asm instructions
#[macro_export]
macro_rules! asm {
    ($inst:expr, $arg1:expr, $arg2:expr) => {
        format!("    {:<8}{}, {}\n", $inst, $arg1, $arg2)
    };
    ($inst:expr, $arg:expr) => {
        format!("    {:<8}{}\n", $inst, $arg)
    };
    ($inst:expr) => {
        format!("    {}\n", $inst)
    };
}

/// Asm builder helper struct
#[derive(Debug)]
pub struct Builder {
    bss: Segment,
    text: Segment,
    data: Segment,
    insert_point: InsertPoint,
    file: PathBuf,
}

impl Builder {
    /// Creates a new builder object
    pub fn new(file: PathBuf) -> Self {
        Self {
            file,
            bss: Segment {
                name: ".bss".to_string(),
                blocks: Vec::new(),
            },
            text: Segment {
                name: ".text".to_string(),
                blocks: Vec::new(),
            },
            data: Segment {
                name: ".data".to_string(),
                blocks: Vec::new(),
            },
            insert_point: InsertPoint {
                segment: SegmentKind::Text,
                loc: InsertLoc::SegmentEnd,
            },
        }
    }

    /// Gets the current insert point (to save and restore later)
    pub fn get_insert_point(&self) -> InsertPoint {
        self.insert_point.clone()
    }

    /// Sets the insert point to the given location
    pub fn set_insert_point(&mut self, insert_point: InsertPoint) {
        self.insert_point = insert_point;
    }

    /// Delete the matching label from the current segment
    /// Panics if the label is not found
    pub fn delete_label(&mut self, label: &str) {
        let segment = match self.insert_point.segment {
            SegmentKind::Bss => &mut self.bss,
            SegmentKind::Text => &mut self.text,
            SegmentKind::Data => &mut self.data,
        };

        let mut found = false;
        segment.blocks.iter_mut().for_each(|b| {
            if let Some(blk_label) = &b.label {
                if blk_label.name == label {
                    b.label = None;
                    found = true;
                }
            }
        });
        if !found {
            panic!("Label not found: {}", label);
        }
    }

    /// Get blocks from the current segment which match a predicate
    pub fn blocks_mut(&mut self, mut p: impl FnMut(&Block) -> bool) -> Vec<&mut Block> {
        let segment = match self.insert_point.segment {
            SegmentKind::Bss => &mut self.bss,
            SegmentKind::Text => &mut self.text,
            SegmentKind::Data => &mut self.data,
        };

        segment.blocks.iter_mut().filter(|b| p(b)).collect()
    }

    /// Gets the first block in the current segment which matches a predicate
    pub fn block_mut(&mut self, mut p: impl FnMut(&Block) -> bool) -> Option<&mut Block> {
        let segment = match self.insert_point.segment {
            SegmentKind::Bss => &mut self.bss,
            SegmentKind::Text => &mut self.text,
            SegmentKind::Data => &mut self.data,
        };

        segment.blocks.iter_mut().find(|b| p(b))
    }

    /// Add a label at the current insert point
    pub fn insert_label(&mut self, label: &str) {
        let segment = match self.insert_point.segment {
            SegmentKind::Bss => &mut self.bss,
            SegmentKind::Text => &mut self.text,
            SegmentKind::Data => &mut self.data,
        };

        match self.insert_point.loc.clone() {
            InsertLoc::SegmentStart => {
                segment.blocks.insert(
                    0,
                    Block {
                        label: Some(Label {
                            name: label.to_owned(),
                        }),
                        code: Vec::new(),
                    },
                );
            }
            InsertLoc::SegmentEnd => {
                segment.blocks.push(Block {
                    label: Some(Label {
                        name: label.to_owned(),
                    }),
                    code: Vec::new(),
                });
            }
            InsertLoc::LabelEnd(label_end) => {
                let index = segment
                    .block_index(|b| {
                        b.label
                            .as_ref()
                            .map(|l| l.name == label_end)
                            .unwrap_or(false)
                    })
                    .unwrap();
                segment.blocks.insert(
                    index + 1,
                    Block {
                        label: Some(Label {
                            name: label.to_owned(),
                        }),
                        code: Vec::new(),
                    },
                );
            }
            _ => panic!("Cannot add label to this location"),
        }
    }

    /// Add a const string to the data segment
    pub fn insert_const_string(&mut self, label: &str, value: &str, null_terminated: bool) {
        self.data.blocks.push(Block {
            label: Some(Label {
                name: label.to_owned(),
            }),
            code: vec![asm!("db", {
                let mut s = value.to_string();
                if null_terminated {
                    s.push('\0');
                }
                s
            })],
        });
    }

    /// Insert a line of asm at the current insert point
    pub fn insert(&mut self, code: String) {
        let segment = match self.insert_point.segment {
            SegmentKind::Bss => &mut self.bss,
            SegmentKind::Text => &mut self.text,
            SegmentKind::Data => &mut self.data,
        };
        match self.insert_point.loc.clone() {
            InsertLoc::SegmentStart => {
                if let Some(block) = segment.blocks.first_mut() {
                    block.code.insert(0, code);
                } else {
                    segment.blocks.insert(
                        0,
                        Block {
                            label: None,
                            code: vec![code],
                        },
                    );
                }
            }
            InsertLoc::SegmentEnd => {
                if let Some(block) = segment.blocks.last_mut() {
                    block.code.push(code);
                } else {
                    segment.blocks.push(Block {
                        label: None,
                        code: vec![code],
                    });
                }
            }
            InsertLoc::LabelStart(label) => {
                let block = self
                    .block_mut(|block| {
                        block
                            .label
                            .as_ref()
                            .map(|l| l.name == label)
                            .unwrap_or(false)
                    })
                    .unwrap();
                block.code.insert(0, code);
            }
            InsertLoc::LabelEnd(label) => {
                let block = self
                    .block_mut(|block| {
                        block
                            .label
                            .as_ref()
                            .map(|l| l.name == label)
                            .unwrap_or(false)
                    })
                    .unwrap();
                block.code.push(code);
            }
        }
    }

    /// Insert multiple lines of assembly at the current insert point
    pub fn insert_many(&mut self, lines: Vec<String>) {
        for line in lines {
            self.insert(line);
        }
    }

    /// Write the assembly to disk
    pub fn write(&self) {
        fs::write(&self.file, self.finalize()).unwrap();
    }

    /// Write the assembly to a String
    pub fn finalize(&self) -> String {
        let mut output = String::new();

        output.push_str("segment .text\n");
        // Setup entry point
        output.push_str("global zip_main\n");
        output.push_str("zip_main:\n");
        output.push_str(&asm!("call", "main"));
        output.push_str(&asm!("mov", "rdi", "rax"));
        output.push_str(&asm!("mov", "rax", 60));
        output.push_str(&asm!("syscall"));
        self.text
            .blocks
            .iter()
            .map(|b| {
                let mut block = String::new();
                if let Some(label) = &b.label {
                    block.push_str(&format!("{}:\n", label.name));
                }

                b.code.iter().for_each(|c| {
                    block.push_str(&format!("{}", c));
                });
                block
            })
            .for_each(|b| output.push_str(&b));

        output.push_str("segment .data\n");
        self.data
            .blocks
            .iter()
            .map(|b| {
                let mut block = String::new();
                if let Some(label) = &b.label {
                    block.push_str(&format!("{}:\n", label.name));
                }

                b.code.iter().for_each(|c| {
                    block.push_str(&format!("{}", c));
                });
                block
            })
            .for_each(|b| output.push_str(&b));

        output.push_str("segment .bss\n");
        self.bss
            .blocks
            .iter()
            .map(|b| {
                let mut block = String::new();
                if let Some(label) = &b.label {
                    block.push_str(&format!("{}:\n", label.name));
                }

                b.code.iter().for_each(|c| {
                    block.push_str(&format!("{}", c));
                });
                block
            })
            .for_each(|b| output.push_str(&b));

        output
    }
}

#[derive(Debug)]
pub struct Segment {
    pub name: String,
    pub blocks: Vec<Block>,
}

impl Segment {
    /// Gets the index of the first block in the current segment which matches a predicate
    pub fn block_index(&self, mut p: impl FnMut(&Block) -> bool) -> Option<usize> {
        self.blocks.iter().position(|b| p(b))
    }
}

#[derive(Debug)]
pub struct Block {
    pub label: Option<Label>,
    pub code: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum SegmentKind {
    Bss,
    Text,
    Data,
}

#[derive(Debug, Clone)]
pub struct InsertPoint {
    pub segment: SegmentKind,
    pub loc: InsertLoc,
}

#[derive(Debug, Clone)]
pub enum InsertLoc {
    SegmentStart,
    SegmentEnd,
    LabelStart(String),
    LabelEnd(String),
}
