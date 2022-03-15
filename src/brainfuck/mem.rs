use std::{collections::HashMap};

pub struct MemoryBlock {
    data: Option<Box<[u8; MemoryBlock::BLOCK_SIZE]>>,
    used: usize,
}

impl MemoryBlock {
    /// This represents the number of bytes `MemoryBlock` will hold.
    /// This value should be a power of two.
    pub const BLOCK_SIZE: usize = 1024;

    pub fn new() -> MemoryBlock {
        MemoryBlock {
            data: None,
            used: 0,
        }
    }

    /// Returns true if this `MemoryBlock` is allocated.
    /// This means that at least one element is non-zero.
    pub fn allocated(&self) -> bool {
        self.data.is_some()
    }

    /// Returns true if this `MemoryBlock` is unallocated.
    pub fn unallocated(&self) -> bool {
        self.data.is_none()
    }

    /// Returns the number of cells that are non-zero.
    pub fn used(&self) -> usize {
        self.used
    }

    /// Drops the memory used by `MemoryBlock`.
    pub fn clear(&mut self) {
        self.data = None;
        self.used = 0;
    }

    pub fn get(&self, offset: usize) -> u8 {
        // Allows us to wrap the offset to the block size.
        let offset = MemoryBlock::sub_index(offset);
        if let Some(data) = self.data.as_ref() {
            if let Some(value) = data.get(offset) {
                return *value;
            }
        }
        // By default, if the memory block is not present, return 0.
        0
    }

    pub fn set(&mut self, offset: usize, value: u8) {
        // Allows us to wrap the offset to the block size.
        let offset = MemoryBlock::sub_index(offset);
        if let Some(data) = self.data.as_deref_mut() {
            // If a cell is becoming non-zero
            if data[offset] == 0 && value != 0 {
                self.used += 1;
            }
            // If a cell is becoming zero
            else if data[offset] != 0 && value == 0 {
                self.used -= 1;
            }
            data[offset] = value;
        } else if value != 0 {
            self.data = Some(Box::new([u8::default(); MemoryBlock::BLOCK_SIZE]));
            // It's safe to call unwrap because we just assigned it.
            let mut data = self.data.as_mut().unwrap();
            data[offset] = value;
            self.used = 1;
        }
        if self.used == 0 && self.data.is_some() {
            self.data = None;
        }
    }

    /// Wraps `offset` to `BLOCK_SIZE`.
    fn sub_index(offset: usize) -> usize {
        offset.rem_euclid(MemoryBlock::BLOCK_SIZE)
    }
}

struct Memory {
    blocks: HashMap<usize, MemoryBlock>,
    preallocated: Box<[u8; Memory::PREALLOCATED_BLOCK_SIZE]>,
}

impl Memory {

    pub const VIRTUAL_OFFSET: usize = MemoryBlock::BLOCK_SIZE * 2;
    pub const PREALLOCATED_BLOCK_SIZE: usize = Memory::VIRTUAL_OFFSET * 2;

    fn new() -> Memory {
        Memory { 
            blocks: HashMap::new(),
            preallocated: Box::new([0_u8; Memory::PREALLOCATED_BLOCK_SIZE]),
        }
    }

    fn block_index(offset: usize) -> usize {
        offset.div_euclid(MemoryBlock::BLOCK_SIZE)
    }

    pub fn  virtual_addr(offset: usize) -> usize {
        offset.wrapping_add(Memory::VIRTUAL_OFFSET)
    }

    pub fn get(&self, offset: usize) -> u8 {
        let offset = Memory::virtual_addr(offset);
        if offset < Memory::PREALLOCATED_BLOCK_SIZE {
            return self.preallocated[offset];
        } else {
            let key = Memory::block_index(offset);
            if let Some(block) = self.blocks.get(&key) {
                return block.get(offset);
            }
        }
        0
    }

    fn set(&mut self, offset: usize, value: u8) {
        let offset = Memory::virtual_addr(offset);
        if offset < Memory::PREALLOCATED_BLOCK_SIZE {
            self.preallocated[offset] = value;
        } else {
            let key = Memory::block_index(offset);
            if let Some(block) = self.blocks.get_mut(&key) {
                block.set(offset, value);
            } else if value != 0 {
                let mut block = MemoryBlock::new();
                block.set(offset, value);
                self.blocks.insert(key, block);
            }
        }
    }
    
}