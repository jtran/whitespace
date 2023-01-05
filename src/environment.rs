use std::{u16, u8};

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use serde;

// When you declare a variable, you need to know where in the scope it is stored
// in memory.  We currently use a slot index since a scope is implemented with
// an environment, which has a vector of slots.  You cannot currently declare
// a new variable in a distant scope.
#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SlotIndex(u8);

impl SlotIndex {
    pub fn new(index: u8) -> SlotIndex {
        SlotIndex(index)
    }

    pub fn placeholder() -> SlotIndex {
        SlotIndex(u8::MAX)
    }

    pub fn index(&self) -> u8 {
        self.0
    }
}

// When you use an already-declared variable, you need to know which scope it is
// in (distance), and where in the scope it is (index).
#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct VarLoc {
    distance: u16,
    index: u8,
}

impl VarLoc {
    pub fn placeholder() -> VarLoc {
        VarLoc {
            distance: 0,
            index: 0,
        }
    }

    pub fn new(distance: u16, index: u8) -> VarLoc {
        VarLoc { distance, index }
    }

    pub fn index(&self) -> u8 {
        self.index
    }
}

impl From<SlotIndex> for VarLoc {
    fn from(frame_index: SlotIndex) -> VarLoc {
        VarLoc::new(0, frame_index.index())
    }
}

impl From<&VarLoc> for SlotIndex {
    fn from(var_loc: &VarLoc) -> SlotIndex {
        SlotIndex::new(var_loc.index())
    }
}
