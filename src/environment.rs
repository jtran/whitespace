use std::{u16, u8};

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use serde;

// When you declare a variable, you need to know where in the current frame it
// is stored in memory.  You cannot declare a new variable in a distant frame.
#[cfg_attr(
    all(target_arch = "wasm32", target_os = "unknown"),
    derive(serde::Serialize)
)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FrameIndex(u8);

impl FrameIndex {
    pub fn new(index: u8) -> FrameIndex {
        FrameIndex(index)
    }

    pub fn placeholder() -> FrameIndex {
        FrameIndex(u8::MAX)
    }

    pub fn index(&self) -> u8 {
        self.0
    }
}

// When you use an already-declared variable, you need to know which frame it is
// in, and where in the frame it is.
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
        VarLoc {
            distance,
            index,
        }
    }

    pub fn index(&self) -> u8 {
        self.index
    }
}

impl From<FrameIndex> for VarLoc {
    fn from(frame_index: FrameIndex) -> VarLoc {
        VarLoc::new(0, frame_index.index())
    }
}

impl From<&VarLoc> for FrameIndex {
    fn from(var_loc: &VarLoc) -> FrameIndex {
        FrameIndex::new(var_loc.index())
    }
}
