use std::u16;

// When you declare a variable, you need to know where in the current frame it
// is stored in memory.  You cannot declare a new variable in a distant frame.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FrameIndex(u16);

impl FrameIndex {
    pub fn new(index: u16) -> FrameIndex {
        FrameIndex(index)
    }

    pub fn placeholder() -> FrameIndex {
        FrameIndex(u16::MAX)
    }

    pub fn index(&self) -> u16 {
        self.0
    }
}

// When you use an already-declared variable, you need to know which frame it is
// in, and where in the frame it is.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct VarLoc {
    distance: u16,
    index: u16,
    unresolved: bool,
}

impl VarLoc {
    pub fn placeholder() -> VarLoc {
        VarLoc {
            distance: 0,
            index: 0,
            unresolved: true,
        }
    }

    pub fn new(distance: u16, index: u16) -> VarLoc {
        VarLoc {
            distance,
            index,
            unresolved: false,
        }
    }

    pub fn index(&self) -> u16 {
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
