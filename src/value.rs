use std::fmt;

// TODO: Host functions.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NativeFunctionId {
    // Clock,
}

pub fn all_native_ids() -> Vec<NativeFunctionId> {
    vec![
        // NativeFunctionId::Clock,
    ]
}

impl fmt::Display for NativeFunctionId {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        // match self {
        //     Clock => write!(f, "clock"),
        // }
        Ok(())
    }
}
