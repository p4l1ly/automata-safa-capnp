#![allow(non_snake_case)]

pub mod afa {
    pub mod model {
        pub mod Term_capnp { include!(concat!(env!("OUT_DIR"), "/schema/Afa/Model/Term_capnp.rs")); }
        pub mod CnfAfa_capnp { include!(concat!(env!("OUT_DIR"), "/schema/Afa/Model/CnfAfa_capnp.rs")); }
        pub mod Separated_capnp { include!(concat!(env!("OUT_DIR"), "/schema/Afa/Model/Separated_capnp.rs")); }
        pub mod Succinct_capnp { include!(concat!(env!("OUT_DIR"), "/schema/Afa/Model/Succinct_capnp.rs")); }
    }
    pub mod rpc {
        pub mod ModelChecker_capnp { include!(concat!(env!("OUT_DIR"), "/schema/Afa/Rpc/ModelChecker_capnp.rs")); }
        pub mod ModelCheckers_capnp { include!(concat!(env!("OUT_DIR"), "/schema/Afa/Rpc/ModelCheckers_capnp.rs")); }
    }
}

pub mod Lib_capnp { include!(concat!(env!("OUT_DIR"), "/schema/Lib_capnp.rs")); }
