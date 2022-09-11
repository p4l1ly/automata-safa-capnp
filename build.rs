fn main() {
    ::capnpc::CompilerCommand::new()
        .file("schema/Afa/Model/Term.capnp")
        .file("schema/Afa/Model/CnfAfa.capnp")
        .file("schema/Afa/Model/Separated.capnp")
        .file("schema/Afa/Model/Succinct.capnp")
        .file("schema/Afa/Rpc/ModelChecker.capnp")
        .file("schema/Afa/Rpc/ModelCheckers.capnp")
        .file("schema/Lib.capnp")
        .run()
        .expect("compiling schema");
}
