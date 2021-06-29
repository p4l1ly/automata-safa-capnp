@0x98bb6d0a600d783c;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::rpc::model_checker");

using Rust = import "../../rust.capnp";
$Rust.parentModule("afa::rpc");

interface ModelChecker(Model, Meta) {
  load @0 (model: Model) -> (checking :ModelChecking(Meta));
}

interface ModelChecking(Meta) {
  solve @0 () -> (time :UInt32, cancelled :Bool, meta :Meta);
  getControl @1 () -> (control :Control);
}

interface Control {
  pause @0 () -> (oldStatus :Status);
  resume @1 () -> (oldStatus :Status);
  cancel @2 () -> (oldStatus :Status);
  getStatus @3 () -> (status :Status);
}

struct Status {
  time @0 :UInt32;
  state @1 :State;
}

enum State {
  init @0;
  running @1;
  paused @2;
  cancelled @3;
}
