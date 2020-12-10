@0x98bb6d0a600d783c;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::rpc::model_checker");

interface ModelChecker(Model, Result_) {
  load @0 (model: Model) -> (checker :ModelChecking(Result_));
}

interface ModelChecking(Result_) {
  solve @0 () -> (result :Result_);
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

struct TimedResult {
  time @0 :UInt32;
  result @1 :Result;
}

enum Result {
  empty @0;
  nonempty @1;
  cancelled @2;
}
