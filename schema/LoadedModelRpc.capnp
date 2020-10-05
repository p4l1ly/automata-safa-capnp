@0x98bb6d0a600d783c;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::rpc");

interface ModelChecking {
  solve @0 () -> (time :UInt32, result :Result);
  getControl @1 () -> (control :Control);
  foo @2 () -> (results :List(Result));

  interface Control {
    pause @0 () -> (oldStatus :Status);
    resume @1 () -> (oldStatus :Status);
    cancel @2 () -> (oldStatus :Status);
    getStatus @3 () -> (status :Status);
  }

  struct Status {
    time @0 :UInt32;
    state @1 :State;

    enum State {
      init @0;
      running @1;
      paused @2;
      cancelled @3;
    }
  }

  enum Result {
    empty @0;
    nonempty @1;
    cancelled @2;
  }
}
