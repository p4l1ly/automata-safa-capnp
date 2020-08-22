@0xf7c01c6f70140b00;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("afapipe::schema");

struct Afa {
  variableCount @0 :UInt32;
  terms @1 :List(Term);
  states @2 :List(UInt32);
}

struct Term {
  union {
    litTrue @0 :Void;
    litFalse @1 :Void;
    var @2 :UInt32;
    state @3 :UInt32;
    not @4 :Term;
    or @5 :List(Term);
    and @6 :List(Term);
    ref @7 :UInt32;
  }
}
