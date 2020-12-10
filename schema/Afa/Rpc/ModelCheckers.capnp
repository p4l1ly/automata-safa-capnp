@0xe7bb0c034c4bcfab;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::rpc::model_checkers");

using import "ModelChecker.capnp".ModelChecker;
using Sep = import "../Model/Separated.capnp";
using Succ = import "../Model/Succinct.capnp";
using Cnf = import "../Model/CnfAfa.capnp";

using Dummy = ModelChecker(VoidStruct, Emptiness);
using Multi = ModelChecker(List(Wrapper), List(OneResult(AnyPointer)));
using Separated = ModelChecker(Sep.BoolAfa, Emptiness);
using Succinct = ModelChecker(Succ.BoolAfa, Emptiness);
using CnfAfa = ModelChecker(Cnf.Afa, Emptiness);

struct VoidStruct {}
struct Wrapper {
  data @0 :AnyPointer;
}
struct OneResult(Meta) {
  time @0 :UInt32;
  cancelled @1 :Bool;
  meta @2 :Meta;
}
struct Emptiness {
  empty @0 :Bool;
}
