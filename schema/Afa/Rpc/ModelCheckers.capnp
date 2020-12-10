@0xe7bb0c034c4bcfab;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::rpc::model_checkers");

using import "ModelChecker.capnp".ModelChecker;
using import "ModelChecker.capnp".TimedResult;
using Sep = import "../Model/Separated.capnp";
using Succ = import "../Model/Succinct.capnp";
using Cnf = import "../Model/CnfAfa.capnp";

using Dummy = ModelChecker(VoidStruct, TimedResult);
using Multi = ModelChecker(List(Wrapper), List(Wrapper));
using Separated = ModelChecker(Sep.BoolAfa, TimedResult);
using Succinct = ModelChecker(Succ.BoolAfa, TimedResult);
using CnfAfa = ModelChecker(Cnf.Afa, TimedResult);

struct VoidStruct {}
struct Wrapper {
  data @0 :AnyStruct;
}
