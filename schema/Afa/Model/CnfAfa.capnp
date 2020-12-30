@0xe38d81482dce6081;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::model::cnf_afa");

using Java = import "/capnp/java.capnp";
$Java.package("org.automata.safa.capnp.Afa.Model");
$Java.outerClassname("CnfAfa");

struct Afa {
  variableCount @0 :UInt32;
  outputs @1 :List(Lit);
  clauses @2 :List(List(Lit));
}

struct Lit {
  var @0 :UInt32;
  positive @1 :Bool;
}
