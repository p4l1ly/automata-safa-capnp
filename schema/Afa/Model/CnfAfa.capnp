@0xe38d81482dce6081;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::model::cnf_afa");

using Java = import "../../language-support/java.capnp";
$Java.package("org.automata.safa.capnp.Afa.Model");
$Java.outerClassname("CnfAfa");

using Rust = import "../../language-support/rust.capnp";
$Rust.parentModule("afa::model");

struct Afa {
  variableCount @0 :UInt32;
  outputs @1 :List(Lit);
  clauses @2 :List(List(Lit));
  finals @3 :List(UInt32);
  pureVars @4 :List(UInt32);  # Indices to variables that are not outputs and need not be guessed.
  upwardClauses @5 :List(UInt32);  # Indices to positive clauses with no symbols.
}

struct Lit {
  var @0 :UInt32;
  positive @1 :Bool;
}
