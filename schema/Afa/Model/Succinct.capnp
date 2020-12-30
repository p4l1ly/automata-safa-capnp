@0xf7c01c6f70140b00;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::model::succinct");

using Java = import "/capnp/java.capnp";
$Java.package("org.automata.safa.capnp.Afa.Model");
$Java.outerClassname("Succinct");

using Term = import "Term.capnp";

struct BoolAfa {
  aterms @0 :List(Term.BoolTerm11);
  mterms @1 :List(Term.PredicateQTerm111);
  states @2 :List(UInt32);
  varCount @3 :UInt32;
}
