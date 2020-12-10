@0xf7c01c6f70140b00;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::model::succinct");

using Java = import "/capnp/java.capnp";
$Java.package("org.automata.safa.capnp");
$Java.outerClassname("AfaSchema");

using Term = import "Term.capnp";

struct Afa(Terms, States) {
  terms @0 :Terms;
  states @1 :States;  # should be a list but is polymorphic so that it could be a list of non-pointer types (limitation of capnp generics)
}

struct BoolAfa {
  aterms @0 :List(Term.BoolTerm11);
  afa @1 :Afa(List(Term.PredicateQTerm111), List(UInt32));
}
