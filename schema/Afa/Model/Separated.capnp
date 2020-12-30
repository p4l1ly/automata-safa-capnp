@0x9478efb7d68e5dd0;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::model::separated");

using Java = import "/capnp/java.capnp";
$Java.package("org.automata.safa.capnp.Afa.Model");
$Java.outerClassname("Separated");

using Term = import "Term.capnp";

struct BoolAfa {
  aterms @0 :List(Term.BoolTerm11);
  qterms @1 :List(Term.QTerm11);
  states @2 :List(List(Conjunct11));
  varCount @3 :UInt32;
}

struct Conjunct11 {
  aterm @0 :Maybe1;
  qterm @1 :Maybe1;
}

struct Maybe1 {
  union {
    nothing @0 :Void;
    just @1 :UInt32;
  }
}
