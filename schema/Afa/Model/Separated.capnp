@0x9478efb7d68e5dd0;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::model::separated");

using Java = import "/capnp/java.capnp";
$Java.package("org.automata.safa.capnp");
$Java.outerClassname("SeparatedAfaSchema");

using Term = import "Term.capnp";

struct BoolAfa {
  qterms @0 :List(Term.QTerm11);
  states @1 :List(Conjunct11);
}

struct Conjunct11 {
  qterm @0 :UInt32;
  aterm @1 :UInt32;
}
