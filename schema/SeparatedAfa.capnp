@0x9478efb7d68e5dd0;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::separated_afa");

using Java = import "/capnp/java.capnp";
$Java.package("org.automata.safa.capnp");
$Java.outerClassname("SeparatedAfaSchema");

struct SeparatedAfa {
  qterms @0 :List(QTerm);
  aterms @1 :List(ATerm);
  states @2 :List(List(Conjunct));
  variableCount @3 :UInt32;
}

struct QTerm {
  union {
    state @0 :UInt32;
    and @1 :List(QTerm);
    or @2 :List(QTerm);
    ref @3 :UInt32;
  }
}

struct ATerm {
  union {
    var @0 :UInt32;
    and @1 :List(ATerm);
    or @2 :List(ATerm);
    not @3 :ATerm;
    ref @4 :UInt32;
  }
}

struct Conjunct {
  qterm @0 :Int32;
  aterm @1 :Int32;
}
