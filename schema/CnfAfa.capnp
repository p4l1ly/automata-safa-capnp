@0xe38d81482dce6081;

struct CnfAfa {
  variableCount @0 :UInt32;
  outputs @1 :List(Lit);
  clauses @2 :List(List(Lit));
}

struct Lit {
  var @0 :UInt32;
  positive @1 :Bool;
}
