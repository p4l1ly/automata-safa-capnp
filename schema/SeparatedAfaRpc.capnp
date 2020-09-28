@0xd9f6247af5f364df;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::separated_afa_rpc");

using ModelSchema = import "SeparatedAfa.capnp";

interface Loader {
  load @0 (model :ModelSchema.SeparatedAfa) -> (loadedModel :LoadedModel);
}

interface LoadedModel {
  solve @0 () -> (empty :Bool);
}

