@0xd9f6247af5f364df;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::rpc::separated_afa");

using ModelSchema = import "SeparatedAfa.capnp";
using LoadedModel = import "LoadedModelRpc.capnp";

interface Loader {
  load @0 (model :ModelSchema.SeparatedAfa) -> (loadedModel :LoadedModel.ModelChecking);
}
