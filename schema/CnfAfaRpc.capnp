@0xf27389181c8a494e;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::rpc::cnf_afa");

using ModelSchema = import "CnfAfa.capnp";
using LoadedModel = import "LoadedModelRpc.capnp";

interface Loader {
  load @0 (model :ModelSchema.CnfAfa) -> (loadedModel :LoadedModel.ModelChecking);
}
