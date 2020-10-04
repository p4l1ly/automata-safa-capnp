@0xbdea0bfd0da12799;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::rpc::dummy_model_checker");

using LoadedModel = import "LoadedModelRpc.capnp";

interface Loader {
  load @0 () -> (checker :LoadedModel.ModelChecking);
}
