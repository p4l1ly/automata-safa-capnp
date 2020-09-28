@0xf27389181c8a494e;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::cnf_afa_rpc");

using Java = import "/capnp/java.capnp";
$Java.package("org.automata.safa.capnp");
$Java.outerClassname("CnfAfaRpcSchema");

using ModelSchema = import "CnfAfa.capnp";

interface Loader {
  load @0 (model :ModelSchema.CnfAfa) -> (loadedModel :LoadedModel);
}

interface LoadedModel {
  solve @0 () -> (empty :Bool);
}
