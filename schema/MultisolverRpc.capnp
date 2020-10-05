@0xb0d121e2f199f8c9;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::rpc::multisolver");

using SeparatedAfa = import "SeparatedAfa.capnp";
using CnfAfa = import "CnfAfa.capnp";
using LoadedModel = import "LoadedModelRpc.capnp";

interface SeparatedCnfLoader {
    load @0 (separatedAfa :SeparatedAfa.SeparatedAfa, cnfAfa :CnfAfa.CnfAfa)
        -> (loadedModel :ModelChecking);
}

interface ModelChecking  {
    solve @0 () -> (
        times :List(UInt32),
        results :List(LoadedModel.ModelChecking.Result),
    );
}
