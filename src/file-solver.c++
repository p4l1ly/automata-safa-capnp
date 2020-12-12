#include <iostream>
#include <capnp/ez-rpc.h>
#include <capnp/serialize.h>
#include <kj/async-io.h>
#include <vector>
#include <filesystem>
#include <fcntl.h>
#include <unistd.h>

namespace fs = std::filesystem;

#include "automata-safa-capnp/Model/Separated.capnp.h"
#include "automata-safa-capnp/Rpc/ModelChecker.capnp.h"
#include "automata-safa-capnp/Rpc/ModelCheckers.capnp.h"

using capnp::AnyPointer;
using capnp::List;

namespace mc = automata_safa_capnp::rpc::model_checker;
namespace mcs = automata_safa_capnp::rpc::model_checkers;
namespace sep = automata_safa_capnp::model::separated;

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "We need a path to a directory with SeparatedAfa files.\n";
        exit(1);
    }

    capnp::EzRpcClient sep_client("127.0.0.1:4000");
    auto sep_loader(sep_client.getMain<
        mc::ModelChecker<List<mcs::Wrapper>, List<mcs::OneResult<mcs::Emptiness>>>
    >());
    kj::WaitScope& waitScope = sep_client.getWaitScope();

    for (const auto & entry : fs::directory_iterator(std::string(argv[1]))) {
        int fd = open(entry.path().string().c_str(), O_RDONLY);
        capnp::StreamFdMessageReader message(fd);
        AnyPointer::Reader afa = message.getRoot<AnyPointer>();

        auto sep_load_req = sep_loader.loadRequest();
        sep_load_req.initModel(1);
        sep_load_req.getModel()[0].getData().setAs<AnyPointer>(afa);
        auto sep_checking = sep_load_req.send().getChecking();
        auto response = sep_checking.solveRequest().send();
        auto result = response.wait(waitScope);
        close(fd);

        std::cout
            << entry.path().string()
            << "\t" << result.getMeta()[0].getTime()
            << "\t";

        if (result.getMeta()[0].getCancelled()) { std::cout << "-2"; }
        else if (result.getMeta()[0].getMeta().getEmpty()) { std::cout << "0"; }
        else { std::cout << "1"; }

        std::cout << "\n";
    }

    return 0;
}
