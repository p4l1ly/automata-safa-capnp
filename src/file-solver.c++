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

namespace mc = automata_safa_capnp::rpc::model_checker;
namespace mcs = automata_safa_capnp::rpc::model_checkers;
namespace sep = automata_safa_capnp::model::separated;

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "We need a path to a directory with SeparatedAfa files.\n";
        exit(1);
    }

    capnp::EzRpcClient sep_client("127.0.0.1:4001");
    auto sep_loader(sep_client.getMain<mc::ModelChecker<sep::BoolAfa, mc::TimedResult>>());
    kj::WaitScope& waitScope = sep_client.getWaitScope();

    int i = 0;
    for (const auto & entry : fs::directory_iterator(std::string(argv[1]))) {
        int fd = open(entry.path().string().c_str(), O_RDONLY);
        capnp::StreamFdMessageReader message(fd);
        sep::BoolAfa::Reader afa = message.getRoot<sep::BoolAfa>();

        auto sep_load_req = sep_loader.loadRequest();
        sep_load_req.setModel(afa);
        auto sep_checker = sep_load_req.send().getChecker();
        auto response = sep_checker.solveRequest().send();
        auto result = response.wait(waitScope);
        close(fd);

        std::cout << i << " " << result.getResult().getTime();

        switch(result.getResult().getResult()) {
            case mc::Result::CANCELLED:
                std::cout << " CANCELLED"; break;
            case mc::Result::EMPTY:
                std::cout << " EMPTY"; break;
            case mc::Result::NONEMPTY:
                std::cout << " NONEMPTY"; break;
        }

        std::cout << "\n";
        i++;
    }

    return 0;
}
