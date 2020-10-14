#include <iostream>
#include <capnp/ez-rpc.h>
#include <capnp/serialize.h>
#include <kj/async-io.h>
#include <vector>
#include <filesystem>
#include <fcntl.h>
#include <unistd.h>

namespace fs = std::filesystem;

#include "LoadedModelRpc.capnp.h"
#include "SeparatedAfaRpc.capnp.h"
#include "SeparatedAfa.capnp.h"

namespace rpc = automata_safa_capnp::rpc;
namespace sep = automata_safa_capnp::rpc::separated_afa;
namespace ssep = automata_safa_capnp::separated_afa;

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "We need a path to a directory with SeparatedAfa files.\n";
        exit(1);
    }

    capnp::EzRpcClient sep_client("127.0.0.1:4001");
    sep::Loader::Client sep_loader(sep_client.getMain<sep::Loader>());
    kj::WaitScope& waitScope = sep_client.getWaitScope();

    int i = 0;
    for (const auto & entry : fs::directory_iterator(std::string(argv[1]))) {
        int fd = open(entry.path().string().c_str(), O_RDONLY);
        capnp::StreamFdMessageReader message(fd);
        ssep::SeparatedAfa::Reader afa = message.getRoot<ssep::SeparatedAfa>();

        auto sep_load_req = sep_loader.loadRequest();
        sep_load_req.setModel(afa);
        auto sep_checker = sep_load_req.send().getLoadedModel();
        auto response = sep_checker.solveRequest().send();
        auto result = response.wait(waitScope);
        close(fd);

        rpc::ModelChecking::Result r = result.getResult();
        std::cout << i << " " << result.getTime();

        switch(result.getResult()) {
            case rpc::ModelChecking::Result::CANCELLED:
                std::cout << " CANCELLED"; break;
            case rpc::ModelChecking::Result::EMPTY:
                std::cout << " EMPTY"; break;
            case rpc::ModelChecking::Result::NONEMPTY:
                std::cout << " NONEMPTY"; break;
        }

        std::cout << "\n";
        i++;
    }

    return 0;
}
