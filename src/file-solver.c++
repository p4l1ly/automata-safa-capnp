#include <iostream>
#include <capnp/ez-rpc.h>
#include <capnp/serialize.h>
#include <kj/async-io.h>
#include <vector>
#include <filesystem>
#include <fcntl.h>
#include <unistd.h>
#include <algorithm>

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

    capnp::EzRpcClient client("127.0.0.1:4043");
    auto loader(client.getMain<
        mc::ModelChecker<List<mcs::Wrapper>, List<mcs::OneResult<mcs::Emptiness>>>
    >());
    kj::WaitScope& waitScope = client.getWaitScope();

    std::string dirstr = std::string(argv[1]);
    fs::path dir(dirstr);
    std::vector<int> filenames;

    for (const auto & entry : fs::directory_iterator(dirstr)) {
        filenames.push_back(std::stoi(entry.path().filename().string()));
    }
    std::sort(filenames.begin(), filenames.end());

    for (int file : filenames) {
        dir.append(std::to_string(file));
        int fd = open(dir.c_str(), O_RDONLY);
        dir.remove_filename();
        capnp::StreamFdMessageReader message(fd);

        AnyPointer::Reader afa = message.getRoot<AnyPointer>();

        auto load_req = loader.loadRequest();
        load_req.initModel(1);
        load_req.getModel()[0].getData().setAs<AnyPointer>(afa);
        auto checking = load_req.send().getChecking();
        auto response = checking.solveRequest().send();
        auto result = response.wait(waitScope);
        close(fd);

        std::cout
            << file
            << "\t" << result.getMeta()[0].getTime()
            << "\t";

        // std::cerr << file << "\t" << result.getMeta()[0].getTime() << "\n";

        if (result.getMeta()[0].getCancelled()) { std::cout << "-2"; }
        else if (result.getMeta()[0].getMeta().getEmpty()) { std::cout << "0"; }
        else { std::cout << "1"; }

        std::cout << "\n";
    }

    return 0;
}
