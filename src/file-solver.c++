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

#include "automata-safa-capnp/Afa/Model/Separated.capnp.h"
#include "automata-safa-capnp/Afa/Rpc/ModelChecker.capnp.h"
#include "automata-safa-capnp/Afa/Rpc/ModelCheckers.capnp.h"

using capnp::AnyPointer;
using capnp::List;

namespace mc = automata_safa_capnp::rpc::model_checker;
namespace mcs = automata_safa_capnp::rpc::model_checkers;
namespace sep = automata_safa_capnp::model::separated;

int main(int argc, char* argv[]) {
    if (argc < 3) {
        std::cerr << "We need a multisolver address and a path to a directory with SeparatedAfa files.\n";
        exit(1);
    }

    capnp::EzRpcClient client(argv[1]);
    auto loader(client.getMain<
        mc::ModelChecker<List<mcs::Wrapper>, List<mcs::OneResult<mcs::Emptiness>>>
    >());
    kj::WaitScope& waitScope = client.getWaitScope();

    std::string dirstr1 = std::string(argv[2]);
    // std::string dirstr2 = std::string(argv[2]);

    std::vector<int> filenames1;
    fs::path dir1(dirstr1);
    for (const auto & entry : fs::directory_iterator(dirstr1)) {
        filenames1.push_back(std::stoi(entry.path().filename().string()));
    }
    std::sort(filenames1.begin(), filenames1.end());

    // std::vector<int> filenames2;
    // fs::path dir2(dirstr2);
    // for (const auto & entry : fs::directory_iterator(dirstr2)) {
    //     filenames2.push_back(std::stoi(entry.path().filename().string()));
    // }
    // std::sort(filenames2.begin(), filenames2.end());

    // std::vector<int> filenames;
    // int i1 = 0;
    // int i2 = 0;
    // while(i1 < filenames1.size() && i2 < filenames2.size()) {
    //     if (filenames1[i1] < filenames2[i2]) i1++;
    //     else if (filenames1[i1] > filenames2[i2]) i2++;
    //     else {
    //       filenames.push_back(filenames1[i1]);
    //       i1++; i2++;
    //     }
    // }

    for (int file : filenames1) {
        dir1.append(std::to_string(file));
        int fd1 = open(dir1.c_str(), O_RDONLY);
        dir1.remove_filename();
        capnp::StreamFdMessageReader message1(fd1);
        AnyPointer::Reader afa1 = message1.getRoot<AnyPointer>();

        // dir2.append(std::to_string(file));
        // int fd2 = open(dir2.c_str(), O_RDONLY);
        // dir2.remove_filename();
        // capnp::StreamFdMessageReader message2(fd2);
        // AnyPointer::Reader afa2 = message2.getRoot<AnyPointer>();

        auto load_req = loader.loadRequest();
        load_req.initModel(1);
        load_req.getModel()[0].getData().setAs<AnyPointer>(afa1);
        // load_req.getModel()[1].getData().setAs<AnyPointer>(afa2);
        auto checking = load_req.send().getChecking();
        auto response = checking.solveRequest().send();
        auto result = response.wait(waitScope);

        // close(fd2);
        close(fd1);

        std::cout
            << file
            << "\t" << result.getMeta()[0].getTime()
            // << "\t" << result.getMeta()[1].getTime()
            << "\t";

        if (result.getMeta()[0].getCancelled()) { std::cout << "-2"; }
        else if (result.getMeta()[0].getMeta().getEmpty()) { std::cout << "0"; }
        else { std::cout << "1"; }

        // std::cout << "\t";

        // if (result.getMeta()[1].getCancelled()) { std::cout << "-2"; }
        // else if (result.getMeta()[1].getMeta().getEmpty()) { std::cout << "0"; }
        // else { std::cout << "1"; }

        std::cout << "\n" << std::flush;
    }

    return 0;
}
