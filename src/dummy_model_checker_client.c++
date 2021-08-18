#include <iostream>
#include <capnp/ez-rpc.h>
#include <kj/thread.h>
#include <unistd.h>
#include <kj/async-io.h>

#include "automata-safa-capnp/Afa/Rpc/ModelChecker.capnp.h"
#include "automata-safa-capnp/Afa/Rpc/ModelCheckers.capnp.h"

namespace mc = automata_safa_capnp::rpc::model_checker;
namespace mcs = automata_safa_capnp::rpc::model_checkers;

int main() {
    capnp::EzRpcClient client("127.0.0.1:4000");
    auto loader = client.getMain<mc::ModelChecker<mcs::VoidStruct, mcs::Emptiness>>();
    auto& waitScope = client.getWaitScope();
    kj::AsyncIoProvider *ioProvider = &client.getIoProvider();

    {
        auto checking = loader.loadRequest().send().getChecking();
        auto control = checking.getControlRequest().send().getControl();
        kj::Promise<void> _ = ioProvider->getTimer()
            .afterDelay(250 * kj::MILLISECONDS)
            .then([control]() mutable {
                return control.cancelRequest().send().then([](auto _) {
                    std::cout << "cancel\n";
                }).eagerlyEvaluate(nullptr);
            });
        auto solvePromise = checking.solveRequest().send();
        auto solveResult = solvePromise.wait(waitScope);
        const char* result;

        if (solveResult.getCancelled()) { std::cout << " CANCELLED"; }
        else if (solveResult.getMeta().getEmpty()) { std::cout << " EMPTY"; }
        else { std::cout << " NONEMPTY"; }

        std::cout << solveResult.getTime() << " " << result << std::endl;
    }

    ioProvider->getTimer().afterDelay(500 * kj::MILLISECONDS).wait(waitScope);
}
