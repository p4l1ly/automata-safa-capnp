#include <iostream>
#include <capnp/ez-rpc.h>
#include <kj/thread.h>
#include <unistd.h>
#include <kj/async-io.h>

#include "automata-safa-capnp/Rpc/ModelChecker.capnp.h"
#include "automata-safa-capnp/Rpc/ModelCheckers.capnp.h"

namespace mc = automata_safa_capnp::rpc::model_checker;
namespace mcs = automata_safa_capnp::rpc::model_checkers;

int main() {
    capnp::EzRpcClient client("127.0.0.1:4000");
    auto loader = client.getMain<mc::ModelChecker<mcs::VoidStruct, mc::TimedResult>>();
    auto& waitScope = client.getWaitScope();
    kj::AsyncIoProvider *ioProvider = &client.getIoProvider();

    {
        auto checker = loader.loadRequest().send().getChecker();
        auto control = checker.getControlRequest().send().getControl();
        kj::Promise<void> _ = ioProvider->getTimer()
            .afterDelay(250 * kj::MILLISECONDS)
            .then([control]() mutable {
                return control.cancelRequest().send().then([](auto _) {
                    std::cout << "cancel\n";
                }).eagerlyEvaluate(nullptr);
            });
        auto solvePromise = checker.solveRequest().send();
        auto solveResult = solvePromise.wait(waitScope);
        const char* result;
        switch(solveResult.getResult().getResult()) {
            case mc::Result::EMPTY:
                result = "empty"; break;
            case mc::Result::NONEMPTY:
                result = "nonempty"; break;
            case mc::Result::CANCELLED:
                result = "cancelled"; break;
        }
        std::cout << solveResult.getResult().getTime() << " " << result << std::endl;
    }

    ioProvider->getTimer().afterDelay(500 * kj::MILLISECONDS).wait(waitScope);
}
