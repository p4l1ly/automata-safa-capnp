#include <iostream>
#include <capnp/ez-rpc.h>
#include <kj/thread.h>
#include <unistd.h>
#include <kj/async-io.h>

#include "LoadedModelRpc.capnp.h"
#include "DummyModelCheckerRpc.capnp.h"

namespace rpc = automata_safa_capnp::rpc;
namespace srv = automata_safa_capnp::rpc::dummy_model_checker;

int main() {
    capnp::EzRpcClient client("127.0.0.1:4000");
    srv::Loader::Client loader = client.getMain<srv::Loader>();
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
        switch(solveResult.getResult()) {
            case rpc::ModelChecking::Result::EMPTY:
                result = "empty"; break;
            case rpc::ModelChecking::Result::NONEMPTY:
                result = "nonempty"; break;
            case rpc::ModelChecking::Result::CANCELLED:
                result = "cancelled"; break;
        }
        std::cout << solveResult.getTime() << " " << result << std::endl;
    }

    ioProvider->getTimer().afterDelay(500 * kj::MILLISECONDS).wait(waitScope);
}
