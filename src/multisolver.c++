#include <iostream>
#include <capnp/ez-rpc.h>
#include <kj/async-io.h>
#include <vector>

#include "LoadedModelRpc.capnp.h"
#include "MultisolverRpc.capnp.h"
#include "SeparatedAfaRpc.capnp.h"
#include "CnfAfaRpc.capnp.h"

namespace rpc = automata_safa_capnp::rpc;
namespace srv = automata_safa_capnp::rpc::multisolver;
namespace sep = automata_safa_capnp::rpc::separated_afa;
namespace cnf = automata_safa_capnp::rpc::cnf_afa;

using kj::Promise;
using kj::Own;
using kj::Array;

kj::WaitScope *wait_scope;
kj::AsyncIoProvider *ioProvider;

class ModelCheckingImpl final: public srv::ModelChecking::Server {
    int count;
    int solvedCount;
    int currentTimeout;
    Array<rpc::ModelChecking::Client> checkers;
    Array<rpc::ModelChecking::Control::Client> controls;
    Promise<void> timeoutPromise;

public:
    ModelCheckingImpl(
        Array<rpc::ModelChecking::Client>&& checkers_,
        Array<rpc::ModelChecking::Control::Client>&& controls_
    )
        : count(checkers_.size())
        , checkers(kj::mv(checkers_))
        , controls(kj::mv(controls_))
        , timeoutPromise(Promise<void>(kj::READY_NOW))
    {
        std::cout << "loaded\n";
    }

    ~ModelCheckingImpl() {
        std::cout << "unloaded\n";
    }

    kj::Promise<void> solve(SolveContext context) override {
        if (!count) return kj::READY_NOW;

        currentTimeout = 60000;
        solvedCount = 0;

        timeoutPromise = promiseTimeout(currentTimeout);

        std::vector<Promise<void>> promises;
        promises.reserve(count);

        Array<Promise<void>> promiseArr(
            &promises[0], count, kj::DestructorOnlyArrayDisposer::instance);

        srv::ModelChecking::SolveResults::Builder resultStructs = context.getResults();
        auto times = resultStructs.initTimes(count);
        auto results = resultStructs.initResults(count);

        for (uint i = 0; i < count; i++) {
            promises.push_back(checkers[i].solveRequest().send()
                .then([i, times, results, &promiseArr, this] (auto result) mutable {
                    uint32_t t = result.getTime();
                    times.set(i, t);
                    rpc::ModelChecking::Result r = result.getResult();
                    results.set(i, r);

                    std::cout << "solved " << i << " " << static_cast<int>(r) << "\n";
                    if (r == rpc::ModelChecking::Result::CANCELLED) return;

                    solvedCount++;

                    if (solvedCount == count) {
                        timeoutPromise = Promise<void>(kj::READY_NOW);
                        return;
                    }

                    uint32_t new_timeout = std::max(t * 2, 1000u);
                    if (new_timeout < currentTimeout) {
                        currentTimeout = new_timeout;
                        timeoutPromise = promiseTimeout(std::max(t, 1000u));
                    }
                }).eagerlyEvaluate(nullptr)
            );
        }

        return kj::joinPromises(kj::mv(promiseArr));
    }

private:
    Promise<void> promiseTimeout(uint32_t time) {
        return ioProvider->getTimer()
        .afterDelay(time * kj::MILLISECONDS)
        .then([&]() mutable {
            std::cout << "timed out\n";
            std::vector<Promise<void>> cancelPromises;
            cancelPromises.reserve(count);
 
            for (int i = 0; i < count; i++) {
                cancelPromises.push_back(
                    controls[i].cancelRequest().send().ignoreResult()
                );
            }
            Array<Promise<void>> arr(
                &cancelPromises[0], count, kj::DestructorOnlyArrayDisposer::instance);
            return kj::joinPromises(kj::mv(arr));
        });
    }
};

class LoaderImpl final: public srv::SeparatedCnfLoader::Server {
    capnp::EzRpcClient sep_client;
    sep::Loader::Client sep_loader;

    capnp::EzRpcClient cnf_client;
    cnf::Loader::Client cnf_loader;

public:
    LoaderImpl()
        : sep_client("127.0.0.1:4001")
        , sep_loader(sep_client.getMain<sep::Loader>())
        , cnf_client("127.0.0.1:4002")
        , cnf_loader(cnf_client.getMain<cnf::Loader>())
        {
        std::cout << "hello\n";
    }

    kj::Promise<void> load(LoadContext context) override {
        auto sep_load_req = sep_loader.loadRequest();
        sep_load_req.setModel(context.getParams().getSeparatedAfa());
        auto sep_checker = sep_load_req.send().getLoadedModel();
        auto sep_control_promise = sep_checker.getControlRequest().send();
        auto sep_control = sep_control_promise.getControl();

        auto cnf_load_req = cnf_loader.loadRequest();
        cnf_load_req.setModel(context.getParams().getCnfAfa());
        auto cnf_checker = cnf_load_req.send().getLoadedModel();
        auto cnf_control_promise = cnf_checker.getControlRequest().send();
        auto cnf_control = cnf_control_promise.getControl();

        Array<rpc::ModelChecking::Client>&& arr1 = Array(
            new rpc::ModelChecking::Client[2]
                {kj::mv(sep_checker), kj::mv(cnf_checker)},
            2,
            kj::DestructorOnlyArrayDisposer::instance
        );
        Array<rpc::ModelChecking::Control::Client>&& arr2 = Array(
            new rpc::ModelChecking::Control::Client[2]
                {kj::mv(sep_control), kj::mv(cnf_control)},
            2,
            kj::DestructorOnlyArrayDisposer::instance
        );

        context.getResults().setLoadedModel(
            kj::heap<ModelCheckingImpl>(kj::mv(arr1), kj::mv(arr2)));

        Array<Promise<void>> arr(
            new Promise<void>[2]{
                sep_control_promise.ignoreResult(),
                cnf_control_promise.ignoreResult()
            },
            2,
            kj::DestructorOnlyArrayDisposer::instance
        );
        return kj::joinPromises(kj::mv(arr));
    }
};

int main() {
    capnp::EzRpcServer server(kj::heap<LoaderImpl>(), "0.0.0.0", 4000);
    kj::WaitScope& waitScope = server.getWaitScope();
    wait_scope = &waitScope;
    ioProvider = &server.getIoProvider();
    kj::NEVER_DONE.wait(waitScope);
    return 0;
}
