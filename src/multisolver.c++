#include <iostream>
#include <capnp/ez-rpc.h>
#include <kj/async-io.h>
#include <vector>
#include <chrono>

#include "automata-safa-capnp/Afa/Rpc/ModelChecker.capnp.h"
#include "automata-safa-capnp/Afa/Rpc/ModelCheckers.capnp.h"

namespace mc = automata_safa_capnp::rpc::model_checker;
namespace mcs = automata_safa_capnp::rpc::model_checkers;

namespace chrono = std::chrono;
using capnp::RemotePromise;
using capnp::EzRpcClient;
using capnp::List;
using capnp::AnyPointer;
using kj::heapArrayBuilder;
using kj::Promise;
using kj::Own;
using kj::Array;

kj::WaitScope *wait_scope;
kj::AsyncIoProvider *ioProvider;

typedef mc::ModelChecking<List<mcs::OneResult<AnyPointer>>> Multichecking;
typedef mc::ModelChecker<List<mcs::Wrapper>, List<mcs::OneResult<AnyPointer>>> Multichecker;
typedef mc::ModelChecker<AnyPointer, AnyPointer> SingleChecker;
typedef mc::ModelChecking<AnyPointer> SingleChecking;

int TIMEOUT = 15000;

class ModelCheckingImpl final: public Multichecking::Server {
    int count;
    int solvedCount;
    int currentTimeout;
    Array<SingleChecking::Client> checkings;
    Array<mc::Control::Client> controls;
    Promise<void> timeoutPromise;

public:
    ModelCheckingImpl(
        Array<SingleChecking::Client>&& checkers_,
        Array<mc::Control::Client>&& controls_
    )
        : count(checkers_.size())
        , checkings(kj::mv(checkers_))
        , controls(kj::mv(controls_))
        , timeoutPromise(Promise<void>(kj::READY_NOW))
    {
    }

    ~ModelCheckingImpl() {
    }

    kj::Promise<void> solve(SolveContext context) override {
        if (!count) return kj::READY_NOW;

        currentTimeout = TIMEOUT;
        solvedCount = 0;

        timeoutPromise = promiseTimeout(currentTimeout);

        auto promiseBuilder = heapArrayBuilder<Promise<void>>(count);

        auto myResponse = context.getResults();
        auto myMeta = myResponse.initMeta(count);

        for (uint i = 0; i < count; i++) {
            promiseBuilder.add(checkings[i].solveRequest().send()
                .then([i, myMeta, this] (auto result) mutable {
                    auto oneResult = myMeta[i];

                    uint32_t t = result.getTime();
                    bool cancelled = result.getCancelled();

                    oneResult.setTime(t);
                    oneResult.setCancelled(cancelled);
                    std::cerr << t << "\n";
                    oneResult.setMeta(result.getMeta());

                    if (cancelled) {
                        std::cerr << "timed out\n";
                        return;
                    }

                    solvedCount++;

                    if (solvedCount == count) {
                        timeoutPromise = Promise<void>(kj::READY_NOW);
                        return;
                    }

                    uint32_t new_timeout = std::max(t * 3 + 5000u, 15000u);
                    if (new_timeout < currentTimeout) {
                        currentTimeout = new_timeout;
                        timeoutPromise = promiseTimeout(t * 3 + 5000u - t < 15000u ? 15000u - t : t * 3 + 5000u - t);
                    }
                }).eagerlyEvaluate([i, myMeta](auto _) mutable {
                  myMeta[i].setCancelled(true);
                  std::cerr << "err1\n";
                })
            );
        }

        auto tic = chrono::steady_clock::now();
        return kj::joinPromises(promiseBuilder.finish())
            .then([myResponse, tic]() mutable {
                chrono::nanoseconds timediff = tic - chrono::steady_clock::now();
                myResponse.setTime(timediff.count() / 1000000);
                myResponse.setCancelled(false);
            })
            .catch_([](auto _) {std::cerr << "err3\n";});
    }

private:
    Promise<void> promiseTimeout(uint32_t time) {
        return ioProvider->getTimer()
        .afterDelay(time * kj::MILLISECONDS)
        .then([&]() mutable {
            std::cerr << "timed out\n";
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
        })
        .catch_([](auto _) {std::cerr << "err2\n";});
    }
};

class ModelCheckerImpl final: public Multichecker::Server {
    Array<EzRpcClient> rpc_clients;
    Array<SingleChecker::Client> clients;

public:
    ModelCheckerImpl(const Array<char *> &addrs) {
        {
            auto builder = heapArrayBuilder<EzRpcClient>(addrs.size());
            for (auto addr: addrs) {
                builder.add(addr);
            }
            rpc_clients = builder.finish();
        }
        {
            auto builder = heapArrayBuilder<SingleChecker::Client>(addrs.size());
            for (auto &rpc_client: rpc_clients) {
                builder.add(rpc_client.getMain<SingleChecker>());
            }
            clients = builder.finish();
        }
    }

    Promise<void> load(LoadContext context) override {
        auto checkings = heapArrayBuilder<SingleChecking::Client>(clients.size());
        auto control_promises = heapArrayBuilder<Promise<void>>(clients.size());
        auto controls = heapArrayBuilder<mc::Control::Client>(clients.size());

        int i = 0;
        for (auto &client: clients) {
            auto load_req = client.loadRequest();
            load_req.setModel(context.getParams().getModel()[i].getData());
            auto checking = load_req.send().getChecking();
            auto control_promise = checking.getControlRequest().send();
            auto control = control_promise.getControl();

            checkings.add(kj::mv(checking));
            control_promises.add(control_promise.ignoreResult());
            controls.add(kj::mv(control));

            i++;
        }

        context.getResults().setChecking(
            kj::heap<ModelCheckingImpl>(checkings.finish(), controls.finish()));

        return kj::joinPromises(control_promises.finish())
            .catch_([](auto _) {std::cerr << "err4\n";});
    }
};

int main(int argc, char* argv[]) {
    int port = atoi(argv[1]);
    TIMEOUT = atoi(argv[2]);
    Array<char *> addrs = kj::heapArray(&argv[3], argc - 3);
    capnp::EzRpcServer server(
        kj::heap<ModelCheckerImpl>(addrs),
        "0.0.0.0",
        port
    );
    ioProvider = &server.getIoProvider();
    kj::NEVER_DONE.wait(server.getWaitScope());
    return 0;
}
