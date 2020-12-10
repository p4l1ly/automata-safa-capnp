#include <capnp/message.h>
#include <capnp/ez-rpc.h>
#include <iostream>
#include <unistd.h>
#include <kj/async-io.h>

#include "automata-safa-capnp/Rpc/ModelChecker.capnp.h"
#include "automata-safa-capnp/Rpc/ModelCheckers.capnp.h"

namespace mc = automata_safa_capnp::rpc::model_checker;
namespace mcs = automata_safa_capnp::rpc::model_checkers;

kj::AsyncIoProvider *ioProvider;

class ControlImpl final: public mc::Control::Server {
    bool* cancel_ptr;

public:
    ControlImpl(bool* cancel_) : cancel_ptr(cancel_) {}

    kj::Promise<void> pause(PauseContext context) override {
        // Not implemented
        return kj::READY_NOW;
    }

    kj::Promise<void> resume(ResumeContext context) override {
        // Not implemented
        return kj::READY_NOW;
    }

    kj::Promise<void> cancel(CancelContext context) override {
        std::cout << "cancel\n";
        *cancel_ptr = true;
        auto status = context.getResults().getOldStatus();
        status.setTime(30);
        status.setState(mc::State::RUNNING);
        return kj::READY_NOW;
    }

    kj::Promise<void> getStatus(GetStatusContext context) override {
        // Not implemented
        return kj::READY_NOW;
    }
};

class ModelCheckingImpl final: public mc::ModelChecking<mcs::Emptiness>::Server {
    bool cancel = false;

public:
    ModelCheckingImpl() {
        std::cout << "create\n";
    }

    kj::Promise<void> solve(SolveContext context) override {
        std::cout << "solve\n";

        kj::MutexGuarded<kj::Maybe<const kj::Executor&>> executor;
        kj::Own<kj::PromiseFulfiller<bool>> fulfiller;

        kj::Thread([&]() noexcept {
            kj::EventLoop loop;
            kj::WaitScope scope(loop);
            *executor.lockExclusive() = kj::getCurrentThreadExecutor();

            auto paf = kj::newPromiseAndFulfiller<bool>();
            fulfiller = kj::mv(paf.fulfiller);
            paf.promise.wait(scope);
        }).detach();

        const kj::Executor *exec;
        {
            auto lock = executor.lockExclusive();
            lock.wait([&](kj::Maybe<const kj::Executor&> value) { return value != nullptr; });
            exec = &KJ_ASSERT_NONNULL(*lock);
        }

        SolveResults::Builder result = context.getResults();

        return exec->executeAsync([result, this, fulfiller{kj::mv(fulfiller)}]() mutable {
            usleep(500000);
            if (cancel) {
                result.setTime(500);
                result.setCancelled(true);
            } else {
                result.setTime(1000);
                result.getMeta().setEmpty(true);
                usleep(500000);
            }
            fulfiller->fulfill(false);
        });
        return ioProvider->getTimer()
            .afterDelay(500 * kj::MILLISECONDS)
            .then([result, this]() mutable {
                if (cancel) {
                    result.setTime(500);
                    result.setCancelled(true);
                    kj::Promise<void> r = kj::READY_NOW;
                    return r;
                } else {
                    result.setTime(1000);
                    result.getMeta().setEmpty(true);
                    kj::Promise<void> r =
                        ioProvider->getTimer().afterDelay(500 * kj::MILLISECONDS);
                    return r;
                }
            });
        return kj::READY_NOW;
    }

    kj::Promise<void> getControl(GetControlContext context) override {
        context.getResults().setControl(kj::heap<ControlImpl>(&cancel));
        return kj::READY_NOW;
    }

    ~ModelCheckingImpl() {
        std::cout << "free\n";
    }
};

class ModelCheckerImpl final: public mc::ModelChecker<mcs::VoidStruct, mcs::Emptiness>::Server {
public:
    ModelCheckerImpl() {}

    kj::Promise<void> load(LoadContext context) override {
        context.getResults().setChecking(kj::heap<ModelCheckingImpl>());
        return kj::READY_NOW;
    }
};

int main() {
    capnp::EzRpcServer server(kj::heap<ModelCheckerImpl>(), "0.0.0.0", 4000);
    ioProvider = &server.getIoProvider();
    auto& waitScope = server.getWaitScope();
    kj::NEVER_DONE.wait(waitScope);
    return 0;
}
