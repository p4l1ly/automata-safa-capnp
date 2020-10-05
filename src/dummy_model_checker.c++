#include <capnp/message.h>
#include <capnp/ez-rpc.h>
#include <iostream>
#include <unistd.h>
#include <kj/async-io.h>

#include "LoadedModelRpc.capnp.h"
#include "DummyModelCheckerRpc.capnp.h"

namespace rpc = automata_safa_capnp::rpc;
namespace srv = automata_safa_capnp::rpc::dummy_model_checker;

class ControlImpl final: public rpc::ModelChecking::Control::Server {
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
        status.setState(rpc::ModelChecking::Status::State::RUNNING);
        return kj::READY_NOW;
    }

    kj::Promise<void> getStatus(GetStatusContext context) override {
        // Not implemented
        return kj::READY_NOW;
    }
};

class ModelCheckingImpl final: public rpc::ModelChecking::Server {
    kj::AsyncIoProvider **ioProvider;
    bool cancel = false;

public:
    ModelCheckingImpl(kj::AsyncIoProvider **ioProvider_) : ioProvider(ioProvider_) {
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

        rpc::ModelChecking::SolveResults::Builder result = context.getResults();

        return exec->executeAsync([result, this, fulfiller{kj::mv(fulfiller)}]() mutable {
            usleep(500000);
            if (cancel) {
                result.setTime(500);
                result.setResult(rpc::ModelChecking::Result::CANCELLED);
            } else {
                result.setTime(1000);
                result.setResult(rpc::ModelChecking::Result::EMPTY);
                usleep(500000);
            }
            fulfiller->fulfill(false);
        });
        return (*ioProvider)->getTimer()
            .afterDelay(500 * kj::MILLISECONDS)
            .then([result, this]() mutable {
                if (cancel) {
                    result.setTime(500);
                    result.setResult(rpc::ModelChecking::Result::CANCELLED);
                    kj::Promise<void> r = kj::READY_NOW;
                    return r;
                } else {
                    result.setTime(1000);
                    result.setResult(rpc::ModelChecking::Result::EMPTY);
                    kj::Promise<void> r =
                        (*ioProvider)->getTimer().afterDelay(500 * kj::MILLISECONDS);
                    return r;
                }
            });
    }

    kj::Promise<void> getControl(GetControlContext context) override {
        context.getResults().setControl(kj::heap<ControlImpl>(&cancel));
        return kj::READY_NOW;
    }

    ~ModelCheckingImpl() {
        std::cout << "free\n";
    }
};

class LoaderImpl final: public srv::Loader::Server {
    kj::AsyncIoProvider **ioProvider;
public:
    LoaderImpl(kj::AsyncIoProvider **ioProvider_) : ioProvider(ioProvider_) {}

    kj::Promise<void> load(LoadContext context) override {
        context.getResults().setChecker(kj::heap<ModelCheckingImpl>(ioProvider));
        return kj::READY_NOW;
    }
};

int main() {
    kj::AsyncIoProvider *ioProvider;
    capnp::EzRpcServer server(kj::heap<LoaderImpl>(&ioProvider), "0.0.0.0", 4000);
    ioProvider = &server.getIoProvider();
    auto& waitScope = server.getWaitScope();
    kj::NEVER_DONE.wait(waitScope);
    return 0;
}
