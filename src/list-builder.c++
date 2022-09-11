#include <capnp/serialize.h>
#include "automata-safa-capnp/Lib.capnp.h"
#include <fcntl.h>

using automata_safa_capnp::lib::Wrapper;
using capnp::AnyPointer;
using capnp::AnyStruct;
using capnp::List;
using capnp::AnyList;
using capnp::Void;

int main(int argc, char* argv[]) {
  ::capnp::MallocMessageBuilder outmsg;

  Wrapper::Builder root = outmsg.initRoot<Wrapper>();
  auto list = root.getData().initAs<List<Wrapper>>(argc - 2);
  for (int i = 2; i < argc; i++) {
    int fd = open(argv[i], O_RDONLY);
    capnp::StreamFdMessageReader inmsg(fd);
    AnyPointer::Reader elem = inmsg.getRoot<AnyPointer>();
    list[i - 2].getData().setAs<AnyPointer>(elem);
  }

  int fd = open(argv[1], O_WRONLY | O_CREAT);
  capnp::writeMessageToFd(fd, outmsg);

  return 0;
}
