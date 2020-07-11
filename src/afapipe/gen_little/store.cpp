#include <utility>
#include <capnp/serialize-packed.h>
#include "gen.h"

int main() {
  writePackedMessageToFd(1, *gen());
}
