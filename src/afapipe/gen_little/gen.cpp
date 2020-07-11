#include "schema/afa.capnp.h"
#include <capnp/message.h>
#include <capnp/serialize-packed.h>
#include <memory>

using afapipe::schema::Afa;
using afapipe::schema::Term;

std::unique_ptr<::capnp::MallocMessageBuilder> gen() {
  auto message = std::unique_ptr<::capnp::MallocMessageBuilder>(
      new ::capnp::MallocMessageBuilder()
  );

  Afa::Builder afa = message->initRoot<Afa>();
  afa.setVariableCount(1);

  ::capnp::List<::uint32_t>::Builder states = afa.initStates(1);
  states.set(0, 0);

  ::capnp::List<::Term>::Builder terms = afa.initTerms(1);

  Term::Builder t0 = terms[0];
  ::capnp::List<::Term>::Builder and_operands = t0.initAnd(2);
  and_operands[0].setVar(0);
  ::capnp::List<::Term>::Builder or_operands = and_operands[1].initOr(2);
  or_operands[0].setLitTrue();
  or_operands[1].setState(0);

  return message;
}
