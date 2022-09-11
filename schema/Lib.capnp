@0xa28a4a5084e3ade4;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("automata_safa_capnp::lib");

using Java = import "./language-support/java.capnp";
$Java.package("org.automata.safa.capnp");
$Java.outerClassname("Lib");

using Rust = import "./language-support/rust.capnp";
$Rust.parentModule("");

struct Wrapper {
  data @0 :AnyPointer;
}
