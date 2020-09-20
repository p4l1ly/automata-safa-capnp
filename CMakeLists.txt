cmake_minimum_required(VERSION 3.17)

project(
  automata-safa-capnp
  VERSION 0.1.0
  DESCRIPTION "Remote procedure call schema for symbolic alternating finite automata"
  LANGUAGES CXX
)

find_package(CapnProto CONFIG REQUIRED)

set(CAPNPC_SRC_PREFIX "${CMAKE_CURRENT_SOURCE_DIR}/schema" CACHE STRING "" FORCE)
set(CAPNPC_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/gensrc" CACHE STRING "" FORCE)
file(MAKE_DIRECTORY "${CAPNPC_OUTPUT_DIR}")

capnp_generate_cpp(
  afaSources afaHeaders
  schema/Afa.capnp schema/CnfAfa.capnp schema/SeparatedAfa.capnp
)

add_library(automata-safa-capnp ${afaSources})
target_link_libraries(automata-safa-capnp PUBLIC CapnProto::capnp)
set_target_properties(
  automata-safa-capnp
  PROPERTIES
  CXX_STANDARD 17
  PUBLIC_HEADER "${afaHeaders}"
)

include(GNUInstallDirs)
install(
  TARGETS automata-safa-capnp
  PUBLIC_HEADER DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/automata-safa-capnp"
)
