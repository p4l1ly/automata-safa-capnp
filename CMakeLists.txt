cmake_minimum_required(VERSION 3.16)

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
  schema/Afa.capnp
  schema/CnfAfa.capnp
  schema/SeparatedAfa.capnp
  schema/LoadedModelRpc.capnp
  schema/CnfAfaRpc.capnp
  schema/SeparatedAfaRpc.capnp
  schema/DummyModelCheckerRpc.capnp
  schema/MultisolverRpc.capnp
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

add_executable(dummy-model-checker src/dummy_model_checker.c++)
target_include_directories(dummy-model-checker PRIVATE "${CAPNPC_OUTPUT_DIR}")
target_link_libraries(
  dummy-model-checker
  PRIVATE
  CapnProto::capnp
  CapnProto::capnp-rpc
  automata-safa-capnp
)
set_target_properties(dummy-model-checker PROPERTIES CXX_STANDARD 17)

add_executable(dummy-model-checker-client src/dummy_model_checker_client.c++)
target_include_directories(dummy-model-checker-client PRIVATE "${CAPNPC_OUTPUT_DIR}")
target_link_libraries(
  dummy-model-checker-client
  PRIVATE
  CapnProto::capnp
  CapnProto::capnp-rpc
  automata-safa-capnp
)
set_target_properties(dummy-model-checker-client PROPERTIES CXX_STANDARD 17)

add_executable(multisolver src/multisolver.c++)
target_include_directories(multisolver PRIVATE "${CAPNPC_OUTPUT_DIR}")
target_link_libraries(
  multisolver
  PRIVATE
  CapnProto::capnp
  CapnProto::capnp-rpc
  automata-safa-capnp
)
set_target_properties(multisolver PROPERTIES CXX_STANDARD 17)

add_executable(file-solver src/file-solver.c++)
target_include_directories(file-solver PRIVATE "${CAPNPC_OUTPUT_DIR}")
target_link_libraries(
  file-solver
  PRIVATE
  CapnProto::capnp
  CapnProto::capnp-rpc
  automata-safa-capnp
)
set_target_properties(file-solver PROPERTIES CXX_STANDARD 17)
