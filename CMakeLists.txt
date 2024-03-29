cmake_minimum_required(VERSION 3.16)

project(
  automata-safa-capnp
  VERSION 0.1.0
  DESCRIPTION "Remote procedure call schema for symbolic alternating finite automata"
  LANGUAGES CXX
)

find_package(CapnProto CONFIG REQUIRED)

set(CAPNPC_SRC_PREFIX "${CMAKE_CURRENT_SOURCE_DIR}/schema" CACHE STRING "" FORCE)
set(CAPNPC_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/gensrc/automata-safa-capnp" CACHE STRING "" FORCE)
file(MAKE_DIRECTORY "${CAPNPC_OUTPUT_DIR}")

capnp_generate_cpp(
  afaSources afaHeaders
  schema/Lib.capnp
  schema/Afa/Model/Succinct.capnp
  schema/Afa/Model/Term.capnp
  schema/Afa/Model/CnfAfa.capnp
  schema/Afa/Model/Separated.capnp
  schema/Afa/Rpc/ModelChecker.capnp
  schema/Afa/Rpc/ModelCheckers.capnp
)

add_library(automata-safa-capnp ${afaSources})
target_link_libraries(automata-safa-capnp PUBLIC CapnProto::capnp)
set_target_properties(
  automata-safa-capnp
  PROPERTIES
  CXX_STANDARD 17
)

include(GNUInstallDirs)
install(TARGETS automata-safa-capnp)
install(
  DIRECTORY "${CMAKE_BINARY_DIR}/gensrc/"
  DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
  FILES_MATCHING PATTERN "*.h"
)

# add_executable(dummy-model-checker src/dummy_model_checker.c++)
# target_include_directories(dummy-model-checker PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/gensrc")
# target_link_libraries(
#   dummy-model-checker
#   PRIVATE
#   CapnProto::capnp
#   CapnProto::capnp-rpc
#   automata-safa-capnp
# )
# set_target_properties(dummy-model-checker PROPERTIES CXX_STANDARD 17)
# 
# add_executable(dummy-model-checker-client src/dummy_model_checker_client.c++)
# target_include_directories(dummy-model-checker-client PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/gensrc")
# target_link_libraries(
#   dummy-model-checker-client
#   PRIVATE
#   CapnProto::capnp
#   CapnProto::capnp-rpc
#   automata-safa-capnp
# )
# set_target_properties(dummy-model-checker-client PROPERTIES CXX_STANDARD 17)
# 
# add_executable(multisolver src/multisolver.c++)
# target_include_directories(multisolver PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/gensrc")
# target_link_libraries(
#   multisolver
#   PRIVATE
#   CapnProto::capnp
#   CapnProto::capnp-rpc
#   automata-safa-capnp
# )
# set_target_properties(multisolver PROPERTIES CXX_STANDARD 17)
# 
# add_executable(file-solver src/file-solver.c++)
# target_include_directories(file-solver PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/gensrc")
# target_link_libraries(
#   file-solver
#   PRIVATE
#   CapnProto::capnp
#   CapnProto::capnp-rpc
#   automata-safa-capnp
# )
# set_target_properties(file-solver PROPERTIES CXX_STANDARD 17)
# 
# add_executable(list-builder src/list-builder.c++)
# target_include_directories(list-builder PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/gensrc")
# target_link_libraries(
#   list-builder
#   PRIVATE
#   CapnProto::capnp
#   automata-safa-capnp
# )
# set_target_properties(file-solver PROPERTIES CXX_STANDARD 17)
