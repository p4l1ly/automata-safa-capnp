cmake_minimum_required(VERSION 3.17)

project(
  afapipe
  VERSION 0.1.0
  DESCRIPTION "Cooperation of various alternating finite automaton tools"
  LANGUAGES CXX
)

find_package(CapnProto CONFIG REQUIRED)

set(CAPNPC_SRC_PREFIX "${CMAKE_CURRENT_SOURCE_DIR}/schema" CACHE STRING "" FORCE)
set(CAPNPC_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/afa_schema/schema" CACHE STRING "" FORCE)
file(MAKE_DIRECTORY "${CAPNPC_OUTPUT_DIR}")

capnp_generate_cpp(afaSources afaHeaders schema/afa.capnp)

add_executable(bypass src/afapipe/bypass/main.cpp ${afaSources})
target_link_libraries(bypass PRIVATE CapnProto::capnp)
target_include_directories(bypass PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/afa_schema")

add_executable(
  store_little
  src/afapipe/gen_little/store.cpp
  src/afapipe/gen_little/gen.cpp
  ${afaSources}
)
target_link_libraries(store_little PRIVATE CapnProto::capnp)
target_include_directories(store_little PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/afa_schema")
set_property(TARGET store_little PROPERTY CXX_STANDARD 17)
