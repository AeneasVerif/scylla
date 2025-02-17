project(test)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Change path from /src if needed, or add more directories
file(GLOB_RECURSE sources
        "${CMAKE_SOURCE_DIR}/*.c"
        )
# Add precompiler definitions like that:
add_definitions(-DKRML_UNROLL_MAX=0)

add_library(test ${sources})

# Add more include directories if needed
target_include_directories(test PUBLIC "${CMAKE_SOURCE_DIR}/include" "${CMAKE_SOURCE_DIR}/include/minimal")
