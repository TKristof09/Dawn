cmake_minimum_required(VERSION 3.28)

project(Dawn)
# set(CMAKE_EXPORT_COMPILE_COMMANDS ON) clangd doesn't work with c++23 compile commands, have to use .clangd file
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
add_executable(Dawn)
file(GLOB_RECURSE sources ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp)
list(REMOVE_ITEM sources ${CMAKE_CURRENT_SOURCE_DIR}/src/TestRunner.cpp)
target_sources(Dawn
    PUBLIC
    ${sources}
)
target_include_directories(Dawn
    PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/src
)

add_executable(DawnTest)
target_sources(DawnTest
    PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/src/TestRunner.cpp
)
