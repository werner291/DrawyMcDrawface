cmake_minimum_required(VERSION 2.8)
project(Test)

add_subdirectory(googletest)

include(CTest)
enable_testing()

set(test_SRCS ParseTreeTest.cpp)

add_executable(runTests gtest.cpp ${test_SRCS})
add_dependencies(runTests googletest)

target_link_libraries(runTests gtest gmock)

set_property(TARGET runTests PROPERTY CXX_STANDARD 11)
set_property(TARGET runTests PROPERTY CXX_STANDARD_REQUIRED ON)