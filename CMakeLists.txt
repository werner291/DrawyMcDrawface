cmake_minimum_required(VERSION 3.0)
project(DrawyMcDrawface)


if(UNIX)
    if(EXISTS "/usr/local/lib/OGRE/cmake")

        set(CMAKE_MODULE_PATH "/usr/local/lib/OGRE/cmake/;${CMAKE_MODULE_PATH}")
        set(OGRE_SAMPLES_INCLUDEPATH "/usr/local/share/OGRE/samples/Common/include/") # We could just *assume* that developers uses this basepath : /usr/local

    elseif(EXISTS "/usr/lib/OGRE/cmake")

        set(CMAKE_MODULE_PATH "/usr/lib/OGRE/cmake/;${CMAKE_MODULE_PATH}")
        set(OGRE_SAMPLES_INCLUDEPATH "/usr/share/OGRE/samples/Common/include/") # Otherwise, this one

    elseif(EXISTS "/usr/share/OGRE/cmake")

        set(CMAKE_MODULE_PATH "/usr/share/OGRE/cmake/modules;${CMAKE_MODULE_PATH}")
        set(OGRE_SAMPLES_INCLUDEPATH "/usr/share/OGRE/samples/Common/include/") # Otherwise, this one

    else ()
        message(SEND_ERROR "Failed to find module path.")
    endif(EXISTS "/usr/local/lib/OGRE/cmake")
endif(UNIX)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES
        src/Interpreter.cpp
        src/Interpreter.h
        src/ParseTree.cpp
        src/ParseTree.h
        #src/Renderer.cpp
        #src/Renderer.h
        src/workflow.cpp
        src/workflow.h
        src/Knowledge.cpp
        src/Knowledge.h
        #src/SceneComputer.cpp
        #src/SceneComputer.h
        src/askUser.cpp
        src/askUser.h
        src/CompositeModel.cpp
        src/CompositeModel.h
        src/SyntaxNetLink.cpp
        src/SyntaxNetLink.h
        src/SceneCommand.cpp)

find_package(OGRE REQUIRED)

add_library(DrawyMcDrawface_core ${SOURCE_FILES})

add_executable(DrawyMcDrawface src/main.cpp)

target_link_libraries(DrawyMcDrawface DrawyMcDrawface_core ${OGRE_LIBRARIES})

add_subdirectory(test)