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
        src/main.cpp
        src/Interpreter.cpp
        src/Interpreter.h
        src/ParseTree.cpp
        src/ParseTree.h 
        src/Renderer.cpp
        src/Renderer.h
        src/SceneComputer.cpp
        src/SceneComputer.h
        src/SceneModel.cpp 
        src/SceneModel.h src/SyntaxNetLink.cpp src/SyntaxNetLink.h)

find_package(OGRE REQUIRED)

add_executable(DrawyMcDrawface ${SOURCE_FILES})

target_link_libraries(DrawyMcDrawface ${OGRE_LIBRARIES})

add_subdirectory(test)