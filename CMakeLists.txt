cmake_minimum_required(VERSION 2.8.12.2)

set(LLVM_RELEASE "release_38")
set(LLVM_SOURCE_DIR "${CMAKE_SOURCE_DIR}/third/llvm")
set(COMPILER_RT_SOURCE_DIR "${CMAKE_SOURCE_DIR}/third/compiler-rt")
# setting below also informs llvm's cmake of clang's whereabouts
set(LLVM_EXTERNAL_CLANG_SOURCE_DIR "${CMAKE_SOURCE_DIR}/third/clang")
set(GIT_ORIGIN "git://github.com/llvm-mirror" CACHE STRING "")

if(NOT EXISTS "${LLVM_SOURCE_DIR}/.git")
    execute_process(
        COMMAND git clone --depth=1 -b ${LLVM_RELEASE} ${GIT_ORIGIN}/llvm
                ${LLVM_SOURCE_DIR}
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
    execute_process(
        COMMAND patch -d ${LLVM_SOURCE_DIR} -p1
        INPUT_FILE ${CMAKE_SOURCE_DIR}/patches/llvm_release_38.patch
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
endif()

if(NOT EXISTS "${LLVM_EXTERNAL_CLANG_SOURCE_DIR}/.git")
    execute_process(
        COMMAND git clone --depth=1 -b ${LLVM_RELEASE} ${GIT_ORIGIN}/clang
                ${LLVM_EXTERNAL_CLANG_SOURCE_DIR}
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
    execute_process(
        COMMAND patch -d ${LLVM_EXTERNAL_CLANG_SOURCE_DIR} -p1
        INPUT_FILE ${CMAKE_SOURCE_DIR}/patches/clang_release_38.patch
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
endif()

if(NOT EXISTS "${COMPILER_RT_SOURCE_DIR}/.git")
    execute_process(
        COMMAND git clone --depth=1 -b ${LLVM_RELEASE} ${GIT_ORIGIN}/compiler-rt
                ${COMPILER_RT_SOURCE_DIR}
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
endif()

execute_process(
    COMMAND "${CMAKE_COMMAND}" "-E" "create_symlink"
            "${CMAKE_SOURCE_DIR}/PRU" "PRU"
    WORKING_DIRECTORY "${LLVM_SOURCE_DIR}/lib/Target")

execute_process(
    COMMAND "${CMAKE_COMMAND}" "-E" "create_symlink"
            "${COMPILER_RT_SOURCE_DIR}" "compiler-rt"
            WORKING_DIRECTORY "${LLVM_SOURCE_DIR}/projects")

add_subdirectory("${LLVM_SOURCE_DIR}" "${CMAKE_BINARY_DIR}/llvm")
