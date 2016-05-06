cmake_minimum_required(VERSION 2.8.12.2)

macro(git_clone repo branch dest)
    execute_process(COMMAND git clone --depth=1 -b ${branch} ${repo} ${dest})
endmacro(git_clone)

macro(patch workdir patchfile)
    execute_process(COMMAND patch -d ${workdir} -p1
                    INPUT_FILE ${patchfile}
                    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
endmacro(patch)

macro(symlink workdir src dest)
    execute_process(
        COMMAND "${CMAKE_COMMAND}" "-E" "create_symlink" "${src}" "${dest}"
        WORKING_DIRECTORY "${workdir}")
endmacro(symlink)

set(LLVM_RELEASE "release_38")
set(LLVM_SOURCE_DIR "${CMAKE_SOURCE_DIR}/third/llvm")
set(COMPILER_RT_SOURCE_DIR "${CMAKE_SOURCE_DIR}/third/compiler-rt")
# setting below also informs llvm's cmake of clang's whereabouts
set(LLVM_EXTERNAL_CLANG_SOURCE_DIR "${CMAKE_SOURCE_DIR}/third/clang")
set(GIT_ORIGIN "git://github.com/llvm-mirror" CACHE STRING "")

if(NOT EXISTS "${LLVM_SOURCE_DIR}/.git")
    git_clone("${GIT_ORIGIN}/llvm" ${LLVM_RELEASE} ${LLVM_SOURCE_DIR})
    patch(${LLVM_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/patches/llvm_release_38.patch)
endif()

if(NOT EXISTS "${LLVM_EXTERNAL_CLANG_SOURCE_DIR}/.git")
    git_clone("${GIT_ORIGIN}/clang" ${LLVM_RELEASE}
              ${LLVM_EXTERNAL_CLANG_SOURCE_DIR})
    patch(${LLVM_EXTERNAL_CLANG_SOURCE_DIR}
          ${CMAKE_SOURCE_DIR}/patches/clang_release_38.patch)
endif()

if(NOT EXISTS "${COMPILER_RT_SOURCE_DIR}/.git")
    git_clone("${GIT_ORIGIN}/compiler-rt" ${LLVM_RELEASE}
              ${COMPILER_RT_SOURCE_DIR})
    symlink("${LLVM_SOURCE_DIR}/projects" ${COMPILER_RT_SOURCE_DIR}
            "compiler-rt")
endif()

symlink("${LLVM_SOURCE_DIR}/lib/Target" "${CMAKE_SOURCE_DIR}/PRU" "PRU")

add_subdirectory("${LLVM_SOURCE_DIR}" "${CMAKE_BINARY_DIR}/llvm")

configure_file("${CMAKE_SOURCE_DIR}/test/lit.cfg.in"
    "${CMAKE_SOURCE_DIR}/test/lit.cfg")

macro(gen2 selector out_file)
    set(GEN2 "${CMAKE_SOURCE_DIR}/scripts/gen2.hs")

    add_custom_command(
        OUTPUT "${out_file}"
        COMMAND runghc ${GEN2} "${selector}" > "${out_file}")
endmacro(gen2)

set(PRU_GENERATED_DIR "${CMAKE_SOURCE_DIR}/PRU/generated")
gen2("decls" "${PRU_GENERATED_DIR}/instructions.td")
gen2("pats" "${PRU_GENERATED_DIR}/patterns.td")
gen2("reverse" "${PRU_GENERATED_DIR}/reversed_branches.h")
gen2("selectcc" "${PRU_GENERATED_DIR}/select_to_branch.h")

add_custom_target(generated
    DEPENDS "${PRU_GENERATED_DIR}/instructions.td"
            "${PRU_GENERATED_DIR}/patterns.td"
            "${PRU_GENERATED_DIR}/reversed_branches.h"
            "${PRU_GENERATED_DIR}/select_to_branch.h")
add_dependencies(LLVMPRUCodeGen generated)
