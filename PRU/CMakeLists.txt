set(LLVM_TARGET_DEFINITIONS tablegen/pru.td)

tablegen(LLVM registerinfo.inc -gen-register-info)
tablegen(LLVM instrinfo.inc -gen-instr-info)
tablegen(LLVM asmwriter.inc -gen-asm-writer)
tablegen(LLVM dagisel.inc -gen-dag-isel)
tablegen(LLVM callingconv.inc -gen-callingconv)
tablegen(LLVM subtargetinfo.inc -gen-subtarget)
add_public_tablegen_target(PRUCommonTableGen)

include_directories(BEFORE
    ${CMAKE_CURRENT_BINARY_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/generated)

add_llvm_target(PRUCodeGen
    codegen/isel.cpp
    codegen/targetlowering.cpp
    codegen/instrinfo.cpp
    codegen/framelowering.cpp
    codegen/registerinfo.cpp
    codegen/subtarget.cpp
    codegen/targetmachine.cpp
    codegen/memopcombiner.cpp
    PRUAsmPrinter.cpp
)

add_subdirectory(asmprinter)
add_subdirectory(info)
add_subdirectory(desc)
