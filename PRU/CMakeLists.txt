macro(gen2 selector out_file)
    set(GEN2 "${CMAKE_SOURCE_DIR}/tablegen/Main.hs"
             "${CMAKE_SOURCE_DIR}/tablegen/Patterns.hs"
             "${CMAKE_SOURCE_DIR}/tablegen/Decls.hs"
             "${CMAKE_SOURCE_DIR}/tablegen/MachNode.hs"
             "${CMAKE_SOURCE_DIR}/tablegen/SDNode.hs")

    add_custom_command(
        OUTPUT "${out_file}"
        COMMAND runghc "Main.hs" "${selector}" "${out_file}"
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/tablegen"
        DEPENDS "${GEN2}")
    set_source_files_properties("${out_file}" PROPERTIES GENERATED 1)
endmacro(gen2)

gen2("decls" "${CMAKE_CURRENT_SOURCE_DIR}/generated/instructions.td")
gen2("pats" "${CMAKE_CURRENT_SOURCE_DIR}/generated/patterns.td")

set(PRU_TD_DIR "${CMAKE_CURRENT_SOURCE_DIR}/tablegen")
set(ALL_PRU_TDS
    "${CMAKE_CURRENT_SOURCE_DIR}/generated/instructions.td"
    "${CMAKE_CURRENT_SOURCE_DIR}/generated/patterns.td"
    "${PRU_TD_DIR}/pru.td"
    "${PRU_TD_DIR}/miscinstrs.td"
    "${PRU_TD_DIR}/optypes.td"
    "${PRU_TD_DIR}/registers.td"
    "${PRU_TD_DIR}/callingconv.td")

# stupid hack needed to add dependency on non-project-root *.tds
add_custom_command(OUTPUT "${PRU_TD_DIR}/main.td"
                   COMMAND ${CMAKE_COMMAND} -E copy "${PRU_TD_DIR}/pru.td"
                                                    "${PRU_TD_DIR}/main.td"
                   DEPENDS ${ALL_PRU_TDS})
set_source_files_properties("${PRU_TD_DIR}/main.td" PROPERTIES GENERATED 1)

set(LLVM_TARGET_DEFINITIONS "${PRU_TD_DIR}/main.td")

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
