cmake_minimum_required(VERSION 3.4)

execute_process(
    COMMAND "grep" "^processor" "/proc/cpuinfo"
    COMMAND "wc" "-l"
    COMMAND "tr" "-d" "'\n'"
    OUTPUT_VARIABLE ncpu
)

message("Building with ${ncpu} cpus")
set(LLVM_PARALLEL_COMPILE_JOBS ${ncpu} CACHE STRING "")
set(LLVM_PARALLEL_LINK_JOBS ${ncpu} CACHE STRING "")

set(BUILD_SHARED_LIBS ON CACHE BOOL "")
set(CMAKE_C_COMPILER "clang" CACHE PATH "")
set(CMAKE_CXX_COMPILER "clang++" CACHE PATH "")
set(CMAKE_LINKER "gold" CACHE PATH "")
set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "")
set(CMAKE_C_FLAGS "-O0" CACHE STRING "")
set(CMAKE_INSTALL_PREFIX "$ENV{HOME}/usr" CACHE PATH "")
set(LLVM_TARGETS_TO_BUILD "PRU;ARM;X86;MSP430" CACHE STRING "")
set(LLVM_BUILD_RUNTIME OFF CACHE BOOL "")
set(LLVM_ENABLE_ASSERTIONS ON CACHE BOOL "")
set(LLVM_TOOL_BUGPOINT_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_GOLD_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLI_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_AR_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_BCANALYZER_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_CXXDUMP_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_COV_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_CONFIG_BUILD ON CACHE BOOL "")
set(LLVM_TOOL_LLVM_C_TEST_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LTO_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LTO_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_OPT_BUILD ON CACHE BOOL "")
set(LLVM_TOOL_BUGPOINT_PASSES_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_DSYMUTIL_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_AS_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_AS_FUZZER_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_DIFF_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_DIS_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_DWARFDUMP_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_EXTRACT_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_GO_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_JITLISTENER_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_LINK_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_LTO_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_MCMARKUP_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_MC_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_MC_FUZZER_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_NM_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_OBJDUMP_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_PDBDUMP_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_PROFDATA_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_READOBJ_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_RTDYLD_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_SHLIB_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_SIZE_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_SPLIT_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_STRESS_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_LLVM_SYMBOLIZER_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_MSBUILD_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_OBJ2YAML_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_SANCOV_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_VERIFY_USELISTORDER_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_XCODE_TOOLCHAIN_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_YAML2OBJ_BUILD OFF CACHE BOOL "")
set(LLVM_TOOL_CLANG_BUILD:BOOL ON CACHE BOOL "")
set(CLANG_ENABLE_ARCMT OFF CACHE BOOL "")
set(CLANG_TOOL_ARCMT_TEST_BUILD OFF CACHE BOOL "")
set(CLANG_TOOL_CLANG_CHECK_BUILD OFF CACHE BOOL "")
set(CLANG_TOOL_CLANG_FORMAT_BUILD OFF CACHE BOOL "")
set(CLANG_TOOL_CLANG_FORMAT_VS_BUILD OFF CACHE BOOL "")
set(CLANG_TOOL_CLANG_FUZZER_BUILD OFF CACHE BOOL "")
set(CLANG_TOOL_DIAGTOOL_BUILD OFF CACHE BOOL "")
set(CLANG_TOOL_LIBCLANG_BUILD ON CACHE BOOL "")
set(CLANG_TOOL_SCAN_BUILD_BUILD OFF CACHE BOOL "")
set(CLANG_TOOL_SCAN_BUILD_BUILD OFF CACHE BOOL "")
set(CLANG_TOOL_SCAN_VIEW_BUILD OFF CACHE BOOL "")
set(CLANG_ENABLE_STATIC_ANALYZER OFF CACHE BOOL "")
set(CLANG_PLUGIN_SUPPORT OFF CACHE BOOL "")
set(CLANG_INCLUDE_DOCS OFF CACHE BOOL "")
