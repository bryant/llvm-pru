NUM_CPU=$(grep ^processor /proc/cpuinfo | wc -l)

echo "Building with $NUM_CPU cpus"

cmake -G Ninja \
    -D BUILD_SHARED_LIBS=ON \
    -D CMAKE_C_COMPILER=clang \
    -D CMAKE_CXX_COMPILER=clang++ \
    -D CMAKE_BUILD_TYPE=Debug \
    -D CMAKE_C_FLAGS=-O0 \
    -D CMAKE_INSTALL_PREFIX=$HOME/usr \
    -D LLVM_TARGETS_TO_BUILD="PRU;ARM;X86;MSP430" \
    -D LLVM_BUILD_RUNTIME=OFF \
    -D LLVM_ENABLE_ASSERTIONS=ON \
    -D LLVM_PARALLEL_COMPILE_JOBS=$NUM_CPU \
    -D LLVM_PARALLEL_LINK_JOBS=$NUM_CPU \
    -D LLVM_TOOL_BUGPOINT_BUILD=OFF \
    -D LLVM_TOOL_GOLD_BUILD=OFF \
    -D LLVM_TOOL_LLI_BUILD=OFF \
    -D LLVM_TOOL_LLVM_AR_BUILD=OFF \
    -D LLVM_TOOL_LLVM_BCANALYZER_BUILD=OFF \
    -D LLVM_TOOL_LLVM_CXXDUMP_BUILD=OFF \
    -D LLVM_TOOL_LLVM_COV_BUILD=OFF \
    -D LLVM_TOOL_LLVM_CONFIG_BUILD=ON \
    -D LLVM_TOOL_LLVM_C_TEST_BUILD=OFF \
    -D LLVM_TOOL_LTO_BUILD=OFF \
    -D LLVM_TOOL_LTO_BUILD=OFF \
    -D LLVM_TOOL_OPT_BUILD=ON \
    -D LLVM_TOOL_BUGPOINT_PASSES_BUILD=OFF \
    -D LLVM_TOOL_DSYMUTIL_BUILD=OFF \
    -D LLVM_TOOL_LLVM_AS_BUILD=OFF \
    -D LLVM_TOOL_LLVM_AS_FUZZER_BUILD=OFF \
    -D LLVM_TOOL_LLVM_DIFF_BUILD=OFF \
    -D LLVM_TOOL_LLVM_DIS_BUILD=OFF \
    -D LLVM_TOOL_LLVM_DWARFDUMP_BUILD=OFF \
    -D LLVM_TOOL_LLVM_EXTRACT_BUILD=OFF \
    -D LLVM_TOOL_LLVM_GO_BUILD=OFF \
    -D LLVM_TOOL_LLVM_JITLISTENER_BUILD=OFF \
    -D LLVM_TOOL_LLVM_LINK_BUILD=OFF \
    -D LLVM_TOOL_LLVM_LTO_BUILD=OFF \
    -D LLVM_TOOL_LLVM_MCMARKUP_BUILD=OFF \
    -D LLVM_TOOL_LLVM_MC_BUILD=OFF \
    -D LLVM_TOOL_LLVM_MC_FUZZER_BUILD=OFF \
    -D LLVM_TOOL_LLVM_NM_BUILD=OFF \
    -D LLVM_TOOL_LLVM_OBJDUMP_BUILD=OFF \
    -D LLVM_TOOL_LLVM_PDBDUMP_BUILD=OFF \
    -D LLVM_TOOL_LLVM_PROFDATA_BUILD=OFF \
    -D LLVM_TOOL_LLVM_READOBJ_BUILD=OFF \
    -D LLVM_TOOL_LLVM_RTDYLD_BUILD=OFF \
    -D LLVM_TOOL_LLVM_SHLIB_BUILD=OFF \
    -D LLVM_TOOL_LLVM_SIZE_BUILD=OFF \
    -D LLVM_TOOL_LLVM_SPLIT_BUILD=OFF \
    -D LLVM_TOOL_LLVM_STRESS_BUILD=OFF \
    -D LLVM_TOOL_LLVM_SYMBOLIZER_BUILD=OFF \
    -D LLVM_TOOL_MSBUILD_BUILD=OFF \
    -D LLVM_TOOL_OBJ2YAML_BUILD=OFF \
    -D LLVM_TOOL_SANCOV_BUILD=OFF \
    -D LLVM_TOOL_VERIFY_USELISTORDER_BUILD=OFF \
    -D LLVM_TOOL_XCODE_TOOLCHAIN_BUILD=OFF \
    -D LLVM_TOOL_YAML2OBJ_BUILD=OFF \
    -D LLVM_TOOL_CLANG_BUILD:BOOL=ON \
    -D CLANG_ENABLE_ARCMT=OFF \
    -D CLANG_TOOL_ARCMT_TEST_BUILD=OFF \
    -D CLANG_TOOL_CLANG_CHECK_BUILD=OFF \
    -D CLANG_TOOL_CLANG_FORMAT_BUILD=OFF \
    -D CLANG_TOOL_CLANG_FORMAT_VS_BUILD=OFF \
    -D CLANG_TOOL_CLANG_FUZZER_BUILD=OFF \
    -D CLANG_TOOL_DIAGTOOL_BUILD=OFF \
    -D CLANG_TOOL_LIBCLANG_BUILD=ON \
    -D CLANG_TOOL_SCAN_BUILD_BUILD=OFF \
    -D CLANG_TOOL_SCAN_BUILD_BUILD=OFF \
    -D CLANG_TOOL_SCAN_VIEW_BUILD=OFF \
    -D CLANG_ENABLE_STATIC_ANALYZER=OFF \
    -D CLANG_PLUGIN_SUPPORT=OFF \
    -D CLANG_INCLUDE_DOCS=OFF \
..
