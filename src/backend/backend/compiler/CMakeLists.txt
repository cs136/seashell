# Now set the header and library paths:
include_directories( ${LLVM_INCLUDE_DIRS} )
link_directories( ${LLVM_LIBRARY_DIRS} )
add_definitions( ${LLVM_DEFINITIONS} )

llvm_map_components_to_libraries(REQ_LLVM_LIBRARIES core)

add_executable(ssclang compiler.cpp)
target_link_libraries(ssclang clangFrontend clangSerialization clangDriver
                              clangTooling clangParse clangSema clangAnalysis
                              clangEdit clangAST clangLex clangBasic
                              ${REQ_LLVM_LIBRARIES})
