# Copyright (C) 2012-2014 The Seashell Maintainers.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
project(seashell-clang)

# A convenience variable:
set(LLVM_ROOT "${CMAKE_BINARY_DIR}/lib/llvm")

# A bit of a sanity check:
if( NOT EXISTS ${LLVM_ROOT}/include/llvm )
  message(FATAL_ERROR "LLVM_ROOT (${LLVM_ROOT}) is not a valid LLVM install.  Something's gone horribly wrong.")
endif()

# We incorporate the CMake features provided by LLVM:
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${LLVM_ROOT}/share/llvm/cmake")
include(LLVMConfig)

# Now set the header and library paths:
# Since we're working out of an out-of-tree uninstalled build of LLVM,
# we use our own calculated directories instead of the ones provided
# by LLVMConfig, which assumes that the build is installed.
include_directories( ${LLVM_BINARY_INCLUDE_DIRS} )
include_directories( ${LLVM_SOURCE_INCLUDE_DIRS} )
include_directories( ${CLANG_SOURCE_INCLUDE_DIRS} )
include_directories( ${CLANG_BINARY_INCLUDE_DIRS} )
add_definitions( ${LLVM_DEFINITIONS} )

# Set the required flags
SET(CMAKE_CXX_FLAGS ${LLVM_CXX_COMPILE_FLAGS} ${CMAKE_CXX_FLAGS})

# C++ 11 mode.
set(CMAKE_CXX_FLAGS "-std=c++11 ${CMAKE_CXX_FLAGS}")

# Query for multiarch
execute_process(COMMAND gcc --print-multiarch RESULT_VARIABLE MA_EXIST OUTPUT_VARIABLE MULTIARCH)
string(STRIP "${MULTIARCH}" MULTIARCH)
if ( MA_EXIST EQUAL 0 AND MULTIARCH)
  add_definitions("-DMULTIARCH_PLATFORM=\"${MULTIARCH}\"")
endif()

# Grab the core LLVM components.
# llvm_map_components_to_libnames(REQ_LLVM_LIBRARIES native nativecodegen)
llvm_map_components_to_libraries(REQ_LLVM_LIBRARIES native nativecodegen) ## FIXME remove when we upgrade to clang 3.5

# Link.
add_library(seashell-clang SHARED compiler.cc)
target_link_libraries(seashell-clang clangFrontend clangFrontendTool clangCodeGen
                              clangSerialization clangDriver
                              clangTooling clangParse clangSema clangAnalysis
                              clangEdit clangAST clangLex clangBasic
                              ${REQ_LLVM_LIBRARIES})
add_executable(seashell-test-clang driver.cc)
target_link_libraries(seashell-test-clang seashell-clang)
INSTALL(TARGETS seashell-clang LIBRARY DESTINATION lib)
INSTALL(TARGETS seashell-test-clang RUNTIME DESTINATION bin)
