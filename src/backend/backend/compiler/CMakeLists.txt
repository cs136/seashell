# Copyright (C) 2012-2013 The Seashell Maintainers
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
set(LLVM_ROOT "${CMAKE_BINARY_DIR}/lib/llvm" CACHE INTERNAL "Root of LLVM install.")

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

# Grab the core LLVM components.
llvm_map_components_to_libraries(REQ_LLVM_LIBRARIES core)

# Link.
add_library(seashell-clang SHARED compiler.cpp)
target_link_libraries(seashell-clang clangFrontend clangSerialization clangDriver
                              clangTooling clangParse clangSema clangAnalysis
                              clangEdit clangAST clangLex clangBasic
                              ${REQ_LLVM_LIBRARIES})
