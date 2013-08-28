# Seashell
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
cmake_minimum_required(VERSION 2.6)

# Hacks to get Clang to build properly.
set(CLANG_BUILD_STANDALONE 1
    CACHE INTERNAL "Build clang as standalone.")
set(CLANG_PATH_TO_LLVM_SOURCE "${CMAKE_CURRENT_SOURCE_DIR}/llvm"
    CACHE INTERNAL "Path to LLVM source directory.")
set(CLANG_PATH_TO_LLVM_BUILD "${CMAKE_CURRENT_BINARY_DIR}/llvm"
    CACHE INTERNAL "Path to LLVM build directory.")

configure_file(
    patches/clang-3.3-CMakeLists.txt
    ${CMAKE_CURRENT_SOURCE_DIR}/lib/clang/CMakeLists.txt
    COPYONLY)

# Build our dependencies.
add_subdirectory(llvm)
add_subdirectory(compiler-rt)
add_subdirectory(clang)
add_subdirectory(libgit2)

# Set include paths.
set(LLVM_INCLUDE_DIRS
    ${CMAKE_BINARY_DIR}/llvm/include
    ${CMAKE_SOURCE_DIR}/llvm/include)
set(CLANG_INCLUDE_DIRS
    ${CMAKE_BINARY_DIR}/clang/include
    ${CMAKE_SOURCE_DIR}/clang/include)
