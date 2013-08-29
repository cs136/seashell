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

# Hacks to get Clang/Compiler-RT to build:
execute_process(COMMAND ln -s "../../clang" "${CMAKE_CURRENT_SOURCE_DIR}/llvm/tools")
execute_process(COMMAND ln -s "../../compiler-rt" "${CMAKE_CURRENT_SOURCE_DIR}/llvm/projects")

# Build our dependencies.
add_subdirectory(llvm)

# libgit2 overrides.
set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS} -fPIC)
add_subdirectory(libgit2)

# Set include paths.
set(LLVM_SOURCE_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/llvm/include
    PARENT_SCOPE)
set(LLVM_BINARY_INCLUDE_DIRS
    ${CMAKE_CURRENT_BINARY_DIR}/llvm/include
    PARENT_SCOPE)
set(CLANG_SOURCE_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/clang/include
    PARENT_SCOPE)
set(CLANG_BINARY_INCLUDE_DIRS
    ${CMAKE_CURRENT_BINARY_DIR}/llvm/tools/clang/include
    PARENT_SCOPE)
set(LIBGIT2_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/libgit2/include
    PARENT_SCOPE)
set(LIBGIT2_LINK_DIRS
    ${CMAKE_CURRENT_BINARY_DIR}/libgit2
    PARENT_SCOPE)
set(LIBGIT2_LIBRARIES
    git2 z crypt
    PARENT_SCOPE)
