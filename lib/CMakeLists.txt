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
cmake_minimum_required(VERSION 2.8)
include(ExternalProject)

# Implementor's note - this file will need to be updated when changing
# versions of a library, due to changes in build system, etc.
# Contact someone at seashell@cs.uwaterloo.ca if you're uncertain about to do
# in this case.

# It's not terribly important to keep LLVM/Clang up to date, but do so
# anyways as it's easier to make incremental changes to the build system.
# We're on 3.3 as of August '13.  Expect 3.4 before the end of the year.

# libgit2 should remain relatively stable.

# Hacks to get Clang/Compiler-RT to build:
execute_process(COMMAND ln -sf "../../clang" "${CMAKE_CURRENT_SOURCE_DIR}/llvm/tools")
execute_process(COMMAND ln -sf "../../compiler-rt" "${CMAKE_CURRENT_SOURCE_DIR}/llvm/projects")

# Build our dependencies.

# LLVM overrides.
add_subdirectory(llvm EXCLUDE_FROM_ALL)

# libgit2 overrides.
set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS} -fPIC)
add_subdirectory(libgit2 EXCLUDE_FROM_ALL)

# libtomcrypt - note that we need to call with -fPIC
ExternalProject_Add(
  libtomcrypt
  URL ${CMAKE_CURRENT_SOURCE_DIR}/libtomcrypt
  CONFIGURE_COMMAND ""
  BUILD_IN_SOURCE 1
  BUILD_COMMAND sh -c "CFLAGS='-fPIC -g3 -DLTC_NO_ASM' GCC_34=1 IGNORE_SPEED=1 make -j"
  INSTALL_COMMAND "")

# sjcl - Stanford JavaScript Crypto Library
ExternalProject_Add(
  sjcl
  URL ${CMAKE_CURRENT_SOURCE_DIR}/sjcl
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --compress=none
  BUILD_IN_SOURCE 1
  BUILD_COMMAND make -j
  INSTALL_COMMAND "")

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
set(LTC_INCLUDE_DIRS
    ${CMAKE_CURRENT_BINARY_DIR}/libtomcrypt-prefix/src/libtomcrypt/src/headers
    PARENT_SCOPE)
set(LTC_LINK_DIRS
    ${CMAKE_CURRENT_BINARY_DIR}/libtomcrypt-prefix/src/libtomcrypt/
    PARENT_SCOPE)
set(LTC_LIBRARIES
    tomcrypt
    PARENT_SCOPE)

# Set LLVM CXX Flags
set(LLVM_CXX_COMPILE_FLAGS
    "-fno-rtti"
    PARENT_SCOPE)
