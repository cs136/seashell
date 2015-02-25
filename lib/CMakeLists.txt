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
# Contact someone at seashell@cs.uwaterloo.ca if you're uncertain about what to do
# in this case.

# It's not terribly important to keep LLVM/Clang up to date, but do so
# anyways as it's easier to make incremental changes to the build system.
# We're on 3.4 as of January '14.  Expect 3.5 before the end of the year.
# (At any rate, we should try to keep in sync with the version of clang used in RunC)

# libgit2 should remain relatively stable.

# Hacks to get Clang/Compiler-RT to build:
execute_process(COMMAND ln -sf "../../clang" "${CMAKE_CURRENT_SOURCE_DIR}/llvm/tools")
execute_process(COMMAND ln -sf "../../compiler-rt" "${CMAKE_CURRENT_SOURCE_DIR}/llvm/projects")

# Build our dependencies.

# ZLIB library (libssh2)
find_package(ZLIB)

# OpenSSL (libssh2)
find_package(OpenSSL)

# External projects (not CMake based)
# libtomcrypt - note that we need to call with -fPIC
ExternalProject_Add(
  libtomcrypt
  URL ${CMAKE_CURRENT_SOURCE_DIR}/libtomcrypt
  CONFIGURE_COMMAND ""
  BUILD_IN_SOURCE 1
  BUILD_COMMAND sh -c "CC=${CMAKE_C_COMPILER} CXX=${CMAKE_CXX_COMPILER} CFLAGS='-fPIC' make"
  INSTALL_COMMAND "")

# libssh2 - libssh2 SSH library
ExternalProject_Add(
  libssh2
  URL ${CMAKE_CURRENT_SOURCE_DIR}/libssh2
  PATCH_COMMAND <SOURCE_DIR>/buildconf
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --with-openssl --enable-static --disable-shared
                    CFLAGS=-fPIC CC=${CMAKE_C_COMPILER} CXX=${CMAKE_CXX_COMPILER}
  BUILD_COMMAND make -j
  INSTALL_COMMAND "")

# External projects (CMake based)
# Global overrides 
set(CMAKE_BUILD_TYPE "MinSizeRel")

# llvm/clang/compiler-rt (overrides).
set(LLVM_BUILD_TOOLS OFF CACHE STRING "Build LLVM tools." FORCE)
add_subdirectory(llvm)
add_custom_target(_build_clang ALL)
add_dependencies(_build_clang clang llvm-symbolizer clang-tblgen)
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/llvm/bin/clang-tblgen DESTINATION bin)
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/llvm/bin/llvm-symbolizer DESTINATION bin)

# libgit2 overrides.
# (Libraries) - the set of libraries needs to stay in sync with libssh2.  These are overrides
# for libgit2.
# (Note: As we compile libgit2 as a static library, we don't actually need to pass along the library
# directories).
set(LIBSSH2_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/libssh2/include/)
set(LIBSSH2_LIBRARY "")
set(LIBSSH2_FOUND true)
set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS} -fPIC)
set(THREADSAFE ON)
add_subdirectory(libgit2 EXCLUDE_FROM_ALL)
add_dependencies(git2 libssh2)

# Set include paths.
set(LIBSSH2_LIBRARIES
    ssh2 
    ${CMAKE_DL_LIBS}
    ${OPENSSL_LIBRARIES}
    ${ZLIB_LIBRARIES}
    PARENT_SCOPE)
set(LIBSSH2_LIBRARIES
    ssh2 
    ${CMAKE_DL_LIBS}
    ${OPENSSL_LIBRARIES}
    ${ZLIB_LIBRARIES})
set(LIBSSH2_LINK_DIRS
    ${CMAKE_CURRENT_BINARY_DIR}/libssh2-prefix/src/libssh2-build/src/.libs/
    PARENT_SCOPE)
set(LIBSSH2_LINK_DIRS
    ${CMAKE_CURRENT_BINARY_DIR}/libssh2-prefix/src/libssh2-build/src/.libs/)
set(LIBSSH2_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/libssh2/include/
    PARENT_SCOPE)
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
    ${LIBSSH2_LINK_DIRS}
    PARENT_SCOPE)
set(LIBGIT2_LIBRARIES
    git2 ${ZLIB_LIBRARIES} ${OPENSSL_LIBRARIES} ${LIBSSH2_LIBRARIES}
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
