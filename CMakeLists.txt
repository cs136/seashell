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
project(seashell)

include(CTest)
enable_testing()

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Debug' as none was specified.")
  set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
endif(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)

# Versioning Information
# Get the current working branch
find_program(GIT git)
execute_process(
  COMMAND ${GIT} rev-parse --abbrev-ref HEAD
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_BRANCH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Get the latest abbreviated commit hash of the working branch
execute_process(
  COMMAND ${GIT} log -1 --format=%h
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_COMMIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Run diff-index to check whether the tree is clean or not.
EXECUTE_PROCESS(
  COMMAND ${GIT} diff-index --name-only HEAD
  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
  RESULT_VARIABLE GIT_DIFF_INDEX_RESULT
  OUTPUT_VARIABLE GIT_DIFF_INDEX_OUTPUT
  ERROR_VARIABLE GIT_DIFF_INDEX_ERROR
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )

# Check if the tree is clean.
IF(NOT GIT_DIFF_INDEX_RESULT AND NOT GIT_DIFF_INDEX_OUTPUT)
  SET(GIT_DIRTY False)
ELSE()
  SET(GIT_DIRTY True)
ENDIF()

set(SEASHELL_VERSION "2.5.7")
if(GIT_DIRTY)
  set(GIT_COMMIT_HASH "${GIT_COMMIT_HASH}-dirty")
endif()
set(SEASHELL_API_VERSION "7")

set(SEASHELL_WL_PROJ_BASE_URL "" CACHE STRING "Location of whitelisted project .zips")

# Build our components.
add_subdirectory(doc)
add_subdirectory(lib)
add_subdirectory(src)
