# Seashell
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
cmake_minimum_required(VERSION 2.6)

# Versioning Information
# Get the current working branch
execute_process(
  COMMAND git rev-parse --abbrev-ref HEAD
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_BRANCH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Get the latest abbreviated commit hash of the working branch
execute_process(
  COMMAND git log -1 --format=%h
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_COMMIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

set(SEASHELL_VERSION "2.4.1")
set(SEASHELL_API_VERSION "4")

# Variable needed for backend and collects.
if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
  set(SEASHELL_DEBUG 1)
else(${CMAKE_BUILD_TYPE} STREQUAL "Release")
  set(SEASHELL_DEBUG 0)
endif(${CMAKE_BUILD_TYPE} STREQUAL "Debug")

# (Racket binary location)
find_program(SEASHELL_RACKET racket)

# Components
add_subdirectory(backend)
add_subdirectory(collects)
add_subdirectory(frontend)
add_subdirectory(runtime)
add_subdirectory(tests)
