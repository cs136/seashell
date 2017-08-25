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


# Variable needed for backend and collects.
if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
  set(SEASHELL_DEBUG 1)
else() # Release build
  set(SEASHELL_DEBUG 0)
endif(${CMAKE_BUILD_TYPE} STREQUAL "Debug")

# (Racket binary location)
find_program(SEASHELL_RACKET racket)

# Processor count.
if(NOT DEFINED PROCESSOR_COUNT)
  # Unknown:
  set(PROCESSOR_COUNT 0)

  # Linux:
  set(cpuinfo_file "/proc/cpuinfo")
  if(EXISTS "${cpuinfo_file}")
    file(STRINGS "${cpuinfo_file}" procs REGEX "^processor.: [0-9]+$")
    list(LENGTH procs PROCESSOR_COUNT)
  endif()

  # Mac:
  if(APPLE)
    find_program(cmd_sys_pro "system_profiler")
    if(cmd_sys_pro)
      execute_process(COMMAND ${cmd_sys_pro} OUTPUT_VARIABLE info)
      string(REGEX REPLACE "^.*Total Number Of Cores: ([0-9]+).*$" "\\1"
        PROCESSOR_COUNT "${info}")
    endif()
  endif()

  # Windows:
  if(WIN32)
    set(PROCESSOR_COUNT "$ENV{NUMBER_OF_PROCESSORS}")
  endif()
endif()

# Option for controlling how the binaries are built
option(ENABLE_LIGHTWEIGHT_LAUNCHERS "Enable lightweight launcher binaries" OFF)
if (ENABLE_LIGHTWEIGHT_LAUNCHERS)
  set(SEASHELL_LIGHTWEIGHT_LAUNCHERS 1)
else ()
  set(SEASHELL_LIGHTWEIGHT_LAUNCHERS 0)
endif (ENABLE_LIGHTWEIGHT_LAUNCHERS)

# Components
add_subdirectory(backend)
add_subdirectory(collects)
add_subdirectory(frontend)
add_subdirectory(runtime)
add_subdirectory(tests)
