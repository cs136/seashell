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

# Global Definitions
set(SEASHELL_COLLECTS share/collects/seashell-backend)

# Dependencies
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/CMakeModules")

# Configuration file
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.h.in"
  "${CMAKE_CURRENT_BINARY_DIR}/seashell-config.h")
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in"
  "${CMAKE_CURRENT_BINARY_DIR}/seashell-config.rkt")

# Components
add_subdirectory(backend)
add_subdirectory(server)

# Racket files to install. 
# Note that we handle everything up here to preserve path structure.
# Do NOT install Racket files from a subdirectory.
install(DIRECTORY ./ DESTINATION ${SEASHELL_COLLECTS}
	FILES_MATCHING PATTERN "*.rkt")
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/seashell-config.h" DESTINATION include)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/seashell-config.rkt" DESTINATION ${SEASHELL_COLLECTS})
