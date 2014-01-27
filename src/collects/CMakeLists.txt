# Seashell
# Copyright (C) 2012-2014 The Seashell Maintainers
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
project(seashell-collects)
cmake_minimum_required(VERSION 2.6)

# Global Definitions
set(SEASHELL_COLLECTS share/collects)

# Install the configuration.
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in"
  "${CMAKE_CURRENT_BINARY_DIR}/seashell-config.rkt")

# Racket files to install.
# Note that we handle everything up here to preserve path structure.
# Do NOT install Racket files from a subdirectory.
install(DIRECTORY ./ DESTINATION ${SEASHELL_COLLECTS}
	FILES_MATCHING PATTERN "*.rkt")
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/seashell-config.rkt" DESTINATION
  ${SEASHELL_COLLECTS}/seashell)
install(CODE "execute_process(COMMAND ${SEASHELL_RACKET} -S ${CMAKE_INSTALL_PREFIX}/${SEASHELL_COLLECTS} -l seashell/compile-collects)")
