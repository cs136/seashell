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

# Grab our dependencies.
file(GLOB_RECURSE seashell_collects "${CMAKE_CURRENT_SOURCE_DIR}" *.rkt)
# Copy the source directory to the build directory.
file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/seashell" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}"
     FILES_MATCHING PATTERN *.rkt)
# Install the configuration.
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in"
  "${CMAKE_CURRENT_BINARY_DIR}/seashell/seashell-config.rkt")
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/login.cgi.in"
  "${CMAKE_CURRENT_BINARY_DIR}/login.cgi")
# Build the binaries.
add_custom_command(OUTPUT seashell-main
                   COMMAND ${SEASHELL_RACKET} -S "${CMAKE_CURRENT_BINARY_DIR}" -l- raco/main exe -o "${CMAKE_CURRENT_BINARY_DIR}/seashell-main"
                           ++lib seashell/compiler/place-main ++lib seashell/git/place-main "${CMAKE_CURRENT_SOURCE_DIR}/seashell-main.rkt"
                           DEPENDS ${seashell_collects} ${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in ${CMAKE_CURRENT_SOURCE_DIR}/seashell-main.rkt)
add_custom_target(seashell-binary ALL DEPENDS seashell-main)
# Install step.
# First add helper scripts.
install(PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/login.cgi" DESTINATION cgi-bin)
# Then copy over real programs.
install(CODE "execute_process(COMMAND ${SEASHELL_RACKET} -l- raco/main distribute \"${CMAKE_INSTALL_PREFIX}\" \"${CMAKE_CURRENT_BINARY_DIR}/seashell-main\")")
