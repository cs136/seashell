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

# Copy the source directory to the build directory.
file(COPY seashell DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
file(GLOB_RECURSE seashell_collects "${CMAKE_CURRENT_SOURCE_DIR}" *.rkt)
# Install the configuration.
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in"
  "${CMAKE_CURRENT_BINARY_DIR}/seashell/seashell-config.rkt")
# Build the binaries.
add_custom_command(OUTPUT login.cgi
                   COMMAND ${SEASHELL_RACKET} -S "${CMAKE_CURRENT_BINARY_DIR}" -l- raco/main exe -o "${CMAKE_CURRENT_BINARY_DIR}/login.cgi"
                           "${CMAKE_CURRENT_SOURCE_DIR}/login.cgi.rkt"
                   DEPENDS ${seashell_collects})
add_custom_command(OUTPUT seashell-user-launcher
                   COMMAND ${SEASHELL_RACKET} -S "${CMAKE_CURRENT_BINARY_DIR}" -l- raco/main exe -o "${CMAKE_CURRENT_BINARY_DIR}/seashell-user-launcher"
                           ++lib seashell/compiler/place-main ++lib seashell/git/place-main ${CMAKE_CURRENT_SOURCE_DIR}/seashell-user-launcher.rkt
                   DEPENDS ${seashell_collects})
add_custom_target(seashell-all-collects ALL DEPENDS login.cgi seashell-user-launcher)
# Install step.
# First add files to location.
install(PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/login.cgi" DESTINATION cgi-bin)
install(PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/seashell-user-launcher" DESTINATION bin)
# Then copy over real programs and overwrite.
install(CODE "execute_process(COMMAND ${SEASHELL_RACKET} -l- raco/main distribute \"${CMAKE_INSTALL_PREFIX}\" \"${CMAKE_CURRENT_BINARY_DIR}/login.cgi\")")
install(CODE "execute_process(COMMAND ${SEASHELL_RACKET} -l- raco/main distribute \"${CMAKE_INSTALL_PREFIX}\" \"${CMAKE_CURRENT_BINARY_DIR}/seashell-user-launcher\")")
install(CODE "file(RENAME \"${CMAKE_INSTALL_PREFIX}/bin/login.cgi\" \"${CMAKE_INSTALL_PREFIX}/cgi-bin/login.cgi\")")
