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
project(seashell-frontend)
cmake_minimum_required(VERSION 2.6)

# Global Definitions
find_program(SEASHELL_MZTEXT mztext)

# Custom build rules
function(html_compile filename)
  get_filename_component(base ${filename} NAME_WE)
  set(base_abs ${CMAKE_CURRENT_BINARY_DIR}/${base})
  set(output ${base_abs}.html)
  add_custom_target(
    ${base} ALL
    COMMAND ${SEASHELL_MZTEXT} -o ${output} ${filename})
  set_source_files_properties(${output} PROPERTIES GENERATED TRUE)
endfunction(html_compile filename)

html_compile(${CMAKE_CURRENT_SOURCE_DIR}/frontend.html)
html_compile(${CMAKE_CURRENT_SOURCE_DIR}/index.html)

# Files to install.
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/index.html
  DESTINATION ${SEASHELL_FRONTEND})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/frontend.html
  DESTINATION ${SEASHELL_FRONTEND})
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/css DESTINATION ${SEASHELL_FRONTEND} FILES_MATCHING PATTERN "*")
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/js DESTINATION ${SEASHELL_FRONTEND} FILES_MATCHING PATTERN "*")
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/img DESTINATION ${SEASHELL_FRONTEND} FILES_MATCHING PATTERN "*")
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/index DESTINATION ${SEASHELL_FRONTEND} FILES_MATCHING PATTERN "*")
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/frontend DESTINATION ${SEASHELL_FRONTEND} FILES_MATCHING PATTERN "*")
