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
project(seashell-frontend)
cmake_minimum_required(VERSION 2.6)

# Global Definitions
set(SEASHELL_FRONTEND share/frontend)

# Custom build rules
function(html_compile filename)
  get_filename_component(base ${filename} NAME_WE)
  set(base_abs ${CMAKE_CURRENT_BINARY_DIR}/${base})
  set(output ${base_abs}.html)
  add_custom_target(
    ${base} ALL 
    COMMAND ${CMAKE_C_COMPILER} -E -x c -P -C -o ${output} ${filename})
  set_source_files_properties(${output} PROPERTIES GENERATED TRUE)
endfunction(html_compile filename)

html_compile(${CMAKE_CURRENT_SOURCE_DIR}/frontend.html)

# Files to install.
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/frontend.html
  DESTINATION ${SEASHELL_FRONTEND})
install(FILES ${CMAKE_BINARY_DIR}/lib/sjcl-prefix/src/sjcl/sjcl.js
  DESTINATION ${SEASHELL_FRONTEND}/sjcl/)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/css DESTINATION ${SEASHELL_FRONTEND}/css)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/js DESTINATION ${SEASHELL_FRONTEND}/js)
install(DIRECTORY "${CMAKE_SOURCE_DIR}/lib/codemirror/"
  DESTINATION "${SEASHELL_FRONTEND}/codemirror"
  FILES_MATCHING PATTERN "*"
  PATTERN ".git" EXCLUDE
  PATTERN ".gitignore" EXCLUDE)
