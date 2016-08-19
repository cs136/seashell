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
# Global Destinations
set(SEASHELL_FRONTEND share/frontend)
# Global Definitions
find_program(SEASHELL_MZTEXT mztext)
# Copy to build directory.
file(GLOB_RECURSE seashell_js *.js)
file(GLOB_RECURSE seashell_html *.html)
file(GLOB_RECURSE seashell_css *.css)
add_custom_target(seashell-frontend-copy
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_CURRENT_BINARY_DIR}"
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_SOURCE_DIR}/lib/jslib" "${CMAKE_CURRENT_BINARY_DIR}/js/lib"
  SOURCES ${seashell_js} ${seashell_html} ${seashell_css})

find_program(JS_MINIFY uglifyjs)
find_program(CSS_MINIFY uglifycss)

if (JS_MINIFY)
  message("JavaScript compressor found: ${JS_MINIFY}")
  set(JSMIN_ARG "\"${JS_MINIFY}\"")
else()
  set(JSMIN_ARG "")
endif()

if (CSS_MINIFY)
  message("CSS compressor found: ${CSS_MINIFY}")
  set(CSSMIN_ARG "\"${CSS_MINIFY}\"")
else()
  set(CSSMIN_ARG "")
endif()

# Custom build rules
function(html_compile filename)
  get_filename_component(base ${filename} NAME_WE)
  set(base_abs ${CMAKE_CURRENT_BINARY_DIR}/${base})
  set(output ${base_abs}.html)
  add_custom_target(
    ${base} ALL
    COMMAND ${SEASHELL_MZTEXT}
    -E '(require \"build/build-tools.rkt\")'
    -E '(define SEASHELL_API_VERSION ${SEASHELL_API_VERSION})'
    -E '(define SEASHELL_VERSION \"${SEASHELL_VERSION}\")'
    -E '(define SEASHELL_COMMIT \"${GIT_COMMIT_HASH}\")'
    -E '(define SEASHELL_BRANCH \"${GIT_BRANCH}\")'
    -E '(define SEASHELL_DEBUG (= 1 ${SEASHELL_DEBUG}))'
    -E '(define-values (scripts styles) (prepare-compile-file \"${base}\"
          \"${CMAKE_CURRENT_SOURCE_DIR}/build/js.${base}\"
          \"${CMAKE_CURRENT_SOURCE_DIR}/build/style.${base}\"
          \"${CMAKE_CURRENT_SOURCE_DIR}/build/extra.${base}\"
          (not SEASHELL_DEBUG)
          ${JSMIN_ARG} ${CSSMIN_ARG}))'
    -o ${output} ${filename}
    DEPENDS seashell-frontend-copy)
  set_source_files_properties(${output} PROPERTIES GENERATED TRUE)
endfunction(html_compile filename)


html_compile(${CMAKE_CURRENT_SOURCE_DIR}/frontend.html)
html_compile(${CMAKE_CURRENT_SOURCE_DIR}/index.html)

# Lint the source JavaScript
find_program(JS_LINTER jshint)
if (JS_LINTER)
  message("JavaScript linter found: ${JS_LINTER}")
  add_custom_target(_lint ALL COMMAND ${JS_LINTER} --exclude "'js/lib/*'" . WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endif (JS_LINTER)

# Files to install.
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/index.html
  DESTINATION ${SEASHELL_FRONTEND})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/index.manifest
  DESTINATION ${SEASHELL_FRONTEND})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/frontend.html
  DESTINATION ${SEASHELL_FRONTEND})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/frontend.manifest
  DESTINATION ${SEASHELL_FRONTEND})
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/css DESTINATION ${SEASHELL_FRONTEND} FILES_MATCHING PATTERN "*")
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/js DESTINATION ${SEASHELL_FRONTEND} FILES_MATCHING PATTERN "*")
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/img DESTINATION ${SEASHELL_FRONTEND} FILES_MATCHING PATTERN "*")
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/index DESTINATION ${SEASHELL_FRONTEND} FILES_MATCHING PATTERN "*")
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/frontend DESTINATION ${SEASHELL_FRONTEND} FILES_MATCHING PATTERN "*")
if (NOT SEASHELL_DEBUG)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/index.js.map
    DESTINATION ${SEASHELL_FRONTEND})
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/index.min.js
    DESTINATION ${SEASHELL_FRONTEND})
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/frontend.js.map
    DESTINATION ${SEASHELL_FRONTEND})
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/frontend.min.js
    DESTINATION ${SEASHELL_FRONTEND})
endif (SEASHELL_DEBUG)
