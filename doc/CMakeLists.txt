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
project(seashell-docs)

# (Scribble location)
find_program(SCRIBBLE scribble)

# Target location - under frontend
set(SEASHELL_DOC share/frontend)

# Build scribble files.
file(GLOB_RECURSE seashell_doc *.scrbl)
file(GLOB_RECURSE seashell_collects *.rkt)
add_custom_command(OUTPUT docs
  COMMAND ${SCRIBBLE} --htmls --dest-name docs --dest ${CMAKE_CURRENT_BINARY_DIR} +m --redirect-main "http://docs.racket-lang.org/" ++arg "${SEASHELL_VERSION} \\(${GIT_BRANCH}-${GIT_COMMIT_HASH}\\)" ++arg ${SEASHELL_API_VERSION} ${CMAKE_CURRENT_SOURCE_DIR}/main.scrbl
  DEPENDS ${seashell_doc} ${seashell_collects})
add_custom_target(seashell-docs ALL DEPENDS docs)
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/docs DESTINATION ${SEASHELL_DOC})
