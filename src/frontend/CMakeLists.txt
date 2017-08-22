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

# This needs to get updated when the build setup changes.
file(GLOB_RECURSE seashell_frontend_src src/*.*)
add_custom_target(seashell-frontend-src-copy
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/src" "${CMAKE_CURRENT_BINARY_DIR}/src"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/typings" "${CMAKE_CURRENT_BINARY_DIR}/typings"
    COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/tsconfig.json" "${CMAKE_CURRENT_BINARY_DIR}/tsconfig.json"
    COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/tslint.json" "${CMAKE_CURRENT_BINARY_DIR}/tslint.json"
    COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/package.json" "${CMAKE_CURRENT_BINARY_DIR}/package.json"
    COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/webpack.config.prod.js" "${CMAKE_CURRENT_BINARY_DIR}/webpack.config.prod.js"
    COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/manifest.json" "${CMAKE_CURRENT_BINARY_DIR}/manifest.json"
    COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/favicon.ico" "${CMAKE_CURRENT_BINARY_DIR}/favicon.ico"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/test" "${CMAKE_CURRENT_BINARY_DIR}/test"
    SOURCES ${seashell_frontend_src} tsconfig.json tslint.json package.json webpack.config.prod.js manifest.json favicon.ico
)
find_program(SEASHELL_NPM npm)
add_custom_target(seashell-frontend-configure
    COMMAND ${SEASHELL_NPM} install
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    DEPENDS seashell-frontend-src-copy)
add_custom_command(OUTPUT dist.tar.gz
    COMMAND ${SEASHELL_NPM} run build -- --env.version=\"${SEASHELL_VERSION}-${GIT_COMMIT_HASH}\"
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    DEPENDS seashell-frontend-configure ${seashell_frontend_src} webpack.config.prod.js tsconfig.json tslint.json package.json manifest.json)
# This is _really_ a dummy target (used for incremental builds).
# During the install step, we copy dist/* -> frontend/*
add_custom_target(seashell-frontend-binary ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/dist.tar.gz)
add_test(seashell-frontend-test ${SEASHELL_NPM} test)
install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/dist/" DESTINATION share/frontend)
