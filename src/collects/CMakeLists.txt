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
file(GLOB_RECURSE seashell_collects *.rkt)
add_custom_target(seashell-src-copy
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/seashell" "${CMAKE_CURRENT_BINARY_DIR}/seashell"
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/seashell-main.rkt" "${CMAKE_CURRENT_BINARY_DIR}/seashell-main.rkt"
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/seashell-cli.rkt" "${CMAKE_CURRENT_BINARY_DIR}/seashell-cli.rkt"
  SOURCES ${seashell_collects})
file(GLOB_RECURSE seashell_racket_tests "${CMAKE_SOURCE_DIR}/src/tests/*.rkt")
add_custom_target(seashell-racket-tests-copy
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_SOURCE_DIR}/src/tests" "${CMAKE_CURRENT_BINARY_DIR}/racket-tests-src"
  SOURCES ${seashell_racket_tests})
# Install the configuration.
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in"
  "${CMAKE_CURRENT_BINARY_DIR}/seashell/seashell-config.rkt")
# Old copy of login.cgi (for backwards compatibility with UWaterloo CAS)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/login.cgi.in"
  "${CMAKE_CURRENT_BINARY_DIR}/login.cgi")
# New version of login.cgi (to enable resets on any environment)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/login.cgi.in"
  "${CMAKE_CURRENT_BINARY_DIR}/login2.cgi")

# Precompile the Racket sources...
add_custom_target(seashell-racket-compile
  COMMAND ${SEASHELL_RACKET} -S "${CMAKE_CURRENT_BINARY_DIR}" -l- raco/main make -j ${PROCESSOR_COUNT}
    # Main Entry Points
    seashell-main.rkt seashell-cli.rkt
    # Dynamic-Require'd entry points.
    seashell/backend/server.rkt seashell/login/login-gateway.rkt
    seashell/compiler/place-main.rkt
  SOURCES ${seashell_racket_tests} ${seashell_collects} ${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in ${CMAKE_CURRENT_BINARY_DIR}/seashell/seashell-config.rkt
  DEPENDS seashell-src-copy)
add_custom_target(seashell-test-racket-compile
  COMMAND ${SEASHELL_RACKET} -S "${CMAKE_CURRENT_BINARY_DIR}" -l- raco/main make -j ${PROCESSOR_COUNT}
          racket-tests-src/run-tests.rkt
  SOURCES ${seashell_racket_tests} ${seashell_collects} ${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in ${CMAKE_CURRENT_BINARY_DIR}/seashell/seashell-config.rkt
  DEPENDS seashell-racket-compile seashell-racket-tests-copy)


# Build the binaries.
if (NOT SEASHELL_LIGHTWEIGHT_LAUNCHERS)
  add_custom_command(OUTPUT seashell-main
                     COMMAND ${SEASHELL_RACKET} -S "${CMAKE_CURRENT_BINARY_DIR}" -l- raco/main exe -o "${CMAKE_CURRENT_BINARY_DIR}/seashell-main"
                             ++lib seashell/compiler/place-main ++lib seashell/backend/server ++lib seashell/login/login-gateway "${CMAKE_CURRENT_BINARY_DIR}/seashell-main.rkt"
                             DEPENDS ${seashell_collects} ${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in ${CMAKE_CURRENT_SOURCE_DIR}/seashell-main.rkt
                                     ${CMAKE_CURRENT_BINARY_DIR}/seashell/seashell-config.rkt
                                     seashell-racket-compile)
  add_custom_command(OUTPUT seashell-cli
                     COMMAND ${SEASHELL_RACKET} -S "${CMAKE_CURRENT_BINARY_DIR}" -l- raco/main exe -o "${CMAKE_CURRENT_BINARY_DIR}/seashell-cli"
                             ++lib seashell/compiler/place-main ++lib seashell/backend/server "${CMAKE_CURRENT_BINARY_DIR}/seashell-cli.rkt"
                             DEPENDS ${seashell_collects} ${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in ${CMAKE_CURRENT_SOURCE_DIR}/seashell-cli.rkt
                                     ${CMAKE_CURRENT_BINARY_DIR}/seashell/seashell-config.rkt
                                     seashell-racket-compile)
  add_custom_command(OUTPUT seashell-racket-test
                             COMMAND ${SEASHELL_RACKET} -S "${CMAKE_CURRENT_BINARY_DIR}" -l errortrace  -l- raco/main exe -o "${CMAKE_CURRENT_BINARY_DIR}/seashell-racket-test"
                             ++lib seashell/compiler/place-main ++lib errortrace "${CMAKE_CURRENT_BINARY_DIR}/racket-tests-src/run-tests.rkt"
                             DEPENDS ${seashell_collects} ${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in ${CMAKE_SOURCE_DIR}/src/tests/run-tests.rkt
                                     ${CMAKE_CURRENT_BINARY_DIR}/seashell/seashell-config.rkt ${seashell_racket_tests}
                                     seashell-test-racket-compile)
  # Install step: copy over real programs.  NOTE:  Make sure that the destination of raco distribute agrees with the configuration.
  install(CODE "execute_process(COMMAND ${SEASHELL_RACKET} -l- raco/main distribute \"${CMAKE_INSTALL_PREFIX}\" \"${CMAKE_CURRENT_BINARY_DIR}/seashell-main\")")
  install(CODE "execute_process(COMMAND ${SEASHELL_RACKET} -l- raco/main distribute \"${CMAKE_INSTALL_PREFIX}\" \"${CMAKE_CURRENT_BINARY_DIR}/seashell-cli\")")
  add_custom_target(seashell-binary ALL DEPENDS seashell-main seashell-cli seashell-racket-test)
else ()
  set(SEASHELL_COLLECTS share/collects)
  add_custom_command(OUTPUT seashell-racket-test
                             COMMAND ${SEASHELL_RACKET} -l- raco/main exe -l -o "${CMAKE_CURRENT_BINARY_DIR}/seashell-racket-test"
                             --exf-clear ++exf -l ++exf errortrace ++exf -S ++exf "${CMAKE_CURRENT_BINARY_DIR}" ++exf -U ++exf --
                             "${CMAKE_CURRENT_BINARY_DIR}/racket-tests-src/run-tests.rkt"
                             DEPENDS ${seashell_collects} ${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in ${CMAKE_SOURCE_DIR}/src/tests/run-tests.rkt
                                     ${CMAKE_CURRENT_BINARY_DIR}/seashell/seashell-config.rkt ${seashell_racket_tests}
                                     seashell-test-racket-compile)
  add_custom_command(OUTPUT seashell-cli
                             COMMAND ${SEASHELL_RACKET} -l- raco/main exe -l -o "${CMAKE_CURRENT_BINARY_DIR}/seashell-cli"
                             --exf-clear ++exf -l ++exf errortrace ++exf -S ++exf "${CMAKE_CURRENT_BINARY_DIR}" ++exf -U ++exf --
                             "${CMAKE_CURRENT_BINARY_DIR}/seashell-cli.rkt"
                             DEPENDS ${seashell_collects} ${CMAKE_CURRENT_SOURCE_DIR}/seashell-config.rkt.in ${CMAKE_CURRENT_SOURCE_DIR}/seashell-cli.rkt
                                     ${CMAKE_CURRENT_BINARY_DIR}/seashell/seashell-config.rkt
                                     seashell-racket-compile)
  add_custom_target(seashell-binary ALL DEPENDS seashell-racket-test seashell-cli)
  # Install step: copy over real programs.  NOTE:  Make sure that the destination of raco distribute agrees with the configuration.
  install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/ DESTINATION ${SEASHELL_COLLECTS}
    FILES_MATCHING
      PATTERN "CMakeFiles" EXCLUDE
      PATTERN "racket-tests-src" EXCLUDE
      PATTERN "*.dep" PATTERN "*.rkt" PATTERN "*.zo")
  install(CODE "execute_process(COMMAND ${SEASHELL_RACKET} -l- raco/main exe -l -o \"${CMAKE_INSTALL_PREFIX}/bin/seashell-main\" --exf-clear ++exf -S ++exf \"${CMAKE_INSTALL_PREFIX}/${SEASHELL_COLLECTS}\"  ++exf -U ++exf -- \"${CMAKE_INSTALL_PREFIX}/share/collects/seashell-main.rkt\")")
  install(CODE "execute_process(COMMAND ${SEASHELL_RACKET} -l- raco/main exe -l -o \"${CMAKE_INSTALL_PREFIX}/bin/seashell-cli\" --exf-clear ++exf -S ++exf \"${CMAKE_INSTALL_PREFIX}/${SEASHELL_COLLECTS}\"  ++exf -U ++exf -- \"${CMAKE_INSTALL_PREFIX}/share/collects/seashell-cli.rkt\")")
endif(NOT SEASHELL_LIGHTWEIGHT_LAUNCHERS)

add_test(seashell-racket-test "${CMAKE_CURRENT_BINARY_DIR}/seashell-racket-test")
# Install step.
install(PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/login.cgi" DESTINATION cgi-bin)
install(PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/login2.cgi" DESTINATION cgi-bin)
