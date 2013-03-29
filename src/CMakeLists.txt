project(Seashell)
cmake_minimum_required(VERSION 2.8)

include_directories("${PROJECT_SOURCE_DIR}"
                    "${PROJECT_BINARY_DIR}")

# Offer the user the choice of overriding the installation directories
set(INSTALL_LIB_DIR lib CACHE PATH "Installation directory for libraries")
set(INSTALL_BIN_DIR bin CACHE PATH "Installation directory for executables")

# Make relative paths absolute (needed later on)
foreach(p LIB BIN)
  set(var INSTALL_${p}_DIR)
  if(NOT IS_ABSOLUTE "${${var}}")
    set(${var} "${CMAKE_INSTALL_PREFIX}/${${var}}")
  endif()
endforeach()

add_subdirectory(user)
add_subdirectory(compiler)
