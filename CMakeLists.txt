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
cmake_minimum_required(VERSION 2.6)

# Build our components.
add_subdirectory(lib)
add_subdirectory(src)

# Package configuration
SET(CPACK_GENERATOR "DEB")
SET(CPACK_PACKAGE_NAME "seashell")
SET(CPACK_PACKAGE_VENDOR "The Seashell Maintainers")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Seashell is an in-browser IDE.")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
SET(CPACK_PACKAGE_VERSION_MAJOR "0")
SET(CPACK_PACKAGE_VERSION_MINOR "0")
SET(CPACK_PACKAGE_VERSION_PATCH "1")
SET(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
SET(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6 (>= 2.4), libgcc1 (>= 1:4.1.1), libstdc++6 (>= 4.2.1), libssh2-1 (>= 1.2.8-2), racket (>= 5.3.4)")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER "Marc Burns <m4burns@csclub.uwaterloo.ca>")
SET(CPACK_DEBIAN_PACKAGE_SECTION "devel")
SET(CPACK_DEBIAN_PACKAGE_PRIORITY "optional")
SET(CPACK_SYSTEM_NAME "debian")
SET(CPACK_PACKAGING_INSTALL_PREFIX "/opt/seashell")
SET(CPACK_STRIP_FILES "bin/*" "lib/*")
SET(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${CMAKE_CURRENT_SOURCE_DIR}/debian/postinst;")

# Hack to get around LLVM's use of CPack.
SET(CPack_CMake_INCLUDED 0)
include(CPack)

