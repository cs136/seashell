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

# Main library: seashell-tunnel
add_executable(seashell-tunnel tunnel.c)

# Dependency: libssh2
find_package(LIBSSH2 REQUIRED)
include_directories(${LIBSSH2_INCLUDE_DIR})
target_link_libraries(seashell-tunnel ${LIBSSH2_LIBRARIES})

install(TARGETS seashell-tunnel RUNTIME DESTINATION bin)
