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

link_directories(${LIBSSH2_LINK_DIRS})
include_directories(${LIBSSH2_INCLUDE_DIR})

# Main library: seashell-tunnel
add_executable(seashell-tunnel tunnel.c)
add_dependencies(seashell-tunnel libssh2)
target_link_libraries(seashell-tunnel ${LIBSSH2_LIBRARIES} ${CMAKE_DL_LIBS} pthread)

install(TARGETS seashell-tunnel RUNTIME DESTINATION bin)
