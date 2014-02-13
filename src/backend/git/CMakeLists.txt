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
project(seashell-git)

# Now set the header and library paths:
include_directories(${LIBGIT2_INCLUDE_DIRS})
link_directories(${LIBGIT2_LINK_DIRS})

# C++ 11 mode.
set(CMAKE_CXX_FLAGS "-std=c++0x" ${CMAKE_CXX_FLAGS})

# Link.
add_library(seashell-git SHARED git.cc)
target_link_libraries(seashell-git ${LIBGIT2_LIBRARIES})

INSTALL(TARGETS seashell-git LIBRARY DESTINATION lib)
