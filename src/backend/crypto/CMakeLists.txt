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

# Dependencies: in-tree copy of libtomcrypt
include_directories(${LTC_INCLUDE_DIRS})
link_directories(${LTC_LINK_DIRS})

# Main library: seashell-crypto
add_library(seashell-crypto SHARED crypto.c)
target_link_libraries(seashell-crypto ${LTC_LIBRARIES})
add_dependencies(seashell-crypto libtomcrypt)

install(TARGETS seashell-crypto LIBRARY DESTINATION lib)
