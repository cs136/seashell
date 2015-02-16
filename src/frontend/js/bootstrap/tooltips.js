"use strict";

/**
 * Seashell.
 * Copyright (C) 2013-2015 The Seashell Maintainers.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * Sets the global tooltip properties and attaches a 
 * tooltip to the appropriate divs.
 */
function setupTooltips() {
  var tooltip = $.fn.tooltip.Constructor.DEFAULTS;

  tooltip.placement = 'bottom'; /** sets positioning of the tooltips at the bottom of the div */
  tooltip.delay = { show: 175, hide: 0 } /** sets the delay before the tooltip is displayed/hidden */

  $('[rel="tooltip"]').each(function() {
    $(this).tooltip();
  });
}
