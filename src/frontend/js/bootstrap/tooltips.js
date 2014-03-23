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
