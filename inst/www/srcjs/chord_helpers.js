// Returns an event handler for fading a given chord group.
function fade(opacity) {
  return function(d, i) {
    svg.selectAll("path.chord")
    .filter(function(d) { return d.source.index != i && d.target.index != i; })
    .transition()
    .style("stroke-opacity", opacity)
    .style("fill-opacity", opacity);
  };
}//fade

// Returns an array of tick angles and labels, given a group.
function groupTicks(d) {
  var k = (d.endAngle - d.startAngle) / d.value;
  return d3.range(0, d.value, 10).map(function(v, i) {
    return {
      angle: v * k + d.startAngle,
      label: i % 5 ? null : v
    };
  });
}//groupTicks
