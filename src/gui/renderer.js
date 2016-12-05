// This file is required by the index.html file and will
// be executed in the renderer process for that window.
// All of the Node.js APIs are available in this process.
const fs = require('fs')

const fn = "treeData.json";

var step = -1; // -1 = overview
var treeObj = null;
var treeString = "";


function showOverview() {
  updateTree(treeObj[1]);
  document.getElementById("result").innerHTML = (treeObj[1].expr+" is "+treeObj[0].result+"! Here's the proof:");
}

function pruneTree(root) {
  var newchildren = [];
  for (var i = 0; i < root.children.length; i++) {
    if (parseInt(root.children[i].id)-1 <= step) {  // TODO change to step ID
      pruneTree(root.children[i]);
      newchildren.push(root.children[i]);
    }
  }
  root.children = newchildren;
}

function showStepByStep() {
  // up until step
  var filtered = JSON.parse(treeString)[1];
  // Remove all subtrees with stepID >  step:
  pruneTree(filtered);
  updateTree(filtered);
  document.getElementById("result").innerHTML = ("Proof of " + treeObj[1].expr +": Step " + step);
}

function updateTreeData(filename) {
  svg.selectAll("g.node").remove();
  fs.readFile(filename, 'utf8', function (err, data) {
    if (err) {
      return console.log(err);
    }
    treeString = data;
    treeObj = JSON.parse(data);
    if (step == -1) {
      showOverview();
    } else {
      showStepByStep();
    }
  });
}

document.getElementById("refreshLink").onclick = function() {
  step = -1;
  updateTreeData(fn);
}

document.getElementById("overviewLink").onclick = function() {
  step = -1;
  showOverview();
}

document.getElementById("stepByStepLink").onclick = function() {
  step = 0;
  showStepByStep();
}

document.getElementById("nextStepLink").onclick = function() {
  step++;
  showStepByStep();
}

document.getElementById("prevStepLink").onclick = function() {
  step--;
  showStepByStep();
}

// ************** Generate the tree diagram  *****************

var margin = {top: 0, right: 0, bottom: 0, left: 0},
  width = 650 - margin.right - margin.left,
  height = 650 - margin.top - margin.bottom;
  
var i = 0,
  duration = 150,
  root,
  tree,
  diagonal,
  svg;

var diagonal = d3.svg.diagonal()
  .projection(function(d) { return [d.x, d.y]; });

var svg = d3.select("#wrapper").append("svg")
  .attr("width", width + margin.right + margin.left)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform", "translate(" + 0 + "," + 50 + ")")

function moveTreeRight(byX) {
  svg.attr("transform", "translate("+byX+", "+50+")");
}

function updateTree(treeData) {
  console.log(treeData);
  tree = d3.layout.tree().size([height, width]);
  root = treeData;
  root.x0 = width / 2;
  root.y0 = 0;
  update(root);
}
updateTreeData(fn);

function circleFill(d) {
  if (d.isContra == "true") return "#000";
  return d._children ? "lightsteelblue" : "#fff";
}

function getDisplayName(d) {
  var str = d.id+". ";
  if (d.show == "false") {
    str += "F "+d.expr;
  } else {
    str += "T "+d.expr;
  }
  if (d.godparent != "null") str += " (by "+d.godparent+")";
  return str;
}

function update(source) {

  // Compute the new tree layout.
  var nodes = tree.nodes(root).reverse(),
    links = tree.links(nodes);

  // Normalize for fixed-depth.
  var maxDepth = 0;
  for (var i = 0; i < nodes.length; i++) {
    if (nodes[i].depth > maxDepth) {
      maxDepth = nodes[i].depth;
    }
  }
  if (maxDepth > 0 && maxDepth < 4) maxDepth = 4;
  if (maxDepth == 0) maxDepth = height-100;
  nodes.forEach(function(d) { d.y = d.depth * (height-100)/maxDepth; });

  // Update the nodes…
  var node = svg.selectAll("g.node")
    .data(nodes, function(d) { return d.id || (d.id = ++i); });

  // Enter any new nodes at the parent's previous position.
  var nodeEnter = node.enter().append("g")
    .attr("class", "node")
    .attr("transform", function(d) { return "translate(" + source.y0 + "," + source.x0 + ")"; })
    .on("click", click);

  nodeEnter.append("circle")
    .attr("r", 1e-6)
    .style("fill", function(d) { return circleFill(d); });

  nodeEnter.append("text")
    .attr("y", function(d) { return d.children || d._children ? -18 : 18; })
    .attr("dy", ".35em")
    .attr("text-anchor", "middle")
    .text(function(d) { return getDisplayName(d); })
    .style("fill-opacity", 1e-6);

  // Transition nodes to their new position.
  var nodeUpdate = node.transition()
    .duration(duration)
    .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });

  nodeUpdate.select("circle")
    .attr("r", 10)
    .style("fill", function(d) { return circleFill(d); });

  nodeUpdate.select("text")
    .style("fill-opacity", 1);

  // Transition exiting nodes to the parent's new position.
  var nodeExit = node.exit().transition()
    .duration(duration)
    .attr("transform", function(d) { return "translate(" + source.x + "," + source.y + ")"; })
    .remove();

  nodeExit.select("circle")
    .attr("r", 1e-6);

  nodeExit.select("text")
    .style("fill-opacity", 1e-6);

  // Update the links…
  var link = svg.selectAll("path.link")
    .data(links, function(d) { return d.target.id; });

  // Enter any new links at the parent's previous position.
  link.enter().insert("path", "g")
    .attr("class", "link")
    .attr("d", function(d) {
    var o = {x: source.x0, y: source.y0};
    return diagonal({source: o, target: o});
    });

  // Transition links to their new position.
  link.transition()
    .duration(duration)
    .attr("d", diagonal);

  // Transition exiting nodes to the parent's new position.
  link.exit().transition()
    .duration(duration)
    .attr("d", function(d) {
    var o = {x: source.x, y: source.y};
    return diagonal({source: o, target: o});
    })
    .remove();

  // Stash the old positions for transition.
  nodes.forEach(function(d) {
    d.x0 = d.x;
    d.y0 = d.y;
  });
  //moveTreeRight(width/2-root.x);
}

// Toggle children on click.
function click(d) {
  if (d.children) {
  d._children = d.children;
  d.children = null;
  } else {
  d.children = d._children;
  d._children = null;
  }
  update(d);
}
