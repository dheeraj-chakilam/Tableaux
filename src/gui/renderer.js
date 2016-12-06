// This file is required by the index.html file and will
// be executed in the renderer process for that window.
// All of the Node.js APIs are available in this process.
const fs = require('fs')
const exec = require('child_process').exec;

const filename = "treeData.json";

var step = -1; // -1 = overview
var numSteps = 0;
var treeObj = null;
var treeString = "";
var mode = 0; // 0 = overview, 1 = step-by-step
var showinstr = 0;

function showInstructions() {
  showinstr = 1;
  document.getElementById("instructions").style.display = "block";
  document.getElementById("instrLink").innerHTML = "hide instructions";
}

function hideInstructions() {
  showinstr = 0;
  document.getElementById("instructions").style.display = "none";
  document.getElementById("instrLink").innerHTML = "show instructions";
}

function toggleInstructions() {
  showinstr == 0 ? showInstructions() : hideInstructions();
}

function updateButtonsMode() {
  if (mode == 0) {
    document.getElementById("sbskey").style.display="none";
    document.getElementById("overviewLink").style.display = "none";
    document.getElementById("stepByStepLink").style.display = "inline-block";
    document.getElementById("nextStepLink").style.display = "none";
    document.getElementById("prevStepLink").style.display = "none";
  } else {
    document.getElementById("sbskey").style.display="inline-block";
    document.getElementById("overviewLink").style.display = "inline-block";
    document.getElementById("stepByStepLink").style.display = "none";
    document.getElementById("nextStepLink").style.display = step < numSteps ? "inline-block" : "none";
    document.getElementById("prevStepLink").style.display = step > 0 ? "inline-block" : "none";
  }
}
document.body.innerHTML += "<div id = \"loading\"><div id =\"spinner-wrap\"><div class=\"spinner\"></div><div id=\"loading-text\">Evaluating expression...</div></div></div>";
updateButtonsMode();

function showLoading() {
  var element = document.getElementById("loading");
  element.style.visibility = "visible";
}

function hideLoading() {
  var element = document.getElementById("loading");
  element.style.visibility = "hidden";
}

function executeTableaux(expression, failurecallback) {
  showLoading();
  exec('scala -classpath "../" Tableaux "'+expression+'"', (error, stdout, stderr) => {
    hideLoading();
    if (error) {
      console.error(`exec error: ${error}`);
      failurecallback("Error: an unknown error occured.")
    } else if (stdout.startsWith("Invalid input expression!")) {
      failurecallback("Error: could not parse the expression "+expression+"!");
    } else {
      document.getElementById("intro").style.display="none";
      document.getElementById("content").style.display="block";
      document.getElementById("error").style.display="none";
      updateTreeData();
    }
  });
}

function showOverview() {
  mode = 0; updateButtonsMode();
  var tree = treeObj[2];
  updateTree(tree);
  document.getElementById("result").innerHTML = (treeObj[2].expr+" is "+treeObj[0].result+".");
}

function decorateNumChildren(n) {
  n.numchildren = n.children.length;
  for (var i = 0; i < n.children.length; i++) decorateNumChildren(n.children[i]);
}

function decorateOpenBranches(n) {
  if (n.isContra == "false" && n.children.length == 0) n.openBranch = true;
  for (var i = 0; i < n.children.length; i++) decorateOpenBranches(n.children[i]);
}

function pruneTree(n) {
  var newchildren = [];
  for (var i = 0; i < n.children.length; i++) {
    if (parseInt(n.children[i].stepid) <= step) {
      pruneTree(n.children[i]);
      newchildren.push(n.children[i]);
    }
  }
  n.processed = parseInt(n.id) < step;
  n.processing = parseInt(n.id) == step;
  n.openBranch = n.isContra == "false" && n.children.length == 0 && n.processed;
  n.isContra = n.isContra == "true" && n.processed ? "true" : "false";
  n.children = newchildren;
}

function showStepByStep() {
  mode = 1; updateButtonsMode();
  // up until step
  var parse = JSON.parse(treeString);
  var filtered = parse[2];
  if (step < 0) step = 0;
  if (step > numSteps) step = numSteps;
  // Remove all subtrees with stepID >  step:
  decorateNumChildren(filtered);
  pruneTree(filtered);
  updateTree(filtered);
  document.getElementById("result").innerHTML = (step == numSteps) ? ((treeObj[0].result ? 
    "All branches have been closed off, hence " : "We have an open branch in the tree, hence ") + 
      (treeObj[2].expr+" is "+treeObj[0].result+".")) : parse[1][step+""];
}

function updateTreeData() {
  svg.selectAll("g.node").remove();
  fs.readFile(filename, 'utf8', function (err, data) {
    if (err) {
      return console.log(err);
    }
    treeString = data;
    treeObj = JSON.parse(data);
    numSteps = Object.keys(treeObj[1]).length;
    decorateOpenBranches(treeObj[2]);
    decorateNumChildren(treeObj[2]);
    console.log(treeObj);
    if (step == -1) {
      showOverview();
    } else {
      showStepByStep();
    }
  });
}

document.getElementById("introExecuteLink").onclick = function() {
  step = -1;
  executeTableaux(document.getElementById("introExpr").value, function(error) {
    document.getElementById("introError").innerHTML = error;
  });
}

document.getElementById("executeLink").onclick = function() {
  step = -1;
  executeTableaux(document.getElementById("expr").value, function(error) {
    document.getElementById("error").style.display="block";
    document.getElementById("error").innerHTML = error;
  });
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

document.getElementById("instrLink").onclick = function() {
  console.log("hey");
  toggleInstructions();
}

// ************** Generate the tree diagram  *****************

var margin = {top: 0, right: 0, bottom: 0, left: 0},
  width = 620 - margin.right - margin.left,
  height = 620 - margin.top - margin.bottom;
  
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
  tree = d3.layout.tree().size([height, width]);
  root = treeData;
  root.x0 = width / 2;
  root.y0 = 0;
  update(root);
}

function circleFill(d) {
  if (d.isContra == "true") return "#000";
  if (d.openBranch) return "#f26246";
  if (d._children) return "lightsteelblue";
  if (d.processed) return "#ccc";
  if (d.processing) return "#5FBA7D";
  return "#fff";
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
    .attr("y", function(d) { return d.numchildren > 0 ? -28 : 28; })
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
