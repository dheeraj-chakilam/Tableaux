/**
* Created by Dheeraj on 11/19/2016.
*/

import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import java.io._

sealed trait Expr
case class Literal(pVar: Char) extends Expr
case class Not(e: Expr) extends Expr
case class And(e1: Expr, e2: Expr) extends Expr
case class Or(e1: Expr, e2: Expr) extends Expr
case class Impl(e1: Expr, e2: Expr) extends Expr

class Node(p1: Expr, p2: List[Node], p3: Boolean, p4: Node, p5: Int, p6: Int) {
  var e: Expr = p1
  var children: List[Node] = p2
  var env: Map[Literal, Boolean] = Map()
  var show: Boolean = p3
  var godparent: Node = p4
  var isContra: Boolean = false
  var nodeID: Int = p5
  var stepID: Int = p6
}

object Tableaux {

  var pQ: Queue[Node] = null
  var root: Node = null

  var node_counter = 1
  var step_counter = 0

  var explanations: Map[Int, String] = Map()

  def init(e: Expr) = {
    root = new Node(e, Nil, false, null, node_counter, step_counter)
    node_counter = node_counter + 1
    step_counter = step_counter + 1
    pQ = new Queue[Node]
    pQ.enqueue(root)
    explanations += (0 -> ("To check if " + eToStr(e)  + " is a tautology, we will try to falsify it."))
  }

  def postProcess(n: Node): Unit = {
    var numNodes: Int = 0
    var nodeIDSet: List[Int] = Nil
    var queue: Queue[Node] = new Queue[Node]
    queue.enqueue(n)

    while(!queue.isEmpty){
      var head : Node = queue.dequeue
      nodeIDSet ::= head.nodeID
      numNodes = numNodes + 1
      head.children.foreach(queue.enqueue(_))
    }
    nodeIDSet = nodeIDSet.sorted
    var nodeIDConversion: Map[Int, Int] = Map()
    for (i <- 0 to (numNodes - 1)){
      nodeIDConversion += (nodeIDSet(i) -> (i + 1))
    }

    var queue2: Queue[Node] = new Queue[Node]
    queue2.enqueue(n)
    while(!queue2.isEmpty) {
      var head: Node = queue2.dequeue
      head.nodeID = nodeIDConversion(head.nodeID)
      head.children.foreach(queue2.enqueue(_))
    }
  }

  def deepCopy(n: Node): Node = {
    var nn = new Node(n.e, null, n.show, n.godparent, node_counter, n.stepID)
    node_counter = node_counter + 1
    val newChildren = n.children.map(deepCopy)
    nn.children = newChildren
    nn.env = Map[Literal, Boolean]() ++= n.env
    nn
  }

  def deepCopy(nl: List[Node]): List[Node] = {
    nl.map(deepCopy)
  }

  def appendTail(head: Node, nList: List[Node]): Unit = {
    if (head.children.isEmpty) {
      head.children = deepCopy(nList)
    } else {
      head.children.foreach(n => appendTail(n, nList))
    }
  }

  def eval(): Unit = {
    // Step0: Get the head of the queue, if any
    if (pQ.isEmpty) {
      return ()
    }
    val current: Node = pQ.dequeue()

    // Step1: Check for contradictions and update env, children (make empty)
    current.e match {
      case Literal(pVar) if (current.env.contains(Literal(pVar))) =>
        if (current.show != current.env(Literal(pVar))) {
          current.isContra = true
          current.children = Nil
          explanations += (step_counter -> ("We have found a contradiction and thus closed off this branch."))
        }
      case Literal(pVar) =>
        current.env += (Literal(pVar) -> current.show)
        if (current.children.length == 0) {
          explanations += (step_counter -> ("We have found an open branch and thus a falsifying assignment to the " +
            " proposed tautology. For completeness, we will show you how to evaluate the rest of the tree."))
        } else {
          explanations += (step_counter ->
            ("This node cannot be processed any further, so we continue further down this branch."))
        }
      case _ => ;
    }

    // Step2: Apply Tableaux logic to construct and append the consequent godchildren
    var godChildren: List[Node] = Nil
    current.e match {
      case Literal(pVar) => ;
      case Not(e) =>
        godChildren ::= new Node(e, Nil, !current.show, current, -1, step_counter)
        if (current.show){
          explanations += (step_counter -> ("Showing that " + eToStr(Not(e)) +
            " is true is the same as showing that " + eToStr(e) + " is false."))
        }
        else{
          explanations += (step_counter -> ("Showing that " + eToStr(Not(e)) +
            " is false is the same as showing that " + eToStr(e) + " is true."))
        }
      case And(e1, e2) if current.show =>
        val innerNode: Node = new Node(e2, Nil, current.show, current, -1, step_counter)
        godChildren ::= new Node(e1, List(innerNode), current.show, current, -1, step_counter)
        explanations += (step_counter -> ("Showing that " + eToStr(And(e1, e2)) +
          " is true is the same as showing that both " + eToStr(e1) + " and " + eToStr(e2) + " are true."))
      case And(e1, e2) =>
        godChildren ::= new Node(e2, Nil, current.show, current, -1, step_counter)
        godChildren ::= new Node(e1, Nil, current.show, current, -1, step_counter)
        explanations += (step_counter -> ("Showing that " + eToStr(And(e1, e2)) +
          " is false is the same as showing that either " + eToStr(e1) + " or " + eToStr(e2) + " is false."))
      case Or(e1, e2) if current.show =>
        godChildren ::= new Node(e2, Nil, current.show, current, -1, step_counter)
        godChildren ::= new Node(e1, Nil, current.show, current, -1, step_counter)
        explanations += (step_counter -> ("Showing that " + eToStr(Or(e1, e2)) +
          " is true is the same as showing that either " + eToStr(e1) + " or " + eToStr(e2) + " is true."))
      case Or(e1, e2) =>
        val innerNode: Node = new Node(e2, Nil, current.show, current, -1, step_counter)
        godChildren ::= new Node(e1, List(innerNode), current.show, current, -1, step_counter)
        explanations += (step_counter -> ("Showing that " + eToStr(Or(e1, e2)) +
          " is false is the same as showing that both " + eToStr(e1) + " and " + eToStr(e2) + " are false."))
      case Impl(e1, e2) if current.show =>
        godChildren ::= new Node(e1, Nil, !current.show, current, -1, step_counter)
        val innerNode: Node = new Node(e2, Nil, current.show, current, -1, step_counter)
        godChildren ::= new Node(e1, List(innerNode), current.show, current, -1, step_counter)
        explanations += (step_counter -> ("Showing that " + eToStr(Impl(e1, e2)) +
          " is true is the same as showing either of two things. First,  " + eToStr(e1) + " is false. Second, " +
          eToStr(e1) + " and " + eToStr(e2) + "are both true. "))
      case Impl(e1, e2) =>
        val innerNode: Node = new Node(e2, Nil, current.show, current, -1, step_counter)
        godChildren ::= new Node(e1, List(innerNode), !current.show, current, -1, step_counter)
        explanations += (step_counter -> ("Showing that " + eToStr(Impl(e1, e2)) +
          " is false is the same as showing that " + eToStr(e1) + " is true and " + eToStr(e2) + " is false."))
    }

    /* current.e match {
      case Literal(pVar) => ;
      case default => step_counter = step_counter + 1
    } */
    step_counter += 1

    appendTail(current, godChildren)

    // Step3: Copies the current node's environment to its immediate children and adds them to the pQ
    current.children.foreach { n =>
      n.env ++= current.env
      pQ.enqueue(n)
    }

    // Step4: Run eval recursively
    eval()
  }

  def isTautology(n: Node): Boolean = {
    if (n.children.isEmpty) {
      // Is contradiction = Path closed = Tautology
      n.isContra
    } else {
      n.children.map(isTautology).foldLeft(true)(_ && _)
    }
  }

  /* Returns the Expr which matches the input string. Note that there is no assumed order of operations and thus,
  * parentheses must be used around any inner operator, ie: ~(A v B v C) ^ D -> D is NOT okay, but
  * ((~((A v B) v C)) ^ D) -> D is okay*/
  def parse(arg: String): Expr = {
    //Get rid of spaces and change "->" to ">"
    var input: String = arg.replaceAll("-", "")
    input = input.replaceAll(" ", "")

    //Add parentheses around unary operator, ie: ~A becomes (~A)
    for (x <- 'A' to 'Z') {
      input = input.replaceAll("~" + x, "(~" + x + ")")
    }

    return parse_helper(input);
  }

  def parse_helper(arg: String): Expr = {
    //Base Case 1: empty string
    if (arg.length() == 0) {
      throw new IllegalArgumentException("Invalid input")
    }

    //Base Case 2: Single character
    if (arg.length() == 1) {
      if (arg.charAt(0) >= 'A' && arg.charAt(0) <= 'Z') {
        return Literal(arg.charAt(0))
      }
      else {
        throw new IllegalArgumentException("Invalid input")
      }
    }

    //Step 1: find operator with highest precedence (leftmost)
    var num_paren: Int = 0
    var operator: Char = '\u0000'
    var operator_index: Int = 0
    for (i <- 0 to arg.length() - 1) {
      if (arg.charAt(i) == '(') {
        num_paren += 1
      }
      else if (arg.charAt(i) == ')') {
        num_paren -= 1
      }
      else if (num_paren == 0 && (arg.charAt(i) == '~' || arg.charAt(i) == 'v' || arg.charAt(i) == '^' ||
        arg.charAt(i) == '>')) {
        operator = arg.charAt(i)
        operator_index = i
      }
    }

    //Step 2: recursively call parse_helper on the sub-expression(s) of the above operator
    if (operator == '\u0000' && arg.charAt(0) == '(' && arg.charAt(arg.length() - 1) == ')') {
      return parse_helper(arg.substring(1, arg.length() - 1))
    }
    if (operator == '~') {
      return Not(parse_helper(arg.substring(1)))
    }
    if (operator == 'v') {
      return Or(parse_helper(arg.substring(0, operator_index)), parse_helper(arg.substring(operator_index + 1)))
    }
    if (operator == '^') {
      return And(parse_helper(arg.substring(0, operator_index)), parse_helper(arg.substring(operator_index + 1)))
    }
    if (operator == '>') {
      return Impl(parse_helper(arg.substring(0, operator_index)), parse_helper(arg.substring(operator_index + 1)))
    }

    throw new IllegalArgumentException("Invalid input")
  }

  def eToStr(e: Expr): String = {
    e match {
      case Literal(pVar) => pVar + ""
      case Not(e) => "~" + eToStr(e)
      case And(e1, e2) => "(" + eToStr(e1) + " ^ " + eToStr(e2) + ")"
      case Or(e1, e2) => "(" + eToStr(e1) + " v " + eToStr(e2) + ")"
      case Impl(e1, e2) => "(" + eToStr(e1) + " -> " + eToStr(e2) + ")"
    }
  }

  def nodeToJSON(n: Node, parent: String, offset: String, spaces: Int, appendComma: Boolean): String = {

    // First, get the inner offset:
    var innerOffset = offset
    for (x <- 1 to spaces) innerOffset += " "

    // Get the next offset:
    var nextOffset = innerOffset
    for (x <- 1 to spaces) nextOffset += " "

    var json = offset + "{\n"
    json += innerOffset + "\"id\": \"" + n.nodeID + "\",\n"
    json += innerOffset + "\"parent\": \"" + parent + "\",\n"
    json += innerOffset + "\"stepid\": \"" + n.stepID + "\",\n"
    json += innerOffset + "\"expr\": \"" + eToStr(n.e) + "\",\n"
    json += innerOffset + "\"show\": \"" + n.show + "\",\n"
    var godp = "null"
    if (n.godparent != null) godp = n.godparent.nodeID + ""
    json += innerOffset + "\"godparent\": \"" + godp + "\",\n"
    json += innerOffset + "\"isContra\": \"" + n.isContra + "\",\n"
    json += innerOffset + "\"children\": [\n"
    for (x <- 1 to n.children.length) {
      json += nodeToJSON(n.children(x - 1), n.nodeID + "", nextOffset, spaces, x < n.children.length)
    }
    json += innerOffset + "]\n"
    json += offset + "}"
    if (appendComma) json += ","
    json += "\n"

    return json
  }

  def writeTreeToFile(filename: String) = {
    val isTaut = isTautology(root)
    val result = eToStr(root.e) + " is " + isTaut
    println(result)

    var spaces = 2
    var offset = ""
    for (x <- 1 to spaces) offset += " "
    var toWrite = "[\n"
    toWrite += (offset + "{\n")
    toWrite += (offset + offset + "\"result\":" + isTaut + "\n")
    toWrite += (offset + "},\n")
    toWrite += (offset + "{\n")
    var counter = 1;
    for ((k, v) <- explanations) {
      toWrite += (offset + offset + "\"" + k + "\"" + ": \"" + v + "\"")
      if (counter != explanations.size) toWrite += ","
      toWrite += "\n"
      counter += 1
    }
    toWrite += (offset + "},\n")
    toWrite += nodeToJSON(root, "null", offset, spaces, false) // 2-spacing for JSON formatting
    toWrite += "]"

    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(toWrite)
    bw.close()
  }

  def main(args: Array[String]) = {
    val start = System.currentTimeMillis()
    if (args.length == 0) {
      println("Usage: Tableaux \"[expression]\"")
      System.exit(0)
    }
    try {
      val e: Expr = parse(args(0).map(c => if (c != 'v') c.toUpper else c))
      init(e)
    } catch {
      case exc: Exception => {
        println("Invalid input expression!")
        System.exit(0)
      }
    }
    eval()
    postProcess(root)
    writeTreeToFile("treeData.json")
  }
}