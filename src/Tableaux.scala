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

class Node(a: Expr, b: List[Node], c: Boolean, d: Node) {
  var e: Expr = a
  var children: List[Node] = b
  var env: Map[Literal, Boolean] = Map()
  var show: Boolean = c
  var godparent: Node = d
  var isContra: Boolean = false
  var nodeId: Int = 0
}

object Tableaux {

  var pQ: Queue[Node] = null
  var root: Node = null

  def init(e: Expr) = {
    root = new Node(e, Nil, false, null)
    pQ = new Queue[Node]
    pQ.enqueue(root)
  }

  def deepCopy(n: Node): Node = {
    val newChildren = n.children.map(deepCopy)
    val nn = new Node(n.e, newChildren, n.show, n.godparent)
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
        if (current.show != current.env(Literal(pVar))){
          current.isContra = true
          current.children = Nil
        }
      case Literal(pVar) =>
        current.env += (Literal(pVar) -> current.show)
      case _ => ;
    }

    // Step2: Apply Tableaux logic to construct and append the consequent godchildren
    var godChildren: List[Node] = Nil
    current.e match {
      case Literal(pVar) => ;
      case Not(e) => godChildren ::= new Node(e,Nil,!current.show,current)
      case And(e1, e2) if current.show =>
        val innerNode: Node = new Node(e2,Nil,current.show,current)
        godChildren ::= new Node(e1,List(innerNode),current.show,current)
      case And(e1, e2) =>
        godChildren ::= new Node(e1,Nil,current.show,current)
        godChildren ::= new Node(e2,Nil,current.show,current)
      case Or(e1, e2) if current.show =>
        godChildren ::= new Node(e1,Nil,current.show,current)
        godChildren ::= new Node(e2,Nil,current.show,current)
      case Or(e1, e2) =>
        val innerNode: Node = new Node(e2,Nil,current.show,current)
        godChildren ::= new Node(e1,List(innerNode),current.show,current)
      case Impl(e1, e2) if current.show =>
        godChildren ::= new Node(e1,Nil,!current.show,current)
        val innerNode: Node = new Node(e2,Nil,current.show,current)
        godChildren ::= new Node(e1,List(innerNode),current.show,current)
      case Impl(e1,e2) =>
        val innerNode: Node = new Node(e2,Nil,current.show,current)
        godChildren ::= new Node(e1,List(innerNode),!current.show,current)
    }
    appendTail(current,godChildren)

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
  def parse(arg: String) : Expr = {
    //Get rid of spaces and change "->" to ">"
    var input: String = arg.replaceAll("-", "")
    input = input.replaceAll(" ", "")

    //Add parentheses around unary operator, ie: ~A becomes (~A)
    for (x <- 'A' to 'Z'){
      input = input.replaceAll("~" + x, "(~" + x + ")")
    }

    return parse_helper(input);
  }

  def parse_helper(arg: String) : Expr = {
    //Base Case 1: empty string
    if (arg.length() == 0){
      throw new IllegalArgumentException("Invalid input")
    }
    
    //Base Case 2: Single character
    if (arg.length() == 1){
      if (arg.charAt(0) >= 'A' && arg.charAt(0) <= 'Z'){
        return Literal(arg.charAt(0))
      }
      else{
        throw new IllegalArgumentException("Invalid input")
      }
    }

    //Step 1: find operator with highest precedence (leftmost)
    var num_paren : Int = 0
    var operator : Char = '\u0000'
    var operator_index : Int = 0
    for (i <- 0 to arg.length()-1){
      if (arg.charAt(i) == '('){
        num_paren += 1
      }
      else if (arg.charAt(i) == ')'){
        num_paren -= 1
      }
      else if (num_paren == 0 && (arg.charAt(i) == '~' || arg.charAt(i) == 'v' || arg.charAt(i) == '^' || 
        arg.charAt(i) == '>')){
        operator = arg.charAt(i)
        operator_index = i
      }
    }

    //Step 2: recursively call parse_helper on the sub-expression(s) of the above operator
    if (operator == '\u0000' && arg.charAt(0) == '(' && arg.charAt(arg.length() - 1) == ')'){
      return parse_helper(arg.substring(1, arg.length() - 1))
    }
    if (operator == '~'){
      return Not(parse_helper(arg.substring(1)))
    }
    if (operator == 'v'){
      return Or(parse_helper(arg.substring(0, operator_index)), parse_helper(arg.substring(operator_index + 1)))
    }
    if (operator == '^'){
      return And(parse_helper(arg.substring(0, operator_index)), parse_helper(arg.substring(operator_index + 1)))
    }
    if (operator == '>') {
      return Impl(parse_helper(arg.substring(0, operator_index)), parse_helper(arg.substring(operator_index + 1)))
    }
    
    throw new IllegalArgumentException("Invalid input")
  }

  def assignNodeIds(n: Node, startIndex: Int): Int = {
    n.nodeId = startIndex
    var index = startIndex
    n.children.foreach(child => index = assignNodeIds(child, index+1))
    index
  }

  def exprToString(e: Expr): String = {
    e match {
      case Literal(pVar) => pVar + ""
      case Not(e) => "~" + exprToString(e)
      case And(e1, e2) => "(" + exprToString(e1) + " ^ " + exprToString(e2) + ")"
      case Or(e1, e2) => "(" + exprToString(e1) + " v " + exprToString(e2) + ")"
      case Impl(e1, e2) => "(" + exprToString(e1) + " -> " + exprToString(e2) + ")"
    }
  }

  def nodeToJSON(n: Node, parent: String, offset: String, spaces: Int, appendComma: Boolean): String = {
    
    // First, get the inner offset:
    var innerOffset = offset
    for (x <- 1 to spaces) innerOffset += " "
    
    // Get the next offset:
    var nextOffset = innerOffset
    for (x <- 1 to spaces) nextOffset += " "

    var json = offset+"{\n"
    json += innerOffset+"\"id\": \""+n.nodeId+"\",\n"
    json += innerOffset+"\"parent\": \""+parent+"\",\n"
    json += innerOffset+"\"expr\": \""+exprToString(n.e)+"\",\n"
    json += innerOffset+"\"show\": \""+n.show+"\",\n"
    var godp = "null"
    if (n.godparent != null) godp = n.godparent.nodeId+""
    json += innerOffset+"\"godparent\": \""+godp+"\",\n"
    json += innerOffset+"\"isContra\": \""+n.isContra+"\",\n"
    json += innerOffset+"\"children\": [\n"
    for (x <- 1 to n.children.length) {
      json += nodeToJSON(n.children(x-1), n.nodeId+"", nextOffset, spaces, x < n.children.length)
    }
    json += innerOffset+"]\n"
    json += offset+"}"
    if (appendComma) json += ","
    json += "\n"

    return json
  }

  def writeTreeToFile(filename: String) = {
    val isTaut = isTautology(root)
    val result = exprToString(root.e)+" is "+isTaut
    println(result)

    assignNodeIds(root, 1)

    var spaces = 2
    var offset = ""
    for (x <- 1 to spaces) offset += " "
    var toWrite = "[\n"
    toWrite += (offset+"{\n")
    toWrite += (offset+offset+"\"result\":"+isTaut+"\n")
    toWrite += (offset+"},\n")
    toWrite += nodeToJSON(root, "null", offset, spaces, false) // 2-spacing for JSON formatting
    toWrite += "]"

    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(toWrite)
    bw.close()
  }

  def main(args: Array[String]) = {
    val start = System.currentTimeMillis()
    /*val a: Expr = Literal('A')
    val b: Expr = Literal('B')
    val e1: Expr = And(a, b)
    val e2: Expr = Or(a, b)
    val e3: Expr = Or(a, Not(a))
    val e4: Expr = Impl(Not(e1),Or(Not(a),Not(b)))
    val e5: Expr = Impl(Impl(And(a,Not(b)),a),e4)*/
    if (args.length == 0) {
      println("Usage: Tableaux \"[expression]\"")
      System.exit(0)
    }
    try {
      val e: Expr = parse(args(0))
      init(e)
    } catch {
      case exc: Exception => {
        println("Invalid input expression!")
        System.exit(0)
      }
    }
    eval()
    writeTreeToFile("treeData.json")
    println(System.currentTimeMillis() - start)
  }

}