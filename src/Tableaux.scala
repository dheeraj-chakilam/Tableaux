/**
  * Created by Dheeraj on 11/19/2016.
  */

import scala.collection.mutable.Queue
import scala.collection.mutable.Map

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

  def main(args: Array[String]) = {
    val start = System.currentTimeMillis()
    val a: Expr = Literal('A')
    val b: Expr = Literal('B')
    val e1: Expr = And(a, b)
    val e2: Expr = Or(a, b)
    val e3: Expr = Or(a, Not(a))
    val e4: Expr = Impl(Not(e1),Or(Not(a),Not(b)))
    val e5: Expr = Impl(Impl(And(a,Not(b)),a),e4)
    init(e4)
    eval()
    println(isTautology(root))
    println(System.currentTimeMillis() - start)
  }

}