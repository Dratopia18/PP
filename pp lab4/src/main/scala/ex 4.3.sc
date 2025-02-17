trait BTree
case object EmptyTree extends BTree
case class Node(value: Int, left: BTree, right: BTree) extends BTree
//ex 1
def depth(tree: BTree): Int = tree match {
  case EmptyTree => 0
  case Node(_, left, right) => 1 + (depth(left) max depth(right))
}

depth(Node(1, Node(2, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)))
//ex 2
def subtree(tree: BTree): Int = tree match {
  case EmptyTree => 0
  case Node(_, left, right) => 1 + subtree(left) + subtree(right)
}

subtree(Node(1, Node(2, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)))
// ex 3
def evenChildCount(tree: BTree): Int = tree match {
  case EmptyTree => 0
  case Node(_, left, right) => {
    val leftCount = evenChildCount(left)
    val rightCount = evenChildCount(right)
    if ((leftCount + rightCount) % 2 == 0) 1 + leftCount + rightCount
    else leftCount + rightCount
  }
}

evenChildCount(Node(1, Node(2, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)))
// ex 4
def flatten(tree: BTree): List[Int] = tree match {
  case EmptyTree => Nil
  case Node(value, left, right) => value :: flatten(left) ::: flatten(right)
}

flatten(Node(1, Node(2, Node(4, EmptyTree, EmptyTree), Node(5, EmptyTree, EmptyTree)), Node(3, EmptyTree, EmptyTree)))
// ex 5
def countNodes(tree: BTree, cond: Int => Boolean): Int = tree match{
  case EmptyTree => 0
  case Node(value, left, right) => {
    val leftCount = countNodes(left, cond)
    val rightCount = countNodes(right, cond)
    if (cond(value)) 1 + leftCount + rightCount
    else leftCount + rightCount
  }
}

countNodes(Node(1, Node(2, Node(4, EmptyTree, EmptyTree), Node(5, EmptyTree, EmptyTree)), Node(3, EmptyTree, EmptyTree)), _ % 2 == 1)
// ex 6
def mirror(tree: BTree): BTree = tree match {
  case EmptyTree => EmptyTree
  case Node(value, left, right) => Node(value, mirror(right), mirror(left))
}

mirror(Node(1, Node(2, Node(4, EmptyTree, EmptyTree), Node(5, EmptyTree, EmptyTree)), Node(3, EmptyTree, EmptyTree)))
// ex 7
def append(tree1: BTree, tree2: BTree): Option[BTree] = (tree1, tree2) match {
  case (EmptyTree, EmptyTree) => None
  case (EmptyTree, _) => Some(tree2)
  case (_, EmptyTree) => Some(tree1)
  case (Node(value1, left1, right1), Node(value2, left2, right2)) => Some(Node(value1, left1, append(right1, tree2).getOrElse(left2)))
}

append(Node(1, Node(2, EmptyTree, EmptyTree), Node(3, EmptyTree, EmptyTree)), Node(4, Node(5, EmptyTree, EmptyTree), Node(6, EmptyTree, EmptyTree)))