import scala.io.Source

val input = Source.fromFile(s"${System.getProperty("user.home")}/test_trees.txt")
  .getLines
  .toList

case class Tree(xPos: Int, yPos: Int, height: Int)

val trees = for {
  (line, yIndex) <- input.zipWithIndex
  (tree, xIndex) <- line.toCharArray.zipWithIndex.map{case(c, index) => (c.asDigit, index)}
} yield Tree(xIndex, yIndex, tree)

val width = trees.maxBy { case tree => tree.xPos }.xPos
val depth = trees.maxBy { case tree => tree.yPos }.yPos

def isEdge(tree: Tree) =
    tree.xPos == 0 || tree.xPos == width || tree.yPos == 0 || tree.yPos == depth


type TreeFilter = Tree => Boolean

val treeFilters: List[TreeFilter] = List(
  //Visable to left
  tree => trees.find{ t => t.yPos == tree.yPos && t.xPos < tree.xPos && t.height > tree.height}.isDefined,
  //Visable to right
  tree => trees.find{ t => t.yPos == tree.yPos && t.xPos < tree.xPos && t.height > tree.height}.isDefined,
  //you get the ide,
  tree => trees.find{ t => t.yPos < tree.yPos && t.xPos == tree.xPos && t.height > tree.height}.isDefined,
  tree => trees.find{ t => t.yPos > tree.yPos && t.xPos == tree.xPos && t.height > tree.height}.isDefined,
)

val (edgeTrees, innerTrees) = trees.partition { case t: Tree => isEdge(t) }
val visibleTrees = edgeTrees.length + innerTrees.filterNot { tree => treeFilters.forall{ filter => filter(tree) } }.length