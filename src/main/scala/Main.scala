import Tree._
import Table._
import scala.collection.mutable.ListBuffer

@main def helloTree: Unit = 
  var t: Table = Table("test.csv")
  var tree: Tree = Tree(0,0)
  var (data_array, categorical_features) = t.toArray(t.data)

  var class_labels: Array[Float] = Array.ofDim[Float](data_array.length)
  for
    i <- 0 until data_array.length
  do
    class_labels(i) = data_array(i).last

  tree.fit(data_array, class_labels)

  print(tree)
