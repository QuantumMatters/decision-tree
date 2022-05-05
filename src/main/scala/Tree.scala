import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

class Tree(
  var feature_index: Int,
  var decision_value: Float,
  var label: Option[Float] = None,
  var left: Option[Tree] = None,
  var right: Option[Tree] = None,
  var max_depth: Int = 7
):

    def decide(row: Array[Float]): Float =
      if this.label.isDefined then  // is a leaf
        this.label.get
      else
        if this.decision_value < row(this.feature_index) then
          this.left.get.decide(row)
        else
          this.right.get.decide(row)

    def predict(X: Array[Array[Float]]): Array[Float] =
      for row <- X yield decide(row)

    /**
     * Gini Impurity
     */
    def criterion(class_labels: Array[Float]) =
      var impurity: Float = 0
      for
        label <- class_labels.distinct
      do
        var count: Float = 0
        for
          i <- 0 until class_labels.length
        do
          if class_labels(i) == label then
            count += 1
        impurity += count/class_labels.length * (1 - count/class_labels.length)
      impurity

    def compute_gain(previous_class_labels: Array[Float], split_class_labels: Array[Array[Float]]) =
      val previous_criterion = criterion(previous_class_labels)

      // weighted average of criterion by split
      val new_criterion =
        (for labels <- split_class_labels
         yield (1F * labels.length / previous_class_labels.length) * criterion(labels)).sum

      previous_criterion - new_criterion

    def split(feature: Array[Float], class_labels: Array[Float], threshold: Float) =
      var split_features: Array[ArrayBuffer[Float]] = Array(new ArrayBuffer[Float], new ArrayBuffer[Float])
      var split_class_labels: Array[ArrayBuffer[Float]] = Array(new ArrayBuffer[Float], new ArrayBuffer[Float])

      for
        i <- 0 until feature.length
      do
        if feature(i) >= threshold then
          split_features(0).addOne(feature(i))
          split_class_labels(0).addOne(class_labels(i))
        else
          split_features(1).addOne(feature(i))
          split_class_labels(1).addOne(class_labels(i))

      (split_features.map(_.toArray), split_class_labels.map(_.toArray))

    def split(feature: Array[Array[Float]], class_labels: Array[Float], threshold: Float, feature_index: Int) =

      var split_features: Array[ArrayBuffer[Array[Float]]] =
        Array(new ArrayBuffer[Array[Float]], new ArrayBuffer[Array[Float]])
      var split_class_labels: Array[ArrayBuffer[Float]] = Array(new ArrayBuffer[Float], new ArrayBuffer[Float])

      for
        i <- 0 until feature.length
      do
        if feature(i)(feature_index) >= threshold then
          split_features(0).addOne(feature(i))
          split_class_labels(0).addOne(class_labels(i))
        else
          split_features(1).addOne(feature(i))
          split_class_labels(1).addOne(class_labels(i))

      (split_features.map(_.toArray), split_class_labels.map(_.toArray))


    def find_split(feature: Array[Float], class_labels: Array[Float]) =
      var gain: Float = Float.NegativeInfinity
      var best_split: Float = 0
      for
        x <- feature.distinct
      do
        var (split_features, split_class_labels) = split(feature, class_labels, x)
        var new_gain = compute_gain(class_labels, split_class_labels)
        if new_gain > gain then
          gain = new_gain
          best_split = x

      (best_split, gain)

    def find_feature_with_best_split(X: Array[Array[Float]], y: Array[Float]) =
      var best_gain: Float = Float.NegativeInfinity
      var best_split: Float = 0
      var best_feature_index: Int = 0
      for
        feature_index <- 0 until X(0).length
      do
        val feature: Array[Float] = Array.ofDim[Float](X.length)
        for
          row_index <- 0 until X.length
        do
          feature(row_index) = X(row_index)(feature_index)
        val (split, gain) = find_split(feature, y)
        if gain > best_gain then
          best_gain = gain
          best_split = split
          best_feature_index = feature_index
      (best_gain, best_split, best_feature_index)

    def mode(X: Array[Float]) =
      var counts: HashMap[Float,Int] = new HashMap()
      for
        x <- X
      do
        counts.addOne((x, counts.getOrElse(x, 0) + 1))
      var largest = 0F
      var mode = 0F
      for
        x <- counts
      do
        if x(1) > largest then
          largest = x(1)
      mode

    def add_node(X: Array[Array[Float]], y: Array[Float], current_depth: Int = 0): Tree =
      val new_depth = current_depth + 1
      val (best_gain, best_split, best_feature_index) = find_feature_with_best_split(X, y)
      val (split_features, split_class_labels) = split(X, y, best_split, best_feature_index)

      if new_depth == this.max_depth then
        var left: Tree = Tree(0, 0, label=Option(mode(split_class_labels(0))))
        var right: Tree = Tree(0, 0, label=Option(mode(split_class_labels(1))))

      if split_class_labels(0).distinct.length == 1 then
        var left: Tree = Tree(0, 0, label=Option(split_class_labels(0)(0)))
      else
        var left: Tree = add_node(split_features(0), split_class_labels(0), new_depth)
      
      if split_class_labels(1).distinct.length == 1 then
        var right: Tree = Tree(0, 0, label=Option(split_class_labels(1)(0)))
      else
        var right: Tree = add_node(split_features(1), split_class_labels(1), new_depth)

      Tree(best_feature_index, best_split, left=left, right=right)

    def fit(X: Array[Array[Float]], y: Array[Float]) =
      if y.distinct.length == 1 then
        this.label = Option(y(0))
      else
        var (best_gain, best_split, best_feature_index) = find_feature_with_best_split(X, y)
        val (split_features, split_class_labels) = split(X, y, best_split, best_feature_index)
        this.feature_index = best_feature_index
        this.decision_value = best_split
        this.left = Option(add_node(split_features(0), split_class_labels(0)))
        this.right = Option(add_node(split_features(1), split_class_labels(1)))