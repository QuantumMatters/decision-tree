import scala.collection.immutable.Vector
import com.github.tototoshi.csv._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

class Table(csv_file_path: String):
  import Table.*

  // get data from CSV
  var data: List[List[String]] = fromCSV(csv_file_path)
  var header: List[String] = data(1)
  data = data.slice(1, data.length)
  var data_array = toArray(data)

  /* parse the strings into an array -- this is nasty! */
  def toArray(data: List[List[String]]) =
    var categorical_features: ListBuffer[Tuple] = new ListBuffer()
    var data_array: Array[Array[Float]] = Array.ofDim[Float](data.length, data(0).length)

    for // iterate through features first
      j <- 0 until data(0).length
    do
      // check if the feature is String-valued
      var is_categorical: Boolean = false
      try
        data_array(0)(j) = data(0)(j).toFloat
      catch  // fragile
        case e: java.lang.NumberFormatException =>
          is_categorical = true

      // if it is, then categorical-encode
      if is_categorical then
        var (encoded_feature, encode_map) = 
          categoricalEncode((for (i <- 0 until data.length) yield data(i)(j)).toList)
        categorical_features.addOne((j, encode_map))
        for
          i <- 0 until data.length
        do
          data_array(i)(j) = encoded_feature(i)

      // otherwise, cast to float
      else
        for
          i <- 0 until data.length
        do
          data_array(i)(j) = data(i)(j).toFloat

    (data_array, categorical_features)


object Table:

  def fromCSV(csv_file_path: String) =
    val reader = CSVReader.open(csv_file_path)
    reader.all()

  def categoricalEncode(feature: List[String]) =
    val distinct = feature.distinct.sorted
    var encode_map: HashMap[String, Float] = new HashMap()
    for
      i <- 0 until distinct.length
    do
      encode_map.addOne((distinct(i), i))
    var encoded: Array[Float] = Array.ofDim[Float](feature.length)
    for
      i <- 0 until feature.length
    do
      encoded(i) = encode_map.get(feature(i)).get
    (encoded, encode_map)
