import scala.annotation.tailrec

object Regression {

  def regression(dataset_file: String,
                attribute_columns: List[String],
                value_column: String,
                test_percentage: Double,
                alpha: Double,
               gradient_descent_steps: Int): (Matrix, Double) = {
    val dataset = Dataset.apply(dataset_file)
    val (train, test) = dataset.split(test_percentage)

    val trainX = train.selectColumns(attribute_columns)
    val trainY = train.selectColumn(value_column)

    val testX = test.selectColumns(attribute_columns)
    val testY = test.selectColumn(value_column)


    val XTrainMatrix = Matrix.apply(trainX) ++ 1.0
    val YTrainMatrix = Matrix.apply(trainY)

    val XTestMatrix = Matrix.apply(testX) ++ 1.0
    val YTestMatrix = Matrix.apply(testY)

   // val m = XTestMatrix.height.getOrElse(0)
    val n = XTestMatrix.width.getOrElse(0)

    val W = Matrix.apply(List.fill(n)(List(0.0)))

    val newW = gradientDescent(W, XTrainMatrix, YTrainMatrix, alpha, gradient_descent_steps)

    val testPredictions = XTestMatrix * newW

    val testError = testPredictions - YTestMatrix

    val mesaj = testError.data.flatMap(d => Some(d.flatten.map(math.pow(_, 2)).sum / d.size)).getOrElse(0.0)

    (newW, mesaj)
  }

  @tailrec
  def gradientDescent(W: Matrix, X: Matrix, Y: Matrix, alpha: Double, steps: Int): Matrix = {
    if (steps == 0) {
      W
    } else {
      //trebuie sa il completezi pe W cu valorile tale
      val predictions = X * W

      val error = predictions - Y

      val m = X.height.getOrElse(0)

      val gradient = (X.transpose * error).map(_ / m)

      val newW = W - gradient.map(_ * alpha)

      gradientDescent(newW, X, Y, alpha, steps - 1)
    }
  }



  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}