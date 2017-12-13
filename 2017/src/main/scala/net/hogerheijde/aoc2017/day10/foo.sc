val values = IndexedSeq(1, 2, 3, 4, 5, 6)
val position = 4

values.drop(position) ++ values.take(position)

IndexedSeq().drop(10)

val input = "1,2,3"

//49,44,50,44,51
val lengths = input.split("").map(_.toCharArray.head.toInt)