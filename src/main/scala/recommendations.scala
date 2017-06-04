/**
  * Created by Chenna Reddy
  */
object recommendations extends App {

  val critics = Map(
    "Lisa Rose" -> Map(
      "Lady in the Water" -> 2.5,
      "Snakes on a Plane" -> 3.5,
      "Just My Luck" -> 3.0,
      "Superman Returns" -> 3.5,
      "You, Me and Dupree" -> 2.5,
      "The Night Listener" -> 3.0
    ),
    "Gene Seymour" -> Map(
      "Lady in the Water" -> 3.0,
      "Snakes on a Plane" -> 3.5,
      "Just My Luck" -> 1.5,
      "Superman Returns" -> 5.0,
      "The Night Listener" -> 3.0,
      "You, Me and Dupree" -> 3.5
    ),
    "Michael Phillips" -> Map(
      "Lady in the Water" -> 2.5,
      "Snakes on a Plane" -> 3.0,
      "Superman Returns" -> 3.5,
      "The Night Listener" -> 4.0
    ),
    "Claudia Puig" -> Map(
      "Snakes on a Plane" -> 3.5,
      "Just My Luck" -> 3.0,
      "The Night Listener" -> 4.5,
      "Superman Returns" -> 4.0,
      "You, Me and Dupree" -> 2.5
    ),
    "Mick LaSalle" -> Map(
      "Lady in the Water" -> 3.0,
      "Snakes on a Plane" -> 4.0,
      "Just My Luck" -> 2.0,
      "Superman Returns" -> 3.0,
      "The Night Listener" -> 3.0,
      "You, Me and Dupree" -> 2.0
    ),
    "Jack Matthews" -> Map(
      "Lady in the Water" -> 3.0,
      "Snakes on a Plane" -> 4.0,
      "The Night Listener" -> 3.0,
      "Superman Returns" -> 5.0,
      "You, Me and Dupree" -> 3.5
    ),
    "Toby" -> Map(
      "Snakes on a Plane" -> 4.5,
      "You, Me and Dupree" -> 1.0,
      "Superman Returns" -> 4.0
    )
  )

  val r1 = math.sqrt(math.pow(5 - 4, 2) + math.pow(4 - 1, 2))
  val r2 = 1 / (1 + math.sqrt(math.pow(5 - 4, 2) + math.pow(4 - 1, 2)))

  def simDistance(prefs: Map[String, Map[String, Double]], person1: String, person2: String): Double = {
    val si = common(prefs, person1, person2)
    val ss = si map (i => math.pow(prefs(person1)(i) - prefs(person2)(i), 2))
    1 / (1 + math.sqrt(ss.sum))
  }

  def simPearson(prefs: Map[String, Map[String, Double]], person1: String, person2: String): Double = {
    val si = common(prefs, person1, person2)

    def sum(person: String) = si map (x => prefs(person)(x)) sum

    def sumSq(person: String) = si map (x => math.pow(prefs(person)(x), 2)) sum

    val pSum = si map (x => prefs(person1)(x) * prefs(person2)(x)) sum
    val sum1 = sum(person1)
    val sum2 = sum(person2)
    val sum1Sq = sumSq(person1)
    val sum2Sq = sumSq(person2)
    val num = pSum - (sum1 * sum2 / si.size)
    val den = math.sqrt((sum1Sq - math.pow(sum1, 2) / si.size) * (sum2Sq - math.pow(sum2, 2) / si.size))
    if (den == 0) 0 else num / den
  }

  def common(prefs: Map[String, Map[String, Double]], person1: String, person2: String) = {
    val p1 = prefs.getOrElse(person1, Map())
    val p2 = prefs.getOrElse(person2, Map())
    p1.keySet intersect p2.keySet
  }

  println(s"r1: $r1")
  println(s"r2: $r2")

  println("simDistance: " + simDistance(critics, "Lisa Rose", "Gene Seymour"))
  println("simPearson: " + simPearson(critics, "Lisa Rose", "Gene Seymour"))

}
