package kata

object Cards {

  def getHighest(cards: Seq[Int]): Option[Int] = cards match {
    case Nil => None
    case _ => Some(cards.max)
  }

  def getPairs(cards: Seq[Int]): Seq[Int] =
    cards.groupBy(identity)
      .collect { case (k, v) if v.size > 1 => List.fill(v.size / 2)(k) }
      .toSeq.flatten.sorted

  def getHighestPairs(cards: Seq[Int]): Option[Int] = getHighest(getPairs(cards))
}
