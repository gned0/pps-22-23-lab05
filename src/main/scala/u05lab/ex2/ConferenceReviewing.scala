package u05lab.ex2

import u05lab.ex2.Question._

/**
 * A trait modelling the results of reviewing articles of a conference
 * Each reviewer (revisore) reads an article (articolo), and answers to a number of questions
 * with a score from 0 (bad) to 10 (excellent).
 * Note that each article can be reviewed by many reviewers (typically, from 2 to 4), but the
 * system does not keep track of the identity of reviewers
 *
 */
trait ConferenceReviewing:

  /**
   * @param article
   * @param scores
   * loads a review for the specified article, with complete scores as a map
   */
  def loadReview(article: Int, scores: Map[Question, Int]): Unit

  /**
   * @param article
   * @param relevance
   * @param significance
   * @param confidence
   * @param fin
   * loads a review for the specified article, with the 4 explicit scores
   */
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  /**
   * @param article
   * @param question
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   */
  def orderedScores(article: Int, question: Question): List[Int]

  /**
   * @param article
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore(article: Int): Double

  /**
   * An article is considered accepted if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  def acceptedArticles(): Set[Int]

  /**
   * @return accepted articles as a list of pairs article + averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles(): List[(Int, Double)]

  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10
   */
  def averageWeightedFinalScoreMap(): Map[Int, Double]

/**
 * For each article, the reviewer has to reply to all the following questions
 */
enum Question:
  case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

object ConferenceReviewing:

  def apply(): ConferenceReviewing = new ConferenceReviewingImpl()

  private val RELEVANCE_THRESHOLD = 8
  private val AVERAGE_THRESHOLD = 5

  private class ConferenceReviewingImpl extends ConferenceReviewing:

    private var reviews: Map[Int, List[Map[Question, Int]]] = Map.empty

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      reviews = reviews.updated(article, scores :: reviews.getOrElse(article, Nil))

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      loadReview(article, Map(RELEVANCE -> relevance, SIGNIFICANCE -> significance, CONFIDENCE -> confidence, FINAL -> fin))

    override def orderedScores(article: Int, question: Question): List[Int] =
      reviews(article).map(_(question)).sorted

    override def averageFinalScore(article: Int): Double =
      reviews(article).map(_(FINAL)).sum.toDouble / reviews(article).length

    override def acceptedArticles(): Set[Int] =
      reviews.filter { case (article, scores) =>
        averageFinalScore(article) > AVERAGE_THRESHOLD &&
        scores.exists(_(RELEVANCE) >= RELEVANCE_THRESHOLD)
      }.keySet

    override def sortedAcceptedArticles(): List[(Int, Double)] =
      acceptedArticles().map(article => (article, averageFinalScore(article))).toList.sortBy(_._2)

    private def averageWeightedFinalScore(scores: List[Map[Question, Int]]): Double =
      scores.map(review =>
        review(CONFIDENCE) * review(FINAL) / 10.0
      ).sum / scores.length

    override def averageWeightedFinalScoreMap(): Map[Int, Double] =
      reviews.map((a, s) => a -> averageWeightedFinalScore(s))