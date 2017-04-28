package scripts.train

case class Corpus(name: String, baseFileName: String)

object Corpus {
  val LDC2014T12_PROXY = Corpus("LDC2014T12-proxy", "data")
  val Little_Prince_v1_6 = Corpus("Little_Prince_v1_6", "amr-bank-struct-v1.6")
}
