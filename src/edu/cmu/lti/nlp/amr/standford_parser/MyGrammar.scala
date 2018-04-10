package edu.cmu.lti.nlp.amr.standford_parser

import edu.stanford.nlp.parser.lexparser._
import edu.stanford.nlp.util.Index
import ReflectionUtils._

/**
  * BEWARE!!! Everything everywhere is MUTABLE!!!
  * op, ug, dg, bg, lex, stateIndex....
  * and those instances are used outside, e.g. in CollocationExtendedParserWrapper.java
  */
class MyGrammar private(val lexicalizedParser: LexicalizedParser,
                        val op: Options,
                        val bg: BinaryGrammar,
                        val ug: UnaryGrammar,
                        val lex: BaseLexicon,
                        val dg: DependencyGrammar,
                        val stateIndex: Index[String],
                        val wordIndex: Index[String],
                        val tagIndex: Index[String]) {

  /**
    * WARNING: wordIndex and tagIndex must be shared by all BaseLexicon and current Grammar class
    *
    * That method extends Grammar by one word with given tag and given count.
    * It works really simple: given (word, tag) pair is added to grammar as if
    * during train phase that word occurred only with that tag `cnt` times!
    *
    * So as a result in grammar next things will be changed:
    * <ul>
    * <li>Total count of seen words will be increased by `cnt` </li>
    * <li>Total count of seen words, which occurred with tag `tag`} will be increased by `cnt`</li>
    * <li>New word `word` will be added to word index</li>
    * <li>Number of times given word occurred at all will be set to `cnt`</li>
    * <li>Number of times given word occurred with given tag will be set to `cnt` </li>
    * </ul>
    */
  def addWord(word: String, tag: String, cnt: Int): Unit = {
    addWordNoRefresh(word, tag, cnt)
    val wid = wordIndex.indexOf(word)
    refresh()
    assert(lex.isKnown(word))
    assert(lex.isKnown(wid))
  }

  /**
    * Same as `addWord` method, but no refresh is made after adding word to index.
    * Use that method if you want to add a lot of words to grammar, but don't forget to
    * class `Grammar.refresh()` method for grammar to work correctly with newly added
    * words
    */
  def addWordNoRefresh(word: String, tag: String, cnt: Int): Unit = {
    if (!tagIndex.contains(tag)) throw new IllegalArgumentException("Tag index must contain given tag! [" + tag + "]")
    if (wordIndex.contains(word)) throw new IllegalArgumentException("Word already is in word index! [" + word + "]")
    wordIndex.add(word)
    val wid = wordIndex.indexOf(word)
    val tid = tagIndex.indexOf(tag)

    var itw: IntTaggedWord = null
    itw = new IntTaggedWord(wid, tid)
    lex.seenCounter.incrementCount(new IntTaggedWord(wid, tid), cnt)
    itw = new IntTaggedWord(IntTaggedWord.ANY_WORD_INT, tid)
    lex.seenCounter.incrementCount(itw, cnt)
    itw = new IntTaggedWord(wid, IntTaggedWord.ANY_TAG_INT)
    lex.seenCounter.incrementCount(itw, cnt)
    assert(lex.seenCounter.getCount(itw) == cnt)
  }

  /**
    * Adds given tag to state index and tag index
    */
  def addNewTagAndState(tag: String): Unit = {
    tagIndex.add(tag)
    stateIndex.add(tag)
  }

  /**
    * Adds given unary rule to grammar
    *
    * ALERT 1: if any state (parent or child) not added to state index and tag index (i.e. method `addNewTag` not
    * called) method will throw `IllegalArgumentException`
    *
    * ALERT 2: `UnaryGrammar` instance, which is provided by parser data must be shared between every object,
    * which have a link to it.
    *
    * ALERT 3: `refresh()` method of cur. class MUST be invoked after.
    */
  def addNewUnaryRule(parent: String, child: String, score: Float): Unit = {
    if (!stateIndex.contains(parent) || !stateIndex.contains(child)) throw new IllegalArgumentException
    ug.addRule(new UnaryRule(stateIndex.indexOf(parent), stateIndex.indexOf(child), score))
    // ug.addCoreRule(new UnaryRule(stateIndex.indexOf(parent), stateIndex.indexOf(child), score))
  }

  /**
    * That method must be used if you made any changes in grammar with any of next methods:
    * <ul>
    * <li>`addWordNoRefresh(String word, String tag, int cnt)`</li>
    * </ul>
    */
  def refresh(): Unit = {
    lex.callVoidMethod("initRulesWithWord")
//    lex.initRulesWithWord()
//    ug.refresh()
//    bg.refresh()
  }
}

object MyGrammar {
  def apply(lp: LexicalizedParser): MyGrammar = new MyGrammar(
    lp,
    lp.getOp,
    lp.bg,
    lp.ug,
    lp.lex.asInstanceOf[BaseLexicon],
    lp.dg,
    lp.stateIndex,
    lp.wordIndex,
    lp.tagIndex
  )
}