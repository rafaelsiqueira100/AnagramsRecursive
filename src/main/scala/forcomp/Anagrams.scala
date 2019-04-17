package forcomp

import com.sun.corba.se.impl.orbutil.ObjectStreamClassUtil_1_3
import com.sun.xml.internal.bind.v2.schemagen.xmlschema.Occurs

import scala.collection.immutable.List
import java.lang.Exception

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase().groupBy((letter: Char) => letter ).toList.map((element: (Char, String)) => (element._1, element._2.length)).sortWith((element1:(Char, Int), element2:(Char, Int)) => element1._1 < element2._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    if(s != List()) {
      val megaWord = s.reduceLeft((leftiestElement: Word, rightiestElement: Word) => leftiestElement + rightiestElement)
      wordOccurrences(megaWord)
    }
    else
      List()
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.groupBy((element: Word) => wordOccurrences(element))
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    if (word == "" || word == null || word == Nil || word == None)
      List()
    else {
      try {
        val anagrams = dictionaryByOccurrences(wordOccurrences(word))
        if (anagrams == None)
          List()
        else
          anagrams
      }
      catch{
        case _:Throwable => List()
      }
    }
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  //List[Occurrences] = List[List[(Char, Int)]]
  //Occurrences = List[(Char, Int)]
  //Occurrences[i] = (Char, Int)
  //Occurrences[i]._1 = Char
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def adicionarLetra(resultadoParcial: List[Occurrences], combinacaoOriginal: Occurrences,  combinacaoAtual: Occurrences, indLetraAtual: Int, numLetrasTotal: Int, flagPrint: Boolean): List[Occurrences] = {
      def gerarPossivelFrequenciaLetra (resultadoParcial: List[Occurrences], combinacaoOriginal: Occurrences, combinacaoAtual: Occurrences, indLetraAtual:Int, frequenciaLetraAtual:Int, numLetrasTotal:Int, flagPrint: Boolean): List[Occurrences]={
      //  if (flagPrint)
      //    println("entrou gerarPossivelFrequenciaLetra (resultadoParcial: "+resultadoParcial+",  combinacaoAtual: "+combinacaoAtual+", indLetraAtual:"+indLetraAtual+", frequenciaLetraAtual:"+frequenciaLetraAtual)
        try {
          if (indLetraAtual >= numLetrasTotal) {
            if (combinacaoAtual.length == numLetrasTotal) {
              //  if(flagPrint)
              //    println("resultadoParcial ::: List(combinacaoAtual) "+ (resultadoParcial ::: List(combinacaoAtual)))
              resultadoParcial ::: List(combinacaoAtual)
            }
            else {
              resultadoParcial
            }
          }
          else
          {

            val letraAtual: Char = combinacaoOriginal(indLetraAtual)._1
            val frequenciaMaximaLetraAtual: Int = combinacaoOriginal(indLetraAtual)._2
            if (frequenciaLetraAtual > frequenciaMaximaLetraAtual)
              resultadoParcial
            else {
              val novoResultadoParcial = adicionarLetra(resultadoParcial, combinacaoOriginal, combinacaoAtual ::: List((letraAtual, frequenciaLetraAtual)), indLetraAtual + 1, numLetrasTotal, flagPrint)
              gerarPossivelFrequenciaLetra(novoResultadoParcial, combinacaoOriginal, combinacaoAtual, indLetraAtual, frequenciaLetraAtual + 1, numLetrasTotal, flagPrint)

            }

          }
        }catch{//letra atual não existe, pula para a próxima
          case ex:ClassCastException =>  gerarPossivelFrequenciaLetra(resultadoParcial, combinacaoOriginal, combinacaoAtual, indLetraAtual+1, 0, numLetrasTotal, flagPrint)//letra não existe
        }

      }//fim de gerarPossivelFrequenciaLetra
      //início de adicionarLetra
      //if (flagPrint)
      //  println("entrou adicionarLetra(resultadoParcial: "+resultadoParcial+", combinacaoAtual: "+combinacaoAtual+", indLetraAtual: "+indLetraAtual+", numLetrasTotal:"+numLetrasTotal)


      if(indLetraAtual >= numLetrasTotal) {
        if(combinacaoAtual.length == numLetrasTotal) {
        //  if(flagPrint)
        //    println("resultadoParcial ::: List(combinacaoAtual) "+ (resultadoParcial ::: List(combinacaoAtual)))
          resultadoParcial ::: List(combinacaoAtual)
        }
        else
          resultadoParcial
      }
      else{
        if(combinacaoAtual.length == numLetrasTotal) {
        //  if(flagPrint)
        //    println("combinação atual: "+  List(combinacaoAtual))
        //    println("resultado parcial:"+ resultadoParcial)
          val novoResultadoParcial = List(combinacaoAtual) ::: gerarPossivelFrequenciaLetra(resultadoParcial , combinacaoOriginal, combinacaoAtual, indLetraAtual, 0, numLetrasTotal, flagPrint)

          adicionarLetra(novoResultadoParcial, combinacaoOriginal, combinacaoAtual, indLetraAtual + 1, occurrences.length, flagPrint)
        }
        else{
          val novoResultadoParcial = gerarPossivelFrequenciaLetra(resultadoParcial, combinacaoOriginal, combinacaoAtual, indLetraAtual, 0, numLetrasTotal, flagPrint)
          adicionarLetra(novoResultadoParcial, combinacaoOriginal, combinacaoAtual, indLetraAtual + 1, occurrences.length, flagPrint)
        }
      }
    }//fim de adicionarLetra
    //início de combinations
    val flagPrint:Boolean = false//occurrences.indexWhere((elem:(Char, Int)) => elem._1 == 'b') > -1
    //if (flagPrint)
    //  println("entrou combinations(occurrences: "+occurrences)
    if(occurrences == List())
      List(List())
    else
      adicionarLetra(List(), occurrences, List(), 0, occurrences.length, flagPrint).map((subset: List[(Char, Int)]) => subset.filter((elem: (Char, Int)) => elem._2 >0))
  }



  /** Subtracts occurrence list `listaOcorrencia2` from occurrence list `listaOcorrencia1`.
   *  (X-Y)                              Y                                          X
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  //Occurrences = List[(Char, Int)]
  //Occurrences[i] = (Char, Int)
  def subtract(listaOcorrencia1: Occurrences, listaOcorrencia2: Occurrences): Occurrences = {
    //listaOcorrencia1 - listaOcorrencia2
    //Dica: usar foldLeft, -, apply, updated
    def subtractTheLetter(indLetraAtual: Int, listaOcorrencia1: Occurrences, listaOcorrencia2: Occurrences): Occurrences = {
      /*if(listaOcorrencia2 == (List(('n',1), ('u',1))))
        println("subtractTheLetter(indLetraAtual: "+indLetraAtual+", listaOcorrencia1: "+listaOcorrencia1+", listaOcorrencia2:"+listaOcorrencia2)*/
      val maxIndLetra = listaOcorrencia1.length
      if(indLetraAtual >= maxIndLetra)
        listaOcorrencia1
      else{
        try {
          val letraAtual: Char = listaOcorrencia1(indLetraAtual)._1
          val frequenciaLetraAtual1: Int = listaOcorrencia1(indLetraAtual)._2
          try{
            val frequenciaLetraAtual2 = listaOcorrencia2.find((elemento : (Char, Int)) => elemento._1 == letraAtual).get._2
            val frequenciaSubtraida = frequenciaLetraAtual1 - frequenciaLetraAtual2
            /*if(indLetraAtual == 4 && listaOcorrencia2 == (List(('n',1), ('u',1)))){
              println("letraAtual="+letraAtual)
              println("frequenciaLetraAtual1="+frequenciaLetraAtual1)
              println("frequenciaLetraAtual2="+frequenciaLetraAtual2)
              println("frequenciaSubtraida="+frequenciaSubtraida)
            }*/
            if(frequenciaSubtraida>0)
              subtractTheLetter(indLetraAtual + 1, listaOcorrencia1.map((elem : (Char, Int) ) => if(elem._1 == letraAtual) (elem._1,frequenciaSubtraida) else  (elem._1,elem._2)), listaOcorrencia2)
            else
              subtractTheLetter(indLetraAtual + 1, listaOcorrencia1.filter (  (elem : (Char, Int) ) => elem._1 != letraAtual ) . asInstanceOf[List[(Char, Int)]], listaOcorrencia2)
          }
          catch{//se o elemento não está em lista2
            case ex: NoSuchElementException => subtractTheLetter(indLetraAtual + 1, listaOcorrencia1, listaOcorrencia2)
          }
        }
        catch{
          case ex : ClassCastException =>/* println("ex.message:"+ex.getMessage)
            println("listaOcorrencia1 = "+listaOcorrencia1)
            println("indLetraAtual = "+indLetraAtual)
            println("listaOcorrencia1(indLetraAtual)"+listaOcorrencia1(indLetraAtual))*/
            subtractTheLetter(indLetraAtual + 1, listaOcorrencia1, listaOcorrencia2)
        }



      }

    }//fim de subtractTheLetter

    subtractTheLetter(0, listaOcorrencia1, listaOcorrencia2)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] =  {
    def adicionarPossiveisCombinacoes(remainingOccurrence: Occurrences, currentCombination: Sentence, partialResult: List[Sentence]):List[Sentence] = {


      def adicionarCombinacao	(remainingOccurrence: Occurrences, currentCombination: Sentence, indSentenceOccurrencesCombination: Int, sentenceOccurrencesCombinations:List[Occurrences], partialResult: List[Sentence] ):List[Sentence] = {


        def adicionarPalavra( possiveisPalavras: List[Word], indPossivelPalavraAtual: Int, resto: Occurrences, currentCombination: Sentence, partialResult: List[Sentence]): List[Sentence] = {
          if(possiveisPalavras!=List("nu"))
            println("\nadicionarPalavra( possiveisPalavras: "+possiveisPalavras+", indPossivelPalavraAtual: "+indPossivelPalavraAtual+/*", resto: "+resto+*/", currentCombination: "+currentCombination+", partialResult: "+partialResult+")")
          else
            println("\nadicionarPalavra( possiveisPalavras: "+possiveisPalavras+", indPossivelPalavraAtual: "+indPossivelPalavraAtual+", resto: "+resto+", currentCombination: "+currentCombination+", partialResult: "+partialResult+")")
          if(indPossivelPalavraAtual >= possiveisPalavras.length)
            partialResult
          else{
            val currentWord : Word = possiveisPalavras(indPossivelPalavraAtual)
            val novoPartialResult = partialResult ::: adicionarPossiveisCombinacoes(resto, (currentCombination.asInstanceOf[List[Word]] ::: List(currentWord)), partialResult)

            adicionarPalavra(possiveisPalavras, indPossivelPalavraAtual + 1, resto, currentCombination, novoPartialResult)
          }
        }//fim de adicionarPalavra

        //início de adicionarCombinacao


        /*if(indSentenceOccurrencesCombination == 28){
          val a = 5
        }*/
        if(indSentenceOccurrencesCombination != 28)
          println("\nadicionarCombinacao	("/*remainingOccurrence: "+remainingOccurrence+",*/+ "currentCombination: "+currentCombination+", indSentenceOccurrencesCombination: "+indSentenceOccurrencesCombination+/*", sentenceOccurrencesCombinations:"+sentenceOccurrencesCombinations+*/", partialResult:"+partialResult+")")
        else
          println("\nadicionarCombinacao	(remainingOccurrence: "+remainingOccurrence+",currentCombination: "+currentCombination+", indSentenceOccurrencesCombination: "+indSentenceOccurrencesCombination+", sentenceOccurrencesCombinations:"+sentenceOccurrencesCombinations+", partialResult:"+partialResult+")")
        if(indSentenceOccurrencesCombination >= sentenceOccurrencesCombinations.length)
          partialResult
        else{
          val currentSentenceOccurrencesCombinations: Occurrences = sentenceOccurrencesCombinations(indSentenceOccurrencesCombination)

          try {
            val wordsThisOccurrence: List[Word] = dictionaryByOccurrences(currentSentenceOccurrencesCombinations)
            //Occurrences-Occurrences
            val resto: Occurrences = subtract(remainingOccurrence, currentSentenceOccurrencesCombinations)
            if(indSentenceOccurrencesCombination == 28)
              println("\n\ncurrentSentenceOccurrencesCombinations:"+currentSentenceOccurrencesCombinations)
              println("\nwordsThisOccurrence:"+wordsThisOccurrence)
              println("\nresto:"+resto)
            //partialResult ::: adicionarPalavra(wordsThisOccurrence, 0, resto, currentCombination, partialResult)
            if(remainingOccurrence == currentSentenceOccurrencesCombinations)//achamos uma combinação!!
              partialResult:::List(currentCombination)
            else {
              val novoPartialResult = partialResult ::: adicionarPalavra(wordsThisOccurrence, 0, resto, currentCombination, partialResult)

              adicionarCombinacao(remainingOccurrence, currentCombination, indSentenceOccurrencesCombination + 1, sentenceOccurrencesCombinations, novoPartialResult)
            }
          }
          catch{
            case noSuchElementEx: NoSuchElementException =>  adicionarCombinacao(remainingOccurrence, currentCombination, indSentenceOccurrencesCombination+1, sentenceOccurrencesCombinations, partialResult)//there is not any word with this occurrences in the dictionary
          }
        }
      }//fim de adicionarCombinacao
      //início de adicionarPossiveisCombinacoes
      println("\nadicionarPossiveisCombinacoes("/*remainingOccurrence: "+remainingOccurrence+"*/+", currentCombination: "+currentCombination+", partialResult: "+partialResult+")")
      if(remainingOccurrence == List())
        partialResult
      else {
        val sentenceOccurrencesCombinations: List[Occurrences] = combinations(remainingOccurrence)
        partialResult ::: adicionarCombinacao(remainingOccurrence, currentCombination, 0, sentenceOccurrencesCombinations, partialResult)
      }
    }//fim de adicionarPossiveisCombinacoes
    //início de sentenceAnagrams
    println("\nsentenceAnagrams(sentence: "+sentence+")")
    val totalSentenceOccurrences: Occurrences = sentenceOccurrences(sentence)
    if(totalSentenceOccurrences != List())
      adicionarPossiveisCombinacoes(totalSentenceOccurrences, List(), List())
    else
      List(List())
  }
}
