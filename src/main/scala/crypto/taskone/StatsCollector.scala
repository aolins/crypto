package crypto.taskone

import scala.io.Source
import scala.util.Random

object StatsCollector extends App {
    val upper = Source.fromFile("src/main/resources/latvian.txt").getLines().next.toUpperCase
    val languageStats = collectStats(upper)

    private def collectStats(upper: String) = {
        val languageTuples = (upper zip upper.substring(1).concat(" ")).toList
        val totalNumber = languageTuples.length
        val grouped = languageTuples.groupBy(identity)
        val counted = grouped.map(x => (x._1, x._2.length)).toList.sortWith((x, y) => x._2 > y._2)
        (counted, totalNumber)
    }

    type C = List[((Char,Char),Int)]
    type Stats = (C,Int)
    case class PartlyTranslated(encryptedFrom:Char, encryptedTo:Char, decryptedFrom:Option[Char],decryptedTo:Option[Char], count:Int)

    def countDiff(c:Stats):Double = {
        c._1.foldLeft(0D){(acc, tuple) => val found = languageStats._1.find(x => x._1 == tuple._1).getOrElse(((0,0),0))
            acc + (tuple._2/c._2 - found._2/languageStats._2)
        }
    }


    case class WaveFrontElement(translated:C, remaining:List[PartlyTranslated], remainingExemplar:C, diff:Double){}// translated, remaining, remaining exemplar, current
    type Front = List[WaveFrontElement]

    def translateRemaining( fromOne: Char, fromTwo: Char, actual: ((Char, Char), Int), translateds: List[PartlyTranslated]):(List[PartlyTranslated],C) = {
        val raw = translateds.map{ x=>
           x match {
               case PartlyTranslated(a1, y, _, yOpt, c) if a1 == fromOne  => PartlyTranslated(fromOne, y, Some(actual._1._1), yOpt,c)
               case PartlyTranslated(a1, y, _, yOpt, c) if a1 == fromTwo  => PartlyTranslated(fromTwo, y, Some(actual._1._2), yOpt,c)
               case PartlyTranslated(y,a1, yOpt, _, c) if a1 == fromOne  => PartlyTranslated(y, fromOne, yOpt,Some(actual._1._1),c)
               case PartlyTranslated(y,a1, yOpt, _, c) if a1 == fromTwo  => PartlyTranslated(y, fromTwo, yOpt,Some(actual._1._2),c)
               case xx => xx
           }
        }
        val filterAccidentallyTranslated = raw.filter(_ match {
            case PartlyTranslated(_,_,Some(_), Some(_),_) => true
            case _ => false
        }).map(x => ((x.decryptedFrom.get, x.decryptedTo.get),x.count))

        (raw.filter(_ match {
            case PartlyTranslated(_,_,Some(_), Some(_),_) => false
            case _ => true
        }), filterAccidentallyTranslated )
    }

    def nextWaveFront(f:Front)(implicit exampleCount:Int):Front = {

        val element = f.head

        def advanceWF(x: Char, y: Char, count: Int, nextTranslation: ((Char, Char), Int)) = {
            val tempRemainingExemplar = element.remainingExemplar.filterNot(_._1 == nextTranslation._1)
            val (newRemaining, additionalTranslated) = translateRemaining(x, y, nextTranslation, element.remaining.tail)
            val newTranslated: C = ((nextTranslation._1, count) :: element.translated) ::: additionalTranslated
            val newRemainExemplar = tempRemainingExemplar.filter(x => !additionalTranslated.find(_._1 == x._1).isDefined)
            val newStats = (newTranslated ::: newRemaining.map(x =>
                ((x.decryptedFrom.getOrElse(x.encryptedFrom), x.decryptedTo.getOrElse(x.encryptedTo)), x.count))
                           )
            val newDiff = countDiff((newStats, exampleCount))
            WaveFrontElement(newTranslated, newRemaining, newRemainExemplar, newDiff)
        }

        val newelement:WaveFrontElement = element.remaining.head match {
            case PartlyTranslated(x,y,None,None, count) => {
                val nextTranslation = element.remainingExemplar.head
                advanceWF(x, y, count, nextTranslation)
            }
            case PartlyTranslated(x,y,Some(found),None, count) => {
                val nextTranslation = element.remainingExemplar.find(_._1._1 == found).getOrElse(((found,y),count))
                advanceWF(x, y, count, nextTranslation)
            }
            case PartlyTranslated(x,y,None,Some(found), count) => {
                val nextTranslation = element.remainingExemplar.find(_._1._2 == found).getOrElse(((y,found),count))
                advanceWF(x, y, count, nextTranslation)
            }
        }


        f
    }

    {
        val encrypted = Crypter.encrypt("labdien dargie vēlētāji")
        val encryptedStats = collectStats(encrypted)
        val start:List[PartlyTranslated] = encryptedStats._1.map(x => PartlyTranslated(x._1._1,x._1._2,None,None, x._2))
        val front:Front = List(WaveFrontElement(List(), start, languageStats._1, countDiff(encryptedStats)))

        nextWaveFront(front)(encryptedStats._2)

    }





    println(languageStats._2)
    println(languageStats._1)
    println(languageStats._1.length)

    println(countDiff(languageStats))

}

object Crypter {
    def encrypt(s:String):String = {

        val alphabet = "AĀBCČDEĒFGĢHIĪJKĶLĻMNŅOPRSŠTUŪVZŽ ".toCharArray.toList

        val source = Random.shuffle(alphabet)
        val dest = Random.shuffle(alphabet)

        val key:Map[Char,Char] = (source zip dest).toMap

        val encrypted = s.toUpperCase().map(key(_))

        encrypted
    }
}