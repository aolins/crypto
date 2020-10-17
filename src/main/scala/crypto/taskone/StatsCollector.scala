package crypto.taskone

import scala.io.Source

object StatsCollector extends App {
    val upper = Source.fromFile("src/main/resources/latvian.txt").getLines().next.toUpperCase
    val languageTuples = (upper zip upper.substring(1).concat(" ")).toList
    val totalNumber = languageTuples.length
    val grouped = languageTuples.groupBy(identity)
    val counted = grouped.map(x => (x._1, x._2.length)).toList.sortWith((x,y) => x._2 > y._2)


    for (examples <- Source.fromFile("src/main/resources/latvian.examples.txt").getLines()){

    }

}