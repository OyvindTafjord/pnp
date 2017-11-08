package org.allenai.wikitables

import org.allenai.pnp.PnpModel
import org.allenai.pnp.semparse.SemanticParser

import com.jayantkrish.jklol.ccg.lambda2.{ExpressionSimplifier, SimplificationComparator}
import edu.cmu.dynet.{Initialize, ModelLoader}
import edu.stanford.nlp.sempre.tables.TableKnowledgeGraph
import org.json4s.{DefaultFormats, Formats}
import org.scalatra._
import org.scalatra.json._

class WikiTablesServlet extends ScalatraServlet with JacksonJsonSupport {

  val BEAM_SIZE = 10
  val NUM_ANSWERS = 10
  val MODEL_FILE = "/Users/tafjord/data/pnp_wikitables/trained_models/fold5/parser_final.ser"
  val URL_ROUTE = "/wikitables"

  // scalastyle:off
  val examples = Seq(
    ("Which food from the chart has the lowest percentage of calcium per serving?",
      "Food,Percentage of Calcium Per serving\nMilk,30\nOrange juice,2\nYogurt,30\nCheese,10\nBaked beans,6"),
    ("What was the sky condition on the day with the most snow?",
      "Day,Temperature,Sky Condition,Snow\nMonday,31 F,partly cloudy,none\nTuesday,UL 31,cloudy,1 inch\nWednesday,29 F,mostly cloudy,2 inches\nThursday,18 F,cloudy,3 inches\nFriday,32 F,clear Clear,none"),
    ("Which two objects are both smooth cubes?",
      "Object,A Mass,B Color,C,D\n1,25 grams,red,smooth,sphere\n2,35 grams,yellow,rough,cylinder\n3,30 grams,green,smooth,cube\n4,25 grams,red,rough,sphere\n5,30 grams,blue,smooth,cube"),
    ("Identify the mineral in the table that is hard, has a black streak, and has no reaction with acid.",
      "Mineral,Hardness,Streak,Reaction with Acid\ncalcite,soft,colorless or white,bubbles\nchalcopyrite,hard,gray or black,rotten-egg smell\nfeldspar,hard,colorless or white,no reaction\ngalena,soft,gray or black,rotten-egg smell\ngraphite,soft,gray or black,no reaction\ngypsum,soft,colorless or white,no reaction\nhornblende,hard,gray or black,no reaction"),
    ("One serving of which food item on the data table provides the most energy?",
      "Food Item (one serving),Number of Calories\nboiled egg,82\nhamburger,347\nice cream,240\nlow-fat milk,121")
  )
  // scalastyle:on


  protected implicit val jsonFormats: Formats = DefaultFormats

  case class Result(question: String, topAnswer: Answer, allAnswers: Seq[Answer])
  case class Answer(text: String, score: Double, logicalForm: String)

  def loadSerializedParser(modelFilename: String): SemanticParser = {
    val loader = new ModelLoader(modelFilename)
    val model = PnpModel.load(loader)
    val parser = SemanticParser.load(loader, model)
    loader.done()
    parser
  }

  Initialize.initialize(Map("dynet-mem" -> "4096"))

  private val simplifier = ExpressionSimplifier.lambdaCalculus()
  private val comparator = new SimplificationComparator(simplifier)
  private val parser = loadSerializedParser(MODEL_FILE)
  private val featureGenerator = parser.config.featureGenerator.get
  private val typeDeclaration = parser.config.typeDeclaration
  private val lfPreprocessor = parser.config.preprocessor


  private def makeHtmlTable(data: Seq[Seq[Any]], headers: Option[Seq[Any]] = None) = {
    <table border="1">
    {if (headers.isDefined) {
      <tr>
      {for (col <- headers.get) yield <td><b>{col.toString}</b></td>}
      </tr>
    }}
    {for (row <- data) yield {
      <tr>
        {for (col <- row) yield {
          <td>{col.toString}</td>
      }}
      </tr>
    }}
    </table>
  }

  get("/") {
    <html><body>Hello!</body></html>
  }

  get(URL_ROUTE) {
    val question = params.getOrElse("question", "")
    val tableCsv = params.getOrElse("tablecsv", "")
    val json = params.get("json") match {
      case Some("true") => true
      case _ => false
    }

    val questionPatched = "\"" + question + "\""
    val tableCsvPatched = tableCsv.replace("\n", "###").replace("\r", "")

    var answersOption: Option[List[Answer]] = None
    if (question != "" && tableCsv != "") {
      // Are there better alternatives than assigning the current timestamp as the exampleId?
      val exampleId: String = (System.currentTimeMillis / 1000).toString
      val sempreExample = WikiTablesDataProcessor.makeCustomExample(questionPatched,
        tableCsvPatched, exampleId)
      val pnpExample = WikiTablesUtil.convertCustomExampleToWikiTablesExample(sempreExample)
      val entityLinking = new WikiTablesEntityLinker().getEntityLinking(pnpExample)
      val contextValue = pnpExample.getContext()
      val graph = contextValue.graph.asInstanceOf[TableKnowledgeGraph]
      // Reusing example id  as the table id. The original idea of assigning table ids was to use
      // them to serialize them as json files. We don't need to do that at test time anyway.
      val table = Table.knowledgeGraphToTable(exampleId, graph)
      val processedExample = RawExample(pnpExample, entityLinking, table)
      WikiTablesUtil.preprocessExample(processedExample, parser.vocab,
        featureGenerator, typeDeclaration)
      val (testResult, denotations) = TestWikiTablesCli.test(Seq(processedExample.ex), parser,
        BEAM_SIZE, false, false, typeDeclaration, comparator,
        lfPreprocessor, _ => ())
      answersOption = Some(denotations.values.head.map {case (value, score, lf) =>
        Answer(TestWikiTablesCli.valueToStrings(value).mkString(", "), Math.exp(score), lf.toString)}
        .take(NUM_ANSWERS))
    }


    if (json) {
      contentType = formats("json")
      if (answersOption.isDefined) {
        val answers = answersOption.get
        val topAnswer = answers.find(_.text.nonEmpty).getOrElse(answers.head)
        Result(question = question, topAnswer = topAnswer, allAnswers = answers)
      }
    } else {
      var renderAnswer = <p></p>
      if (answersOption.isDefined) {
        val answers = answersOption.get
        val topAnswer = answers.find(_.text.nonEmpty).getOrElse(answers.head)
        val answerGrid = answers.map(_.productIterator.toList)
        val tableGrid = tableCsvPatched.split("###").toVector.map(_.split(",").toVector)
        renderAnswer =
          <p>
          <hr/>
          <b>Top answer: </b>{topAnswer.text}<br/>
          <b>Top confidence: </b>{topAnswer.score}
          <p/>
          {makeHtmlTable(answerGrid, Some(Seq("Answer", "Confidence", "Logical Form")))}
          <p/>
          {makeHtmlTable(tableGrid.tail, Some(tableGrid.head))}
          </p>
      }
      <html>
        <head><title>WikiTables Demo</title></head>
        <body>
          <h1>WikiTables Semantic Parser Demo</h1>
          <form action={URL_ROUTE}>
            <b>Question: </b>
            <input type="text" name="question" size="100" value={question}/>
            <p/>
            <b>Table CSV: </b>
            <br/>
            <textarea name="tablecsv" rows='10' cols='100'>{tableCsv}</textarea>
            <p/>
            <input type="submit" value="Submit"/>
          </form>
          {renderAnswer}
          <hr/>
          <h2>Examples</h2>
          {for (example <- examples) yield {
          <p>
            <a href={url(URL_ROUTE, Map("question" -> example._1, "tablecsv" -> example._2))}>
              {example._1}
            </a>
          </p>}}
        </body>
      </html>
    }
  }

}