package org.allenai.wikitables

import org.allenai.pnp.PnpModel
import org.allenai.pnp.semparse.SemanticParser

import com.jayantkrish.jklol.ccg.lambda2.{ExpressionSimplifier, SimplificationComparator}
import edu.cmu.dynet.{Initialize, ModelLoader}
import edu.stanford.nlp.sempre.tables.TableKnowledgeGraph
import org.scalatra._

class WikiTablesServlet extends ScalatraServlet {

  val BEAM_SIZE = 10
  val NUM_ANSWERS = 10
  val MODEL_FILE = "data/pnp_wikitables/trained_models/fold5/parser_final.ser"

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
  private val parser = loadSerializedParser("MODEL_FILE")
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

  get("/wikitables/answer") {
    val question = params.getOrElse("question", "")
    val tableCsv = params.getOrElse("tablecsv", "")
    val json = params.get("json") match {
      case Some("true") => true
      case _ => false
    }

    val questionPatched = "\"" + question+ "\""
    val tableCsvPatched = tableCsv.replace("\n", "###").replace("\r", "")

    var answers: Option[List[(String, Double, String)]] = None
    if (question != "" && tableCsv != "") {
      // Are there better alternatives than assigning the current timestamp as the exampleId?
      val exampleId: String = (System.currentTimeMillis / 1000).toString
      val sempreExample = WikiTablesDataProcessor.makeCustomExample(questionPatched,
        tableCsvPatched, exampleId)
      val pnpExample = WikiTablesUtil.convertCustomExampleToWikiTablesExample(sempreExample)
      val entityLinking = new WikiTablesEntityLinker().getEntityLinking(pnpExample)
      val contextValue = pnpExample.getContext()
      val graph = contextValue.graph.asInstanceOf[TableKnowledgeGraph]
      // Reusing example id  as the table id. The original idea of assigning table ids was to use them
      // to serialize them as json files. We don't need to do that at test time anyway.
      val table = Table.knowledgeGraphToTable(exampleId, graph)
      val processedExample = RawExample(pnpExample, entityLinking, table)
      WikiTablesUtil.preprocessExample(processedExample, parser.vocab,
        featureGenerator, typeDeclaration)
      val (testResult, denotations) = TestWikiTablesCli.test(Seq(processedExample.ex), parser,
        BEAM_SIZE, false, false, typeDeclaration, comparator,
        lfPreprocessor, _ => ())
      answers = Some(denotations.values.head.map {case (value, score, lf) =>
        (TestWikiTablesCli.valueToStrings(value).mkString(", "), Math.exp(score), lf.toString)}
        .take(NUM_ANSWERS))
    }

    if (json) {
      "Not implemented"
    } else {
      var renderAnswer = <p></p>
      if (answers.isDefined) {
        val topAnswer = answers.get.find(_._1.nonEmpty).map(_._1).getOrElse("")
        val topScore = answers.get.find(_._1.nonEmpty).map(_._2).getOrElse(0d)
        val answerGrid = answers.get.map(_.productIterator.toList)
        val tableGrid = tableCsvPatched.split("###").toVector.map(_.split(",").toVector)
        renderAnswer =
          <p>
          <hr/>
          <b>Top answer: </b>{topAnswer}<br/>
          <b>Top confidence: </b>{topScore}
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
          <form action="/wikitables/answer">
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
        </body>
      </html>
    }
  }

}
