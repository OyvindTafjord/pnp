package org.allenai.pnp.semparse

import scala.collection.JavaConverters._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map => MutableMap }
import scala.collection.mutable.MultiMap
import scala.collection.mutable.{ Set => MutableSet }
import scala.collection.mutable.SetBuilder

import org.allenai.pnp.CompGraph
import org.allenai.pnp.Env
import org.allenai.pnp.ExecutionScore.ExecutionScore
import org.allenai.pnp.Pnp
import org.allenai.pnp.PnpModel
import org.allenai.pnp.util.Trie

import com.google.common.base.Preconditions
import com.jayantkrish.jklol.ccg.lambda.Type
import com.jayantkrish.jklol.ccg.lambda.TypeDeclaration
import com.jayantkrish.jklol.ccg.lambda2.Expression2
import com.jayantkrish.jklol.ccg.lambda2.StaticAnalysis
import com.jayantkrish.jklol.util.IndexedList

import edu.cmu.dynet._
import org.allenai.wikitables.SemanticParserFeatureGenerator

// Jayant notes:
// 1. partial entity name matches are a problem. E.g., one entity is "temagami lorraine mine",
// and the other is "temagami". Both have equivalent scores for matching the string "temagami",
// but the second score should be greater.
// 2. getting the column applicable to a cell is a problem
// 3.
/** A parser mapping token sequences to a distribution over
  * logical forms.
  */
class SemanticParser(val actionSpace: ActionSpace, val vocab: IndexedList[String],
    val config: SemanticParserConfig, forwardBuilder: LstmBuilder, backwardBuilder: LstmBuilder,
    actionBuilder: LstmBuilder, val model: PnpModel) {

  var dropoutProb = -1.0
  
  import SemanticParser._

  private def initializeRnns(computationGraph: CompGraph): Unit = {
    forwardBuilder.newGraph()
    backwardBuilder.newGraph()
    actionBuilder.newGraph()
  }

  /** Compute the input encoding of a list of tokens
    */
  def encode(tokens: Array[Int], entityLinking: EntityLinking): Pnp[InputEncoding] = {

    for {
      compGraph <- Pnp.computationGraph()
      _ = initializeRnns(compGraph)
      inputEncoding = encodeInput(compGraph, entityLinking, tokens)
    } yield {
      inputEncoding
    }
  }
  
  private def encodeInput(compGraph: CompGraph,
      entityLinking: EntityLinking, tokens: Array[Int]): InputEncoding = {
    
    val wordEmbeddings = compGraph.getLookupParameter(SemanticParser.WORD_EMBEDDINGS_PARAM)
    def tokenIdToEmbedding(tokenId: Int): Expression = {
      if (tokenId < vocab.size) {
        Expression.lookup(wordEmbeddings, tokenId)
      } else {
        Expression.lookup(wordEmbeddings, vocab.size)
      }
    }

    val entityEncoding = encodeEntities(compGraph, entityLinking, tokens, tokenIdToEmbedding)
    val inputEncoding = rnnEncode(compGraph, tokens, tokenIdToEmbedding)
    InputEncoding(tokens, inputEncoding._1, inputEncoding._2, inputEncoding._3,
        inputEncoding._4, entityEncoding)
  }

  private def rnnEncode(computationGraph: CompGraph, tokens: Seq[Int],
      tokenIdToEmbedding: Int => Expression
      ): (ExpressionVector, Expression, Expression, Expression) = {
    import Expression.{ dropout, reshape }

    val inputEmbeddings = tokens.map(tokenIdToEmbedding(_)).toArray

    // TODO: should we add dropout to the builders using the .set_dropout methods?
    forwardBuilder.startNewSequence()
    val fwOutputs = ListBuffer[Expression]()
    for (inputEmbedding <- inputEmbeddings) {
      val fwOutput = forwardBuilder.addInput(inputEmbedding)
      val fwOutputDropout = if (dropoutProb > 0.0) {
        dropout(fwOutput, dropoutProb.asInstanceOf[Float])
      } else {
        fwOutput
      }
      fwOutputs += fwOutputDropout
    }

    backwardBuilder.startNewSequence()
    val bwOutputs = ListBuffer[Expression]()
    for (inputEmbedding <- inputEmbeddings.reverse) {
      val bwOutput = backwardBuilder.addInput(inputEmbedding)
      val bwOutputDropout = if (dropoutProb > 0.0) {
        dropout(bwOutput, dropoutProb.asInstanceOf[Float])
      } else {
        bwOutput
      }
      bwOutputs += bwOutputDropout
    }
    
    val outputEmbeddings = fwOutputs.toArray.zip(bwOutputs.toList.reverse).map(
        x => concatenateArray(Array(x._1, x._2)))
 
    val sentEmbedding = concatenateArray(Array(fwOutputs.last, bwOutputs.last)) 
    val inputMatrix = concatenateArray(inputEmbeddings.map(x => reshape(x, Dim(1, config.inputDim))).toArray)
    val outputMatrix = concatenateArray(outputEmbeddings.map(reshape(_, Dim(1, 2 * config.hiddenDim))).toArray)
    
    // TODO: figure out how to initialize the decoder from both the
    // forward and backward LSTMs
    (forwardBuilder.finalS, sentEmbedding, inputMatrix, outputMatrix)
  }

  private def encodeEntities(computationGraph: CompGraph, entityLinking: EntityLinking,
      tokens: Array[Int], tokenIdToEmbedding: Int => Expression): EntityEncoding = {

    val tokenEmbeddings = tokens.map(tokenIdToEmbedding(_))
    
    val typedEntityTokenMatrices = for {
      t <- entityLinking.entityTypes
    } yield {
      val entities = entityLinking.getEntitiesWithType(t)
      val entityLinkingFeaturizedParam = Expression.parameter(computationGraph.getParameter(
          ENTITY_LINKING_FEATURIZED_PARAM + t))
      val entityScores = entities.map(e => scoreEntityTokensMatch(e,
          tokens, tokenEmbeddings, tokenIdToEmbedding, entityLinking, entityLinkingFeaturizedParam))
      
      val entityScoreMatrix = Expression.concatenateCols(new ExpressionVector(entityScores))
      
      (t, entityScoreMatrix)
    }
    
    val entityEmbeddingMatrices = for {
      t <- entityLinking.entityTypes
    } yield {
      val entities = entityLinking.getEntitiesWithType(t)
          
      val entityEmbeddings = entities.map(e => encodeBow(e, tokenIdToEmbedding))
      val entityEmbeddingMatrix = Expression.concatenateCols(new ExpressionVector(entityEmbeddings))
      
      (t, entityEmbeddingMatrix)
    }

    EntityEncoding(typedEntityTokenMatrices.toMap, entityEmbeddingMatrices.toMap, entityLinking)
  }

  private def scoreEntityTokensMatch(entity: Entity, tokens: Array[Int],
      tokenEmbeddings: Array[Expression], tokenIdToEmbedding: Int => Expression,
      entityLinking: EntityLinking, entityLinkingFeaturizedParam: Expression): Expression = {
    
    val inVocabNameTokens = entity.nameTokensSet.filter(_ < vocab.size)
    val entityNameEmbeddings = inVocabNameTokens.map(tokenIdToEmbedding(_)).toArray
    
    val tokenScores = tokens.zipWithIndex.map(t =>
      scoreEntityTokenMatch(entity, t._1,
          tokenEmbeddings(t._2), t._2, entityNameEmbeddings,
          entityLinkingFeaturizedParam))

    var tokenScoresExpr = Expression.concatenate(new ExpressionVector(tokenScores))
    if (config.featureGenerator.isDefined) {
      val (dim, floatVector) = entityLinking.getTokenFeatures(entity)
      val featureMatrix = Expression.input(dim, floatVector)

      tokenScoresExpr += featureMatrix * entityLinkingFeaturizedParam
    }

    tokenScoresExpr
  }

  private def scoreEntityTokenMatch(entity: Entity, tokenId: Int,
      tokenEmbedding: Expression, tokenIndex: Int, entityNameEmbeddings: Array[Expression],
      entityLinkingFeaturizedParam: Expression): Expression = {
    var score = Expression.input(0.0f)

    if (config.entityLinkingLearnedSimilarity && tokenId < vocab.size) {
      if (entityNameEmbeddings.size > 0) {
        val entityTokenSimilarities = entityNameEmbeddings.map(x =>
          Expression.dotProduct(x, tokenEmbedding))

        if (entityTokenSimilarities.size > 1) {
          score = score + entityTokenSimilarities.reduce((x, y) => Expression.max(x, y))
        } else {
          score = score + entityTokenSimilarities(0)
        }
      }
    }

    score
  }

  private def encodeBow(entity: Entity, tokenIdToEmbedding: Int => Expression): Expression = {
    entity.nameTokens.map(tokenIdToEmbedding).reduce(_ + _) / entity.nameTokens.length
  }

  def generateExpression(tokens: Array[Int], entityLinking: EntityLinking): Pnp[Expression2] = {
    for {
      state <- parse(tokens, entityLinking)
    } yield {
      state.decodeExpression
    }
  }

  /** Generate a distribution over logical forms given 
    * tokens.
    */
  def parse(tokens: Array[Int], entityLinking: EntityLinking): Pnp[SemanticParserState] = {
    for {
      // Encode input tokens using an LSTM.
      input <- encode(tokens, entityLinking)

      state = SemanticParserState.start
      
      // Choose the root type for the logical form given the
      // final output of the LSTM.
      rootWeights <- Pnp.param(SemanticParser.ROOT_WEIGHTS_PARAM)
      rootBias <- Pnp.param(SemanticParser.ROOT_BIAS_PARAM)
      rootScores = (rootWeights * input.sentEmbedding) + rootBias
      rootType <- Pnp.choose(actionSpace.rootTypes, rootScores, state)
      
      // Recursively generate a logical form using an LSTM to
      // select logical form templates to expand on typed holes
      // in the partially-generated logical form.  
      expr <- parse(input, actionBuilder, state.addRootType(rootType))
    } yield {
      expr
    }
  }

  private def parse(input: InputEncoding, builder: RnnBuilder,
      startState: SemanticParserState): Pnp[SemanticParserState] = {
    // Initialize the output LSTM before generating the logical form.
    builder.startNewSequence(input.rnnState)
    val startRnnState = builder.state()

    for {
      beginActionsParam <- Pnp.param(SemanticParser.BEGIN_ACTIONS + startState.unfilledHoleIds(0).t)
      e <- parse(input, builder, beginActionsParam, startRnnState, startState)
    } yield {
      e
    }
  }

  /** Recursively generates a logical form from a partial logical
    * form containing typed holes. Each application fills a single
    * hole with a template representing a constant, application or
    * lambda expression. These templates may contain additional holes
    * to be filled in the future. An LSTM is used to encode the history
    * of previously generated templates and select which template to
    * apply. 
    */
  private def parse(input: InputEncoding, builder: RnnBuilder, prevInput: Expression,
      rnnState: Int, state: SemanticParserState): Pnp[SemanticParserState] = {
    if (state.unfilledHoleIds.length == 0) {
      // If there are no holes, return the completed logical form.
      Pnp.value(state)
    } else {
      // Select the first unfilled hole and select the
      // applicable templates given the hole's type.
      val hole = state.unfilledHoleIds.head
      val actionTemplates = actionSpace.getTemplates(hole.t)
      val allVariableTemplates = hole.scope.getVariableTemplates(hole.t)
      val variableTemplates = if (allVariableTemplates.length > config.maxVars) {
        // The model only has parameters for MAX_VARS variables. 
        allVariableTemplates.slice(0, config.maxVars)
      } else {
        allVariableTemplates
      }
      val baseTemplates = actionTemplates ++ variableTemplates

      val entities = input.entityEncoding.entityLinking.getEntitiesWithType(hole.t)
      val entityTemplates = entities.map(_.template)
      val entityTokenMatrix = input.entityEncoding.tokenEntityScoreMatrices.getOrElse(hole.t, null)

      val allTemplates = baseTemplates ++ entityTemplates

      // Update the LSTM and use its output to score
      // the applicable templates.
      val rnnOutput = builder.addInput(rnnState, prevInput)
      val rnnOutputDropout = if (dropoutProb > 0.0) {
        Expression.dropout(rnnOutput, dropoutProb.asInstanceOf[Float])
      } else {
        rnnOutput
      }
      val nextRnnState = builder.state
      for {
        // Compute an attention vector
        attentionWeights <- Pnp.param(SemanticParser.ATTENTION_WEIGHTS_PARAM)
        wordAttentions = Expression.transpose(
          Expression.softmax(input.encodedTokenMatrix * attentionWeights * rnnOutputDropout))

        // Attention vector using the input token vectors 
        // attentionVector = transpose(wordAttentions * input.tokenMatrix)
        // Attention vector using the encoded tokens 
        attentionVector = Expression.transpose(wordAttentions * input.encodedTokenMatrix)
        
        /*
        attentionActionWeights <- param(SemanticParser.ATTENTION_ACTION_WEIGHTS_PARAM + hole.t)
        attentionActionScores = attentionActionWeights * attentionVector

        actionWeights <- param(SemanticParser.ACTION_WEIGHTS_PARAM + hole.t)
        actionBias <- param(SemanticParser.ACTION_BIAS_PARAM + hole.t)
        rnnActionScores = actionWeights * rnnOutputDropout
        */

        actionHiddenWeights <- Pnp.param(SemanticParser.ACTION_HIDDEN_WEIGHTS)
        actionHiddenWeights2 <- Pnp.param(SemanticParser.ACTION_HIDDEN_ACTION + hole.t)
        actionHiddenEntityWeights <- Pnp.param(SemanticParser.ACTION_HIDDEN_ENTITY + hole.t)
        attentionAndRnn = concatenateArray(Array(attentionVector, rnnOutputDropout))
        actionHidden = Expression.tanh(actionHiddenWeights * attentionAndRnn)
        actionHiddenDropout = if (dropoutProb > 0.0) {
          Expression.dropout(actionHidden, dropoutProb.asInstanceOf[Float])
        } else {
          actionHidden
        }
        actionHiddenScores = actionHiddenWeights2 * actionHidden
 
        // Score the templates.
        actionScores = Expression.pickrange(actionHiddenScores, 0, baseTemplates.length)

        // Score the entity templates
        entityBias <- Pnp.param(SemanticParser.ENTITY_BIAS_PARAM + hole.t)
        entityWeights <- Pnp.param(SemanticParser.ENTITY_WEIGHTS_PARAM + hole.t)

        allScores = if (entities.size > 0) {
          val entityScores = wordAttentions * entityTokenMatrix
          val entityScoresVector = Expression.transpose(entityScores)
          concatenateArray(Array(actionScores, entityScoresVector))
        } else {
          actionScores
        }

        // Nondeterministically select which template to update
        // the parser's state with. The tag for this choice is 
        // its index in the sequence of generated templates, which
        // can be used to supervise the parser.
        _ = if (allTemplates.size == 0) {
          println("Warning: no actions from hole " + hole)
        } else {
          ()
        }
        
        templateTuple <- Pnp.choose(allTemplates.zipWithIndex.toArray, allScores, state)
        nextState = templateTuple._1.apply(state).addAttention(wordAttentions)

        // Get the LSTM input parameters associated with the chosen
        // template.
        cg <- Pnp.computationGraph()
        actionLookup = cg.getLookupParameter(SemanticParser.ACTION_LOOKUP_PARAM + hole.t)
        entityLookup = cg.getLookupParameter(SemanticParser.ENTITY_LOOKUP_PARAM + hole.t)
        index = templateTuple._2
        actionInput = if (index < baseTemplates.length) {
          Expression.lookup(actionLookup, templateTuple._2)
        } else {
          // TODO: using a single parameter vector for all entities seems suboptimal. 
          Expression.lookup(entityLookup, 0)
        }

        // Recursively fill in any remaining holes.
        returnState <- parse(input, builder, concatenateArray(Array(actionInput, attentionVector)),
            nextRnnState, nextState)
      } yield {
        returnState
      }
    }
  }

  private def concatenateArray(exprs: Array[Expression]): Expression = {
    val expressionVector = new ExpressionVector(exprs.length)
    for (i <- 0 until exprs.length) {
      expressionVector.update(i, exprs(i))
    }
    Expression.concatenate(expressionVector)
  }

  /** Generate the sequence of parser actions that produces exp.
    * This method assumes that only one such action sequence exists.  
    */
  def generateActionSequence(exp: Expression2, entityLinking: EntityLinking,
      typeDeclaration: TypeDeclaration): Option[(List[Type], List[Template])] = {
    val holeIndexMap = MutableMap[Int, Int]()
    val actionTypes = ListBuffer[Type]()
    val actions = ListBuffer[Template]()

    val typeMap = StaticAnalysis.inferTypeMap(exp, TypeDeclaration.TOP, typeDeclaration)
      .asScala.toMap

    var state = SemanticParserState.start().addRootType(typeMap(0))
    holeIndexMap(state.nextHole.get.id) = 0  
    
    while (state.nextHole.isDefined) {
      val hole = state.nextHole.get
      val expIndex = holeIndexMap(hole.id)
      val currentScope = hole.scope

      val curType = typeMap(expIndex)
      Preconditions.checkState(curType.equals(hole.t),
          "type-checked type %s does not match hole %s at index %s of %s",
          curType, hole.t, expIndex.asInstanceOf[Integer], exp)

      val templates = actionSpace.getTemplates(curType) ++
        currentScope.getVariableTemplates(curType) ++
        entityLinking.getEntitiesWithType(curType).map(_.template)
      val matches = templates.filter(_.matches(expIndex, exp, typeMap))
      if (matches.size != 1) {
        println("Found " + matches.size + " for expression " + exp.getSubexpression(expIndex) +
            " : "  + curType + " (expected 1)")
        return None
      }
      
      val theMatch = matches.toList(0)
      state = theMatch.apply(state)
      
      actionTypes += curType
      actions += theMatch

      var holeOffset = 0 
      for ((holeIndex, hole) <- theMatch.holeIndexes.zip(
          state.unfilledHoleIds.slice(0, theMatch.holeIndexes.length))) {
        val curIndex = expIndex + holeIndex + holeOffset
        holeIndexMap(hole.id) = curIndex
        holeOffset += (exp.getSubexpression(curIndex).size - 1)
      }
    }

    val decoded = state.decodeExpression
    Preconditions.checkState(decoded.equals(exp), "Expected %s and %s to be equal", decoded, exp)
    
    Some((actionTypes.toList, actions.toList))
  }
  
  /** Generate an execution score that constrains the
    * parser to generate exp. This oracle can be used 
    * to supervise the parser when training with loglikelihood.
    */
  def getLabelScore(exp: Expression2, entityLinking: EntityLinking,
      typeDeclaration: TypeDeclaration): Option[SemanticParserExecutionScore] = {
    for {
      (holeTypes, templates) <- generateActionSequence(exp, entityLinking, typeDeclaration)
    } yield {
      new SemanticParserExecutionScore(holeTypes.toArray, templates.toArray,
          Double.NegativeInfinity)
    }
  }
  
  def getMarginScore(exp: Expression2, entityLinking: EntityLinking,
      typeDeclaration: TypeDeclaration): Option[SemanticParserExecutionScore] = {
    for {
      (holeTypes, templates) <- generateActionSequence(exp, entityLinking, typeDeclaration)
    } yield {
      new SemanticParserExecutionScore(holeTypes.toArray, templates.toArray, 1.0)
    }
  }
  
  /**
   * Generate an execution score that constrains the parser
   * to produce any expression in {@code exprs}. This oracle
   * can be used to supervise the parser when training with
   * loglikelihood with weak supervision, i.e., when multiple
   * logical forms may be correct.
   */
  def getMultiLabelScore(exprs: Iterable[Expression2], entityLinking: EntityLinking,
      typeDeclaration: TypeDeclaration): Option[SemanticParserMultiExecutionScore] = {
    getMultiScore(exprs, entityLinking, typeDeclaration, Double.NegativeInfinity)
  }

  def getMultiMarginScore(exprs: Iterable[Expression2], entityLinking: EntityLinking,
      typeDeclaration: TypeDeclaration): Option[SemanticParserMultiExecutionScore] = {
    getMultiScore(exprs, entityLinking, typeDeclaration, 1.0)
  }
  
  private def getMultiScore(exprs: Iterable[Expression2], entityLinking: EntityLinking,
      typeDeclaration: TypeDeclaration, incorrectCost: Double
      ): Option[SemanticParserMultiExecutionScore] = {
    val oracles = for {
      lf <- exprs
      oracle <- getLabelScore(lf, entityLinking, typeDeclaration)
    } yield {
      oracle
    }

    if (oracles.size > 0) {
      val score = new SemanticParserMultiExecutionScore(oracles, incorrectCost)
      Some(score)
    } else {
      None
    }
  }

  
  /**
   * Serialize this parser into {@code saver}. This method
   * assumes that the {@code model} has already been 
   * serialized (using {@code model.save}).
   */
  def save(saver: ModelSaver): Unit = {
    saver.addObject(actionSpace)
    saver.addObject(vocab)
    saver.addObject(config)
    saver.addLstmBuilder(forwardBuilder)
    saver.addLstmBuilder(backwardBuilder)
    saver.addLstmBuilder(actionBuilder)
  }
}

/** Execution score that constrains a SemanticParser
  * to generate the given rootType and sequence of
  * templates. 
  */
class SemanticParserExecutionScore(val holeTypes: Array[Type],
    val templates: Array[Template], val incorrectCost: Double)
extends ExecutionScore {

  val rootType = holeTypes(0)
  val reversedTemplates = templates.reverse

  def apply(tag: Any, choice: Any, env: Env): Double = {
    if (tag != null && tag.isInstanceOf[SemanticParserState]) {
      val state = tag.asInstanceOf[SemanticParserState]
      if (state.numActions == 0 && state.unfilledHoleIds.length == 0) {
        // This state represents the start of parsing where we
        // choose the root type.
        Preconditions.checkArgument(choice.isInstanceOf[Type])
        if (choice.asInstanceOf[Type].equals(rootType)) {
          0.0
        } else {
          incorrectCost
        }
      } else {
        val actionInd = state.numActions
        if (actionInd < templates.size) {
          // TODO: this test may be too inefficient.
          val chosen = choice.asInstanceOf[(Template, Int)]._1
          val myTemplates = reversedTemplates.slice(reversedTemplates.length - actionInd, reversedTemplates.length)
          if (chosen.equals(templates(actionInd)) &&
              (actionInd == 0 || state.templates.zip(myTemplates).map(x => x._1.equals(x._2)).reduce(_ && _))) { 
            0.0
          } else {
            incorrectCost
          }
        } else {
          incorrectCost
        }
      }
    } else {
      0.0
    }
  }
}

class MaxExecutionScore(val scores: Seq[ExecutionScore]) extends ExecutionScore {
  def apply(tag: Any, choice: Any, env: Env): Double = {
    return scores.map(s => s.apply(tag, choice, env)).max
  }
}

class SemanticParserMultiExecutionScore(val scores: Iterable[SemanticParserExecutionScore],
    val incorrectCost: Double)
  extends ExecutionScore {
  
  val trie = new Trie[AnyRef]()
  
  for (score <- scores) {
    val key = List(score.rootType) ++ score.templates
    trie.insert(key)
  }
  
  def apply(tag: Any, choice: Any, env: Env): Double = {
    if (tag != null && tag.isInstanceOf[SemanticParserState]) {
      val state = tag.asInstanceOf[SemanticParserState]
      val key = if (state.hasRootType) {
        Array(state.rootType) ++ state.getTemplates
      } else {
        Array()
      }

      val trieNodeId = trie.lookup(key)      
      if (trieNodeId.isDefined) {
        val nextStates = trie.getNextMap(trieNodeId.get)

        // First action is a type. Remaining actions are templates, but they
        // come paired with an index that the parser uses for selection purposes.
        val action: AnyRef = if (state.hasRootType) {
          choice.asInstanceOf[(Template, Int)]._1
        } else {
          choice.asInstanceOf[Type]
        }

        if (nextStates.contains(action)) {
          0.0
        } else {
          incorrectCost
        }
      } else {
        incorrectCost
      }
    } else {
      0.0
    }
  }
}

class SemanticParserConfig extends Serializable {
  var inputDim = 200
  var hiddenDim = 100
  var actionDim = 100
  var actionHiddenDim = 100
  var maxVars = 10

  var featureGenerator: Option[SemanticParserFeatureGenerator] = None
  
  var entityLinkingLearnedSimilarity = false
  var distinctUnkVectors = false
}

object SemanticParser {
  
  // Parameter names used by the parser
  val WORD_EMBEDDINGS_PARAM = "wordEmbeddings"
  val ROOT_WEIGHTS_PARAM = "rootWeights"
  val ROOT_BIAS_PARAM = "rootBias"
  
  val BEGIN_ACTIONS = "beginActions:"
  val ACTION_WEIGHTS_PARAM = "actionWeights:"
  val ACTION_BIAS_PARAM = "actionBias:"
  val ACTION_LOOKUP_PARAM = "actionLookup:"
  
  val ATTENTION_WEIGHTS_PARAM = "attentionWeights:"
  val ATTENTION_ACTION_WEIGHTS_PARAM = "attentionActionWeights:"

  val ACTION_HIDDEN_WEIGHTS = "actionHidden"
  val ACTION_HIDDEN_ACTION = "actionHiddenOutput:"
  val ACTION_HIDDEN_ENTITY = "actionHiddenEntityOutput:"
  
  val ENTITY_BIAS_PARAM = "entityBias:"
  val ENTITY_WEIGHTS_PARAM = "entityWeights:"
  val ENTITY_LOOKUP_PARAM = "entityLookup:"
  
  val ENTITY_LINKING_FEATURIZED_PARAM = "entityLinkingFeaturized:"
  val ENTITY_TYPE_INPUT_PARAM = "entityTypeInput"
  
  def create(actionSpace: ActionSpace, vocab: IndexedList[String],
      config: SemanticParserConfig, model: PnpModel): SemanticParser = {
    // Initialize model
    // TODO: document these parameters.
    model.addParameter(ROOT_WEIGHTS_PARAM, Dim(actionSpace.rootTypes.length, 2 * config.hiddenDim))
    model.addParameter(ROOT_BIAS_PARAM, Dim(actionSpace.rootTypes.length))
    model.addParameter(ATTENTION_WEIGHTS_PARAM, Dim(2 * config.hiddenDim, config.actionDim))

    model.addParameter(ACTION_HIDDEN_WEIGHTS, Dim(config.actionHiddenDim, config.inputDim + config.hiddenDim))

    // The last entry will be the unknown word.
    model.addLookupParameter(WORD_EMBEDDINGS_PARAM, vocab.size + 1, Dim(config.inputDim))
    
    for (t <- actionSpace.getTypes) {
      val actions = actionSpace.getTemplates(t)
      val dim = actions.length + config.maxVars

      model.addParameter(BEGIN_ACTIONS + t, Dim(config.actionDim + config.inputDim))
      model.addParameter(ACTION_WEIGHTS_PARAM + t, Dim(dim, config.hiddenDim))
      model.addParameter(ACTION_BIAS_PARAM + t, Dim(dim))
      model.addLookupParameter(ACTION_LOOKUP_PARAM + t, dim, Dim(config.actionDim))

      model.addParameter(ATTENTION_ACTION_WEIGHTS_PARAM + t, Dim(dim, config.inputDim))
      
      model.addParameter(ACTION_HIDDEN_ACTION + t, Dim(dim, config.actionHiddenDim))
      model.addParameter(ACTION_HIDDEN_ENTITY + t, Dim(config.inputDim, config.actionHiddenDim))
      
      model.addParameter(ENTITY_BIAS_PARAM + t, Dim(1))
      model.addParameter(ENTITY_WEIGHTS_PARAM + t, Dim(config.hiddenDim))
      model.addLookupParameter(ENTITY_LOOKUP_PARAM + t, 1, Dim(config.actionDim))

      if (config.featureGenerator.isDefined) {
        model.addParameter(ENTITY_LINKING_FEATURIZED_PARAM + t,
            Dim(config.featureGenerator.get.numFeatures))
      }
    }

    model.addLookupParameter(ENTITY_TYPE_INPUT_PARAM, actionSpace.typeIndex.size,
        Dim(config.inputDim))

    // Forward and backward RNNs for encoding the input token sequence
    val forwardBuilder = new LstmBuilder(1, config.inputDim, config.hiddenDim, model.model)
    val backwardBuilder = new LstmBuilder(1, config.inputDim, config.hiddenDim, model.model)
    // RNN for generating actions given previous actions (and the input)
    val actionBuilder = new LstmBuilder(1, config.actionDim + config.inputDim, config.hiddenDim, model.model)

    new SemanticParser(actionSpace, vocab, config, forwardBuilder,
        backwardBuilder, actionBuilder, model)
  }

  def load(loader: ModelLoader, model: PnpModel): SemanticParser = {
    val actionSpace = loader.loadObject(classOf[ActionSpace])
    val vocab = loader.loadObject(classOf[IndexedList[String]])
    val config = loader.loadObject(classOf[SemanticParserConfig])
    val forwardBuilder = loader.loadLstmBuilder()
    val backwardBuilder = loader.loadLstmBuilder()
    val actionBuilder = loader.loadLstmBuilder()
    
    new SemanticParser(actionSpace, vocab, config, forwardBuilder,
        backwardBuilder, actionBuilder, model)
  }
  
  // TODO: move this method somewhere else.
  def seqToMultimap[A, B](s: Seq[(A, B)]) = { 
    s.foldLeft(new HashMap[A, MutableSet[B]] with MultiMap[A, B]){ 
      (acc, pair) => acc.addBinding(pair._1, pair._2)
    }
  }
}

/**
 * The row dimension of tokenMatrix, encodedTokenMatrix and 
 * tokenEntityScoreMatrices corresponds to tokens. 
 */
case class InputEncoding(val tokens: Array[Int], val rnnState: ExpressionVector,
    val sentEmbedding: Expression, val tokenMatrix: Expression, val encodedTokenMatrix: Expression,
    val entityEncoding: EntityEncoding) {
}

/**
 * The columns of tokenEntityScoreMatrices and entityEmbeddingMatrices
 * correspond to entities of that type, in the same order as in
 * entityLinking. 
 */
case class EntityEncoding(val tokenEntityScoreMatrices: Map[Type, Expression],
    val entityEmbeddingMatrices: Map[Type, Expression], val entityLinking: EntityLinking)

class ExpressionDecodingException extends RuntimeException {
  
}
