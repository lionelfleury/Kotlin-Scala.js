package ch.epfl.k2sjsir

import java.lang.Float
import java.util.Collections
import java.{util => ju}

import org.jetbrains.kotlin.builtins.KotlinBuiltIns
import org.jetbrains.kotlin.descriptors.{DeclarationDescriptor, ModuleDescriptor}
import org.jetbrains.kotlin.idea.MainFunctionDetector
import org.jetbrains.kotlin.js.backend.ast._
import org.jetbrains.kotlin.js.backend.ast.metadata.MetadataProperties
import org.jetbrains.kotlin.js.config.{JSConfigurationKeys, JsConfig}
import org.jetbrains.kotlin.js.facade.MainCallParameters
import org.jetbrains.kotlin.js.facade.exceptions.{TranslationException, TranslationRuntimeException, UnsupportedFeatureException}
import org.jetbrains.kotlin.js.sourceMap.SourceFilePathResolver
import org.jetbrains.kotlin.js.translate.callTranslator.CallTranslator
import org.jetbrains.kotlin.js.translate.context.{Namer, StaticContext, TranslationContext}
import org.jetbrains.kotlin.js.translate.expression.{ExpressionVisitor, PatternTranslator}
import org.jetbrains.kotlin.js.translate.general.{AstGenerationResult, Merger}
import org.jetbrains.kotlin.js.translate.general.ModuleWrapperTranslation.wrapIfNecessary
import org.jetbrains.kotlin.js.translate.test.JSTestGenerator
import org.jetbrains.kotlin.js.translate.utils.BindingUtils.getFunctionDescriptor
import org.jetbrains.kotlin.js.translate.utils.{JsAstUtils, TranslationUtils}
import org.jetbrains.kotlin.js.translate.utils.JsAstUtils.{convertToStatement, toStringLiteralList}
import org.jetbrains.kotlin.js.translate.utils.mutator.AssignToExpressionMutator
import org.jetbrains.kotlin.js.translate.utils.mutator.LastExpressionMutator.mutateLastExpression
import org.jetbrains.kotlin.psi.{KtExpression, KtFile, KtUnaryExpression}
import org.jetbrains.kotlin.resolve.BindingTrace
import org.jetbrains.kotlin.resolve.bindingContextUtil.BindingContextUtilsKt
import org.jetbrains.kotlin.resolve.constants.evaluate.ConstantExpressionEvaluator
import org.jetbrains.kotlin.resolve.constants.{CompileTimeConstant, ConstantValue, NullValue}
import org.jetbrains.kotlin.types.TypeUtils
import org.jetbrains.kotlin.utils.ExceptionUtilsKt

import scala.collection.JavaConverters._
import scala.collection.mutable

object Translation {

  def patternTranslator(context: TranslationContext): PatternTranslator =
    PatternTranslator.newInstance(context)

  def translateExpression(expression: KtExpression, context: TranslationContext): JsNode =
    translateExpression(expression, context, context.dynamicContext.jsBlock)

  def translateExpression(expression: KtExpression, context: TranslationContext, block: JsBlock): JsNode = {
    val aliasForExpression = context.aliasingContext.getAliasForExpression(expression)
    if (aliasForExpression != null) return aliasForExpression
    val compileTimeValue = ConstantExpressionEvaluator.getConstant(expression, context.bindingContext)
    if (compileTimeValue != null) {
      val `type` = context.bindingContext.getType(expression)
      if (`type` != null) if (KotlinBuiltIns.isLong(`type`) ||
        (KotlinBuiltIns.isInt(`type`) && expression.isInstanceOf[KtUnaryExpression])) {
        val constantResult = translateConstant(compileTimeValue, expression, context)
        if (constantResult != null) return constantResult
      }
    }
    val innerContext = context.innerBlock
    val result = doTranslateExpression(expression, innerContext)
    context.moveVarsFrom(innerContext)
    block.getStatements.addAll(innerContext.dynamicContext.jsBlock.getStatements)
    result
  }

  def translateConstant(compileTimeValue: CompileTimeConstant[_],
                        expression: KtExpression,
                        context: TranslationContext): JsExpression = {

    val expectedType = context.bindingContext.getType(expression)
    val constant = compileTimeValue.toConstantValue(
      if (expectedType != null)
        expectedType
      else
        TypeUtils.NO_EXPECTED_TYPE
    )

    val result = translateConstantWithoutType(constant)
    if (result != null)
      MetadataProperties.setType(result, expectedType)

    result
  }

  private def translateConstantWithoutType(constant: ConstantValue[_]): JsExpression = {

    if (constant.isInstanceOf[NullValue])
      return new JsNullLiteral

    val value = constant.getValue
    if (value.isInstanceOf[Integer] || value.isInstanceOf[Short] || value.isInstanceOf[Byte]) {
      return new JsIntLiteral(value.asInstanceOf[Number].intValue)
    }
    else if (value.isInstanceOf[Long]) {
      return JsAstUtils.newLong(value.asInstanceOf[Long])
    }
    else if (value.isInstanceOf[Float]) {
      val floatValue = value.asInstanceOf[Float]
      var doubleValue = .0
      if (Float.isInfinite(floatValue) || Float.isNaN(floatValue)) {
        doubleValue = floatValue.toDouble
      }
      else {
        doubleValue = Float.toString(floatValue).toDouble
      }

      return new JsDoubleLiteral(doubleValue)
    }
    else if (value.isInstanceOf[Number]) {
      return new JsDoubleLiteral(value.asInstanceOf[Number].doubleValue)
    }
    else if (value.isInstanceOf[Boolean]) {
      return new JsBooleanLiteral(value.asInstanceOf[Boolean])
    }

    //TODO: test
    if (value.isInstanceOf[String]) {
      return new JsStringLiteral(value.asInstanceOf[String])
    }

    if (value.isInstanceOf[Character]) {
      return new JsIntLiteral(value.asInstanceOf[Character].charValue)
    }

    null
  }

  private def doTranslateExpression(expression: KtExpression, context: TranslationContext) =
    try
      expression.accept(new ExpressionVisitor, context)
    catch {
      case e: TranslationRuntimeException =>
        throw e
      case e: RuntimeException =>
        throw new TranslationRuntimeException(expression, e)
      case e: AssertionError =>
        throw new TranslationRuntimeException(expression, e)
    }

  def translateAsExpression(expression: KtExpression, context: TranslationContext): JsExpression =
    translateAsExpression(expression, context, context.dynamicContext.jsBlock)

  def translateAsExpression(expression: KtExpression, context: TranslationContext, block: JsBlock): JsExpression = {
    val jsNode = translateExpression(expression, context, block)

    if (jsNode.isInstanceOf[JsExpression]) {
      val jsExpression = jsNode.asInstanceOf[JsExpression]
      val expressionType = context.bindingContext.getType(expression)
      if (MetadataProperties.getType(jsExpression) == null) {
        MetadataProperties.setType(jsExpression, expressionType);
      }
      else if (expressionType != null) {
        return TranslationUtils.coerce(context, jsExpression, expressionType)
      }

      return jsExpression
    }

    assert(jsNode.isInstanceOf[JsStatement], "Unexpected node of type: " + jsNode.getClass.toString)
    if (BindingContextUtilsKt.isUsedAsExpression(expression, context.bindingContext)) {
      val result = context.declareTemporary(null, expression)
      val saveResultToTemporaryMutator = new AssignToExpressionMutator(result.reference)
      block.getStatements.add(mutateLastExpression(jsNode, saveResultToTemporaryMutator))
      val tmpVar = result.reference
      MetadataProperties.setType(tmpVar, context.bindingContext().getType(expression))

      return tmpVar
    }

    block.getStatements.add(convertToStatement(jsNode))

    new JsNullLiteral().source(expression)
  }

  def translateAsStatement(expression: KtExpression, context: TranslationContext): JsStatement =
    translateAsStatement(expression, context, context.dynamicContext.jsBlock)

  def translateAsStatement(expression: KtExpression, context: TranslationContext, block: JsBlock): JsStatement =
    convertToStatement(translateExpression(expression, context, block))

  def translateAsStatementAndMergeInBlockIfNeeded(expression: KtExpression, context: TranslationContext): JsStatement = {
    val block = new JsBlock
    val node = translateExpression(expression, context, block)
    JsAstUtils.mergeStatementInBlockIfNeeded(convertToStatement(node), block)
  }

  @throws[TranslationException]
  def generateAst(bindingTrace: BindingTrace, files: ju.Collection[KtFile], mainCallParameters: MainCallParameters, moduleDescriptor: ModuleDescriptor, config: JsConfig, pathResolver: SourceFilePathResolver): AstGenerationResult =
    try
      doGenerateAst(bindingTrace, files, mainCallParameters, moduleDescriptor, config, pathResolver)
    catch {
      case e: UnsupportedOperationException =>
        throw new UnsupportedFeatureException("Unsupported feature used.", e)
      case e: Throwable =>
        throw ExceptionUtilsKt.rethrow(e)
    }

  private def doGenerateAst(bindingTrace: BindingTrace,
                            files: ju.Collection[KtFile],
                            mainCallParameters: MainCallParameters,
                            moduleDescriptor: ModuleDescriptor,
                            config: JsConfig,
                            pathResolver: SourceFilePathResolver): AstGenerationResult = {

    val staticContext = new StaticContext(bindingTrace, config, moduleDescriptor, pathResolver)
    val program = staticContext.getProgram
    val rootFunction = new JsFunction(program.getRootScope, new JsBlock(), "root function")
    val internalModuleName = program.getScope.declareName("_")
    val merger = new Merger(rootFunction, internalModuleName, moduleDescriptor)
    val context = TranslationContext.rootContext(staticContext)

    PackageDeclarationTranslator.translateFiles(files, context)

    val fragmentMap = new java.util.HashMap[KtFile, JsProgramFragment]
    val fragments = new java.util.ArrayList[JsProgramFragment]
    val newFragments = new java.util.ArrayList[JsProgramFragment]
    val statements = new java.util.ArrayList[JsStatement]
    val fileMemberScopes = new java.util.HashMap[KtFile, java.util.List[DeclarationDescriptor]]
    val importedModuleList = merger.getImportedModules

    new AstGenerationResult(program, internalModuleName, fragments, fragmentMap, newFragments, statements, fileMemberScopes, importedModuleList)
  }
  /*
  private def defineModule(context: TranslationContext, statements: ju.List[JsStatement], moduleId: String) = {
    val rootPackageName = context.scope.findName(Namer.getRootPackageName)
    if (rootPackageName != null) statements.add(new JsInvocation(context.namer.kotlin("defineModule"), context.program.getStringLiteral(moduleId), rootPackageName.makeRef).makeStmt)
  }

  private def mayBeGenerateTests(files: ju.Collection[KtFile], config: JsConfig, rootBlock: JsBlock, context: TranslationContext) = {
    val tester = if (config.getConfiguration.getBoolean(JSConfigurationKeys.UNIT_TEST_CONFIG)) new JSRhinoUnitTester
    else new QUnitTester
    tester.initialize(context, rootBlock)
    JSTestGenerator.generateTestCalls(context, files, tester)
    tester.deinitialize()
  }

  //TODO: determine whether should throw exception
  private def generateCallToMain(context: TranslationContext, files: ju.Collection[KtFile], arguments: ju.List[String]) = {
    val mainFunctionDetector = new MainFunctionDetector(context.bindingContext)
    val mainFunction = mainFunctionDetector.getMainFunction(files)
    if (mainFunction != null) {
      val functionDescriptor = getFunctionDescriptor(context.bindingContext, mainFunction)
      val argument = new JsArrayLiteral(toStringLiteralList(arguments, context.program))
      CallTranslator.INSTANCE.buildCall(context, functionDescriptor, Collections.singletonList(argument), null).makeStmt
    }
    null
  }
  */

}
