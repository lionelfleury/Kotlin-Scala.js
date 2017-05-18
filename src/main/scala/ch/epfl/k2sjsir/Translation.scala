package ch.epfl.k2sjsir

import java.lang.Float
import java.util.Collections
import java.{util => ju}

import org.jetbrains.kotlin.builtins.KotlinBuiltIns
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.idea.MainFunctionDetector
import org.jetbrains.kotlin.js.backend.ast._
import org.jetbrains.kotlin.js.config.{JSConfigurationKeys, JsConfig}
import org.jetbrains.kotlin.js.facade.MainCallParameters
import org.jetbrains.kotlin.js.facade.exceptions.{TranslationException, TranslationRuntimeException, UnsupportedFeatureException}
import org.jetbrains.kotlin.js.translate.callTranslator.CallTranslator
import org.jetbrains.kotlin.js.translate.context.{Namer, StaticContext, TranslationContext}
import org.jetbrains.kotlin.js.translate.expression.{ExpressionVisitor, PatternTranslator}
import org.jetbrains.kotlin.js.translate.general.ModuleWrapperTranslation.wrapIfNecessary
import org.jetbrains.kotlin.js.translate.test.{JSRhinoUnitTester, JSTestGenerator, QUnitTester}
import org.jetbrains.kotlin.js.translate.utils.BindingUtils.getFunctionDescriptor
import org.jetbrains.kotlin.js.translate.utils.JsAstUtils
import org.jetbrains.kotlin.js.translate.utils.JsAstUtils.{convertToStatement, toStringLiteralList}
import org.jetbrains.kotlin.js.translate.utils.mutator.AssignToExpressionMutator
import org.jetbrains.kotlin.js.translate.utils.mutator.LastExpressionMutator.mutateLastExpression
import org.jetbrains.kotlin.psi.{KtExpression, KtFile, KtUnaryExpression}
import org.jetbrains.kotlin.resolve.BindingTrace
import org.jetbrains.kotlin.resolve.bindingContextUtil.BindingContextUtilsKt
import org.jetbrains.kotlin.resolve.constants.evaluate.ConstantExpressionEvaluator
import org.jetbrains.kotlin.resolve.constants.{CompileTimeConstant, NullValue}
import org.jetbrains.kotlin.types.TypeUtils
import org.jetbrains.kotlin.utils.ExceptionUtilsKt

import scala.collection.JavaConverters._

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

  def translateConstant(compileTimeValue: CompileTimeConstant[_], expression: KtExpression, context: TranslationContext): JsExpression = {
    val expectedType = context.bindingContext.getType(expression)
    val constant = compileTimeValue.toConstantValue(if (expectedType != null) expectedType
                                                    else TypeUtils.NO_EXPECTED_TYPE)
    if (constant.isInstanceOf[NullValue]) return JsLiteral.NULL
    val value = constant.getValue
    if (value.isInstanceOf[Integer] || value.isInstanceOf[Short] || value.isInstanceOf[Byte]) return context.program.getNumberLiteral(value.asInstanceOf[Number].intValue)
    else if (value.isInstanceOf[Long]) return JsAstUtils.newLong(value.asInstanceOf[Long], context)
    else if (value.isInstanceOf[Float]) {
      val floatValue = value.asInstanceOf[Float]
      var doubleValue = .0
      if (Float.isInfinite(floatValue) || Float.isNaN(floatValue)) doubleValue = floatValue.toDouble
      else doubleValue = Float.toString(floatValue).toDouble
      return context.program.getNumberLiteral(doubleValue)
    }
    else if (value.isInstanceOf[Number]) return context.program.getNumberLiteral(value.asInstanceOf[Number].doubleValue)
    else if (value.isInstanceOf[Boolean]) return JsLiteral.getBoolean(value.asInstanceOf[Boolean])
//TODO: test
    if (value.isInstanceOf[String]) return context.program.getStringLiteral(value.asInstanceOf[String])
    if (value.isInstanceOf[Character]) return context.program.getNumberLiteral(value.asInstanceOf[Character].charValue)
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
    var jsNode = translateExpression(expression, context, block)
    if (jsNode.isInstanceOf[JsExpression]) {
      val expressionType = context.bindingContext.getType(expression)
      if (expressionType != null && KotlinBuiltIns.isCharOrNullableChar(expressionType) && (jsNode.isInstanceOf[JsInvocation] || jsNode.isInstanceOf[JsNameRef] || jsNode.isInstanceOf[JsArrayAccess])) jsNode = JsAstUtils.boxedCharToChar(jsNode.asInstanceOf[JsExpression])
      return jsNode.asInstanceOf[JsExpression]
    }
    assert(jsNode.isInstanceOf[JsStatement], "Unexpected node of type: " + jsNode.getClass.toString)
    if (BindingContextUtilsKt.isUsedAsExpression(expression, context.bindingContext)) {
      val result = context.declareTemporary(null)
      val saveResultToTemporaryMutator = new AssignToExpressionMutator(result.reference)
      block.getStatements.add(mutateLastExpression(jsNode, saveResultToTemporaryMutator))
      return result.reference
    }
    block.getStatements.add(convertToStatement(jsNode))
    JsLiteral.NULL
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
  def generateAst(bindingTrace: BindingTrace, files: ju.Collection[KtFile], mainCallParameters: MainCallParameters, moduleDescriptor: ModuleDescriptor, config: JsConfig): TranslationContext = try
    doGenerateAst(bindingTrace, files, mainCallParameters, moduleDescriptor, config)
  catch {
    case e: UnsupportedOperationException =>
      throw new UnsupportedFeatureException("Unsupported feature used.", e)
    case e: Throwable =>
      throw ExceptionUtilsKt.rethrow(e)
  }

  private def doGenerateAst(bindingTrace: BindingTrace, files: ju.Collection[KtFile], mainCallParameters: MainCallParameters, moduleDescriptor: ModuleDescriptor, config: JsConfig) = {
    val staticContext = StaticContext.generateStaticContext(bindingTrace, config, moduleDescriptor)
//    val program = staticContext.getProgram
//    val rootPackageName = program.getRootScope.declareName(Namer.getRootPackageName)
    val rootFunction = staticContext.getRootFunction
//    val rootBlock = rootFunction.getBody
//    val statements = rootBlock.getStatements
//    program.getScope.declareName("_")
    val context = TranslationContext.rootContext(staticContext, rootFunction)

    PackageDeclarationTranslator.translateFiles(files, context)

//    staticContext.postProcess()
//    statements.add(0, program.getStringLiteral("use strict").makeStmt)
//    if (!staticContext.isBuiltinModule) defineModule(context, statements, config.getModuleId)
//    mayBeGenerateTests(files, config, rootBlock, context)
//    rootFunction.getParameters.add(new JsParameter(rootPackageName))
//    // Invoke function passing modules as arguments
//    // This should help minifier tool to recognize references to these modules as local variables and make them shorter.
//    val importedModuleList = new ju.ArrayList[StaticContext.ImportedModule]
//    for (importedModule <- staticContext.getImportedModules.asScala) {
//      rootFunction.getParameters.add(new JsParameter(importedModule.getInternalName))
//      importedModuleList.add(importedModule)
//    }
//    if (mainCallParameters.shouldBeGenerated) {
//      val statement = generateCallToMain(context, files, mainCallParameters.arguments)
//      if (statement != null) statements.add(statement)
//    }
//    statements.add(new JsReturn(rootPackageName.makeRef))
//    val block = program.getGlobalBlock
//    block.getStatements.addAll(wrapIfNecessary(config.getModuleId, rootFunction, importedModuleList, program, config.getModuleKind))
    context
  }

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

}
