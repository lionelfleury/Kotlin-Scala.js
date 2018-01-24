package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.SJSIRCodegen
import ch.epfl.k2sjsir.utils.Utils._
import ch.epfl.k2sjsir.utils.{GenExprUtils, NameEncoder}
import org.jetbrains.kotlin.config.CommonConfigurationKeys
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.descriptors.impl.LocalVariableDescriptor
import org.jetbrains.kotlin.fileClasses.JvmFileClassUtil
import org.jetbrains.kotlin.js.translate.context.{TranslationContext, UsageTracker, UsageTrackerKt}
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.LazyClassDescriptor
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.scopes.receivers.{ExpressionReceiver, ExtensionReceiver, ImplicitClassReceiver}
import org.jetbrains.kotlin.resolve.{BindingContext, BindingContextUtils, DescriptorUtils, PropertyImportedFromObject}
import org.scalajs.core.ir.Trees
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{AnyType, ClassType}

import scala.collection.JavaConverters._

case class GenExpr(d: KtExpression)(implicit val c: TranslationContext) extends Gen[KtExpression] {

  //TODO: Unit expression is not translated
  override def tree: Tree = {
    d match {
      case ce: KtCallExpression =>
        GenCall(ce).tree
      case ks: KtStringTemplateExpression =>
        ks.getEntries.foldLeft(StringLiteral(""): Tree)((acc, expr) => {
          BinaryOp(BinaryOp.String_+, acc, expr match {
            case sl: KtLiteralStringTemplateEntry =>
              StringLiteral(sl.getText)
            case est: KtEscapeStringTemplateEntry =>
              StringLiteral(est.getUnescapedValue)
            case st: KtStringTemplateEntry =>
              GenExpr(st.getExpression).tree
            case _ => notImplemented("after KtStringTemplateExpression")
          })
        })
      case kp: KtProperty =>
        val expr = GenExpr(kp.getDelegateExpressionOrInitializer).tree
        val desc = c.bindingContext().get(BindingContext.VARIABLE, kp)
        VarDef(desc.toJsIdent, desc.getType.toJsType, kp.isVar, expr)
      case kn: KtNameReferenceExpression =>
        BindingUtils.getDescriptorForReferenceExpression(c.bindingContext(), kn) match {
          case m: PropertyImportedFromObject =>
            val propDesc = m.getCallableFromObject
            val dispatchReceiver = propDesc.getDispatchReceiverParameter

            val recv = getClassDescriptorForType(dispatchReceiver.getValue.getType)

            Apply(LoadModule(recv.toJsClassType), m.getterIdent(), Nil)(m.getType.toJsType)

          case m: PropertyDescriptor =>
            val tpe = m.getType.toJsType
            val dispatchReceiver = m.getDispatchReceiverParameter
            val recv = {
              if (dispatchReceiver != null)
                getClassDescriptorForType(dispatchReceiver.getValue.getType)
              else
                null
            }
            // Check is the receiver is the instance of the current class or not

            val isObj = recv != null && (recv.isCompanionObject || DescriptorUtils.isObject(recv))

            val enclosingDesc = BindingContextUtils.getEnclosingDescriptor(c.bindingContext(), kn)
            val enclosingClass = DescriptorUtils.getContainingClass(enclosingDesc)

            val insideDeclaringObject = recv != null && enclosingClass != null && enclosingClass == recv

            if(m.isTopLevel) {
              val name = JvmFileClassUtil.getFileClassInfoNoResolve(d.getContainingKtFile).getFileClassFqName.asString()
              val encodedName = NameEncoder.encodeClassName(name, "")

              val clsTpe = ClassType(encodedName)

              ApplyStatic(clsTpe, m.getterIdent(), Nil)(m.getType.toJsType)
            }
            else if(isObj && !insideDeclaringObject) {
              Apply(LoadModule(recv.toJsClassType), m.getterIdent(), List())(tpe)
            }
            else if(DescriptorUtils.isLocal(m)) {
              VarRef(m.toJsIdent)(m.getReturnType.toJsType)
            }
            else {
              val a = CallUtilKt.getResolvedCallWithAssert(d, c.bindingContext())
              a.getDispatchReceiver match {
                case x: ExtensionReceiver =>
                  val receiver =
                    Option(x.getDeclarationDescriptor.getDispatchReceiverParameter)
                      .getOrElse(x.getDeclarationDescriptor.getExtensionReceiverParameter)
                  val ref = VarRef(receiver.toJsIdent)(x.getType.toJsType)

                  Apply(ref, m.getterIdent(), List())(tpe)

                case _: ImplicitClassReceiver | _: ExpressionReceiver =>
                  val funDesc = BindingContextUtils.getEnclosingFunctionDescriptor(c.bindingContext(), kn)
                  val rcv = genThisFromContext(recv.toJsClassType, funDesc)
                  Apply(rcv, m.getterIdent(), List())(tpe)

                case _ =>
                  notImplemented("after KtNameReferenceExpression > PropertyDescriptor")
              }
            }
          case lv: LocalVariableDescriptor =>
            val tpe = lv.getType.toJsType
            val ident = lv.toJsIdent
            VarRef(ident)(tpe)

          case vd: ValueParameterDescriptor =>
            val tpe = vd.getType.toJsType
            val ident = vd.toJsIdent
            VarRef(ident)(tpe)

          case lc: LazyClassDescriptor =>
            val external = if(lc.isCompanionObject) {
              lc.getContainingDeclaration match {
                case x: ClassDescriptor => x.isExternal
                case _ => lc.isExternal
              }
            } else lc.isExternal
            if(external)
              LoadJSModule(lc.toJsClassType)
            else {
              if (lc.isEnumClass)
                LoadModule(lc.toJsEnumCompanionType)
              else
                LoadModule(lc.toJsClassType)
            }

          case t =>
            notImplemented(s"after KtNameReferenceExpression (default case) $t")
        }
      case k: KtConstantExpression => GenConst(k).tree
      case k: KtObjectLiteralExpression =>
        /*
         * Note that anonymous objects can be used as types only in local and private declarations.
         * If you use an anonymous object as a return type of a public function or the type of a
         * public property, the actual type of that function or property will be the declared
         * supertype of the anonymous object, or Any if you didn't declare any supertype.
         * Members added in the anonymous object will not be accessible.
         */
        val output = c.getConfig.getConfiguration.get(CommonConfigurationKeys.MODULE_NAME)
        val objDeclaration = k.getObjectDeclaration
        val objDescriptor = BindingUtils.getClassDescriptor(c.bindingContext(), objDeclaration)
        val classDef = GenClass(objDeclaration).tree
        // TODO: Move this IR file creation in the PackageDeclarationTranslator file
        SJSIRCodegen.genIRFile(output, objDescriptor, classDef)

        New(objDescriptor.toJsClassType, Ident("init___"), Nil)
      case k: KtArrayAccessExpression => GenArrayAccess(k).tree
      case k: KtBinaryExpression => GenBinary(k).tree
      case k: KtBinaryExpressionWithTypeRHS =>
        val opRef = k.getOperationReference
        val left = GenExpr(k.getLeft).tree
        val ktType = c.bindingTrace().get(BindingContext.TYPE, k.getRight)

        opRef.getReferencedNameElementType match {
          case KtTokens.AS_KEYWORD =>
            cast(left, ktType)

          case _ =>
            notImplemented(s"Unsupported '${opRef.getReferencedName}' to $ktType")
        }
      case k: KtForExpression => GenFor(k).tree
      case k: KtSafeQualifiedExpression =>
        // receiver?.selector => if (receiver != null) receiver.selector else null
        GenExprUtils.genDotQualified(k, notImplemented)
      case k: KtDotQualifiedExpression =>
        GenExprUtils.genDotQualified(k, notImplemented)

      case k: KtUnaryExpression =>
        GenUnary(k).tree
      case k: KtTryExpression =>
        val content = GenBody(k.getTryBlock).tree
        val catches = k.getCatchClauses.asScala.map(ctch => ({
          val d = ctch.getCatchParameter
          val varDesc = BindingUtils.getDescriptorForElement(c.bindingContext(), d).asInstanceOf[VariableDescriptor]
          val tpe = varDesc.getType.toJsType
          VarRef(varDesc.toJsIdent)(tpe)
        }, GenBody(ctch.getCatchBody).tree))
        val fnl = Option(k.getFinallyBlock).map(x => GenBody(x.getFinalExpression).tree)

        val tc = catches.foldLeft(content) { case (b, (p, h)) => TryCatch(b, p.ident, h)(b.tpe) }
        if (fnl.nonEmpty) TryFinally(tc, fnl.get) else tc
      case k: KtThrowExpression => Throw(GenExpr(k.getThrownExpression).tree)
      case k: KtIfExpression =>
        val ifTpe = BindingUtils.getTypeForExpression(c.bindingContext(), k).toJsType

        val cond = GenExpr(k.getCondition).tree
        val thenB = GenExpr(k.getThen).tree
        val elseB = Option(k.getElse).map(x => GenExpr(x).tree)

        If(cond, thenB, elseB.getOrElse(Skip()))(ifTpe)

      case k: KtReturnExpression =>
        val retExpr = if (k.getReturnedExpression != null) GenExpr(k.getReturnedExpression).tree else Undefined()
        Return(retExpr)
      case k: KtWhenExpression => GenWhen(k).tree
      case kp: KtParenthesizedExpression => GenExpr(kp.getExpression).tree
      case kc: KtCallableReferenceExpression =>
        CallUtilKt.getResolvedCall(kc.getCallableReference, c.bindingContext()).getResultingDescriptor match {
          case f: FunctionDescriptor => genClosure(f)
          case _ => notImplemented()
        }

      case l: KtLambdaExpression => genLambda(l)
      case w: KtWhileExpression =>
        val body = GenBody(w.getBody).tree
        val cond = GenExpr(w.getCondition).tree
        While(cond, body)

      case dw: KtDoWhileExpression =>
        val body = GenBody(dw.getBody).tree
        val cond = GenExpr(dw.getCondition).tree
        DoWhile(body, cond)

      case k: KtIsExpression => GenIs(k).tree
      case k: KtThisExpression =>
        val tpe = c.bindingContext().getType(k)
        val desc = getClassDescriptorForType(tpe)

        val funDesc = BindingContextUtils.getEnclosingFunctionDescriptor(c.bindingContext(), k)
        if (funDesc != null && DescriptorUtils.isExtension(funDesc) || funDesc.isAnonymousFunction)
          genThisFromContext(tpe.toJsType, funDesc)
        else
          genThisFromContext(tpe.toJsType, desc)

      case k: KtSuperExpression =>
        // Super calls must be translated to ApplyStatically but
        // here we know neither the method name nor the arguments.
        // Super calls are therefore detected where Apply nodes are generated (GenCall)
        val tpe = c.bindingContext().getType(k)

        genThisFromContext(tpe.toJsType)

      case k: KtDestructuringDeclaration =>
        for {
          entry: KtDestructuringDeclarationEntry <- k.getEntries.asScala.toList
          varDesc: VariableDescriptor = BindingContextUtils.getNotNull(c.bindingContext(), BindingContext.VARIABLE, entry)
          if !varDesc.getName.isSpecial
        } yield {
          val entryInitCall = c.bindingContext().get(BindingContext.COMPONENT_RESOLVED_CALL, entry)

          assert(entryInitCall != null, "Entry init call must not be null")


          //TranslationUtils.coerce(c, ???, varDesc.getType)
        }

        notImplemented(s"Destructuring")
        /*
        for (KtDestructuringDeclarationEntry entry : multiDeclaration.getEntries()) {
          VariableDescriptor descriptor = BindingContextUtils.getNotNull(context().bindingContext(), BindingContext.VARIABLE, entry);
            // Do not call `componentX` for destructuring entry called _
          if (descriptor.getName().isSpecial()) continue;

          ResolvedCall<FunctionDescriptor> entryInitCall = context().bindingContext().get(BindingContext.COMPONENT_RESOLVED_CALL, entry);
          assert entryInitCall != null : "Entry init call must be not null";
          JsExpression entryInitializer = CallTranslator.translate(context(), entryInitCall, multiObjectExpr);
          FunctionDescriptor candidateDescriptor = entryInitCall.getCandidateDescriptor();
          if (CallExpressionTranslator.shouldBeInlined(candidateDescriptor, context())) {
          setInlineCallMetadata(entryInitializer, entry, entryInitCall, context());
          }

          entryInitializer = TranslationUtils.coerce(context(), entryInitializer, descriptor.getType());

          JsName name = context().getNameForDescriptor(descriptor);
          if (isVarCapturedInClosure(context().bindingContext(), descriptor)) {
          JsNameRef alias = getCapturedVarAccessor(name.makeRef());
          entryInitializer = JsAstUtils.wrapValue(alias, entryInitializer);
          }

          JsVars.JsVar jsVar = new JsVars.JsVar(name, entryInitializer);
          jsVar.setSource(entry);
          jsVars.add(jsVar);
      }
      */
      case b: KtBlockExpression =>
        GenBody(b).tree

      case f: KtNamedFunction =>
        notImplemented("Function declarations are not supported inside other functions")

      case t =>
        notImplemented(s"default case on tree, element was of type $t")
    }
  }

  private def genLambda(l: KtLambdaExpression) : Tree = {
    val lambdaContext = l.getContext
    // Pass parameters so that they can be used inside the lambda
    val params = lambdaContext match {
      case nf: KtNamedFunction => Some(nf.getValueParameters.asScala.toList)
      case _ => None
    }

    val body = GenBody(l.getBodyExpression).treeOption
    val desc = BindingUtils.getFunctionDescriptor(c.bindingContext(), l.getFunctionLiteral)

    genClosure(desc, body, params)
  }

  private def genClosure(desc: FunctionDescriptor, body: Option[Tree] = None, funArgs: Option[List[KtParameter]] = None) : Tree = {
    val containingClass = Option(DescriptorUtils.getContainingClass(desc))
    val ct = containingClass.fold(ClassType(NameEncoder.encodeWithSourceFile(desc)))(cc => cc.toJsClassType)

    val b0 : Tree = body.getOrElse({
      val static = DescriptorUtils.isStaticDeclaration(desc)
      val methodName = desc.toJsMethodIdent
      val parameters = desc.getValueParameters.asScala.map(x => VarRef(x.toJsIdent)(x.getType.toJsType)).toList

      if (static) ApplyStatic(ct, methodName, parameters)(desc.getReturnType.toJsType)
      else Apply(VarRef(Ident("$this"))(ct), methodName, parameters)(desc.getReturnType.toJsType)
    })

    /*
     We generate parameters with "fresh" names and declare variables with the original name
     inside the body with a cast to the correct type (cast from Any to Type).
    */
    val varDefs = desc.getValueParameters.asScala.map{
      param =>
        val paramIdt = param.toJsClosureIdent
        val paramRef = cast(VarRef(paramIdt)(AnyType), param.getType)
        VarDef(param.toJsIdent, param.getType.toJsType, mutable = false, paramRef)
    }.toList

    val b1 = ensureBoxed(b0)
    val b = Block(varDefs ++ List(b1))

    // TODO: Either make use of a context or perform free variable analysis to capture variables
    val closureParams = desc.getValueParameters.asScala.map(_.toJsClosureParamDef).toList
    val closure = containingClass match {
      case Some(_) =>
        val funcCaptureParams = funArgs.getOrElse(Nil)
          .map(GenParam(_).tree)

        val funcCaptureValues = funArgs.getOrElse(Nil)
          .map(x => {
            val varDesc = BindingUtils.getDescriptorForElement(c.bindingContext(), x).asInstanceOf[VariableDescriptor]
            val tpe = varDesc.getType.toJsType
            VarRef(varDesc.toJsIdent)(tpe)
          })

        val captureParams = List(ParamDef(Ident("$this"), ct, mutable = false, rest = false)) ++ funcCaptureParams
        val captureValues = List[Trees.Tree](genThisFromContext(ct)) ++ funcCaptureValues
        Closure(captureParams, closureParams, b, captureValues)

      case None =>
        Closure(List(), closureParams, b, List())
    }

    closure
  }

  def treeOption: Option[Tree] = if (d == null) None else Some(tree)

}
