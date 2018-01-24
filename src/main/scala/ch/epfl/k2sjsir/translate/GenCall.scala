package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.{NameEncoder, Utils}
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.fileClasses.JvmFileClassUtil
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.{BindingContextUtils, DescriptorToSourceUtils, DescriptorUtils}
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.scopes.receivers.{ExpressionReceiver, ExtensionReceiver, ImplicitClassReceiver, ReceiverValue}
import org.jetbrains.kotlin.types.TypeUtils
import org.scalajs.core.ir.Position
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._

import scala.collection.JavaConverters._

case class GenCall(d: KtCallExpression)(implicit val c: TranslationContext) extends Gen[KtCallExpression] {

  private val resolved = CallUtilKt.getResolvedCall(d, c.bindingContext())
  private val desc = resolved.getResultingDescriptor
  private lazy val rtpe = desc.getOriginal.getReturnType.toJsType
  private val name = desc.getName.asString()
  private val args = genArgs().toList

  /*
   * isDirectInvokeCall is used to differentiate when a call is performed on a class property
   * This should be changed as its really not reflecting the underlying logic (use resolved call and desc for that)
   * TODO: Replace the isDirectInvokeCall with a retrieval of the property the call is applied to
   */
  private val isLambdaCall = name == "invoke"
  private val isDirectInvokeCall = d.getText.matches("^invoke[(].*[)]$")

  override def tree: Tree = withReceiver(None)

  def withReceiver(rcv: Option[KtExpression] = None, isSafe: Boolean = false): Tree = {
    desc match {
      case cc: ClassConstructorDescriptor =>
        val ctpe = {
          val ccName = cc.getContainingDeclaration.getName.toString
          ccName match {
            case "Exception" => ClassType("jl_Exception")
            case _ => cc.getContainingDeclaration.toJsClassType
          }
        }

        // TODO: Remove this first 'if' when using the stdlib
        if (GenArray.isNewKtArray(desc))
          GenArray(d, args).tree
        else if(!cc.getContainingDeclaration.isExternal)
          New(ctpe, desc.toJsMethodIdent, args)
        else
          JSNew(LoadJSConstructor(ctpe), args)

      case sf: SimpleFunctionDescriptor =>
        if (name == "println" || name == "print") {
          val rec = LoadModule(ClassType("s_Predef$"))
          val method = Ident(s"${name}__O__V", Some(name))
          Apply(rec, method, args)(rtpe)
        }
        else if (name == "js") {
          GenJsFunc(d).tree
        }
        else if (GenArray.isArrayOps(desc)) {
          if (rcv.isEmpty)
            GenArray(d, args).tree
          else
            arrayOps(GenExpr(rcv.get).tree, rtpe, name, args).getOrElse(notImplemented("Missing array operation"))
        }
        else {
          if (rcv.nonEmpty) {
            val receiver = GenExpr(rcv.get).tree

            val isArray = receiver.tpe.isInstanceOf[ArrayType]
            val isSuperCall = rcv.get.isInstanceOf[KtSuperExpression]


            if (DescriptorUtils.isExtension(desc)) {
              genExtensionCall(receiver)
            }
            else if (isArray) {
              arrayOps(receiver, rtpe, name, args).getOrElse(notImplemented("Missing array operation"))
            }
            else if (isUnaryOp(name)) {
              val exprTpe = BindingUtils.getTypeForExpression(c.bindingContext(), rcv.get)
              val castRcv = cast(receiver, TypeUtils.makeNotNullable(exprTpe))
              adaptPrimitive(castRcv, rtpe)
            }
            else {
              // FIXME: do not match on the receiver but on its tpe
              // if the type is AnyType then its either a call to java.lang.Object or a JS call
              receiver match {
                case _: LoadJSModule | _: JSNew =>
                  cast(JSBracketMethodApply(receiver, StringLiteral(name), args), desc.getReturnType)
                case _ =>
                  val name = {
                    if (isLambdaCall) {
                      if (isDirectInvokeCall)
                        NameEncoder.encodeApplyLambda(desc)
                      else {
                        // TODO: This will occur only when we have a call of the form class.lambdainprop(args)
                        // and should be replaced with a call to GenExpr with the expression corresponding to
                        // lambdainprop. This can be done through the dispatch receiver of the resolved call
                        val lambdaFuncName = """((.+)\(.*\))""".r.replaceAllIn(d.getText, "$2")
                        Ident(s"${lambdaFuncName}__O")
                      }
                    }
                    else desc.toJsMethodIdent
                  }

                  // If it's a DOT_QUALIFIED call (class.prop(args))
                  if (isLambdaCall && !isDirectInvokeCall) {
                    val funcApply = JSFunctionApply(Apply(receiver, name, Nil)(AnyType), args)

                    castJsFunctionApply(funcApply, desc)
                  }
                  else if (isLambdaCall && isDirectInvokeCall)
                    castJsFunctionApply(JSFunctionApply(receiver, args), desc)
                  else if (isSuperCall) {
                    GenCall.genSuperCallFromContext(desc, rcv.get, name, args)
                  }
                  else
                    Apply(receiver, name, args)(rtpe)
              }
            }

          } else {

            val extRcvParam = Option(desc.getExtensionReceiverParameter)
            val extRcv = {
              if (extRcvParam.nonEmpty)
                Option(VarRef(extRcvParam.get.toJsIdent)(extRcvParam.get.getType.toJsType))
              else
                None
            }

            if(DescriptorUtils.isExtension(desc)) {
              genExtensionCall(
                extRcv.getOrElse(
                  throw new Exception("No extension receiver instance defined")
                )
              )

            }
            else if(isTopLevelFunction(sf)) {
              ApplyStatic(ClassType(NameEncoder.encodeWithSourceFile(sf)), desc.toJsMethodIdent, args)(desc.getReturnType.toJsType)

            }
            else {
              val dispatchReceiver = resolved.getDispatchReceiver

              if (isDirectCallToJsFunc(dispatchReceiver))
                GenJsFunc(d).tree
              else {
                val receiver = dispatchReceiver match {
                  case i: ImplicitClassReceiver =>
                    val cls = i.getClassDescriptor
                    if (cls.isCompanionObject)
                      LoadModule(cls.toJsClassType)
                    else {
                      val funDesc = BindingContextUtils.getEnclosingFunctionDescriptor(c.bindingContext(), d)
                      genThisFromContext(i.getClassDescriptor.toJsClassType, funDesc)
                    }

                  case _: ExtensionReceiver =>
                    val dr = desc.getDispatchReceiverParameter
                    VarRef(dr.toJsIdent)(dr.getType.toJsType)

                  case e: ExpressionReceiver =>
                    if (isReceiverJsFunc(dispatchReceiver))
                      GenJsFunc(e.getExpression.asInstanceOf[KtCallExpression]).tree
                    else
                      GenExpr(e.getExpression).tree

                  case _ => notImplementedReceiver("while looking for receiver")

                }

                // If it's a call to kotlin js func, pass the arguments to the call directly
                if (isReceiverJsFunc(dispatchReceiver)) {
                  receiver match {
                    case a@Apply(r, n, _) =>
                      Apply(r, n, args)(a.tpe)
                  }
                }
                else {
                  if (isLambdaCall)
                    castJsFunctionApply(JSFunctionApply(receiver, args), desc)
                  else
                    Apply(receiver, desc.toJsMethodIdent, args)(rtpe)
                }
              }
            }
          }
        }

      case _ =>
        notImplemented("in tree method")
    }
  }

  private def castJsFunctionApply(jsFuncApply: JSFunctionApply, desc: CallableDescriptor): Tree = {
    val rtpe = desc.getReturnType
    cast(jsFuncApply, rtpe)
  }

  private def isUnaryOp(n: String): Boolean =
    n == "toChar" || n == "toByte" || n == "toShort" || n == "toLong" || n == "toInt" || n == "toDouble" || n == "toFloat" || n == "not" || isUnaryToBinary(n)

  private def isUnaryToBinary(n: String) = n == "unaryMinus"

  private def isDirectCallToJsFunc(rcv: ReceiverValue): Boolean = {
    rcv == null && name == "js"
  }

  private def isReceiverJsFunc(rcv: ReceiverValue): Boolean = {
    rcv match {
      case e: ExpressionReceiver if name == "invoke" =>
        val subResolvedCall = CallUtilKt
          .getResolvedCall(e.getExpression, c.bindingContext())

        if (subResolvedCall != null) {

          val name = subResolvedCall
            .getResultingDescriptor
            .getName
            .asString()

          name == "js"
        } else {
          false
        }
      case _ => false
    }
  }

  private def genArgs(): Seq[Tree] = {
    resolved.getCall.getValueArguments.asScala.map(x => GenExpr(x.getArgumentExpression).tree)
  }

  /*
  The instance of the class in which the extension is declared is called dispatch receiver,
  and the instance of the receiver type of the extension method is called extension receiver.
  -->
  dispatchReceiver = instance (this) of the current class
  extensionReceiver = instance (this) of the extended class
   */
  def genExtensionCall(receiver: Tree) : Tree = {
    val dispRcvParameter = desc.getDispatchReceiverParameter

    if (null != dispRcvParameter) {
      // We are inside a class declaration
      val cnt = dispRcvParameter.getContainingDeclaration
      val clsTpe = cnt match {
        case clsDesc: ClassDescriptor => clsDesc.toJsClassType
      }

      val rcv = genThisFromContext(clsTpe)
      Apply(rcv, desc.toJsMethodIdent, receiver :: args)(rtpe)

    } else {
      val o = desc.getExtensionReceiverParameter
      val name = JvmFileClassUtil.getFileClassInfoNoResolve(d.getCalleeExpression.getContainingKtFile).getFileClassFqName.asString()
      val file = DescriptorToSourceUtils.getSourceFromDescriptor(o.getContainingDeclaration).getContainingFile.asInstanceOf[KtFile]

      val clsTpe = file.toJsClassType

      ApplyStatic(clsTpe, desc.toJsMethodIdent, receiver :: args)(rtpe)
    }
  }

  private def isTopLevelFunction(sf: SimpleFunctionDescriptor) =
    sf.getExtensionReceiverParameter == null && DescriptorUtils.isTopLevelDeclaration(sf)


  private def arrayOps(receiver: Tree, tpe: Type, method: String, args: List[Tree]) : Option[Tree] = {
    method match {
      case "get" =>
        require(args.size == 1)
        Some(ArraySelect(receiver, args.head)(tpe))
      case "set" =>
        require(args.size == 2)
        Some(Assign(ArraySelect(receiver, args.head)(args(1).tpe), args(1)))
      case "size" =>
        Some(ArrayLength(receiver))
      case "iterator" =>
        None
      case _ => None
    }
  }
}

object GenCall {

  def genSuperCallFromContext(callDesc: CallableDescriptor, rcv: KtExpression, name: Ident, args: List[Tree])(implicit c: TranslationContext, pos: Position) = {
    val receiver = GenExpr(rcv).tree
    val rtpe = callDesc.getOriginal.getReturnType.toJsType

    val superTpe = c.bindingContext().getType(rcv)
    val superDesc = DescriptorUtils.getClassDescriptorForType(superTpe)
    val superArgs = Option(rcv)
      .filter(_ => superDesc.isInterface)
      .map(_ => receiver)
      .toList

    val clsTpe = receiver.tpe match {
      case c:ClassType =>
        if (superDesc.isInterface)
          ClassType(superDesc.toJsDefaultImplName)
        else
          c
      case t => throw new Exception(s"Got a super call with type $t")
    }

    if (superDesc.isInterface)
      ApplyStatic(clsTpe, callDesc.toJsMethodDeclIdent, superArgs ++ args)(rtpe)
    else
      ApplyStatically(receiver, clsTpe, name, args)(rtpe)
  }
}
