package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.ir.descriptors.IrBuiltinOperatorDescriptor
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._

import scala.collection.JavaConverters._

case class GenCall(d: IrCall, p: Positioner) extends Gen[IrCall] {

  def tree: Tree = d.getDescriptor match {
    case pd: PropertyGetterDescriptor =>
      val cp = pd.getCorrespondingProperty.getGetter
      val idt = cp.toJsMethodIdent
      val tpe = cp.getReturnType.toJsType
      val r = d.getDispatchReceiver
      val receiver = if (r != null) GenExpr(r, p).tree else VarRef(idt)(tpe)
      Apply(receiver, idt, Nil)(tpe)
    case cd: ClassConstructorDescriptor =>
      val tpe = cd.getReturnType.toJsType
      val args = cd.getValueParameters.asScala.map(_.toJsVarRef)
      Apply(This()(tpe), cd.toJsMethodIdent, args.toList)(tpe)
    case bi : IrBuiltinOperatorDescriptor =>
      //@TODO: EQEQ and NEQ are not builtin and not SimpleFunctionDescriptor
      notImplemented
    case sf: SimpleFunctionDescriptor =>
      val tpe = sf.getReturnType.toJsType
      val args = genArgs(sf.getValueParameters.asScala)
      if (sf.getName.toString == "println") {
        val rec = LoadModule(ClassType("s_Predef$"))
        val method = Ident("println__O__V", Some("println"))
        Apply(rec, method, args.toList)(tpe)
      } else if (sf.isOperator || sf.isInfix) { // @TODO: definitely not be the best way to detect OP
      // @TODO: check if unary or binary
        val lhs = GenExpr(d.getDispatchReceiver, p).tree
        val rhs = args.head
        val op =
          if(lhs.tpe == rhs.tpe) getBinaryOp(sf.getName.asString(), lhs.tpe, rhs.tpe)
          else throw new Exception("Binary op between different types not yet implemented")
        BinaryOp(op, lhs, rhs)
      } else {
        val method = sf.toJsMethodIdent
        val r = d.getDispatchReceiver
        if (r == null) println("Null receiver in GenCall!!")
        val rec = if (r == null) This()(AnyType) else GenExpr(r, p).tree
        Apply(rec, method, args.toList)(tpe)
      }
    case _ => notImplemented
  }

  private def genArgs(as: Seq[ValueParameterDescriptor]): Seq[Tree] =
    for (i <- as.indices) yield GenExpr(d.getValueArgument(i), p).tree

  private def getBinaryOp(op: String, lhsTpe: Type, rhsTpe: Type) : BinaryOp.Code = {

    def getLongBinaryOp = op match {
      case "plus" =>  BinaryOp.Long_+
      case "minus" => BinaryOp.Long_-
      case "times" => BinaryOp.Long_*
      case "div" => BinaryOp.Long_/
      case "rem" => BinaryOp.Long_%
      case "or" => BinaryOp.Long_|
      case "and" => BinaryOp.Long_&
      case "xor" => BinaryOp.Long_^
      case "shl" => BinaryOp.Long_<<
      case "shr" => BinaryOp.Long_>>>
      case "ushr" => BinaryOp.Long_>>
      case x => throw new Exception(s"unimplemented op $x for type Long")
    }

    def getIntBinaryOp = op match {
      case "plus" =>  BinaryOp.Int_+
      case "minus" => BinaryOp.Int_-
      case "times" => BinaryOp.Int_*
      case "div" => BinaryOp.Int_/
      case "rem" => BinaryOp.Int_%
      case "or" => BinaryOp.Int_|
      case "and" => BinaryOp.Int_&
      case "xor" => BinaryOp.Int_^
      case "shl" => BinaryOp.Int_<<
      case "shr" => BinaryOp.Int_>>>
      case "ushr" => BinaryOp.Int_>>
      case x => throw new Exception(s"unimplemented op $x for type Int")
    }

    def getDoubleBinaryOp = op match {
      case "plus" =>  BinaryOp.Double_+
      case "minus" => BinaryOp.Double_-
      case "times" => BinaryOp.Double_*
      case "div" => BinaryOp.Double_/
      case "rem" => BinaryOp.Double_%
      case x => throw new Exception(s"unimplemented op $x for type Int")
    }

    def getFloatBinaryOp = op match {
      case "plus" =>  BinaryOp.Float_+
      case "minus" => BinaryOp.Float_-
      case "times" => BinaryOp.Float_*
      case "div" => BinaryOp.Float_/
      case "rem" => BinaryOp.Float_%
      case x => throw new Exception(s"unimplemented op $x for type Int")
    }

    def getBooleanBinaryOp = op match  {
      case "or" => BinaryOp.Boolean_|
      case "and" => BinaryOp.Boolean_&
      case x => throw new Exception(s"unimplemented op $x for type Boolean")
    }

    def getStringBinaryOp = op match {
      case "plus" => BinaryOp.String_+
      case x => throw new Exception(s"unimplemented op $x for type String")
    }

    (lhsTpe, rhsTpe) match {
      case (BooleanType, BooleanType) => getBooleanBinaryOp
      case (IntType, IntType) => getIntBinaryOp
      case (LongType, LongType) => getLongBinaryOp
      case (FloatType, FloatType) => getFloatBinaryOp
      case (DoubleType, DoubleType) => getDoubleBinaryOp
      case (StringType, StringType) => getStringBinaryOp
      /* Mixed types */
      case (LongType, IntType) => getLongBinaryOp
    }


  }

}
