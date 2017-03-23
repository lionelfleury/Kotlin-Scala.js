package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.ir.descriptors.IrBuiltinOperatorDescriptor
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees.{BinaryOp, _}
import org.scalajs.core.ir.Types._

import scala.collection.JavaConverters._

case class GenCall(d: IrCall, p: Positioner) extends Gen[IrCall] {

  def tree: Tree = {
    d.getDescriptor match {
      case bi: IrBuiltinOperatorDescriptor =>
        //@TODO: EQEQ and NEQ are builtin and not SimpleFunctionDescriptor
        notImplemented
      case sf: SimpleFunctionDescriptor =>
        val name = sf.getName.toString
        val tpe = sf.getReturnType.toJsType
        val args = genArgs(sf.getValueParameters.asScala)
        if (name == "println") {
          val rec = LoadModule(ClassType("s_Predef$"))
          val method = Ident("println__O__V", Some("println"))
          Apply(rec, method, args.toList)(tpe)
        } else if (sf.isOperator || sf.isInfix) {
          // @TODO: definitely not be the best way to detect OP
          genBinaryOp(sf, args.head)
        } else if (name == "toLong" ||
          name == "toInt" ||  name == "toDouble" || name == "toFloat"){
          genUnaryOp(sf)
        } else {
          val method = sf.toJsMethodIdent
          val r = d.getDispatchReceiver
          if (r == null) println("Null receiver in GenCall!!")
          val rec = if (r == null) This()(AnyType) else GenExpr(r, p).tree
          Apply(rec, method, args.toList)(tpe)
        }

      case _ => notImplemented
    }
  }

  private def genArgs(as: Seq[ValueParameterDescriptor]): Seq[Tree] =
    for (i <- as.indices) yield GenExpr(d.getValueArgument(i), p).tree

  private def genUnaryOp(sf: SimpleFunctionDescriptor): Tree = {
    val from = sf.getDispatchReceiverParameter.getType.toJsType
    val to = sf.getReturnType.toJsType
    UnaryOp(convertToOp(from, to), GenExpr(d.getDispatchReceiver, p).tree)
  }

  private def genBinaryOp(sf: SimpleFunctionDescriptor, rhs: Tree) : Tree = {
    val lhs = GenExpr(d.getDispatchReceiver, p).tree
    val rType = sf.getReturnType.toJsType
    val op = getBinaryOp(sf.getName.asString(), rType)

    if (lhs.tpe == rhs.tpe) {
      BinaryOp(op, lhs, rhs)
    } else if(isLongOp(op, lhs.tpe, rhs.tpe)) {
      val clhs = intToLong(lhs)
      val crhs = if(BinaryOpMaps.isLongSpecial(op)) longToInt(rhs) else intToLong(rhs)
      BinaryOp(op, clhs, crhs)
    } else {
      val lsrc = convertArg(op, lhs, lhs.tpe, rType)
      val rsrc = convertArg(op, rhs, rhs.tpe, rType)
      BinaryOp(op, lsrc, rsrc)
    }
  }

  private def isLongType(t: Type) = t == LongType
  private def isStringType(t: Type) = t == StringType
  private def isFloatType(t: Type) = t == FloatType
  private def isIntType(t: Type) = t == IntType
  private def isDoubleType(t: Type) = t == DoubleType

  private def intToLong(t: Tree) = if(isLongType(t.tpe)) t else UnaryOp(UnaryOp.IntToLong, t)
  private def longToInt(t: Tree) = if(isIntType(t.tpe)) t else UnaryOp(UnaryOp.LongToInt, t)

  /* Long is special, because of its shift operations, that accepts only int */
  private def isLongOp(op: BinaryOp.Code, ltpe: Type, rtpe: Type) = {
    (isLongType(ltpe) || isLongType(rtpe)) &&
      !(isFloatType(ltpe) ||
        isFloatType(rtpe) ||
        isStringType(ltpe)|| isStringType(rtpe) || isDoubleType(ltpe) || isDoubleType(rtpe))

  }

  /* Convert arg to the correct type for it to be us by the binary op */
  private def convertArg(op: BinaryOp.Code, tree: Tree, t: Type, returnType: Type) = {
    val notLong = {
      if (!isLongType(t)) tree
      else if (BinaryOpMaps.isLongSpecial(op)) UnaryOp(UnaryOp.LongToInt, tree)
      else UnaryOp(UnaryOp.LongToDouble, tree)
    }

    if (!isFloatType(returnType)) notLong
    else if (isFloatType(t)) notLong
    else UnaryOp(UnaryOp.DoubleToFloat, notLong)
  }

  /* Useful for explicit type conversion (toInt, toDouble, ...) */
  private def convertToOp(from: Type, to: Type) = {
    (from, to) match {
      case (IntType, LongType) => UnaryOp.IntToLong
      case (LongType, IntType) => UnaryOp.LongToInt
      case (LongType, DoubleType) => UnaryOp.LongToDouble
      case (DoubleType, IntType) => UnaryOp.DoubleToInt
      case (DoubleType, FloatType) => UnaryOp.DoubleToFloat
      case (DoubleType, LongType) => UnaryOp.DoubleToLong
      case _ => throw new Exception("Unsupported type conversion")
    }
  }

  /* Map of all binary ops, sorted by type */
  private object BinaryOpMaps {

    def isLongSpecial(op: BinaryOp.Code) =
      op ==  BinaryOp.Long_<< ||
        op ==  BinaryOp.Long_>> ||
        op ==  BinaryOp.Long_>>>

    val longBinaryOp = Map(
      "plus" ->  BinaryOp.Long_+,
      "minus" -> BinaryOp.Long_-,
      "times" -> BinaryOp.Long_*,
      "div" -> BinaryOp.Long_/,
      "rem" -> BinaryOp.Long_%,
      "or" -> BinaryOp.Long_|,
      "and" -> BinaryOp.Long_&,
      "xor" -> BinaryOp.Long_^,
      "shl" -> BinaryOp.Long_<<,
      "shr" -> BinaryOp.Long_>>>,
      "ushr" -> BinaryOp.Long_>>
    )

    val intBinaryOp = Map(
      "plus" ->  BinaryOp.Int_+,
      "minus" -> BinaryOp.Int_-,
      "times" -> BinaryOp.Int_*,
      "div" -> BinaryOp.Int_/,
      "rem" -> BinaryOp.Int_%,
      "or" -> BinaryOp.Int_|,
      "and" -> BinaryOp.Int_&,
      "xor" -> BinaryOp.Int_^,
      "shl" -> BinaryOp.Int_<<,
      "shr" -> BinaryOp.Int_>>>,
      "ushr" -> BinaryOp.Int_>>
    )

    val doubleBinaryOp = Map(
      "plus" ->  BinaryOp.Double_+,
      "minus" -> BinaryOp.Double_-,
      "times" -> BinaryOp.Double_*,
      "div" -> BinaryOp.Double_/,
      "rem" -> BinaryOp.Double_%
    )

    val floatBinaryOp = Map(
      "plus" ->  BinaryOp.Float_+,
      "minus" -> BinaryOp.Float_-,
      "times" -> BinaryOp.Float_*,
      "div" -> BinaryOp.Float_/,
      "rem" -> BinaryOp.Float_%
    )

    val booleanBinaryOp = Map(
      "or" -> BinaryOp.Boolean_|,
      "and" -> BinaryOp.Boolean_&
    )

    val stringBinaryOp = Map(
      "plus" -> BinaryOp.String_+
    )
  }

  /* Find the correct binary op for a given type */
  private def getBinaryOp(op: String, tpe: Type) : BinaryOp.Code = {

    import BinaryOpMaps._

    val opMap : Map[String, Int] = tpe match {
      case BooleanType => booleanBinaryOp
      case IntType => intBinaryOp
      case LongType => longBinaryOp
      case FloatType => floatBinaryOp
      case DoubleType => doubleBinaryOp
      case StringType | ClassType("LString") => stringBinaryOp
      case _ => throw new Exception(s"Unable to map type $tpe")
    }

    opMap(op)
  }

}
