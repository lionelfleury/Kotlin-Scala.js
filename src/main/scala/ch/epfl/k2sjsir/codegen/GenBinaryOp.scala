package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions.IrCall
import org.scalajs.core.ir.Trees.{BinaryOp, Tree, UnaryOp}
import org.scalajs.core.ir.Types._

import scala.collection.JavaConverters._

case class GenBinaryOp(d: IrCall, p: Positioner) extends Gen[IrCall] {

  import GenBinaryOp._

  def tree: Tree = {
    val desc = d.getDescriptor
    val args = for (i <- desc.getValueParameters.asScala.indices)
      yield GenExpr(d.getValueArgument(i), p).tree
    val rhs = args.head
    val lhs =
      if(d.getDispatchReceiver != null) GenExpr(d.getDispatchReceiver, p).tree
      else  GenExpr(d.getExtensionReceiver, p).tree
    val rType = desc.getReturnType.toJsType
    val op = getBinaryOp(desc.getName.asString(), rType)
    if (lhs.tpe == rhs.tpe) {
      BinaryOp(op, lhs, rhs)
    } else if (isLongOp(op, lhs.tpe, rhs.tpe)) {
      val clhs = intToLong(lhs)
      val crhs = if (isLongSpecial(op)) longToInt(rhs) else intToLong(rhs)
      BinaryOp(op, clhs, crhs)
    } else {
      val lsrc = convertArg(op, lhs, lhs.tpe, rType)
      val rsrc = convertArg(op, rhs, rhs.tpe, rType)
      BinaryOp(op, lsrc, rsrc)
    }
  }

  private def intToLong(t: Tree) = if (isLongType(t.tpe)) t else UnaryOp(UnaryOp.IntToLong, t)

  private def longToInt(t: Tree) = if (isIntType(t.tpe)) t else UnaryOp(UnaryOp.LongToInt, t)

  /* Convert arg to the correct type for it to be us by the binary op */
  private def convertArg(op: BinaryOp.Code, tree: Tree, t: Type, returnType: Type) = {
    val notLong = {
      if (!isLongType(t)) tree
      else if (isLongSpecial(op)) UnaryOp(UnaryOp.LongToInt, tree)
      else UnaryOp(UnaryOp.LongToDouble, tree)
    }
    if (!isFloatType(returnType)) notLong
    else if (isFloatType(t)) notLong
    else UnaryOp(UnaryOp.DoubleToFloat, notLong)
  }

}

object GenBinaryOp {
  private def isLongType(t: Type) = t == LongType
  private def isStringType(t: Type) = t == StringType
  private def isFloatType(t: Type) = t == FloatType
  private def isIntType(t: Type) = t == IntType
  private def isDoubleType(t: Type) = t == DoubleType


  /* Long is special, because of its shift operations, that accepts only int */
  private def isLongOp(op: BinaryOp.Code, ltpe: Type, rtpe: Type) = {
    (isLongType(ltpe) || isLongType(rtpe)) &&
      !(isFloatType(ltpe) || isFloatType(rtpe) || isStringType(ltpe) ||
        isStringType(rtpe) || isDoubleType(ltpe) || isDoubleType(rtpe))

  }

  private def isLongSpecial(op: BinaryOp.Code): Boolean =
    op == BinaryOp.Long_<< || op == BinaryOp.Long_>> || op == BinaryOp.Long_>>>

  val longBinaryOp = Map(
    "EQEQ" -> BinaryOp.Long_==,
    "plus" -> BinaryOp.Long_+,
    "minus" -> BinaryOp.Long_-,
    "times" -> BinaryOp.Long_*,
    "div" -> BinaryOp.Long_/,
    "rem" -> BinaryOp.Long_%,
    "or" -> BinaryOp.Long_|,
    "and" -> BinaryOp.Long_&,
    "xor" -> BinaryOp.Long_^,
    "shl" -> BinaryOp.Long_<<,
    "shr" -> BinaryOp.Long_>>,
    "ushr" -> BinaryOp.Long_>>>
  )

  val intBinaryOp = Map(
    "EQEQ" -> BinaryOp.Num_==,
    "plus" -> BinaryOp.Int_+,
    "minus" -> BinaryOp.Int_-,
    "times" -> BinaryOp.Int_*,
    "div" -> BinaryOp.Int_/,
    "rem" -> BinaryOp.Int_%,
    "or" -> BinaryOp.Int_|,
    "and" -> BinaryOp.Int_&,
    "xor" -> BinaryOp.Int_^,
    "shl" -> BinaryOp.Int_<<,
    "shr" -> BinaryOp.Int_>>,
    "ushr" -> BinaryOp.Int_>>>
  )

  val doubleBinaryOp = Map(
    "EQEQ" -> BinaryOp.Num_==,
    "plus" -> BinaryOp.Double_+,
    "minus" -> BinaryOp.Double_-,
    "times" -> BinaryOp.Double_*,
    "div" -> BinaryOp.Double_/,
    "rem" -> BinaryOp.Double_%
  )

  val floatBinaryOp = Map(
    "EQEQ" -> BinaryOp.Num_==,
    "plus" -> BinaryOp.Float_+,
    "minus" -> BinaryOp.Float_-,
    "times" -> BinaryOp.Float_*,
    "div" -> BinaryOp.Float_/,
    "rem" -> BinaryOp.Float_%
  )

  val booleanBinaryOp = Map(
    "EQEQ" -> BinaryOp.Boolean_==,
    "or" -> BinaryOp.Boolean_|,
    "and" -> BinaryOp.Boolean_&
  )

  val stringBinaryOp = Map(
    "plus" -> BinaryOp.String_+,
    "EQEQ" -> BinaryOp.===
  )

  val builtinBinarOp = Map(
    "EQEQ" -> BinaryOp.===
  )

  /* Find the correct binary op for a given type */
  def getBinaryOp(op: String, tpe: Type): BinaryOp.Code = {
    val opMap: Map[String, Int] = (tpe: @unchecked) match {
      case BooleanType => booleanBinaryOp
      case IntType => intBinaryOp
      case LongType => longBinaryOp
      case FloatType => floatBinaryOp
      case DoubleType => doubleBinaryOp
      case StringType | ClassType("T") => stringBinaryOp
    }

    opMap.getOrElse(op, throw new Error(s"Binary op not found: $op for type: $tpe"))
  }

  def getBuiltinOp(op: String) : BinaryOp.Code =
    builtinBinarOp.getOrElse(op, throw new Error(s"Binary op not found: $op"))

  def isBinaryOp(op: String): Boolean =
    longBinaryOp.keySet(op) || intBinaryOp.keySet(op)

}
