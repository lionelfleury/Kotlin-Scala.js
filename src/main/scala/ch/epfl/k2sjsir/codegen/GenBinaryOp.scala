package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions.IrCall
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._

import scala.collection.JavaConverters._

case class GenBinaryOp(d: IrCall, p: Positioner) extends Gen[IrCall] {

  import GenBinaryOp._

  def tree: Tree = {
    val desc = d.getDescriptor
    val name = desc.getName.asString()
    val args = for (i <- desc.getValueParameters.asScala.indices)
      yield GenExpr(d.getValueArgument(i), p).tree
    val rhs = if (args.nonEmpty) args.head else IntLiteral(1)
    val rec = if (d.getDispatchReceiver != null) d.getDispatchReceiver else d.getExtensionReceiver
    val lhs = GenExpr(rec, p).tree
    val rType = desc.getReturnType.toJsType
    val op = getBinaryOp(name, rType)
    if (name == "rangeTo") {
      if (lhs.tpe == LongType || rhs.tpe == LongType)
        New(ClassType("Lkotlin_ranges_LongRange"), Ident("init___J__J"), List(lhs, rhs))
      else
        New(ClassType("Lkotlin_ranges_IntRange"), Ident("init___I__I"), List(lhs, rhs))
    } else if (name == "compareTo") {
      if (lhs.tpe == StringType || lhs.tpe == ClassType("T")) {
        Apply(LoadModule(ClassType("sjsr_RuntimeString$")),
              Ident("compareTo__T__T__I", Some("compareTo__T__T__I")),
              List(lhs, AsInstanceOf(rhs, ClassType("T"))))(IntType)
      } else BinaryOp(BinaryOp.Double_-, genConversion(lhs.tpe, IntType, lhs), genConversion(rhs.tpe, IntType, rhs))
    } else if (lhs.tpe == rhs.tpe) {
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

  private def genConversion(from: Type, to: Type, value: Tree): Tree = {
    lazy val int0 = IntLiteral(0)
    lazy val int1 = IntLiteral(1)
    lazy val long0 = LongLiteral(0L)
    lazy val long1 = LongLiteral(1L)
    lazy val float0 = FloatLiteral(0.0f)
    lazy val float1 = FloatLiteral(1.0f)
    (from, to) match {
      case (IntType, BooleanType) => BinaryOp(BinaryOp.Num_!=, value, int0)
      case (LongType, BooleanType) => BinaryOp(BinaryOp.Long_!=, value, long0)
      case (FloatType, BooleanType) => BinaryOp(BinaryOp.Num_!=, value, float0)
      case (BooleanType, IntType) => If(value, int1, int0)(IntType)
      case (BooleanType, LongType) => If(value, long1, long0)(LongType)
      case (BooleanType, FloatType) => If(value, float1, float0)(FloatType)
      case _ => value
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

  private val longBinaryOp = Map(
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
    "ushr" -> BinaryOp.Long_>>>,
    "compareTo" -> BinaryOp.Long_-,
    "inc" -> BinaryOp.Long_+,
    "dec" -> BinaryOp.Long_-,
    "rangeTo" -> -1
  )

  private val intBinaryOp = Map(
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
    "ushr" -> BinaryOp.Int_>>>,
    "compareTo" -> BinaryOp.Int_-,
    "inc" -> BinaryOp.Int_+,
    "dec" -> BinaryOp.Int_-,
    "rangeTo" -> -1
  )

  private val doubleBinaryOp = Map(
    "EQEQ" -> BinaryOp.Num_==,
    "plus" -> BinaryOp.Double_+,
    "minus" -> BinaryOp.Double_-,
    "times" -> BinaryOp.Double_*,
    "div" -> BinaryOp.Double_/,
    "rem" -> BinaryOp.Double_%,
    "compareTo" -> BinaryOp.Double_-,
    "inc" -> BinaryOp.Double_+,
    "dec" -> BinaryOp.Double_-
  )

  private val floatBinaryOp = Map(
    "EQEQ" -> BinaryOp.Num_==,
    "plus" -> BinaryOp.Float_+,
    "minus" -> BinaryOp.Float_-,
    "times" -> BinaryOp.Float_*,
    "div" -> BinaryOp.Float_/,
    "rem" -> BinaryOp.Float_%,
    "compareTo" -> BinaryOp.Float_-,
    "inc" -> BinaryOp.Float_+,
    "dec" -> BinaryOp.Float_-
  )

  private val booleanBinaryOp = Map(
    "EQEQ" -> BinaryOp.Boolean_==,
    "or" -> BinaryOp.Boolean_|,
    "and" -> BinaryOp.Boolean_&
  )

  private val stringBinaryOp = Map(
    "plus" -> BinaryOp.String_+,
    "EQEQ" -> BinaryOp.===
  )

  private val builtinBinarOp = Map(
    "EQEQ" -> BinaryOp.===
  )

  private def opMap(tpe: Type): Map[String, Int] = tpe match {
    case BooleanType => booleanBinaryOp
    case IntType => intBinaryOp
    case LongType => longBinaryOp
    case FloatType => floatBinaryOp
    case DoubleType => doubleBinaryOp
    case StringType | ClassType("T") => stringBinaryOp
    case ClassType("Lkotlin_ranges_IntRange") => intBinaryOp
    case ClassType("Lkotlin_ranges_LongRange") => longBinaryOp
    case _ => builtinBinarOp
  }

  /* Find the correct binary op for a given type */
  def getBinaryOp(op: String, tpe: Type): BinaryOp.Code =
    opMap(tpe).getOrElse(op, throw new Error(s"Binary op not found: $op for type: $tpe"))

  def isBinaryOp(op: String): Boolean =
    longBinaryOp.keySet(op) || intBinaryOp.keySet(op)

}
