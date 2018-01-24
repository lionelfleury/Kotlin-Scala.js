/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Benchmarks        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, Jonas Fonseca    **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \                               **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

// Copyright 2011 Google Inc. All Rights Reserved.
// Copyright 1996 John Maloney and Mario Wolczko
//
// This file is part of GNU Smalltalk.
//
// GNU Smalltalk is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2, or (at your option) any later version.
//
// GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
// Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
//
// Translated first from Smalltalk to JavaScript, and finally to
// Dart by Google 2008-2010.
// Translated to Scala.js by Jonas Fonseca 2013
// Translated to Kotlin JS by Florian Alonso 2017

package benchmarks.deltablue

import benchmarks.Benchmark
import benchmarks.collections.LinkedList
import benchmarks.collections.Stack

/**
 * A KotlinJS implementation of the DeltaBlue constraint-solving
 * algorithm, as described in:
 *
 * "The DeltaBlue Algorithm: An Incremental Constraint Hierarchy Solver"
 *   Bjorn N. Freeman-Benson and John Maloney
 *   January 1990 Communications of the ACM,
 *   also available as University of Washington TR 89-08-06.
 *
 * Beware: this benchmark is written in a grotesque style where
 * the constraint model is built by side-effects from constructors.
 * I've kept it this way to avoid deviating too much from the original
 * implementation.
 */
fun main(args: Array<String>) {
    DeltaBlue.main(args)
}

object DeltaBlue: Benchmark() {

    override val prefix = "DeltaBlue"

    override fun run() {
        chainTest(100)
        projectionTest(100)
    }

    /**
     * This is the standard DeltaBlue benchmark. A long chain of equality
     * constraints is constructed with a stay constraint on one end. An
     * edit constraint is then added to the opposite end and the time is
     * measured for adding and removing this constraint, and extracting
     * and executing a constraint satisfaction plan. There are two cases.
     * In case 1, the added constraint is stronger than the stay
     * constraint and values must propagate down the entire length of the
     * chain. In case 2, the added constraint is weaker than the stay
     * constraint so it cannot be accomodated. The cost in this case is,
     * of course, very low. Typical situations lie somewhere between these
     * two extremes.
     */
    private fun chainTest(n: Int) {
        val planner = Planner()
        var prev: Variable? = null
        var first: Variable? = null
        var last: Variable? = null

        // Build chain of n equality constraints.
        for (i in 0 .. n) {
            val v = Variable("v", 0)
            if (prev != null) EqualityConstraint(prev, v, REQUIRED, planner)
            if (i == 0) first = v
            if (i == n) last = v
            prev = v
        }

        StayConstraint(last!!, STRONG_DEFAULT, planner)
        val edit = EditConstraint(first!!, PREFERRED, planner)
        val plan = planner.extractPlanFromConstraints(LinkedList<Constraint>(edit))
        for (j in 0 until 100) {
            first.value = j
            plan.execute()
            if (last.value != j) {
                print("Chain test failed.\n${last.value})\n$j")
            }
        }
    }

    /**
     * This test constructs a two sets of variables related to each
     * other by a simple linear transformation (scale and offset). The
     * time is measured to change a variable on either side of the
     * mapping and to change the scale and offset factors.
     */
    private fun projectionTest(n: Int) {
        val planner = Planner()
        val scale: Variable = Variable("scale", 10)
        val offset = Variable("offset", 1000)
        var src: Variable? = null
        var dst: Variable? = null

        val dests = arrayOfNulls<Variable>(n)
        for (i in 0 until n) {
            src = Variable("src", i)
            dst = Variable("dst", i)
            dests[i] = dst
            StayConstraint(src, NORMAL, planner)
            ScaleConstraint(src, scale, offset, dst, REQUIRED, planner)
        }

        change(src!!, 17, planner)

        if (dst!!.value != 1170) print("Projection 1 failed")

        change(dst, 1050, planner)

        if (src.value != 5) print("Projection 2 failed")

        change(scale, 5, planner)

        for (j in 0 until n - 1) {
            if (dests[j]!!.value != j * 5 + 1000) print("Projection 3 failed")
        }

        change(offset, 2000, planner)

        for (k in 0 until n - 1) {
            if (dests[k]!!.value != k * 5 + 2000) print("Projection 4 failed")
        }
    }

    private fun change(v: Variable, newValue: Int, planner: Planner) {
        val edit = EditConstraint(v, PREFERRED, planner)
        val plan = planner.extractPlanFromConstraints(LinkedList(edit))
        for (i in 0 until 10) {
            v.value = newValue
            plan.execute()
        }
        edit.destroyConstraint()
    }
}

/**
 * Strengths are used to measure the relative importance of constraints.
 * New strengths may be inserted in the strength hierarchy without
 * disrupting current constraints.  Strengths cannot be created outside
 * this class, so == can be used for value comparison.
 */
sealed class Strength(val value: Int, val name: String) {
    val nextWeaker = when (value) {
        0 -> STRONG_PREFERRED
        1 -> PREFERRED
        2 -> STRONG_DEFAULT
        3 -> NORMAL
        4 -> WEAK_DEFAULT
        5 -> WEAKEST
        else -> null
    }

    // Compile time computed constants.
    companion object Companion {

        fun stronger(s1: Strength, s2: Strength): Boolean =
            s1.value < s2.value

        fun weaker(s1: Strength, s2: Strength): Boolean =
            s1.value > s2.value

        fun weakest(s1: Strength, s2: Strength) =
            if (weaker(s1, s2)) s1 else s2

        fun strongest(s1: Strength, s2: Strength) =
            if (stronger(s1, s2)) s1 else s2
    }
}

object REQUIRED         : Strength(0, "required")
object STRONG_PREFERRED : Strength(1, "strongPreferred")
object PREFERRED        : Strength(2, "preferred")
object STRONG_DEFAULT   : Strength(3, "strongDefault")
object NORMAL           : Strength(4, "normal")
object WEAK_DEFAULT     : Strength(5, "weakDefault")
object WEAKEST          : Strength(6, "weakest")


abstract class Constraint(val strength: Strength, val planner: Planner) {

    abstract fun isSatisfied(): Boolean
    abstract fun markUnsatisfied(): Unit
    abstract fun addToGraph(): Unit
    abstract fun removeFromGraph(): Unit
    abstract fun chooseMethod(mark: Int): Unit
    abstract fun markInputs(mark: Int): Unit
    abstract fun inputsKnown(mark: Int): Boolean
    abstract fun output(): Variable
    abstract fun execute(): Unit
    abstract fun recalculate(): Unit

    /// Activate this constraint and attempt to satisfy it.
    fun addConstraint() {
        addToGraph()
        planner.incrementalAdd(this)
    }

    /**
     * Attempt to find a way to enforce this constraint. If successful,
     * record the solution, perhaps modifying the current dataflow
     * graph. Answer the constraint that this constraint overrides, if
     * there is one, or nil, if there isn't.
     * Assume: I am not already satisfied.
     */
    fun satisfy(mark: Int): Constraint? {
        chooseMethod(mark)
        if (!isSatisfied()) {
            if (strength == REQUIRED) {
                print("Could not satisfy a required constraint!")
            }
            return null
        } else {
            markInputs(mark)
            val out = output()
            val overridden = out.determinedBy
            if (overridden != null)
                overridden.markUnsatisfied()
            out.determinedBy = this
            if (!planner.addPropagate(this, mark))
                print("Cycle encountered")
            out.mark = mark
            return overridden
        }
    }

    fun destroyConstraint() {
        if (isSatisfied())
            planner.incrementalRemove(this)
        removeFromGraph()
    }

    /**
     * Normal constraints are not input constraints.  An input constraint
     * is one that depends on external state, such as the mouse, the
     * keybord, a clock, or some arbitraty piece of imperative code.
     */
    open fun isInput() = false
}

/**
 * Abstract superclass for constraints having a single possible output variable.
 */
abstract class UnaryConstraint(val myOutput: Variable, strength: Strength, planner: Planner) : Constraint(strength, planner) {

    private var satisfied = false

    init {
        addConstraint()
    }


    /// Adds this constraint to the constraint graph
    override fun addToGraph() {
        myOutput.addConstraint(this)
        satisfied = false
    }

    /// Decides if this constraint can be satisfied and records that decision.
    override fun chooseMethod(mark: Int) {
        satisfied = (myOutput.mark != mark) &&
                Strength.stronger(strength, myOutput.walkStrength)
    }

    /// Returns true if this constraint is satisfied in the current solution.
    override fun isSatisfied(): Boolean = satisfied

    override fun markInputs(mark: Int) {
        // has no inputs.
    }

    /// Returns the current output variable.
    override fun output() = myOutput

    /**
     * Calculate the walkabout strength, the stay flag, and, if it is
     * 'stay', the value for the current output of this constraint. Assume
     * this constraint is satisfied.
     */
    override fun recalculate() {
        myOutput.walkStrength = strength
        myOutput.stay = !isInput()
        if (myOutput.stay) execute(); // Stay optimization.
    }

    /// Records that this constraint is unsatisfied.
    override fun markUnsatisfied() {
        satisfied = false
    }

    override fun inputsKnown(mark: Int) = true

    override fun removeFromGraph() {
        myOutput.removeConstraint(this)
        satisfied = false
    }
}

/**
 * Variables that should, with some level of preference, stay the same.
 * Planners may exploit the fact that instances, if satisfied, will not
 * change their output during plan execution.  This is called "stay
 * optimization".
 */
class StayConstraint(v: Variable, str: Strength, planner: Planner) : UnaryConstraint(v, str, planner) {
    override fun execute() {
        // Stay constraints do nothing.
    }
}

/**
 * A unary input constraint used to mark a variable that the client
 * wishes to change.
 */
class EditConstraint(v: Variable, str: Strength, planner: Planner) : UnaryConstraint(v, str, planner) {

    /// Edits indicate that a variable is to be changed by imperative code.
    override fun isInput() = true

    override fun execute() {
        // Edit constraints do nothing.
    }
}

object Direction {
    final val NONE = 1
    final val FORWARD = 2
    final val BACKWARD = 0
}

/**
 * Abstract superclass for constraints having two possible output
 * variables.
 */
abstract class BinaryConstraint(val v1: Variable, val v2: Variable, strength: Strength, planner: Planner) : Constraint(strength, planner) {

    protected var direction = Direction.NONE

    /**
     * Decides if this constraint can be satisfied and which way it
     * should flow based on the relative strength of the variables related,
     * and record that decision.
     */
    override fun chooseMethod(mark: Int) {
        if (v1.mark == mark) {
            direction =
                    if ((v2.mark != mark && Strength.stronger(strength, v2.walkStrength)))
                        Direction.FORWARD
                    else
                        Direction.NONE
        }
        if (v2.mark == mark) {
            direction =
                    if (v1.mark != mark && Strength.stronger(strength, v1.walkStrength))
                        Direction.BACKWARD
                    else
                        Direction.NONE
        }
        if (Strength.weaker(v1.walkStrength, v2.walkStrength)) {
            direction =
                    if (Strength.stronger(strength, v1.walkStrength))
                        Direction.BACKWARD
                    else
                        Direction.NONE
        } else {
            direction =
                    if (Strength.stronger(strength, v2.walkStrength))
                        Direction.FORWARD
                    else
                        Direction.BACKWARD
        }
    }

    /// Add this constraint to the constraint graph.
    override fun addToGraph() {
        v1.addConstraint(this)
        v2.addConstraint(this)
        direction = Direction.NONE
    }

    /// Answer true if this constraint is satisfied in the current solution.
    override fun isSatisfied() = direction != Direction.NONE

    /// Mark the input variable with the given mark.
    override fun markInputs(mark: Int) {
        input().mark = mark
    }

    /// Returns the current input variable
    fun input() = if (direction == Direction.FORWARD) v1 else v2

    /// Returns the current output variable.
    override fun output() = if (direction == Direction.FORWARD) v2 else v1

    /**
     * Calculate the walkabout strength, the stay flag, and, if it is
     * 'stay', the value for the current output of this
     * constraint. Assume this constraint is satisfied.
     */
    override fun recalculate() {
        val ihn = input()
        val out = output()
        out.walkStrength = Strength.weakest(strength, ihn.walkStrength)
        out.stay = ihn.stay
        if (out.stay) execute()
    }

    /// Record the fact that this constraint is unsatisfied.
    override fun markUnsatisfied() {
        direction = Direction.NONE
    }

    override fun inputsKnown(mark: Int): Boolean {
        val i = input()
        return i.mark == mark || i.stay || i.determinedBy == null
    }

    override fun removeFromGraph() {
        v1.removeConstraint(this)
        v2.removeConstraint(this)
        direction = Direction.NONE
    }
}

/**
 * Relates two variables by the linear scaling relationship: "v2 =
 * (v1 * scale) + offset". Either v1 or v2 may be changed to maintain
 * this relationship but the scale factor and offset are considered
 * read-only.
 */

class ScaleConstraint(v1: Variable, val scale: Variable, val offset: Variable, v2: Variable, strength: Strength, planner: Planner): BinaryConstraint(v1, v2, strength, planner) {

    init {
        addConstraint()
    }

    /// Adds this constraint to the constraint graph.
    override fun addToGraph() {
        super.addToGraph()
        scale.addConstraint(this)
        offset.addConstraint(this)
    }

    override fun removeFromGraph() {
        super.removeFromGraph()
        scale.removeConstraint(this)
        offset.removeConstraint(this)
    }

    override fun markInputs(mark: Int) {
        super.markInputs(mark)
        scale.mark = mark
        offset.mark = mark
    }

    /// Enforce this constraint. Assume that it is satisfied.
    override fun execute() {
        if (direction == Direction.FORWARD) {
            v2.value = v1.value * scale.value + offset.value
        } else {
            // XXX: Truncates the resulting value
            v1.value = (v2.value - offset.value) / scale.value
        }
    }

    /**
     * Calculate the walkabout strength, the stay flag, and, if it is
     * 'stay', the value for the current output of this constraint. Assume
     * this constraint is satisfied.
     */
    override fun recalculate() {
        val ihn = input()
        val out = output()
        out.walkStrength = Strength.weakest(strength, ihn.walkStrength)
        out.stay = ihn.stay && scale.stay && offset.stay
        if (out.stay) execute()
    }

}

/**
 * Constrains two variables to have the same value.
 */
class EqualityConstraint(v1: Variable, v2: Variable, strength: Strength, planner: Planner) : BinaryConstraint(v1, v2, strength, planner) {

    init {
        addConstraint()
    }

    /// Enforce this constraint. Assume that it is satisfied.
    override fun execute() {
        output().value = input().value
    }
}

/**
 * A constrained variable. In addition to its value, it maintain the
 * structure of the constraint graph, the current dataflow graph, and
 * various parameters of interest to the DeltaBlue incremental
 * constraint solver.
 */
class Variable(val name: String, var value: Int) {

    val constraints = LinkedList<Constraint>()
    var determinedBy: Constraint? = null
    var mark = 0
    var walkStrength: Strength = WEAKEST
    var stay = true

    /**
     * Add the given constraint to the set of all constraints that refer
     * this variable.
     */
    fun addConstraint(c: Constraint) {
        constraints.add(c)
    }

    /// Removes all traces of c from this variable.
    fun removeConstraint(c: Constraint) {
        constraints.remove(c)
        if (determinedBy == c) determinedBy = null
    }
}

class Planner {

    var currentMark = 0

    /**
     * Attempt to satisfy the given constraint and, if successful,
     * incrementally update the dataflow graph.  Details: If satifying
     * the constraint is successful, it may override a weaker constraint
     * on its output. The algorithm attempts to resatisfy that
     * constraint using some other method. This process is repeated
     * until either a) it reaches a variable that was not previously
     * determined by any constraint or b) it reaches a constraint that
     * is too weak to be satisfied using any of its methods. The
     * variables of constraints that have been processed are marked with
     * a unique mark value so that we know where we've been. This allows
     * the algorithm to avoid getting into an infinite loop even if the
     * constraint graph has an inadvertent cycle.
     */
    fun incrementalAdd(c: Constraint) {
        val mark = newMark()
        var overridden = c.satisfy(mark)
        while (overridden != null)
            overridden = overridden.satisfy(mark)
    }

    /**
     * Entry point for retracting a constraint. Remove the given
     * constraint and incrementally update the dataflow graph.
     * Details: Retracting the given constraint may allow some currently
     * unsatisfiable downstream constraint to be satisfied. We therefore collect
     * a list of unsatisfied downstream constraints and attempt to
     * satisfy each one in turn. This list is traversed by constraint
     * strength, strongest first, as a heuristic for avoiding
     * unnecessarily adding and then overriding weak constraints.
     * Assume: [c] is satisfied.
     */
    fun incrementalRemove(c: Constraint) {
        val out = c.output()
        c.markUnsatisfied()
        c.removeFromGraph()
        val unsatisfied = removePropagateFrom(out)
        var strength: Strength = REQUIRED
        do {
            val iter = unsatisfied.iterator()
            while(iter.hasNext()){
                val u = iter.next() as Constraint
                if (u.strength == strength) incrementalAdd(u)
            }
            strength = strength.nextWeaker!!
        } while (strength != WEAKEST)
    }

    /// Select a previously unused mark value.
    fun newMark(): Int {
        currentMark += 1
        return currentMark
    }

    /**
     * Extract a plan for resatisfaction starting from the given source
     * constraints, usually a set of input constraints. This method
     * assumes that stay optimization is desired; the plan will contain
     * only constraints whose output variables are not stay. Constraints
     * that do no computation, such as stay and edit constraints, are
     * not included in the plan.
     * Details: The outputs of a constraint are marked when it is added
     * to the plan under construction. A constraint may be appended to
     * the plan when all its input variables are known. A variable is
     * known if either a) the variable is marked (indicating that has
     * been computed by a constraint appearing earlier in the plan), b)
     * the variable is 'stay' (i.e. it is a constant at plan execution
     * time), or c) the variable is not determined by any
     * constraint. The last provision is for past states of history
     * variables, which are not stay but which are also not computed by
     * any constraint.
     * Assume: [sources] are all satisfied.
     */
    fun makePlan(sources: Stack<Constraint>): Plan {
        val mark = newMark()
        val plan = Plan()
        val todo = sources
        while (!todo.isEmpty()) {
            val c = todo.pop() as Constraint
            if (c.output().mark != mark && c.inputsKnown(mark)) {
                plan.addConstraint(c)
                c.output().mark = mark
                addConstraintsConsumingTo(c.output(), todo)
            }
        }
        return plan
    }

    /**
     * Extract a plan for resatisfying starting from the output of the
     * given [constraints], usually a set of input constraints.
     */
    fun extractPlanFromConstraints(constraints: LinkedList<Constraint>): Plan {
        val sources = Stack<Constraint>()
        val iter = constraints.iterator()
        while(iter.hasNext()) {
            val c = iter.next() as Constraint
            // if not in plan already and eligible for inclusion.
            if (c.isInput() && c.isSatisfied()) sources.push(c)
        }
        return makePlan(sources)
    }

    /**
     * Recompute the walkabout strengths and stay flags of all variables
     * downstream of the given constraint and recompute the actual
     * values of all variables whose stay flag is true. If a cycle is
     * detected, remove the given constraint and answer
     * false. Otherwise, answer true.
     * Details: Cycles are detected when a marked variable is
     * encountered downstream of the given constraint. The sender is
     * assumed to have marked the inputs of the given constraint with
     * the given mark. Thus, encountering a marked node downstream of
     * the output constraint means that there is a path from the
     * constraint's output to one of its inputs.
     */
    fun addPropagate(c: Constraint, mark: Int): Boolean {
        val todo = Stack<Constraint>().push(c)
        while (!todo.isEmpty()) {
            val d = todo.pop() as Constraint
            if (d.output().mark == mark) {
                incrementalRemove(c)
                return false
            }
            d.recalculate()
            addConstraintsConsumingTo(d.output(), todo)
        }
        return true
    }

    /**
     * Update the walkabout strengths and stay flags of all variables
     * downstream of the given constraint. Answer a collection of
     * unsatisfied constraints sorted in order of decreasing strength.
     */
    fun removePropagateFrom(out: Variable): LinkedList<Constraint> {
        out.determinedBy = null
        out.walkStrength = WEAKEST
        out.stay = true
        val unsatisfied = LinkedList<Constraint>()
        val todo = Stack<Variable>().push(out)
        while (!todo.isEmpty()) {
            val v = todo.pop() as Variable
            val iter = v.constraints.iterator()
            while (iter.hasNext()){
                val c = iter.next() as Constraint
                if (!c.isSatisfied()) unsatisfied.add(c)
            }
            val determining = v.determinedBy
            val iter2 = v.constraints.iterator()
            while (iter2.hasNext()) {
                val next = iter2.next() as Constraint
                if (next != determining && next.isSatisfied()) {
                    next.recalculate()
                    todo.push(next.output())
                }
            }
        }
        return unsatisfied
    }

    fun addConstraintsConsumingTo(v: Variable, coll: Stack<Constraint>) {
        val determining = v.determinedBy
        val iter = v.constraints.iterator()
        while (iter.hasNext()) {
            val c = iter.next() as Constraint
            if (c != determining && c.isSatisfied()) coll.push(c)
        }
    }
}

/**
 * A Plan is an ordered list of constraints to be executed in sequence
 * to resatisfy all currently satisfiable constraints in the face of
 * one or more changing inputs.
 */
class Plan {
    private val list = LinkedList<Constraint>()

    fun addConstraint(c: Constraint) {
        list.add(c)
    }

    fun execute() {
        val iter = list.iterator()
        while (iter.hasNext()) {
            val constraint = iter.next() as Constraint
            constraint.execute()
        }
    }
}
