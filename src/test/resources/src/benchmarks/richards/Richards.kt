/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Benchmarks        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, Jonas Fonseca    **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \                               **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

// Copyright 2006-2008 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// Ported by the Dart team to Dart.
// Ported by Jonas Fonseca to Scala.js.
// Translated by Florian Alonso to Kotlin JS 2017

// This is a Kotlin JS implementation of the Richards benchmark from:
//
//    http://www.cl.cam.ac.uk/~mr10/Bench.html
//
// The benchmark was originally implemented in BCPL by
// Martin Richards.

package benchmarks.richards

import benchmarks.Benchmark

fun main(args: Array<String>) {
    Richards.main(args)
}

/**
 * Richards simulates the task dispatcher of an operating system.
 */
object Richards: Benchmark() {

    override val prefix: String = "Richards"

    override fun run() {
        val scheduler = Scheduler()
        scheduler.addIdleTask(ID_IDLE, 0, null, COUNT)

        var queue = Packet(null, ID_WORKER, KIND_WORK)
        queue = Packet(queue, ID_WORKER, KIND_WORK)
        scheduler.addWorkerTask(ID_WORKER, 1000, queue)

        queue = Packet(null, ID_DEVICE_A, KIND_DEVICE)
        queue = Packet(queue, ID_DEVICE_A, KIND_DEVICE)
        queue = Packet(queue, ID_DEVICE_A, KIND_DEVICE)
        scheduler.addHandlerTask(ID_HANDLER_A, 2000, queue)

        queue = Packet(null, ID_DEVICE_B, KIND_DEVICE)
        queue = Packet(queue, ID_DEVICE_B, KIND_DEVICE)
        queue = Packet(queue, ID_DEVICE_B, KIND_DEVICE)
        scheduler.addHandlerTask(ID_HANDLER_B, 3000, queue)

        scheduler.addDeviceTask(ID_DEVICE_A, 4000, null)

        scheduler.addDeviceTask(ID_DEVICE_B, 5000, null)

        scheduler.schedule()

        if (scheduler.queueCount != EXPECTED_QUEUE_COUNT ||
                scheduler.holdCount != EXPECTED_HOLD_COUNT) {
            print("Error during execution: queueCount = ${scheduler.queueCount}, holdCount = ${scheduler.holdCount}.")
        }
        if (EXPECTED_QUEUE_COUNT != scheduler.queueCount) {
            throw Exception("bad scheduler queue-count")
        }
        if (EXPECTED_HOLD_COUNT != scheduler.holdCount) {
            throw Exception("bad scheduler hold-count")
        }
    }

    final val DATA_SIZE = 4
    final val COUNT = 1000

    /**
     * These two constants specify how many times a packet is queued and
     * how many times a task is put on hold in a correct run of richards.
     * They don't have any meaning a such but are characteristic of a
     * correct run so if the actual queue or hold count is different from
     * the expected there must be a bug in the implementation.
     */
    final val EXPECTED_QUEUE_COUNT = 2322
    final val EXPECTED_HOLD_COUNT = 928

    final val ID_IDLE = 0
    final val ID_WORKER = 1
    final val ID_HANDLER_A = 2
    final val ID_HANDLER_B = 3
    final val ID_DEVICE_A = 4
    final val ID_DEVICE_B = 5
    final val NUMBER_OF_IDS = 6

    final val KIND_DEVICE = 0
    final val KIND_WORK = 1
}

/**
 * A scheduler can be used to schedule a set of tasks based on their relative
 * priorities.  Scheduling is done by maintaining a list of task control blocks
 * which holds tasks and the data queue they are processing.
 */
class Scheduler {

    var queueCount = 0
    var holdCount = 0
    var currentTcb: TaskControlBlock? = null
    var currentId: Int = 0
    var list: TaskControlBlock? = null
    val blocks = arrayOfNulls<TaskControlBlock>(Richards.NUMBER_OF_IDS)

    /// Add an idle task to this scheduler.
    fun addIdleTask(id: Int, priority: Int, queue: Packet?, count: Int) {
        addRunningTask(id, priority, queue, IdleTask(this, 1, count))
    }

    /// Add a work task to this scheduler.
    fun addWorkerTask(id: Int, priority: Int, queue: Packet?) {
        addTask(id, priority, queue, WorkerTask(this, Richards.ID_HANDLER_A, 0))
    }

    /// Add a handler task to this scheduler.
    fun addHandlerTask(id: Int, priority: Int, queue: Packet?) {
        addTask(id, priority, queue, HandlerTask(this))
    }

    /// Add a handler task to this scheduler.
    fun addDeviceTask(id: Int, priority: Int, queue: Packet?) {
        addTask(id, priority, queue, DeviceTask(this))
    }

    /// Add the specified task and mark it as running.
    fun addRunningTask(id: Int, priority: Int, queue: Packet?, task: Task) {
        addTask(id, priority, queue, task)
        currentTcb!!.setRunning()
    }

    /// Add the specified task to this scheduler.
    fun addTask(id: Int, priority: Int, queue: Packet?, task: Task) {
        currentTcb = TaskControlBlock(list, id, priority, queue, task)
        list = currentTcb
        blocks[id] = currentTcb
    }

    /// Execute the tasks managed by this scheduler.
    fun schedule() {
        currentTcb = list
        while (currentTcb != null) {
            val tmpTcb = currentTcb!!
            if (tmpTcb.isHeldOrSuspended()) {
                currentTcb = tmpTcb.link
            } else {
                currentId = tmpTcb.id
                currentTcb = tmpTcb.run()
            }
        }
    }

    /// Release a task that is currently blocked and return the next block to run.
    fun release(id: Int): TaskControlBlock? {
        val tcb = blocks[id]
        if (tcb == null) return tcb
        else {
            tcb.markAsNotHeld()
            if (tcb.priority > currentTcb!!.priority) return tcb
            else return currentTcb
        }
    }

    /**
     * Block the currently executing task and return the next task control block
     * to run.  The blocked task will not be made runnable until it is explicitly
     * released, even if new work is added to it.
     */
    fun holdCurrent(): TaskControlBlock? {
        holdCount += 1
        val tmpTcb = currentTcb!!
        tmpTcb.markAsHeld()
        return tmpTcb.link
    }

    /**
     * Suspend the currently executing task and return the next task
     * control block to run.
     * If new work is added to the suspended task it will be made runnable.
     */
    fun suspendCurrent(): TaskControlBlock? {
        currentTcb!!.markAsSuspended()
        return currentTcb
    }

    /**
     * Add the specified packet to the end of the worklist used by the task
     * associated with the packet and make the task runnable if it is currently
     * suspended.
     */
    fun queue(packet: Packet): TaskControlBlock? {
        val t = blocks[packet.id]
        if (t == null) return t
        else {
            queueCount += 1
            packet.link = null
            packet.id = currentId
            return t.checkPriorityAdd(currentTcb!!, packet)
        }
    }
}

object TaskState {
    /// The task is running and is currently scheduled.
    final val RUNNING = 0

    /// The task has packets left to process.
    final val RUNNABLE = 1

    /**
     * The task is not currently running. The task is not blocked as such and may
     * be started by the scheduler.
     */
    final val SUSPENDED = 2

    /// The task is blocked and cannot be run until it is explicitly released.
    final val HELD = 4

    final val SUSPENDED_RUNNABLE = SUSPENDED or RUNNABLE
    final val NOT_HELD = -5
}

/**
 * A task control block manages a task and the queue of work packages associated
 * with it.
 *
 * @param id        The id of this block.
 * @param priority  The priority of this block.
 * @param queue     The queue of packages to be processed by the task.
 */
class TaskControlBlock(val link: TaskControlBlock?, val id: Int, val priority: Int, var queue: Packet?, val task: Task) {

    var state = if (queue == null) TaskState.SUSPENDED else TaskState.SUSPENDED_RUNNABLE

    fun setRunning() {
        state = TaskState.RUNNING
    }

    fun markAsNotHeld() {
        state = state and TaskState.NOT_HELD
    }

    fun markAsHeld() {
        state = state or TaskState.HELD
    }

    fun isHeldOrSuspended(): Boolean {
        return (state and TaskState.HELD) != 0 ||
                (state == TaskState.SUSPENDED)
    }

    fun markAsSuspended() {
        state = state or TaskState.SUSPENDED
    }

    fun markAsRunnable() {
        state = state or TaskState.RUNNABLE
    }

    /// Runs this task, if it is ready to be run, and returns the next task to run.
    fun run(): TaskControlBlock? {
        val packet = if (state == TaskState.SUSPENDED_RUNNABLE) queue else null
        if (packet != null) {
            queue = packet.link
            state = if (queue == null) TaskState.RUNNING else TaskState.RUNNABLE
        }
        return task.run(packet)
    }

    /**
     * Adds a packet to the worklist of this block's task, marks this as
     * runnable if necessary, and returns the next runnable object to run
     * (the one with the highest priority).
     */
    fun checkPriorityAdd(task: TaskControlBlock, packet: Packet): TaskControlBlock {
        if (queue == null) {
            queue = packet
            markAsRunnable()
            if (priority > task.priority) return this
            else return task
        } else {
            queue = packet.addTo(queue)
            return task
        }
    }

    override fun toString() = "tcb { ${task}@${state} }"
}

/**
 *  Abstract task that manipulates work packets.
 *
 * @param scheduler	  The scheduler that manages this task.
 */
sealed abstract class Task(val scheduler: Scheduler) {
    abstract fun run(packet: Packet?): TaskControlBlock?
}

/**
 * An idle task doesn't do any work itself but cycles control between the two
 * device tasks.
 *
 * @param v1	  A seed value that controls how the device tasks are scheduled.
 * @param count	The number of times this task should be scheduled.
 */
class IdleTask(scheduler: Scheduler, var v1: Int, var count: Int) : Task(scheduler) {

    override fun run(packet: Packet?): TaskControlBlock? {
        count -= 1
        if (count == 0) {
            return scheduler.holdCurrent()
        } else if ((v1 and 1) == 0) {
            v1 = v1 shr 1
            return scheduler.release(Richards.ID_DEVICE_A)
        } else {
            v1 = (v1 shr 1) xor 0xD008
            return scheduler.release(Richards.ID_DEVICE_B)
        }
    }

}

/**
 * A task that suspends itself after each time it has been run to simulate
 * waiting for data from an external device.
 */
class DeviceTask(scheduler: Scheduler) : Task(scheduler) {

    var v1: Packet? = null

    override fun run(packet: Packet?): TaskControlBlock? {
        if (packet == null) {
            if (v1 == null)
                return scheduler.suspendCurrent()
            else {
                val v = v1!!
                v1 = null
                return scheduler.queue(v)
            }
        } else {
            v1 = packet
            return scheduler.holdCurrent()
        }
    }

}

/**
 * A task that manipulates work packets.
 *
 * @param v1	A seed used to specify how work packets are manipulated.
 * @param v2	Another seed used to specify how work packets are manipulated.
 */
class WorkerTask(scheduler: Scheduler, var v1: Int, var v2: Int) : Task(scheduler) {

    override fun run(packet: Packet?): TaskControlBlock? {
        if (packet == null) {
            return scheduler.suspendCurrent()
        } else {
            if (v1 == Richards.ID_HANDLER_A) {
                v1 = Richards.ID_HANDLER_B
            } else {
                v1 = Richards.ID_HANDLER_A
            }
            packet.id = v1
            packet.a1 = 0
            for (i in 0 until Richards.DATA_SIZE) {
                v2 += 1
                if (v2 > 26) v2 = 1
                packet.a2[i] = v2
            }
            return scheduler.queue(packet)
        }
    }

}

/**
 * A task that manipulates work packets and then suspends itself.
 */
class HandlerTask(scheduler: Scheduler) : Task(scheduler) {

    var v1: Packet? = null
    var v2: Packet? = null

    override fun run(packet: Packet?): TaskControlBlock? {
        if (packet != null) {
            if (packet.kind == Richards.KIND_WORK) {
                v1 = packet.addTo(v1)
            } else {
                v2 = packet.addTo(v2)
            }
        }

        val v1AsVal = v1
        if (v1AsVal != null) {
            val count = v1AsVal.a1

            if (count < Richards.DATA_SIZE) {
                val v2AsVal = v2
                if (v2AsVal != null) {
                    val v = v2AsVal
                    v2 = v2AsVal.link
                    val c = v1AsVal.a2[count]
                    v.a1 = c!!.toInt()
                    v1AsVal.a1 = (count + 1)
                    return scheduler.queue(v)
                }
            } else {
                val vTmp = v1AsVal
                v1 = v1AsVal.link
                return scheduler.queue(vTmp)
            }
        }

        return scheduler.suspendCurrent()
    }

}

/**
 * A simple package of data that is manipulated by the tasks.  The exact layout
 * of the payload data carried by a packet is not importaint, and neither is the
 * nature of the work performed on packets by the tasks.
 * Besides carrying data, packets form linked lists and are hence used both as
 * data and worklists.
 *
 * @param link	The tail of the linked list of packets.
 * @param id	An ID for this packet.
 * @param kind	The type of this packet.
 */
class Packet(var link: Packet?, var id: Int, val kind: Int) {

    var a1 = 0
    val a2 = arrayOfNulls<Int>(Richards.DATA_SIZE)

    /// Add this packet to the end of a worklist, and return the worklist.
    fun addTo(queue: Packet?): Packet {
        link = null
        if (queue == null) {
            return this
        } else {
            var next = queue
            while (next!!.link != null)
                next = next.link

            next.link = this
            return queue
        }
    }

}