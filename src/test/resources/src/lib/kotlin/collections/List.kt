package kotlin.collections

fun <T> asList(vararg array: T): List<T> = java.util.Arrays.asList(*array)
