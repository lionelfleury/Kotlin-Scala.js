package kotlin.collections

public abstract class ByteIterator : Iterator<Byte> {
    override final fun next() = nextByte()

    public abstract fun nextByte(): Byte
}

public abstract class CharIterator : Iterator<Char> {
    override final fun next() = nextChar()

    public abstract fun nextChar(): Char
}

public abstract class ShortIterator : Iterator<Short> {
    override final fun next() = nextShort()

    public abstract fun nextShort(): Short
}

public abstract class IntIterator : Iterator<Int> {
    override final fun next() = nextInt()

    public abstract fun nextInt(): Int
}

public abstract class LongIterator : Iterator<Long> {
    override final fun next() = nextLong()

    public abstract fun nextLong(): Long
}

public abstract class FloatIterator : Iterator<Float> {
    override final fun next() = nextFloat()

    public abstract fun nextFloat(): Float
}

public abstract class DoubleIterator : Iterator<Double> {
    override final fun next() = nextDouble()

    public abstract fun nextDouble(): Double
}

public abstract class BooleanIterator : Iterator<Boolean> {
    override final fun next() = nextBoolean()

    public abstract fun nextBoolean(): Boolean
}
