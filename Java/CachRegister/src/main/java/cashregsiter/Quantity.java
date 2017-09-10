package cashregsiter;

import java.util.Objects;
import java.util.function.IntPredicate;

public final class Quantity {
    private final int value;

    public Quantity(int value) {
        this.value = value;
    }

    public static QuantityCriteria criteria(IntPredicate predicate) {
        return quantity -> predicate.test(quantity.value);
    }

    public double asDouble() {
        return value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Quantity quantity = (Quantity) o;
        return value == quantity.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public String toString() {
        return "Quantity{" +
                "value=" + value +
                '}';
    }
}
