package cashregsiter;

import functional.Monoid;

import java.util.Objects;

public final class Price implements Monoid<Price> {

    public static final Price ZERO = new Price(0.);

    private final double value;

    public Price(double value) {
        this.value = value;
    }

    public Price multiply(double multiplicand) {
        return new Price(value * multiplicand);
    }

    public Price multiply(Quantity quantity) {
        return new Price(value * quantity.asDouble());
    }

    public Price add(Price otherPrice) {
        return append(otherPrice);
    }

    public Price total(Price... prices) {
        return concat(prices);
    }

    @Override
    public Price append(Price other) {
        return other != null ? new Price(value + other.value) : this;
    }

    @Override
    public Price empty() {
        return ZERO;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Price price = (Price) o;
        return Double.compare(price.value, value) == 0;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public String toString() {
        return "Price{" +
                "value=" + value +
                '}';
    }

}
