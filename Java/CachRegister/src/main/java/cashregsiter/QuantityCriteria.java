package cashregsiter;


import java.util.function.Predicate;

public interface QuantityCriteria extends Predicate<Quantity> {
    boolean isSatisfied(Quantity q);

    default boolean test(Quantity q) {
        return isSatisfied(q);
    }

    default QuantityCriteria and(QuantityCriteria other) {
        return quantity -> Predicate.super.and(other).test(quantity);
    }
}
