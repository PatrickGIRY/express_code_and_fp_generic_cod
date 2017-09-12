package cashregsiter;


import java.util.function.Predicate;

public interface QuantityCriteria extends Predicate<Quantity> {
    boolean isSatisfied(Quantity q);

    default boolean test(Quantity q) {
        return isSatisfied(q);
    }

    static QuantityCriteria from(Predicate<Quantity> predicate) {
        return predicate::test;
    }
}
