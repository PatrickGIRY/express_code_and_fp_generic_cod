package cashregsiter;

public class HistoryPurshase {

    private final Quantity quantity;

    public HistoryPurshase(Quantity quantity) {
        this.quantity = quantity;
    }

    public boolean isEligible(QuantityCriteria quantityCriteria) {
        return quantityCriteria.isSatisfied(quantity);
    }
}
