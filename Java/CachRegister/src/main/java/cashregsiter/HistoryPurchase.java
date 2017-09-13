package cashregsiter;

public class HistoryPurchase {

    private final Quantity quantity;

    public HistoryPurchase(Quantity quantity) {
        this.quantity = quantity;
    }

    public boolean isEligible(QuantityCriteria quantityCriteria) {
        return quantityCriteria.isSatisfied(quantity);
    }
}
