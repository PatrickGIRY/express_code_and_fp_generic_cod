package cashregister;


import cashregsiter.Quantity;
import cashregsiter.QuantityCriteria;
import org.junit.Test;

import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

public class QuantityCriteriaTest {

    @Test
    public void select_quantity() throws Exception {
        QuantityCriteria quantityCriteria = Quantity.criteria(v -> v > 10);

        Quantity[] result = Stream.of(new Quantity(8), new Quantity(10), new Quantity(15))
                .filter(quantityCriteria).toArray(Quantity[]::new);

        assertThat(result).containsExactly(new Quantity(15));
    }

    @Test
    public void select_between_quantities() throws Exception {
        QuantityCriteria over10 = Quantity.criteria(v -> v > 10);
        QuantityCriteria below100 = Quantity.criteria(v -> v < 100);

        QuantityCriteria between10And100 = over10.and(below100);

        Quantity[] result = Stream.of(new Quantity(8), new Quantity(10), new Quantity(15), new Quantity(100))
                .filter(between10And100).toArray(Quantity[]::new);

        assertThat(result).containsExactly(new Quantity(15));
    }
}