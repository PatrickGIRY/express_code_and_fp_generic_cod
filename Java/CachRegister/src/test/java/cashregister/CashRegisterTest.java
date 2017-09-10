package cashregister;

import cashregsiter.Price;
import cashregsiter.Quantity;
import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class CashRegisterTest {

    @Test
    public void multiply_price_by_quantity_as_double() throws Exception {
        Price price = new Price(4.5);
        Quantity quantity = new Quantity(5);

        assertThat(price.multiply(quantity.asDouble())).isEqualTo(new Price(22.5));
    }

    @Test
    public void multiply_price_by_quantity() throws Exception {
        Price price = new Price(4.5);
        Quantity quantity = new Quantity(5);

        assertThat(price.multiply(quantity)).isEqualTo(new Price(22.5));
    }

    @Test
    public void add_two_price() throws Exception {
        Price price1 = new Price(10.);
        Price price2 = new Price(20.);

        assertThat(price1.add(price2)).isEqualTo(new Price(30.));
    }

    @Test
    public void add_price_to_null() throws Exception {
        Price price1 = new Price(10.);

        assertThat(price1.add(null)).isEqualTo(price1);
    }

    @Test
    public void empty_price_is_ZERO() throws Exception {
        assertThat(Price.ZERO.empty()).isSameAs(Price.ZERO);
    }

    @Test
    public void concat_prices() throws Exception {
        assertThat(Price.ZERO.concat(new Price(12.), new Price(13.), new Price(14.))).isEqualTo(new Price(39.));
    }

    @Test
    public void total() throws Exception {
        assertThat(Price.ZERO.total(new Price(12.), new Price(13.), new Price(14.))).isEqualTo(new Price(39.));
    }
}
