package functional;

import java.util.stream.Stream;

public interface Monoid<M extends Monoid<M>> {

    M append(M other);

    M empty();

    default M concat(M... ms) {
        return Stream.of(ms).reduce(empty(), Monoid::append);
    }
}
