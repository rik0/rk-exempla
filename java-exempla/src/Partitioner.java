/**
 * Created by IntelliJ IDEA.
 * User: enrico
 * Date: Sep 12, 2010
 * Time: 10:03:15 AM
 * To change this template use File | Settings | File Templates.
 */


public class Partitioner<E> {
    final Predicate<E> p;

    public Partitioner(final Predicate<E> p) {
        this.p = p;
    }

    public void partition(final FList<E> it, final Continuation k) {
        if (it.isEmpty()) {
            k.call(FList.empty(), FList.empty());
        } else {
            if (p.call(it.car())) {
                partition(it.cdr(), new Continuation() {
                    public void call(Object... arguments) {
                        final FList<E> trueNodes = (FList<E>) arguments[0];
                        final FList<E> falseNodes = (FList<E>) arguments[1];
                        k.call(trueNodes.cons(it.car()), falseNodes);
                    }
                });
            } else {
                partition(it.cdr(), new Continuation() {
                    public void call(Object... arguments) {
                        final FList<E> trueNodes = (FList<E>) arguments[0];
                        final FList<E> falseNodes = (FList<E>) arguments[1];
                        k.call(trueNodes, falseNodes.cons(it.car()));
                    }
                });
            }
        }
    }
}
