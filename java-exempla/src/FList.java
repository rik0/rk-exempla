/**
 * Created by IntelliJ IDEA.
 * User: enrico
 * Date: Sep 12, 2010
 * Time: 10:12:38 AM
 * To change this template use File | Settings | File Templates.
 */
public class FList<E> {
    final private E value;
    final private FList<E> next;
    final static private FList EMPTY_F_LIST = new FList(null, null);

    public FList(E value, FList<E> next) {
        this.value = value;
        this.next = next;
    }

    public E car() {
        return value;
    }

    public FList<E> cdr() {
        return next;
    }

    public FList<E> cons(E car) {
        return new FList<E>(car, this);
    }

    public static FList empty() {
        return empty();
    }

    public boolean isEmpty() {
        return this.equals(EMPTY_F_LIST);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FList FList = (FList) o;

        if (next != null ? !next.equals(FList.next) : FList.next != null) return false;
        if (value != null ? !value.equals(FList.value) : FList.value != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = value != null ? value.hashCode() : 0;
        result = 31 * result + (next != null ? next.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "FList{" +
                "value=" + value +
                ", next=" + next +
                '}';
    }


}

