package it.unipr.aotlab.functional;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: enrico
 * Date: Sep 12, 2010
 * Time: 10:12:38 AM
 * To change this template use File | Settings | File Templates.
 */
public class FList<E> implements Iterable<E> {
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

    public int length() {
        int len = 0;
        for(E e : this) {
            ++len;
        }
        return len;
    }

    public Iterator<E> iterator() {
        return new FListIterator(this);
    }

    public int fillContainer(Collection<? super E> c) {
        int insertions = 0;
        for(E item : this) {
            c.add(item);
            ++insertions;
        }
        return insertions;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FList otherList = (FList) o;
        Iterator<E> otherIterator = otherList.iterator();
        Iterator<E> thisIterator = this.iterator();

        while(otherIterator.hasNext() && thisIterator.hasNext()) {
            if(!otherIterator.next().equals(thisIterator.next())) {
                return false;
            }
        }

        return (otherIterator.hasNext() == thisIterator.hasNext());
    }

    @Override
    public int hashCode() {
        int result = value != null ? value.hashCode() : 0;
        result = 31 * result + (next != null ? next.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append('(');

        for(Iterator<E> it = iterator(); it.hasNext();) {
            sb.append(it.next());
            if(it.hasNext()) {
                sb.append(", ");
            }
        }
        sb.append(')');
        return sb.toString();
    }

    static private class FListIterator<E> implements Iterator<E> {
        private FList<E> currentNode;

        private FListIterator(FList<E> currentNode) {
            this.currentNode = currentNode;
        }

        public boolean hasNext() {
            return !currentNode.isEmpty();
        }

        public E next() {
            if(hasNext()) {
                E tmp =  currentNode.car();
                currentNode = currentNode.cdr();
                return tmp;
            } else {
                throw new NoSuchElementException();
            }
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

}

