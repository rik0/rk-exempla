import itertools as it


def sequence_equals(cmpf, l_seq, r_seq):
    '''
    >>> import operator as op
    >>> sequence_equals(op.eq, range(3), range(3))
    True
    >>> sequence_equals(op.eq, range(3), range(4))
    False
    >>> sequence_equals(op.eq, [], [])
    True
    >>> sequence_equals(op.eq, range(3), [])
    False
    >>> sequence_equals(op.eq, [0, 1, 2, 3, None], range(4))
    False
    '''
    for l_element, r_element in it.izip_longest(l_seq, r_seq):
        if not cmpf(l_element, r_element):
            return False
    else:
        return True


def sequence_equals_f(cmpf, l_seq, r_seq):
    '''
    >>> import operator as op
    >>> sequence_equals_f(op.eq, range(3), range(3))
    True
    >>> sequence_equals_f(op.eq, range(3), range(4))
    False
    >>> sequence_equals_f(op.eq, [], [])
    True
    >>> sequence_equals_f(op.eq, range(3), [])
    False
    >>> sequence_equals_f(op.eq, [0, 1, 2, 3, None], range(4))
    False
    '''
    return all(cmpf(x, y) for x, y in it.izip_longest(l_seq, r_seq))

def sequence_equals_m(cmpf, l_seq, r_seq):
    '''
    >>> import operator as op
    >>> sequence_equals_m(op.eq, range(3), range(3))
    True
    >>> sequence_equals_m(op.eq, range(3), range(4))
    False
    >>> sequence_equals_m(op.eq, [], [])
    True
    >>> sequence_equals_m(op.eq, range(3), [])
    False
    >>> sequence_equals_m(op.eq, [0, 1, 2, 3, None], range(4))
    False
    >>> sequence_equals_m(op.eq, it.count(), range(4))
    False
    '''
    lit = iter(l_seq)
    for el in r_seq:
        try:
            if not cmpf(el, lit.next()):
                return False
        except StopIteration:
            return False
    else:
        try:
            lit.next()
            return False
        except StopIteration:
            return True

def sequence_equals_s(cmpf, l_seq, r_seq):
    '''
    >>> import operator as op
    >>> sequence_equals_s(op.eq, range(3), range(3))
    True
    >>> sequence_equals_s(op.eq, range(3), range(4))
    False
    >>> sequence_equals_s(op.eq, [], [])
    True
    >>> sequence_equals_s(op.eq, range(3), [])
    False
    >>> sequence_equals_s(op.eq, [0, 1, 2, 3, None], range(4))
    False
    '''
    class NullObject(object):
        pass
    return all(cmpf(x, y) for x, y in it.izip_longest(l_seq, r_seq,
                                                      fillvalue=NullObject))

if __name__ == '__main__':
    import doctest
    doctest.testmod()