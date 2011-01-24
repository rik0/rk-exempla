def ilog2(n):
    '''
    Return binary logarithm base two of n.

    >>> ilog2(0)
    Traceback (most recent call last):
    ...
    ValueError: math domain error
    >>> ilog2(-10)
    Traceback (most recent call last):
    ...
    ValueError: math domain error
    >>> [ilog2(i) for i in range(1, 10)]
    [0, 1, 1, 2, 2, 2, 2, 3, 3]
    >>> [ilog2(2**i) for i in range(1,10)]
    [1, 2, 3, 4, 5, 6, 7, 8, 9]
    >>> [ilog2((2**i)+1) for i in range(1,10)]
    [1, 2, 3, 4, 5, 6, 7, 8, 9]
    >>> [ilog2((2**i)-1) for i in range(1,10)]
    [0, 1, 2, 3, 4, 5, 6, 7, 8]
    '''
    if n <= 0:
        raise ValueError('math domain error')
    return len(bin(n)) - 3


if __name__ == '__main__':
    import doctest
    doctest.testmod()
