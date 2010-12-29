def frozensetify(iterable):
    '''Transform a nested structure of iterables (e.g.,
    lists, sets, etc) in a nested structure of frozensets.

    @author: Enrico Franchi
    '''

    if isinstance(iterable, basestring):
        return iterable
    try:
        return frozenset(frozensetify(item) for item in iterable)
    except TypeError:
        return iterable
