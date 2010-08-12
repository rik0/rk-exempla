import collections

def count_occurrences(fobj):
    r'''Return a dict with the occurrencies of each number in the iterable

    >>> occ = count_occurrences(["1", "2", "3", "1"])
    >>> occ[1]
    2
    >>> occ[2]
    1
    >>> occ[4]
    0
    >>> import StringIO
    >>> fobj = StringIO.StringIO("1\n2\n1\n3\n")
    >>> occ = count_occurrences(fobj)
    >>> occ[1]
    2
    >>> occ[2]
    1
    >>> occ[4]
    0
    '''
    occurrences = collections.defaultdict(lambda: 0)
    for line in fobj:
        try:
            int_value = int(line)
        except ValueError:
            pass # do something sensible with the malformed file
        else:
            occurrences[int_value] += 1
    return occurrences

def occurrences_in_file(name):
    with open(name) as fobj:
        return count_occurrences(fobj)

def guess(occurrences):
    while 1:
        try:
            user_number = int(raw_input("int> "))
            print 'The number occurs %d times.' % occurrences[user_number]
            break
        except ValueError:
            print "Integer required!"

if __name__ == '__main__':
    import doctest
    doctest.testmod()

    guess(occurrences_in_file('numbers.txt'))



