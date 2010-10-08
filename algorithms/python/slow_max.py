import random
import time
import itertools as it


def slow_max(lst):
    if lst:
        max_ = lst[0]
        for el in lst[1:]:
            if el > max_:
                max_ = el
        return max_
    else:
        raise ValueError


if __name__ == '__main__':
    elements = range(2000000)
    random.shuffle(elements)
    start = time.clock()
    print slow_max(elements)
    print "slow_max", time.clock() - start

    start = time.clock()
    print max(elements)
    print "max", time.clock() - start

    start = time.clock()
    print sorted(elements)[-1]
    print "sort copy", time.clock() - start

    start = time.clock()
    elements.sort()
    print elements[-1]
    print "sort in place", time.clock() - start