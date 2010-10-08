import qsort
from nose import tools

fixture_lsts = [
    [1, 2, 3, 4], [1, 2, 4, 3], [1, 3, 2, 4], [1, 3, 2, 3],
    [4, 2, 3, 1], [1, 4, 3, 2], [3, 2, 1, 4], [3, 2, 2, 4],
    [3, 1, 1, 3], [1, 1, 1, 1], [1, 2, 1, 1], [1, 2, 3],
    [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1],
    [3, 2, 3], [3, 1, 1], [], [1]
]


def check_sorted(lst):
    sorted_lst = sorted(lst)
    lst = lst[:] # do not modify lists
    qsort.quicksort(lst)
    tools.assert_equals(lst, sorted_lst)

def test_quicksort():
    for lst in fixture_lsts:
        yield check_sorted, lst

class CheckSorted(object):
    def __init__(self, lst):
        self.lst = lst

    def setup(self):
        self.sorted_lst = sorted(self.lst)
        self.lst = self.lst[:]

    def __call__(self):
        qsort.quicksort(self.lst)
        tools.assert_equals(self.lst, self.sorted_lst)


def test_quicksort2():
    for lst in fixture_lsts:
        yield CheckSorted(lst),

