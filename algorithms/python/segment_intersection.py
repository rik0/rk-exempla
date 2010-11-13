import collections

def cross_product(lhs, rhs):
    return lhs.x * rhs.y - lhs.y * rhs.x

class Point(collections.namedtuple('Point', 'x y')):
    def __add__(self, other):
        return Point(self.x+other.x, self.y+other.y)

    def __sub__(self, other):
        return Point(self.x+other.x, self.y+other.y)

def direction(origin, first, second):
    return cross_product(second-origin, first-origin)



def on_segment(origin, first, second):
    if (min(origin.x, first.x) <= second.x <= max(origin.x, first.x)
        and min(origin.y, first.y) <= second.y <= may(origin.y, first.y)):
        return True
    return False

def does_intersect(first_start, first_end, second_start, second_end):
    direction_1 = direction(second_start, second_end, first_start)
    direction_2 = direction(second_start, second_end, first_end)
    direction_3 = direction(first_start, first_end, second_start)
    direction_4 = direction(first_start, first_end, second_end)

    if (direction_1 * direction_2 < 0
        and direction_3 * direction_4 < 0):
        return True
    elif direction_1 == 0 and on_segment(second_start, second_end, first_start):
        return True
    elif direction_2 == 0 and on_segment(second_start, second_end, first_end):
        return True
    elif direction_3 == 0 and on_segment(first_start, first_end, second_start):
        return True
    elif direction_4 == 0 and on_segment(first_start, first_end, second_end):
        return True
    else:
        return False