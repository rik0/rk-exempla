import numbers
import sys

MAX_INT = sys.maxsize

def isign(x, y):
    t = y >> 31
    return (abs(x) ^ t)-t

def icube_root(x):
    if x < 0:
        x = -x
        sign = -1
    else:
        sign = 1
    if isinstance(x, numbers.Integral) and :
        s = 30
        y = 0
        while s >= 0:
            y *= 2
            b = (3*y*(y+1)+1)
            bs = b << s
            if (x >= bs) and (b == (bs >> s)):
                x -= bs
                y += 1
            s -= 3
        return sign * y
    else:
        raise TypeError
