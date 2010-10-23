import sys
import timeit
import functools
import operator as op

from cStringIO import StringIO

def disable_test(f):
    return 'disabled'

def baseline(loop_count):
    for num in xrange(loop_count):
        str(num)


def method1(loop_count):
    out_str = ''
    for num in xrange(loop_count):
        out_str += str(num)
    return out_str

@disable_test
def method2(loop_count):
    from UserString import MutableString
    out_str = MutableString()
    for num in xrange(loop_count):
        out_str += str(num)
    return out_str

def method3(loop_count):
    from array import array
    char_array = array('c')
    for num in xrange(loop_count):
        char_array.fromstring(str(num))
    return char_array.tostring()

def method4(loop_count):
    str_list = []
    for num in xrange(loop_count):
        str_list.append(str(num))
    out_str = ''.join(str_list)
    return out_str

def method5(loop_count):
    file_str = StringIO()
    for num in xrange(loop_count):
        file_str.write(str(num))
    out_str = file_str.getvalue()
    return out_str

def bench(loop_count, methods):
    for name, method in sorted(methods, key=op.itemgetter(0)):
        t = timeit.Timer(functools.partial(method, loop_count))
        print t.timeit(number = 1),


def main():
    try:
        loop_count = int(sys.argv[1])
    except IndexError:
        loop_count = 10000
    except ValueError, e:
        print e
        loop_count = 10000

    selected_method_names = sys.argv[2:]
    if selected_method_names:
        methods = [(name, method) for (name, method) in globals().items()
                   if name in selected_method_names and callable(method)]
    else:
        methods = tuple([('baseline', baseline)] +
                        [(name, f) for (name, f) in globals().items()
                         if ('method' in name) and callable(f)])
    bench(loop_count, methods)


if __name__ == '__main__':
    main()