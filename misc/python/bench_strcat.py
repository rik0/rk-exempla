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
    for method in methods:
        t = timeit.Timer(functools.partial(method, loop_count))
        sys.stdout.write('%f\t' % t.timeit(number = 1))

def evaluate_range(range_str):
    def aux(range_str):
        return range(*(int(p) for p in range_str.split(':')))

    if 'e' in range_str:
        base, range_str = range_str.split('e')
        base = int(base)
        return [base**x for x in aux(range_str)]
    else:
        return aux(range_str)


def main():
    input_sizes = []
    selected_method_names = []
    for arg in sys.argv[1:]:
        try:
            input_size = int(arg)
        except ValueError:
            try:
                range_ = evaluate_range(arg)
                input_sizes.extend(range_)
            except Exception:
                selected_method_names.append(arg)
        else:
            input_sizes.append(input_size)



    if selected_method_names:
        methods = [method for (name, method) in globals().items()
                   if name in selected_method_names and callable(method)]
    else:
        methods = tuple([baseline] +
                        [f for (name, f) in globals().items()
                         if ('method' in name) and callable(f)])

    methods = sorted(methods, key=op.attrgetter('func_name'))
    if not input_sizes:
        input_sizes.append(100000)

    for loop_count in sorted(input_sizes):
        sys.stdout.write('%d\t' % loop_count)
        bench(loop_count, methods)
        print


if __name__ == '__main__':
    main()