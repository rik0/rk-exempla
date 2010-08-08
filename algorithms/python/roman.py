
def with_weights(weights):
    def _aux(f):
        f.weights = weights
        f.sorted_weights_tuples = sorted(weights.iteritems(), reverse=True)
        return f
    return _aux

@with_weights({ 900 : 'CM', 500 : 'D', 400 : 'CD', 100 : 'C',
                90  : 'XC', 50  : 'L', 40  : 'XL', 10  : 'X',
                9   : 'IX', 5   : 'V', 4   : 'IV', 1   : 'I'})
def int2roman_dec(n):
    thousands, n = divmod(n, 1000)
    roman_digits = ['M', ] * thousands

    for m, s in int2roman_dec.sorted_weights_tuples:
        while n >= m:
            roman_digits.append(s)
            n -= m

    return ''.join(roman_digits)


class Int2RomanConverter(object):
    weights = { 900 : 'CM', 500 : 'D', 400 : 'CD', 100 : 'C',
                90  : 'XC', 50  : 'L', 40  : 'XL', 10  : 'X',
                9   : 'IX', 5   : 'V', 4   : 'IV', 1   : 'I'}
    sorted_weights_tuples = sorted(weights.iteritems(), reverse=True)

    def __call__(self, n):
        thousands, n = divmod(n, 1000)
        roman_digits = ['M', ] * thousands

        for m, s in self.sorted_weights_tuples:
            while n >= m:
                roman_digits.append(s)
                n -= m

        return ''.join(roman_digits)

int2roman_cl = Int2RomanConverter()

@apply
def int2roman():
    weights = { 900 : 'CM', 500 : 'D', 400 : 'CD', 100 : 'C',
                90  : 'XC', 50  : 'L', 40  : 'XL', 10  : 'X',
                9   : 'IX', 5   : 'V', 4   : 'IV', 1   : 'I'}
    sorted_weights_tuples = sorted(weights.iteritems(), reverse=True)
    def int2roman(n):
        thousands, n = divmod(n, 1000)
        roman_digits = ['M', ] * thousands

        for m, s in sorted_weights_tuples:
            while n >= m:
                roman_digits.append(s)
                n -= m

        return ''.join(roman_digits)
    return int2roman


if __name__ == "__main__":
    import timeit
    setup = '''import roman
numbers = range(1, 1000)
numbers += [3678, 3286, 5483, 8546, 1234, 5869, 97342]
'''
    statements = [
        '''int2roman = roman.int2roman%s
for n in numbers: int2roman(n)'''% ending
        for ending in ['_cl', '_dec', '']]
    for stmt in statements:
        print stmt, timeit.timeit(stmt, setup, number=1000)
