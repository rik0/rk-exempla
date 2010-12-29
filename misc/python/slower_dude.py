import timeit

setup_method = '''
class Pointless(object):
    def reverse(self, lis):
        for x in lis[::-1]:
            yield x

p = Pointless()
l = range(1000000)'''

setup_builtin = 'l = range(1000000)'

print 'Homemade', timeit.timeit('for x in p.reverse(l): pass',
    setup=setup_method, number=100)
print 'Builtin', timeit.timeit('for x in reversed(l): pass',
    setup=setup_method, number=100)