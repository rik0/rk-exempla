import re, sre


class Nth(object):
    def __init__(self, repl, n=0):
        self.n = n
        self.repl = repl
    def __call__(self, match):
        if self.n == 0:
            self.n -= 1
            return match.expand(self.repl)
        else:
            self.n -= 1
            return match.group(0)



rex = re.compile('a(\w+)')

print rex.sub(Nth(r'A\1', 0), "aardvark abbot abide")
