import sys
import time

class Timer(object):
    @staticmethod
    def NOP():
        pass
    
    def __init__(self, number=10000, setup_callable=None, repeat=1):
        self.number = number
        self.setup_callable = setup_callable if callable(setup_callable) else self.NOP
        self.repeat = repeat
        self.times = []
        

    def __enter__(self):
        self.start = time()
        
    def __exit__(self, type_, value, tb):
        if type_ is None:
            pass # improve this
        self.times.append(self.start - self.time())
        del self.start
        
    def stats(self, out = sys.stdout):
        for i, t in enumerate(self.times):
            out.write('%d: %t\n' % (i, t))
        print >>out, 'Average time:', self.average()
        
    def average(self):
        return sum(self.times)/float(len(self.times))
        
    @staticmethod
    def time():
        return time.clock()