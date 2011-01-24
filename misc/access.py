import inspect

def get_enclosing_class():
    current_frame = inspect.currentframe()
    decorator_frame = current_frame.f_back
    class_frame = decorator_frame.f_back
    return class_frame.f_code

def private(f):
    class_code = get_enclosing_class()
    def aux(self, *args, **kwargs):
        class_ = type(self)
        print (inspect.getsourcelines(class_) 
               == inspect.getsourcelines(type(self)))
        f(self, *args, **kwargs)
    return aux

class A(object):
    @private
    def foo(self):
        print 'foo'
        
    def bar(self):
        self.foo()
        
a = A()
a.foo()
a.bar()