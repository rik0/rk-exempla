import copy
import collections

def slot(attributes):
    def _aux(method):
        attributes[method.func_name] = method
        return method
    return _aux

def fobject(**attributes):
    @slot(attributes)
    def getSlot(slot_name, default=None):
        return attributes.get(slot_name)

    @slot(attributes)
    def show():
        return repr(attributes)

    def ancestors_iterator():
        ancestors = attributes.get('protos', [])
        visited = set()

        while ancestors:
            ancestor = ancestors.pop()
            if ancestor in visited:
                break
            else:
                visited.add(ancestor)
                yield ancestor
                ancestors.extend(ancestor.get('protos', []))


    def lookup_ancestors(attribute):
        pass


    def lookup(attribute):
        try:
            return attributes[attribute]
        except AttributeError:
            return lookup_ancestors(attribute)

    def do(command, *args):
        if command.endswith('='):
            attribute = command[:-1]
            attributes[attribute] = args[0]
        elif command.endswith('!'):
            attribute = command[:-1]
            del attributes[attribute]
        else:
            try:
                attribute = attributes[command]
                if callable(attribute):
                    return attribute(*args)
                else:
                    return attribute
            except KeyError:
                raise AttributeError('object has no attribute "%s"' % (command))
    return do

person=fobject()
person('name=', 'Mark')
person('surname=', 'Twain')

print person('name'), person('surname')
print person('getSlot', 'foo')
print person('show')

#print person('foo')