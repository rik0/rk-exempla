import copy as cp
import collections

def slot(attributes):
    def _aux(method):
        attributes[method.func_name] = method
        _aux.func_name = method.func_name
        return method
    return _aux

def attributes_only(method_slot):
    def _aux(obj, *args):
        attributes = obj('attributes')
        return method_slot(attributes, args)
    _aux.func_name = method_slot.func_name
    return _aux


def meta_object(label, ancestors=[], **attributes):
    attributes.update(locals())
    
    @slot(attributes)
    @attributes_only
    def getSlot(attributes, slot_name, default=None):
        return attributes.get(slot_name)

    @slot(attributes)
    def show(obj):
        attributes = obj('attributes')
        return repr(attributes)

    @slot(attributes)
    @attributes_only
    def slots(attributes):
        return attributes.keys()

    def missing_attribute(obj, attribute):
        raise AttributeError('object %s has no attribute "%s"' % (obj('label'), attribute))

    @slot(attributes)
    def copy(obj):
        return cp.deepcopy(obj)

    @slot(attributes)
    def inherit(obj):
        new_obj = meta_object('clone')
        new_obj('ancestors=', [obj])
        new_obj('type=')
        return


    def ancestors_iterator():
        ancestors = attributes.get('ancestors', [])
        visited = set()

        while ancestors:
            ancestor = ancestors.pop()
            if ancestor in visited:
                break
            else:
                visited.add(ancestor)
                yield ancestor
                ancestors.extend(ancestor.get('ancestors', []))

    def lookup_ancestors(attribute):
        for ancestor in ancestors_iterator():
            method = ancestor('getSlot')
            if method:
                return method
        else:
            raise KeyError

    def lookup(attribute):
        try:
            return attributes[attribute]
        except KeyError:
            return lookup_ancestors(attribute)

    def do(command, *args):
        if command.endswith('='):
            attribute = command[:-1]
            attributes[attribute] = args[0]
        elif command.endswith('!'):
            attribute = command[:-1]
            try:
                del attributes[attribute]
            except KeyError:
                missing_attribute(do, attribute)
        else:
            try:
                attribute = lookup(command)
            except KeyError:
                missing_attribute(do, command)
            else:
                if callable(attribute):
                    return attribute(attributes, *args)
                else:
                    return attribute
    return do

base_object = meta_object()

def test():
    '''
    >>> person=meta_object()
    >>> person('name=', 'Mark')
    >>> person('surname=', 'Twain')
    >>> print person('name'), person('surname')
    Mark Twain
    >>> sorted(person('slots'))
    ['name', 'surname']
    >>> print person('getSlot', 'foo')
    None
    >>> person('name!')
    >>> print person('name')
    Traceback (most recent call last):
        ...
    AttributeError: ...

    '''


if __name__ == '__main__':
    import doctest
    doctest.testmod(optionflags=doctest.IGNORE_EXCEPTION_DETAIL|doctest.ELLIPSIS)
