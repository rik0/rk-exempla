import os
import sys
import subprocess
import select

CHILD_NAME = 'child.py'
CHILD_PATH = os.path.join(os.path.dirname(
    os.path.abspath(__file__)), CHILD_NAME)
CHILD_NO = 3

CHILD_PARAMETERS = [[str(n)] for n in range(CHILD_NO)]

procs = []

for params in CHILD_PARAMETERS:
    args = [CHILD_PATH]
    args.extend(params)
    def preex():
        print "Running %s" % str(params)
    proc = subprocess.Popen(args,
                            stdout = subprocess.PIPE,
                            stderr=subprocess.STDOUT,
                            preexec_fn=preex)
    procs.append(proc)


if sys.platform == 'darwin':
    print 'Select Macros'
    for name in dir(select):
        if name.upper() == name:
            print name, ':', hex(getattr(select, name))

    descriptors = [proc.stdout.fileno() for proc in procs]

    kqueue = select.kqueue()
    kevents = [select.kevent(desc,
                             select.KQ_FILTER_READ,
                             select.KQ_EV_ADD,
                             0) for desc in descriptors]

    events = kqueue.control(kevents, len(kevents))

    while events:
        print '*', events
        for event in events:
            assert isinstance(event, select.kevent)
            if event.flags & select.KQ_EV_ERROR:
                print os.strerror(event.data)
            elif (event.flags & select.KQ_FILTER_READ) and (event.data > 0):
                try:
                    proc_index = descriptors.index(event.ident)
                except ValueError:
                    print os.read(event.ident, event.data)
                else:
                    proc = procs[proc_index]
                    print 'Process %s says: %s' % (proc.pid, proc.stdout.read(event.data))
            if event.flags & select.KQ_EV_EOF:
                os.close(event.ident)
        events = kqueue.control(None, len(kevents), None)

else:
    poll = select.poll()

    descriptors = []
    remaining_processes = len(procs)
    for proc in procs:
        poll.register(proc.stdout)
        descriptors.append(proc.stdout.fileno())

    while remaining_processes:
        poll_list = poll.poll()
        for fd, event in poll_list:
            proc_index = descriptors.index(fd)
            proc = procs[proc_index]
            if event & select.POLLIN:
                print 'Process %s says: %s' % (proc.pid, proc.stdout.readline()),
            if event & select.POLLHUP:
                poll.unregister(fd)
                print 'Process %s quit with %s' % (proc.pid, proc.poll())
                remaining_processes -= 1