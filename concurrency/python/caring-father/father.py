import os
import sys
import time
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
        time.sleep(0.2)
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

    events = []
    for proc in procs:
        events.extend(kqueue.control([
            select.kevent(proc.stdout.fileno(),
                          select.KQ_FILTER_READ)], 10))
        events.extend(kqueue.control([
            select.kevent(proc.pid, select.KQ_FILTER_PROC)], 10))

    while events:
        event = events.pop()
        assert isinstance(event, select.kevent)
        if event.flags & select.KQ_EV_ERROR:
            print os.strerror(event.data)
            continue
        proc_index = descriptors.index(event.ident)
        proc = procs[proc_index]
        if (event.flags & select.KQ_FILTER_READ) and (event.data > 0):
            print '=' * 80
            print 'PROCESS %s (read %d bytes)' % (proc.pid, event.data)
            print '-' * 80
            print os.read(event.ident, event.data)
            print '=' * 80
        if event.flags & select.KQ_EV_EOF:
            print "Closing descriptor!"
            proc.stdout.close()
        events.extend(kqueue.control(None, 10))



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