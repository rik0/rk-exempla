import os
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
        if event | select.POLLIN == event:
            print 'Process %s says: %s' % (proc.pid, proc.stdout.readline()),
        if event | select.POLLHUP == event:
            poll.unregister(fd)
            print 'Process %s quit with %s' % (proc.pid, proc.poll())
            remaining_processes -= 1