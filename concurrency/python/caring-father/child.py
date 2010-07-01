#!/usr/bin/env python

import sys
import random
import time

print "My name is %s, %s" % (sys.argv[1], time.time())


max_value = 4.0
should_repeat = random.uniform(0, max_value) > 0.5

while should_repeat: 
    wait_for = random.gauss(0.5, 0.2)
    time.sleep(wait_for)
    print "[%s] I waited for %s seconds." % (time.time(), wait_for)

    max_value /= 2.0
    should_repeat = random.uniform(0, max_value) > 0.5