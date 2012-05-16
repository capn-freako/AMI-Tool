#! /usr/bin/env python

"""
Python script for generating a PRBS input to AMI models

Original Author: David Banas
Original Date:   May 13, 2012

$Id: part_fract_test.py 89 2012-02-16 16:17:24Z dbanas $

Copyright (c) 2012 David Banas; All rights reserved World wide.
"""

import sys
import random

sample_interval = 25.0e-12
bit_time        = 200.0e-12
stop_time       = 200.0e-9

if __name__ == '__main__':
    n = int(stop_time / sample_interval)
    print "* Created by ami_prbs.py"
    print "Title PRBS Data Pattern"
    print "*,register_length,7"
    print "*,sample_interval,", sample_interval
    print "*,bit_time,", bit_time
    print "*,stop_time,", stop_time
    print "Time,wave_in"
    print "0.0, 0.0"
    print sample_interval, ", 1.0"
    for i in range(2, n):
#                print i * 25.0e-12, ",", random.randrange(2)
        print i * sample_interval, ",", "0.0"

