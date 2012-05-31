#! /usr/bin/env python

"""
Python script for generating an impulse input to AMI models

Original Author: David Banas
Original Date:   May 31, 2012

$Id: part_fract_test.py 89 2012-02-16 16:17:24Z dbanas $

Copyright (c) 2012 David Banas; All rights reserved World wide.
"""

import optparse

def main():
    p = optparse.OptionParser()
    p.add_option('--sample_interval', '-s', default="25.0e-12")
    p.add_option('--bit_time', '-b', default="200.0e-12")
    p.add_option('--num_samples', '-n', default="128")
    p.add_option('--magnitude', '-m', default="0.1")
    options, arguments = p.parse_args()
    
    n               = int(options.num_samples)
    sample_interval = float(options.sample_interval)
    bit_time        = float(options.bit_time)

    stop_time = sample_interval * (n - 1)

    print "* Created by ami_impulse.py"
    print "Title Impulse Data Pattern"
    print "*,register_length,7"
    print "*,sample_interval,", sample_interval
    print "*,bit_time,", bit_time
    print "*,stop_time,", stop_time
    print "Time,wave_in"
    print "0.0, 0.0"
    print sample_interval, ", 0.1"
    for i in range(2, n):
        print i * sample_interval, ",", "0.0"

if __name__ == '__main__':
    main()

