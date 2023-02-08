#!/usr/bin/env python3
#
#
# !!begin
# !!title   shiftaux
# !!author  T.R. Marsh
# !!created 24 July 2007
# !!root    shiftaux
# !!index   shiftaux.py
# !!descr   moves WHT aux port files 
# !!head1   Script to move aux port data out the way
#
# This script has a very specific purpose which is to move aux port
# data out the way before conversion to sdf occurs etc, so that ultracam
# reduction can be applied to it. It uses the pyfits extension of Python
# so you will need that installed to use it at all.
#
# !!head2 Invocation
#
# shiftaux night file1 file2 ...
# 
# !!head2 Arguments
#
# !!table
# !!arg{night}{number of the night in question. Any aux port files will end up in
# a subdirectory of the pwd of form aux/night2}
# !!arg{file1, file2, ...}{Full name of fits files.}
# !!table
#
# !!end

try:
    import pyfits
except ImportError:
    import astropy.io.fits as pyfits
import sys
import os

# get file name
if len(sys.argv) < 3:
    print('usage: night file1 file2 ...')
    exit(1)

# strip off script name, store night number
sys.argv.pop(0)
night = sys.argv.pop(0)
if int(night) < 1 or int(night) > 50:
    print('Night number must lie from 1 to 50')
    exit(1)

for fname in sys.argv:
    hdulist = pyfits.open(fname)
    prihdr  = hdulist[0].header
    
    if prihdr['INSTRUME'] == 'Cass. aux. port':
        pathname = os.path.join('aux', 'night' + night)
        if not os.path.isdir(pathname):
            print('Making ',pathname)
            os.makedirs(pathname)
        os.rename(fname, os.path.join(pathname, fname))
                       
    hdulist.close()

