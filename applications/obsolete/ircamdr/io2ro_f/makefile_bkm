#
# makefile for Io2ro
#
# Sandy Leggett JAC 23rd September 1994
# based on one by Phil Daly, JAC, 1 Aug 1994
# version for CAA development directory, 4 Nov 1994
#

# Macro definitions
LIB1    = -L/home/caa/clred/lib_a -lio2ro
ALINK   = /home/bkm/solstar/bin/alink

# Make the executable
io2ro: io2ro.o  
	$(ALINK) io2ro.o $(LIB1)	
