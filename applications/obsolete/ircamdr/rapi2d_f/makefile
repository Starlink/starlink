#
# makefile for Rapi2d
#
# Sandy Leggett JAC 14th September 1994
# based on one by Phil Daly, JAC, 1 Aug 1994
# version for CAA development directory, 4 Nov 1994
#

# Macro definitions
ALINK   = /home/bkm/solstar/bin/alink
LIB1    = -L/home/caa/clred/lib_a -lrapi2d 
LIB2    = -L/home/caa/clred/lib_a -laspker -latlib -laif -ltrace
LIB3    = -L/home/caa/clred/lib_a -lrapsub -lrapgen -lkersub -lkergen
NAGLIB  = -L/star/nag/nagfl15df    -lnag

# Make the executable
rapi2d: rapi2d.o  
	$(ALINK) rapi2d.o  $(LIB1) $(LIB2) $(LIB3) \
	`ndf_link_adam` `fio_link_adam`	$(NAGLIB)
