#
# makefile for Polrap
#
# Sandy Leggett JAC 14th September 1994
# based on one by Phil Daly, JAC, 1 Aug 1994
# version for CAA development directory, 4 Nov 1994
#

# Macro definitions
LIB1    = -L/home/caa/clred/lib_a -lpolrap -lobsrap -lrapi2d -lplt2d
LIB2    = -L/home/caa/clred/lib_a -laspker -latlib -laif -ltrace
LIB3    = -L/home/caa/clred/lib_a -lrapsub -lrapgen -lkersub -lkergen
NAGLIB  = -L/star/nag/nagfl15df    -lnag
ALINK   = /home/bkm/solstar/bin/alink

# Make the executable
polrap: polrap.o  
	$(ALINK) polrap.o  $(LIB1) $(LIB2) $(LIB3) $(NAGLIB) \
	`ndf_link_adam` `fio_link_adam`	
