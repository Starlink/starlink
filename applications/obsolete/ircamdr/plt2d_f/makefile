#
# makefile for Plt2d
#
# Sandy Leggett JAC 14th September 1994
# based on one by Phil Daly, JAC, 1 Aug 1994
# new version in CAA development directory, 4 Nov 1994
#

# Macro definitions
LIB1    = -L/home/caa/clred/lib_a -lplt2d -lgoodies -latlib
ILINK   = /home/bkm/solstar/bin/ilink

# Make the executable
plt2d: plt2d.o  
	$(ILINK) plt2d.o  $(LIB1)  \
	`ndf_link_adam` `sgs_link_adam` `fio_link_adam` `gks_link_adam`	
