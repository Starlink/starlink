#  File to define development targets for the reference directory.
#  ==============================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Tue Nov  7 17:30:50 GMT 1995


#  Define new development targets here...


#  Pass any unknown targets on to the release makefile.
.DEFAULT: ; @ $(MAKE) -e -f $(REL_MAKE) $@

#  Keywords for use by SCCS.
#$Id$
