#  File to define development targets for the reference directory.
#  ==============================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Wed Jan 17 10:36:49 GMT 2001


#  Define new development targets here...


#  Pass any unknown targets on to the release makefile.
.DEFAULT: ; @ $(MAKE) -e -f $(REL_MAKE) $@

#  Keyword for use by RCS.
# $Id$
