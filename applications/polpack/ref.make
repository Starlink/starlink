#  File to define development targets for the reference directory.
#  ==============================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Fri Jul 18 14:17:32 BST 1997


#  Define new development targets here...


#  Pass any unknown targets on to the release makefile.
.DEFAULT: ; @ $(MAKE) -e -f $(REL_MAKE) $@

#  Keyword for use by RCS.
# $Id$
