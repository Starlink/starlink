#  File to define development targets for the reference directory.
#  ==============================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Thu Apr 23 14:26:39 BST 1998


#  Define new development targets here...


#  Pass any unknown targets on to the release makefile.
.DEFAULT: ; @ $(MAKE) -e -f $(REL_MAKE) $@

#  Keyword for use by RCS.
# $Id$
