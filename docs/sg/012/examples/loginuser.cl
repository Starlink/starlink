#
# loginuser.cl
#
# Customised IRAF start up file.
#
# Author:
#  ACD: A C Davenhall (Edinburgh)
#
# History:
#  17/5/98 (ACD): Original version.
#

#
# Specify that bulk data pixel files are to be kept in subdirectory
# 'pixels'.

set imdir=HDR$pixels/

#
# Set parameter min_lenuserarea to a value which can handle the large
# headers associated with WYFFOS/AUTOFIB2 and FLAIR II data.  If the
# parameter is not set to an appropriate value then some of the header
# information will be lost and erroneous results will ensue.

set min_lenuserarea=300000

#
# The last statement must be keep.

keep
