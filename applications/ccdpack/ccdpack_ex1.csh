#!/bin/csh
#
#   Initialize ccdpack
#
ccdpack
#
#   Clear any existing global parameters.
#
ccdclear reset accept
#
#   Do the general configuration.
#
ccdsetup logto=terminal genvar=false mask=defects.ard reset accept
#
#   Import the FITS headers into CCDPACK.
#
import in='*' table=$CCDPACK_DIR/WHTFLAT.DAT reset accept
#
#   Schedule and run the reduction.
#
schedule in='*' execute=true debias=1 spacesave=some reset accept
exit
