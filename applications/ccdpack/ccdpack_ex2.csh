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
#   Restore the general and CCD configuration.
#
ccdsetup restore=true restorefile=CCDPACK_SETUP.DAT reset accept
#
#   Present the data to CCDPACK.
#
present target='DATAV*' bias='BIAS*' flat='FFV*' onefilter=true \
        filter=V reset accept
present target='DATAR*' flat='FFR*' onefilter=true filter=R \
        reset accept
#
#   Schedule and run the reduction.
#
schedule in='"DATA*,BIAS*,FF*"' execute=true debias=1 spacesave=some \
         reset accept
exit
