#!/bin/csh
#
#  Initialise ccdpack
#
ccdpack
#
#  Clear any existing global parameters.
#
ccdclear reset accept
#
#  Convert the glitch file into an ARD file.
#
$CCDPACK_DIR/glitch2ard GLITCH.LIST glitch.ard
#
#  Set some global preferences.
#
ccdsetup logto=terminal genvar=true mask=glitch.ard reset accept
#
#  Present the darks, H, J and K data to CCDPACK.
#
present target='"^hframes,^darks"' onefilter filter=h modify \
        adddark onedarktime darktime=1 biasvalue=0 reset accept
present target=^jframes onefilter filter=j modify adddark \
        onedarktime darktime=1 biasvalue=0 reset accept
present target=^kframes onefilter filter=k modify adddark \
        onedarktime darktime=1 biasvalue=0 reset accept
#
#  Now reduce all that data.
#
schedule in='"^hframes,^jframes,^kframes,^darks"' irflats=true \
         execute=true debias=4 spacesave=some reset accept
#
exit
