#  The following script shows how you might reduce you data if you want
#  to deglitch and generate errors if your data is already debiassed.
#
#  Clear any existing setup.
#
ccdclear reset accept
#
#  Convert the glitch file into an ARD file.
#
$CCDPACK_DIR/glitch2ard GLITCH.LIST glitch.ard
#
# Debias all the frames using a 0 contribution.
#
debias in='"data*,dark*"' usecon=true zero=0 out='*_db' genvar=false \
       mask=glitch.ard reset accept
#
# Dark subtraction. Note all dark frames and data frames have the same
# exposures
#
makecal in='dark*-db' out=master_dark expose=1 reset accept
calcor in='data*_db' cal=master_dark expose=1 out='*_dk'  reset accept
#
# Median filter of the debias&dark corrected frames to produce the
# flatfield.
#
makeflat in='*_dk' method=median out=master_flat reset accept
#
# Now flatfield all frames.
#
flatcor in='*_dk' flat=master_flat out='*-fl' reset accept
#
