#
# Command file to run a CCDPACK reduction sequence from a
# C shell background job.
#
# Clear all existing global parameters
#
ccdclear reset accept
#
# Now set the new ones.
#
ccdsetup extent='[4,318,3,510]' logto=terminal genvar=false reset accept
#
# Make the master bias frame.
#
makebias in='bias*' out=master_bias zero=false accept
#
# DEBIAS all the frames. Note using a master bias that is just
# subtracted
#
debias in='"data*,ff*"' out='*-db' offset=false accept
#
# Create the master flat field.
#
makeflat in='ff*-db' out=master_flat accept
#
# Flat field all target frames.
#
flatcor in='data*-db' out='*-fl' accept
#
