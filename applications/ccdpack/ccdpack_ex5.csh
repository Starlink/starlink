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
ccdsetup bounds='[1,5,323,349]' extent='[4,318,3,510]' logto=terminal \
         genvar=false reset accept
#
# DEBIAS all the frames. Note using interpolation between the bias strips.
#
debias in='"data*,ff*"' out='*-db' accept
#
# Create the master flat field.
#
makeflat in='ff*-db' out=master_flat accept
#
# Flat field all target frames.
#
flatcor in='data*-db' out='*-fl' accept
#
