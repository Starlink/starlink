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
ccdsetup bounds='[1,5,323,349]' extent='[4,318,3,510]' logto=terminal
         logfile=ccdpack.log preserve=true genvar=false reset accept
#
# DEBIAS all the frames. Note using interpolation between the bias strips.
#
debias in='"data1,data2,data3,ff1,ff2,ff3"' out='*-db' accept
#
# Combine the flatfields using known exposures and avoiding
# the MAKEFLAT cleaning process. Normalize it to have a mean of 1.
#
makecal in='"ff1,ff2,ff3"' expose='"600,900,700"' out=master_tmp
kappa
set mean=`stats master_tmp | grep mean | awk '{print $4}'`
cdiv in=master_tmp scalar=$mean out=master_flat
#
# Flat field all target frames.
#
flatcor in='data[1-3]-db' out='*-fl' flat=master_flat accept
#
