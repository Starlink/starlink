#!/bin/csh
# Make sure a C shell is used
# Run IRAF from the UNIX shell, taking parameters
# up until the word END.
cl  <<END  >log.file
images
task \$submean = home\$submean.cl
submean
logout
END
