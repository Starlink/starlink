$!
$! Command procedure to run a CCDPACK reduction sequence from BATCH
$!
$! Note CCDCLEAR and CCDSETUP are not required if CCDBATCH is used to
$! create a command procedure for submission to batch AND CCDCLEAR
$! and CCDSETUP have been run interactively. RESET is used to clear any
$! CURRENT values which are active for these tasks this ensures that
$! the task default behaviour occurs.
$!
$! Clear unwanted global parameters.
$!
$CCDCLEAR
$!
$! Set up new global parameters.
$!
$CCDSETUP BOUNDS=[323,349] RNOISE=10 ADC=1 DIRECTION=X -
         EXTENT=[4,318,3,510] LOGFILE=CCDPACK.LOG PRESERVE=TRUE RESET \
$!
$! Add some explanatory notes
$!
$CCDNOTE "Test run of CCDPACK atasks" \
$!
$! Make the bias frames.
$!
$MAKEBIAS IN=[.BIAS]* OUT=[.BIAS]MASTER_BIAS METHOD=MEAN RESET \
$!
$! DEBIAS all the frames. Note using a bias frame and normalising to
$! the bias strip.
$!
$DEBIAS IN="[.FLATR]*,[.FLATB]*,[.BDATA]*,[.RDATA]*" -
       OUT=*_DEBIAS MASK=[.MASK]MASK BIAS=[.BIAS]MASTER_BIAS RESET \
$!
$! Create the flat fields
$!
$MAKEFLAT IN=[.FLATR]*_DEBIAS OUT=[.FLATR]MASTER_FLAT RESET \
$MAKEFLAT IN=[.FLATB]*_DEBIAS OUT=[.FLATB]MASTER_FLAT RESET \
$!
$! Flat field all the appropriate frames.
$!
$FLATCOR IN=[.RDATA]*_DEBIAS OUT=*|DEBIAS|FLATTENED| -
        FLAT=[.FLATR]MASTER_FLAT RESET \
$FLATCOR IN=[.BDATA]*_DEBIAS OUT=*|DEBIAS|FLATTENED| -
        FLAT=[.FLATB]MASTER_FLAT RESET \
$!
$! All done.
$exit
$! $Id$
