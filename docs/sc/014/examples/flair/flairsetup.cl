# IRAF script to configure tasks for FLAIR data reduction.
#
# A C Davenhall (Edinburgh) 18/12/98.

# Turn on echoing so that the values set are listed.
echo=yes

# fixhead
fixhead.root="flair"

# zerocombine
zerocombine.ccdtype=""

# flatcombine
flatcombine.ccdtype=""
flatcombine.process=no
flatcombine.lsigma=4.0
flatcombine.hsigma=4.0

# ccdproc
ccdproc.ccdtype=""
ccdproc.darkcor=no
ccdproc.flatcor=no
ccdproc.fixfile="flair$badpix.dat"
ccdproc.biassec="[6:15,6:572]"
ccdproc.trimsec="[18:390,1:578]"

# combine
combine.reject="minmax"
combine.scale="exposure"
combine.weight="exposure"
combine.nhigh=1
combine.nlow=0
combine.lsigma=7.0
combine.hsigma=7.0
combine.rdnoise=11

# dofibers
dofibers.readnoise=11
dofibers.fibers=92
dofibers.width=3.0
dofibers.minsep=2.0
dofibers.maxsep=5.0
dofibers.apidtable="apid.txt"
dofibers.splot=no
dofibers.fitflat=no
dofibers.redo=no

# IRAF parameters
params.nsum=25
params.order="increasing"
params.lower=-2.0
params.upper=2.0
params.ylevel=0.1
params.t_step=25
params.t_order=2
params.coordlist="flair$idhgcdne.dat"
params.reject="none"

# Turn off echoing.
echo=no
