# IRAF script to configure tasks for Hydra tutorial.
#
# A C Davenhall (Edinburgh) 26/10/98.

# Turn on echoing so that the values set are listed.
echo=yes

# hydra
hydra.verbose=yes

# dohydra
dohydra.objects="demoobj"
dohydra.apref="demoflat"
dohydra.flat="demoflat"
dohydra.arcs1="demoarc"
dohydra.fibers=12
dohydra.width=4.0
dohydra.minsep=5.0
dohydra.maxsep=7.0
dohydra.apidtable="demoapid1"
dohydra.scattered=yes
dohydra.clean=no
dohydra.splot=yes
dohydra.redo=yes

# Turn off echoing.
echo=no
