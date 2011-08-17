#!/bin/sh

# This is a script that is used for reducing SCUBA-2 data using the 
# Canadian Advanced Network for Astrophysical Research (CANFAR)
#
# The first argument is the path to the directory containing all of
# the configiration options. Normally this would be set to ~/config
# which is rsync'd from the login host (a subdirectory is an
# alternative).
#
# The second argument is the relative path to a file containing the
# raw scuba-2 data files hosted at CADC that are to be processed.
# The format of this file is that expected by dpRetrieve, and is easiest
# to specify using a trailing wildcard expansions, 
#
# e.g.
#         %s8d20100311_00079%
#
# is expanded to retrieve 850um s8d data files, from UT date
# 20100311, observation 79.
#
# The third argument is an ID for the reduced data. All of the results
# will be copied to VOSpace store, which presently is hard-wired to
# vos://cadc.nrc.ca\!vospace/$4 and may accessed through a browser
# at the following URL: http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/vosui
#
# As mentioned above, the forth argument is the username corresponding to
# the VOSpace path to be accessed at CADC for storing the results.
#
# The fifth optional argument is a file containing additional parameters
# for the oracdr script. Presently the only option of interest is a 
# new dimmconfig file that will be passed on to the iterative map-maker.
# The following is an example file called "recipepar.txt" that could
# be specified as this third option:
#
# [REDUCE_SCAN]
#
# MAKEMAP_CONFIG = /home/echapin/dimmconfig.lis
#

CONFIGDIR=$1
INPUTSFILE=$2
JOBID=$3
VOSPACEUSER=$4
ORACPARAM=$5

SCRATCHDIR=$TMPDIR/scratch
PERSISTDIR=$TMPDIR/persist

# setup directories where we do the work

mkdir $SCRATCHDIR
mkdir $SCRATCHDIR/$JOBID
mkdir $PERSISTDIR
mkdir $PERSISTDIR/$JOBID

export ORAC_LOGDIR=$PERSISTDIR/$JOBID

echo ==========================================================================
echo === Starting to process $JOBID ===
echo ==========================================================================

date

echo SCRATCHDIR is $SCRATCHDIR
echo PERSISTDIR is $PERSISTDIR
echo ORAC_LOGDIR is $ORAC_LOGDIR
echo VOSPACEUSER is $VOSPACEUSER

echo --- Rsyncing config files from login host --------------------------------

# rsync config directory from login host to scratch, and change to the
# directory with the config files relevant for this reduction.

rsync -az -e ssh --delete canfar.dao.nrc.ca:$CONFIGDIR $SCRATCHDIR/

cd $SCRATCHDIR/$CONFIGDIR
pwd
ls -l

echo --- Launch pipeline ------------------------------------------------------

# launch the pipeline
/stardev/Perl/bin/jsawrapdr --inputs=$INPUTSFILE --id=$JOBID -persist -log h --outdir=$SCRATCHDIR --transdir=$PERSISTDIR --mode=public --drparameters "-verbose -recpar $ORACPARAM" --canfar --cleanup all &> $PERSISTDIR/$JOBID/jsawrapdr.log


echo --- Copy results to vospace ----------------------------------------------

# copy data products to vospace

java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --delete --target=vos://cadc.nrc.ca\!vospace/$VOSPACEUSER/$JOBID

java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --create --target=vos://cadc.nrc.ca\!vospace/$VOSPACEUSER/$JOBID

for i in $PERSISTDIR/$JOBID/*.html; do java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --copy --src=$i --dest=vos://cadc.nrc.ca\!vospace/$VOSPACEUSER/$JOBID/`basename $i`; done;

for i in $PERSISTDIR/$JOBID/*.png; do java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --copy --src=$i --dest=vos://cadc.nrc.ca\!vospace/$VOSPACEUSER/$JOBID/`basename $i`; done;

for i in $PERSISTDIR/$JOBID/*.fits; do java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --copy --src=$i --dest=vos://cadc.nrc.ca\!vospace/$VOSPACEUSER/$JOBID/`basename $i`; done;


for i in $PERSISTDIR/$JOBID/s*.sdf; do java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --copy --src=$i --dest=vos://cadc.nrc.ca\!vospace/$VOSPACEUSER/$JOBID/`basename $i`; done;

date
echo === Done! ===

