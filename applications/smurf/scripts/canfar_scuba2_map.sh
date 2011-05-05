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

export MAKEMAP_CONFIG_DIR=$HOME/$1
SCRATCHDIR=/staging/$LOGNAME/scratch
PERSISTDIR=/staging/$LOGNAME/persist

# extra paranoid setup of /staging partition since we may need to
# clean up from the last person who made it

sudo /bin/rm -rf /staging/*
sudo chown root:root /staging
sudo mkdir /staging/$LOGNAME
sudo chown $LOGNAME:$LOGNAME /staging/$LOGNAME
chmod +s /staging/$LOGNAME

mkdir $SCRATCHDIR
mkdir $PERSISTDIR
mkdir $PERSISTDIR/$3

echo "Job started at" >> $PERSISTDIR/$3/scuba2_map.log
date >> $PERSISTDIR/$3/scuba2_map.log

# rsync config directory from login host, and change to the directory
# with the config files relevant for this reduction.

rsync -az -e ssh --delete canfar.dao.nrc.ca:$1 /home/$LOGNAME/
cd $1

# launch the pipeline
/stardev/Perl/bin/jsawrapdr --inputs=$2 --id=$3 -persist --outdir=$SCRATCHDIR --transdir=$PERSISTDIR --mode=public --drparameters "-verbose -recpar $HOME/$1/$5" --canfar --cleanup all &> $PERSISTDIR/$3/jsawrapdr.log

echo "Job finished at" >> $PERSISTDIR/$3/scuba2_map.log

date >> $PERSISTDIR/$3/scuba2_map.log

# copy data products to vospace

java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --delete --target=vos://cadc.nrc.ca\!vospace/$4/$3

java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --create --target=vos://cadc.nrc.ca\!vospace/$4/$3

for i in $PERSISTDIR/$3/*.fits; do java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --copy --src=$i --dest=vos://cadc.nrc.ca\!vospace/$4/$3/`basename $i`; done;

for i in $PERSISTDIR/$3/*_reduced.sdf; do java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --copy --src=$i --dest=vos://cadc.nrc.ca\!vospace/$4/$3/`basename $i`; done;

for i in $PERSISTDIR/$3/*.log; do java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --copy --src=$i --dest=vos://cadc.nrc.ca\!vospace/$4/$3/`basename $i`; done;

for i in $PERSISTDIR/$3/.*log; do java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --copy --src=$i --dest=vos://cadc.nrc.ca\!vospace/$4/$3/`basename $i`; done;

for i in $PERSISTDIR/$3/*fmos.sdf; do java -jar ${CADCVOSCLIENT}/lib/cadcVOSClient.jar --copy --src=$i --dest=vos://cadc.nrc.ca\!vospace/$4/$3/`basename $i`; done;
