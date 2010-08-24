#!/bin/sh

# This is a script that is used for reducing data using the 
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
# The third argument is an ID for the reduced data. All of the results will
# be stored in /data/1/username/id/ and will be accessible from the CANFAR
# login host.
#
# The forth optional argument is a file containing additional parameters
# for the oracdr script. Presently the only option of interest is a 
# new dimmconfig file that will be passed on to the iterative map-maker.
# The following is an example file called "recipepar.txt" that could
# be specified as this third option:
#
# [REDUCE_SCAN]
#
# MAKEMAP_CONFIG = /home/echapin/dimmconfig.lis
#


SCRATCHDIR=/staging/scratch
PERSISTDIR=/staging/persist
export MAKEMAP_CONFIG_DIR=$HOME/$1

if [ ! -d $SCRATCHDIR ]
then
  echo "Scratch directory doesn't exist, creating $SCRATCHDIR" 
  mkdir $SCRATCHDIR
fi

if [ ! -d $PERSISTDIR ]
then
  mkdir $PERSISTDIR
fi

if [ ! -d $PERSISTDIR/$3 ]
then
  mkdir $PERSISTDIR/$3
fi

echo "Job started at" >> $PERSISTDIR/$3/scuba2_map.log
date >> $PERSISTDIR/$3/scuba2_map.log

# rsync config directory from login host, and change to the directory
# with the config files relevant for this reduction.

rsync -az -e ssh --delete canfar.dao.nrc.ca:config /home/$LOGNAME/
cd $1

# launch the pipeline
/stardev/Perl/bin/jsawrapdr --inputs=$2 --id=$3 -persist --outdir=$SCRATCHDIR --transdir=$PERSISTDIR --mode=public --drparameters "-verbose -recpar $HOME/$1/$4" --canfar --cleanup all

echo "Job finished at" >> $PERSISTDIR/$3/scuba2_map.log
date >> $PERSISTDIR/$3/scuba2_map.log


# copy data products back to login host

ssh canfar.dao.nrc.ca "mkdir /data/1/$LOGNAME/$3"

scp $PERSISTDIR/$3/*.fits canfar.dao.nrc.ca:/data/1/$LOGNAME/$3/
scp $PERSISTDIR/$3/*_reduced.sdf canfar.dao.nrc.ca:/data/1/$LOGNAME/$3/
scp $PERSISTDIR/$3/scuba2_map.log canfar.dao.nrc.ca:/data/1/$LOGNAME/$3/
scp $PERSISTDIR/$3/.oracdr*.log canfar.dao.nrc.ca:/data/1/$LOGNAME/$3/
