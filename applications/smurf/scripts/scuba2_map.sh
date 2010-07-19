#!/bin/sh

# This is a script that is used for reducing data using the 
# Canadian Advanced Network for Astrophysical Research (CANFAR)
#
# The first argument for this shell script is the full path to a file
# containing the scuba-2 data files hosted at CADC that are to be processed.
# The format of this file is that expected by dpRetrieve, and is easiest
# to specify using a trailing wildcard expansions, 
#
# e.g.
#         %s8d20100311_00079%
#
# is expanded to retrieve 850um s8d data files, from UT date
# 20100311, observation 79.
#
# The second argument is an ID for the reduced data. All of the results will
# be stored in /data/1/username/id/ and will be accessible from the CANFAR
# login host.
#
# The third optional argument is a file containing additional parameters
# for the oracdr script. Presently the only option of interest is a 
# new dimmconfig file that will be passed on to the iterative map-maker.
# The following is an example file called "recipepar.txt" that could
# be specified as this third option:
#
# [REDUCE_SCAN]
#
# MAKEMAP_CONFIG = /home/echapin/dimmconfig.lis
#


SCRATCHDIR=/staging/$LOGNAME
PERSISTDIR=/data/1/$LOGNAME

if [ ! -d $SCRATCHDIR ]
then
  echo "Scratch directory doesn't exist, creating $SCRATCHDIR" 
  mkdir $SCRATCHDIR
fi


if [ ! -d $PERSISTDIR/$2 ]
then
  mkdir $PERSISTDIR/$2
fi

echo "Job started at" >> $PERSISTDIR/$2/scuba2_map.log
date >> $PERSISTDIR/$2/scuba2_map.log

if [ ! -d $OUTDIR ]
then
  echo "Error! Output directory $OUTDIR does not exist"
else
  /stardev/Perl/bin/jsawrapdr --inputs=$1 --id=$2 -persist --outdir=$SCRATCHDIR --transdir=$PERSISTDIR --mode=public --drparameters "-verbose -recpar $3" --canfar --cleanup all
fi 

echo "Job finished at" >> $PERSISTDIR/$2/scuba2_map.log
date >> $PERSISTDIR/$2/scuba2_map.log

