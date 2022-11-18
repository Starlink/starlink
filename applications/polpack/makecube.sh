#!/bin/sh

#+
#  Name:
#     makecube

#  Purpose:
#     Convert three 2d NDFs holding I, Q and U values into a 3d POLPACK
#     cube holding Stokes vectors.

#  Language:
#     Bourne shell

#  Invocation:
#     makecube i q u cube angrot

#  Description:
#     This stacks the three input 2-dimensional NDFs holding I, Q and U
#     values into a single 3-dimensional NDF which can be processed by
#     POLPACK application POLVEC. The input NDFs must all be aligned
#     pixel-for-pixel.

#  Arguments:
#     i
#        The 2-d input NDF holding the I values.
#     q
#        The 2-d input NDF holding the Q values.
#     u
#        The 2-d input NDF holding the u values.
#     cube ...
#        The 3-d output NDF to create.
#     angrot
#        The anticlockwise angle in degrees from the positive X axis
#        direction, to the reference direction of the Stokes vectors
#        supplied in i, q and u.

#  Prior Requirements:
#     For ease of use, it's recommended that you set up an alias for
#     this script, for example
#        alias makecube 'source /home/bm/scripts/makecube.sh'

#  Authors:
#     DSB: David S. Berry (Starlink)
#     {enter_new_authors_here}

#  History:
#     22-MAR-1999 (DSB):
#        Original version.
#     30-MAR-1999 (DSB):
#        Added angrot parameter.
#     8-JUN-1999 (DSB):
#        Added -h option.
#     19-MAY-2022 (GSB):
#        Convert to sh, use paste shift parameter.
#     {enter_further_changes_here}

#-

#  If the -h option is the first command line argument, run awk on this
#  script to extract those lines comprising the prologue. Use sed to
#  strip off comment characters and pipe the result through more. Then
#  exit.
      if [ $# -gt 0 ]; then
         if [ "$1" = "-h" ]; then

            awk '{if($1=="#-")p=0;if(p)print $0;if($0=="#+"){p=1;print""}}' ${0} \
            | sed -e 's/^#/ /' -e 's/^  //' \
            | more
            exit

         fi
      fi

#  Check that there are 4 arguments present.
      if [ $# -ne 5 ]; then
         echo "Usage: makecube [-h] i q u cube angrot"
         exit
      fi

# Conceal the startup messages.
      . $KAPPA_DIR/kappa.sh > /dev/null

# Take copies of the I, Q and U NDFs, so that the original files
# are left unchanged.
      cp $1.sdf itemp.sdf
      cp $2.sdf qtemp.sdf
      cp $3.sdf utemp.sdf

# Erase an WCS components in these copies. The WCS info will be copied
# back to the final cube. This is necessary because of a bug in
# the NDF library.
      erase itemp.wcs ok
      erase qtemp.wcs ok
      erase utemp.wcs ok

# Paste these three 3d NDFs into a single 3d NDF.
      paste transp noconfine in=itemp p1=qtemp p2=utemp out=$4 shift='[0,0,1]'

# Set up the POLPACK extension in the cube.
      setext $4 xname=polpack xtype=polpack option=put noloop cname=STOKES \
             ctype="_char*3" shape=0 cvalue=IQU

      setext $4 xname=polpack option=put noloop cname=ANGROT ctype="_REAL" \
             shape=0 cvalue=$5

# Set the Label of the cube.
      setlabel $4 "'Stokes parameters (I, Q, U)'"

# Copy the WCS info from the input I image.
      wcscopy $4 "$1(,,1)" noconfirm

# Erase the temporary files.
      rm -f itemp.sdf qtemp.sdf utemp.sdf

      exit
