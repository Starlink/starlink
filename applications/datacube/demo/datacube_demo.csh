#!/bin/csh
#+

#  Name:
#     daatcube_demo.csh

#  Purpose:
#     Demo script for the datacube package

#  Type of Module:
#     C shell script.

#  Usage:
#     datacube_demo

#  Description:
#     This shell script is a simple demo script to check that everything
#     has installed correctly.

#
#  Authors:
#     AALLAN: Alasdair Allan (Starlink, Keele University)
#     {enter_new_authors_here}

#  History:
#     29-DEC-2000 (AALLAN):
#       Original version.
#     18-OCT-2001 (AALLAN):
#       Modified temporary files to use ${tmpdir}/${user}
#     {enter_changes_here}

#  Copyright:
#     Copyright (C) 2000 Central Laboratory of the Research Councils

#-


# on interrupt
onintr cleanup

# get the user name

set user = `whoami`
set tmpdir = "/tmp"

# do variable initialisation

mkdir "${tmpdir}/${user}"
set colfile = "${tmpdir}/${user}/test_col.sdf"
set ripfile = "${tmpdir}/${user}/test_rip.sdf"
set cubfile = "${DATACUBE_DIR}/smirfsdc.sdf"
set wlifile = "${DATACUBE_DIR}/smirfswl.sdf"
set rspfile = "${DATACUBE_DIR}/smirfsrs.sdf"
set plotdev = "xwin"

# do package setup

alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo


echo "DATACUBE TEST SCRIPT"
echo " "
echo "  Datacube file:"
echo "    ${cubfile}"
echo "  White light image:"
echo "    ${wlifile}"
echo " "

# setup the graphics window

gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}

# setup frames

picdef "mode=cl fraction=[0.5,1.0] device=${plotdev} nooutline"
piclabel device=${plotdev} label="left_plot"

picdef "mode=cr fraction=[0.5,1.0] device=${plotdev} nooutline"
piclabel device=${plotdev} label="right_plot"

# display white light image

echo "  Plotting white light image (LEFT)"

picsel label="left_plot" device=${plotdev}
display ${wlifile:r} device=${plotdev} mode=SIGMA sigmas="[-3,2]" >& /dev/null 

echo "  Collapsing datacube in spectral direction"
collapse in=${cubfile:r} out=${colfile:r} axis=3 >& /dev/null

# display collapsed image

echo "  Plotting collapsed cube (RIGHT)"

picsel label="right_plot" device=${plotdev}
display ${colfile:r} device=${plotdev} mode=SIGMA sigmas="[-3,2]" >& /dev/null 
        
echo " "
echo "  LEFT and RIGHT images should be identical"

sleep 5
echo " "

# setup the graphics window

gdclear device=${plotdev}
gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}

# setup frames

picdef "mode=cl fraction=[0.5,1.0] device=${plotdev} nooutline"
piclabel device=${plotdev} label="left_plot"

picdef "mode=cr fraction=[0.5,1.0] device=${plotdev} nooutline"
piclabel device=${plotdev} label="right_plot"

# plot spectra 

echo "  Plotting spectra from pixel (9,19)"
picsel label="left_plot" device=${plotdev}
linplot "${rspfile:r} device=xwin" 

# rip single spectra from pixel (9,19)

echo "  Extracting spectra from datacube"
ndfcopy "in=${cubfile:r}(9,19,) out=${ripfile:r} trim=true trimwcs=true"
settitle "ndf=${ripfile:r} title='Pixel (9,19) EXTRACTED'"

# display extracted spectra
picsel label="right_plot" device=${plotdev}
linplot "${ripfile:r} device=xwin"
        
echo " "
echo "  LEFT and RIGHT spectra should be identical" 

echo " " 
echo "END"

# clean up
cleanup:
rm -f ${colfile}
rm -f ${ripfile} 
rmdir "${tmpdir}/${user}"
