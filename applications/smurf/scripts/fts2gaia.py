#!/usr/bin/env python3

'''
*+
*  Name:
*     FTS2GAIA

*  Purpose:
*     Display in-band FTS-2 spectrum in gaia

*  Language:
*     python (2.7 or 3.*)

*  Description:
*     Display those frames of an FTS-2 spectrum which fall within the band-pass.
*     For 850 um, this band is between 11.2 and 12.2 cm^-1 wave numbers, and
*     for 450 um, it is between 22.1 and 23.3 cm^-1 wave numbers.
*     This translates to the peak of the spectrum of the target,
*     found near the middle of the sensor array plus and minus some number of
*     frames corresponding to the desired portion of the band pass.

*  Usage:
*     fts2gaia in

*  ADAM Parameters:
*     IN = NDF (Read)
*        The NDF FTS-2 spectrum file to be displayed.

*  Copyright:
*     Copyright (C) 2013 University of Lethbridge.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MS: Matt Sherwood (ULeth)
*     {enter_new_authors_here}

*  History:
*     2013-07-02 (MS):
*        Original version

*-
'''

import os
import shutil
import sys
import subprocess
import string
import starutil
from starutil import invoke
from starutil import NDG
from starutil import Parameter
from starutil import ParSys
from starutil import msg_out


if len(sys.argv) < 2:
    print("Usage: fts2gaia <NDF_file>")
    sys.exit(0)

indata = sys.argv[1]
# print "indata="+indata
if not os.path.exists(indata):
    print(indata + "file not found!")
    sys.exit(0)

# Determine the band to process the spectrum in
# NOTE: This assumes oracdr_scuba2_450 or oracdr_scuba2_850 has been run
# which sets the ORAC_INSTRUMENT environment variable to 'SCUBA2_450' or 'SCUBA2_850'
# else default to 850 band
band = os.getenv('ORAC_INSTRUMENT')
if band is None:
    print("ORAC_INSTRUMENT environment variable not set!")
    print("This program expects to be run from a shell")
    print("prepared with the command: oracdr_scuba2_<band> <observation-date>")
    print("where band is: 450 or 850")
    print("and observation-date has the form: YYYYMMDD")
    sys.exit(0)


# print "band={0}".format(band)

# Get WNFACT value and nFrames from data file
wnfact = float(starutil.get_fits_header(indata, "WNFACT"))
# print "wnfact={0}".format(wnfact)
nFrames = int(starutil.get_fits_header(indata, "MIRSTOP")) + 1
# print "nFrames={0}".format(nFrames)

# Gather statistics on the central region of the input spectrum
# We are interested in the z position of the maximum pixel value (peak)
instats = invoke("$KAPPA_DIR/stats ndf={0} quiet".format(indata))
maxpos = starutil.get_task_par("MAXPOS", "stats")
maxposz = maxpos[2]
# print "maxposz={0}".format(maxposz)

# Calculate the band pass frames centered on the peak
if band == "SCUBA2_850":
    wnlbound = 11.2
    wnubound = 12.2
else:
    if band == "SCUBA2_450":
        wnlbound = 22.1
        wnubound = 23.3
# print "wnlbound={0}".format(wnlbound)
# print "wnubound={0}".format(wnubound)
bandwidth = wnubound - wnlbound
# print "bandwidth={0}".format(bandwidth)
# Round band width frames to nearest even integer
bandwidthframes = int(round(bandwidth * (1/(wnfact/2))))
if bandwidthframes % 2:
    bandwidthframes += 1
# print "bandwidthframes={0}".format(bandwidthframes)

bandlf = maxposz - (bandwidthframes/2)
# print "bandlf={0}".format(bandlf)
banduf = maxposz + (bandwidthframes/2)
# print "banduf={0}".format(banduf)

# Invoke gaia with the band pass limited NDF section
ndfsection = "\(,,{0}:{1}\)".format(bandlf, banduf)
# print "ndfsection={0}".format(ndfsection)
invoke("$GAIA_DIR/gaia.sh {0}{1} &".format(indata, ndfsection))
