#!/usr/bin/env python3

'''
*+
*  Name:
*     RAWREGION

*  Purpose:
*     Get a Region describing the approximate field of an observation

*  Language:
*     python (2.7 or 3.*)

*  Description:
*     This script creates an AST Circle region describing the approximate
*     field on the sky associated with a supplied raw data file. The
*     field is determined form the following FITS headers which should be
*     present in the supplied NDF: MAP_HGHT, MAP_WDTH, BASEC1, BASEC2,
*     TRACKSYS (currently only "J2000" and "GAL" tracking systems are
*     supported).

*  Usage:
*     rawregion in region [tiles] [instrument] [retain]

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF. This should have the FITS header
*     INSTRUMENT = LITERAL (Read)
*        Only used if parameter TILES is TRUE. Selects the tiling scheme to
*        be used. The following instrument names are recognised (unambiguous
*        abbreviations may be supplied): "SCUBA-2(450)", "SCUBA-2(850)",
*        "ACSIS", "DAS". If the input NDF contains JCMT data, the default
*        value for this parameter is determined from the FITS headers in the
*        input NDF. Otherwise, there is no default and an explicit value must
*        be supplied. []
*     REGION = LITERAL (Read)
*        Specifies the file in which to return the Region. If null (!) is
*        supplied, the Circle is written to standard output.
*     RETAIN = _LOGICAL (Read)
*        Should the temporary directory containing the intermediate files
*        created by this script be retained? If not, it will be deleted
*        before the script exits. If retained, a message will be
*        displayed at the end specifying the path to the directory. [FALSE]
*     TILES = _LOGICAL (Read)
*        Display JSA tiles touched by the region? [FALSE]

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     29-FEB-2014 (DSB):
*        Original version.
*-
'''

import math
import starutil
from starutil import invoke
from starutil import NDG
from starutil import Parameter
from starutil import ParSys
from starutil import msg_out

#  Assume for the moment that we will not be retaining temporary files.
retain = 0

#  Do not create a log file by default. Setting parameter GLEVEL=ATASK
#  will cause a logfile to be produced.
starutil.glevel = starutil.NONE

#  A function to clean up before exiting. Delete all temporary NDFs etc,
#  unless the script's RETAIN parameter indicates that they are to be
#  retained. Also delete the script's temporary ADAM directory.
def cleanup():
   global retain
   ParSys.cleanup()
   if retain:
      msg_out( "Retaining temporary files in {0}".format(NDG.tempdir))
   else:
      NDG.cleanup()


#  Catch any exception so that we can always clean up, even if control-C
#  is pressed.
try:

#  Declare the script parameters. Their positions in this list define
#  their expected position on the script command line. They can also be
#  specified by keyword on the command line. No validation of default
#  values or values supplied on the command line is performed until the
#  parameter value is first accessed within the script, at which time the
#  user is prompted for a value if necessary. The parameters "MSG_FILTER",
#  "ILEVEL", "GLEVEL" and "LOGFILE" are added automatically by the ParSys
#  constructor.
   params = []

   params.append(starutil.ParNDG("IN", "The input NDF" ))

   params.append(starutil.Par0S("REGION", "Output text file", default=None))

   params.append(starutil.Par0L("TILES", "Display JSA tiles?", False,
                                 noprompt=True))

   params.append(starutil.ParChoice("INSTRUMENT",
                                    ["SCUBA-2(450)", "SCUBA-2(850)", "ACSIS",
                                    "DAS"],
                                    "The JCMT instrument", "SCUBA-2(850)"))

   params.append(starutil.Par0L("RETAIN", "Retain temporary files?", False,
                                 noprompt=True))

#  Initialise the parameters to hold any values supplied on the command
#  line.
   parsys = ParSys( params )

#  Get the input NDF.
   indata = parsys["IN"].value

#  Get the file in which to put the Region.
   region = parsys["REGION"].value

#  See if temp files are to be retained.
   retain = parsys["RETAIN"].value

#  See if JSA tile indices are to be displayed.
   tiles = parsys["TILES"].value

#  Get the required FITS headers from the NDF.
   map_hght = float( starutil.get_fits_header( indata, "MAP_HGHT", report=True ) )
   map_wdth = float( starutil.get_fits_header( indata, "MAP_WDTH", report=True ) )
   basec1 = float( starutil.get_fits_header( indata, "BASEC1", report=True ) )
   basec2 = float( starutil.get_fits_header( indata, "BASEC2", report=True ) )
   tracksys = starutil.get_fits_header( indata, "TRACKSYS", report=True )

#  Convert arc-seconds to degrees.
   map_hght /= 3600.0
   map_wdth /= 3600.0

#  Convert degrees to radians.
   map_hght = math.radians( map_hght )
   map_wdth = math.radians( map_wdth )
   basec1 = math.radians( basec1 )
   basec2 = math.radians( basec2 )

#  Get the radius of the map.
   radius  = 0.5*math.sqrt( map_hght*map_hght + map_wdth*map_wdth )

#  Create a Frame describing the coordinate system.
   if tracksys == "GAL":
      sys = "galactic";
   elif tracksys == "J2000":
      sys = "fk5"
   else:
      raise starutil.InvalidParameterError("The TRACKSYS header in {0} is {1} "
                           "- should be GAL or J2000".format(indata,tracksys) )

   frame = NDG.tempfile()
   invoke( "$ATOOLS_DIR/astskyframe \"'system={0}'\" {1}".format(sys,frame) )

#  Create a Circle describing the map.
   if region is None:
      region = NDG.tempfile()
      display = True
   else:
      display = False

   invoke( "$ATOOLS_DIR/astcircle frame={0} form=1 centre=\[{1},{2}\] point={3} "
           "unc=! options=! result={4}".format(frame,basec1,basec2,radius,region) )

   if display:
      f = open( region, "r" )
      print( f.read() )
      f.close()

#  If required, display the indices of the JSA tiles that touch the region.
   if tiles:

#  See if the supplied NDF holds data from a JCMT instrument by looking at the
#  "INSTRUME", "BACKEND" and "FILTER" FITS headers.
      instrument = None
      cval = starutil.get_fits_header( indata, "INSTRUME" )
      if cval == "SCUBA-2":
         cval = starutil.get_fits_header( indata, "FILTER" )

         if cval == "450":
            instrument = "SCUBA-2(450)"

         elif cval == "850":
            instrument = "SCUBA-2(850)"

      else:
         cval = starutil.get_fits_header( indata, "BACKEND" )

         if cval == "ACSIS":
            instrument = "ACSIS"

         elif cval == "DAS":
            instrument = "DAS"

#  If so, set the default for the INSTRUMENT parameter and prevent the
#  user being prompted for a value.
      if instrument is not None:
         parsys["INSTRUMENT"].default = instrument
         parsys["INSTRUMENT"].noprompt = True

#  Get the chosen instrument.
      instrument = parsys["INSTRUMENT"].value
      instrument = starutil.shell_quote( instrument )

#  Get a list of the tiles that overlap the Region.
      invoke( "$SMURF_DIR/jsatilelist in={0} instrument={1} quiet".format(region,instrument) )
      tiles = starutil.get_task_par( "TILES", "jsatilelist" )

#  List them.
      for tile in tiles:
         msg_out( "Tile {0} touches {1}".format(tile, indata))

#  Remove temporary files.
   cleanup()

#  If an StarUtilError of any kind occurred, display the message but hide the
#  python traceback. To see the trace back, uncomment "raise" instead.
except starutil.StarUtilError as err:
#  raise
   print( err )
   cleanup()

# This is to trap control-C etc, so that we can clean up temp files.
except:
   cleanup()
   raise

