#!/usr/bin/env python

'''
*+
*  Name:
*     POL2NOISE

*  Purpose:
*     Analyse the noise in a POL2 vector catalogue

*  Language:
*     python (2.7 or 3.*)

*  Description:
*     This script compares two estimates of the noise in a specified
*     catalogue column at each point on the sky, based on the values in a
*     supplied POL2 vector catalogue. If the supplied column name is <X>,
*     the first estimate of the noise on <X> is formed from the local
*     spatial variation in the values in column "<X>". Astronomical
*     sources will cause the local spatial variation to be higher than
*     that caused by noise alone. For this reason the comparison is
*     restricted to areas that contain no significant spatial structures
*     in the total intensity (I) map. The second estimate  of the noise is
*     read from the catalogue column "D<X>" (calculated by POL2MAP using
*     the method selected by the MAPVAR parameter).
*
*     Firstly, the values from catalogue columns "I" and "DI" are
*     extracted into a pair of 2-dimensional maps. A basic SNR map is
*     then formed (I/DI) and significant spatial structures are
*     identified and blanked out using the KAPPA:FFCLEAN command on the
*     SNR map. The SNR map is used here, instead of simply using "I", in
*     order to flatten the noise level across the map, which helps FFLCEAN.
*     Each blanked out region in this mask (i.e. each source area) is then
*     enlarged slightly to remove any remaining nearby low-level source
*     pixels.
*
*     Next, the values from catalogue columns "<X>" and "D<X>" are
*     extracted into a pair of 2-dimensional maps and masked (using the
*     mask described above) to remove source regions.
*
*     The first noise estimate measures the spatial variation in pixel
*     value in the neighbourhood of each pixel in the masked "<X>"
*     map. It is formed by first squaring the masked "<X>" map, then
*     smoothing the squared map using a Gaussian smoothing kernel, then
*     taking the square root of the smoothed map. Thus each pixel value
*     in the final map represents the RMS of the nearby pixels in masked
*     "<X>" map. The FWHM of the Gaussian smoothing kernel is chosen in
*     order to maximise the correlation between the first and second
*     estimates of the noise.
*
*     The "D<X>" map, which holds the second noise estimate, may optionally
*     be smoothed using a Gaussian kernel before being used - see parameter
*     PRESMOOTH.
*
*     The maps holding the two masked noise estimates are displayed
*     using a common scaling on the graphics device specified by
*     parameter DEVICE. A scatter plot of the values in these two maps
*     is also displayed. The slope and offset of the best fitting
*     straight line, based on the visible points in the scatter plot, are
*     displayed on standard output, together with the RMS residual of the
*     fit. The upper data limits to be included in the scatter plot can
*     be controlled by parameter PERC, and are displayed as red contours
*     on the two maps.

*  Usage:
*     pol2noise cat column [perc] [presmooth] [style] [device]

*  ADAM Parameters:
*     CAT = LITERAL (Read)
*        The input vector catralogue. This should have been created by
*        POL2MAP.
*     COLUMN = LITERAL (Read)
*        The name of the catalogue column to be used. Both the named column
*        and the associated error column ("<X>" and "D<X>") must exist in
*        the catalogue. The name must be one of "Q", "U", "I" or "PI".
*     DEVICE = DEVICE (Read)
*        The graphics workstation to use. The default is the current
*        graphics device as previously set using KAPPA:GDSET. If GDSET
*        has not been used to establish a current graphics device, the
*        user is prompted for a device. Use KAPPA:GDNAMES to see a list
*        of available device names. []
*     GLEVEL = LITERAL (Read)
*        Controls the level of information to write to a text log file.
*        Allowed values are as for "ILEVEL". The log file to create is
*        specified via parameter "LOGFILE. Note, the default value is
*        "NONE", which caused no log file to be created. Setting this
*        parameter to another value (e.g. "ATASK") causes the log file to
*        be produced. ["NONE"]
*     ILEVEL = LITERAL (Read)
*        Controls the level of information displayed on the screen by the
*        script. It can take any of the following values (note, these values
*        are purposefully different to the SUN/104 values to avoid confusion
*        in their effects):
*
*        - "NONE": No screen output is created
*
*        - "CRITICAL": Only critical messages are displayed such as warnings.
*
*        - "PROGRESS": Extra messages indicating script progress are also
*        displayed.
*
*        - "ATASK": Extra messages are also displayed describing each atask
*        invocation. Lines starting with ">>>" indicate the command name
*        and parameter values, and subsequent lines hold the screen output
*        generated by the command.
*
*        - "DEBUG": Extra messages are also displayed containing unspecified
*        debugging information.
*
*        ["PROGRESS"]
*     LOGFILE = LITERAL (Read)
*        The name of the log file to create if GLEVEL is not NONE (which
*        it is by default). The default log file name is "pol2noise.log"
*        and is created in the current directory. Any file with the same
*        name is over-written. Any old log file will be closed before the
*        new one is opened. []
*     MSG_FILTER = LITERAL (Read)
*        Controls the default level of information reported by Starlink
*        atasks invoked within the executing script. The accepted values
*        are the list defined in SUN/104 ("None", "Quiet", "Normal",
*        "Verbose", etc). ["Normal"]
*     PERC = _REAL (Read)
*        The percentile corresponding to the highest value to include in
*        the scatter plot. In the range 1 to 100. A value below 100 causes
*        the edge pixels, which usually have very high variances, to be
*        excluded from the plot. Points that are outside the bounds of
*        the plot are not used when calculating the best fitting straight
*        line. Visible points in the plot that are statistical outliers
*        are also excluded. Red contours are displayed over the two maps
*        indicating the noise level corresponding to the value of PERC. [60]
*     PRESMOOTH = _REAL (Read)
*        Controls initial smoothing of the "D<X>" map. If a value is
*        supplied for PRESMOOTH, then the "D<X>" map read from the
*        catalogue is first smoothed using a Gaussian kernel before being
*        used. The value of PRESMOOTH gives the FWHM of the Gaussian
*        kernel, in pixels. If a null (!) value is supplied for PRESMOOTH
*        (which is the default value), the "D<X>" values read from the
*        catalogue are used directly as the second noise estimate, without
*        any smoothing. [!]
*     RETAIN = _LOGICAL (Read)
*        Should the temporary directory containing the intermediate files
*        created by this script be retained? If not, it will be deleted
*        before the script exits. If retained, a message will be
*        displayed at the end specifying the path to the directory. [FALSE]
*     STYLE = LITERAL (Read)
*        A group of attribute settings describing the plotting style to
*        use for the annotated axes, etc.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text
*        file preceded by an up-arrow character "^".  Such text files
*        should contain further comma-separated lists which will be read
*        and interpreted in the same manner.  Attribute settings are
*        applied in the order in which they occur within the list, with
*        later settings overriding any earlier settings given for the
*        same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*
*        where <name> is the name of a plotting attribute, and <value>
*        is the value to assign to the attribute.  Default values will
*        be used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!)---the initial default---is
*        supplied.
*
*        See section "Plotting Attributes" in SUN/95 for a description
*        of the available attributes.  Any unrecognised attributes are
*        ignored (no error is reported). The appearance of the markers in
*        the scatter plot is controlled by the attributes "Colour(Markers)",
*        "Width(Markers)", etc. Likewise the appearance of the best fit
*        line (and the contour lines) is controlled using "Colour(Curves)",
*        "Width(Curves)", etc. [current value]

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory
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
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     25-MAR-2020 (DSB):
*        Original version.
*     3-APR-2020 (DSB):
*        Add parameter STYLE.
*-
'''

import numpy as np
import math
import starutil
from starutil import invoke
from starutil import NDG
from starutil import Parameter
from starutil import ParSys
from starutil import msg_out
from starutil import get_task_par

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
   if retain:
      msg_out( "Retaining temporary files in {0}".format(NDG.tempdir))
   else:
      NDG.cleanup()
   ParSys.cleanup()








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

   params.append(starutil.Par0S("CAT", "Input vector catalogue" ))
   params.append(starutil.Par0S("COLUMN", "Catalogue column" ))
   params.append(starutil.Par0F("PERC", "Upper percentile for scatter plot",
                                default=60.0, noprompt=True ))
   params.append(starutil.Par0F("PRESMOOTH", "FWHM (pixels) for pre-smoothing",
                                default=None, noprompt=True ))
   params.append(starutil.ParGrp("STYLE", "Graphics style parameters",
                                 "def", noprompt=True))
   params.append(starutil.Par0S("DEVICE", "Input vector catalogue",
                                default=None, noprompt=True ))
   params.append(starutil.Par0L("RETAIN", "Retain temporary files?", False,
                                 noprompt=True))

#  Initialise the parameters to hold any values supplied on the command
#  line.
   parsys = ParSys( params )

#  Get the input catalogue
   cat = parsys["CAT"].value

#  Get the column name.
   col = parsys["COLUMN"].value.upper()
   if col not in ["Q","U","I","PI"]:
      raise starutil.InvalidParameterError("Cannot use column '{0}' - "
                           "must be one of Q, U, I or PI".format(col))

#  Get the upper percentile for the scatter plots.
   perc = parsys["PERC"].value

#  Get the FWHM to use for any pre-smoothing to be applied to the "D<X>"
#  values read from the catalogue.
   presmooth = parsys["PRESMOOTH"].value

#  Get the current graphics device.
   device = get_task_par( "graphics_device", "GLOBAL", default=None )

#  If it is defined, use it as the default for the DEVICE parameter, and
#  indicate the user should not be prompted. Otherwise, set "xw" as the
#  default and indicate the user should be prompted.
   if device is not None:
      parsys["DEVICE"].default = device
      parsys["DEVICE"].noprompt = True
   else:
      parsys["DEVICE"].default = "xw"
      parsys["DEVICE"].noprompt = False

#  Get the graphics device to use.
   device = parsys["DEVICE"].value

#  The graphics style.
   style = parsys["STYLE"].value

#  See if temp files are to be retained.
   retain = parsys["RETAIN"].value

#  Get maps of the requested column value and its error from the supplied
#  catalogue.
   msg_out( "\nExtracting columns {0} and D{0} from {1}...".format(col,cat) )
   vcat = NDG( 1 )
   invoke( "$POLPACK_DIR/polimage in={0} out={1} coldat={2} box=1".
           format(cat,vcat,col) )
   dvcat = NDG( 1 )
   invoke( "$POLPACK_DIR/polimage in={0} out={1} coldat=D{2} box=1".
           format(cat,dvcat,col) )

#  Get maps of the I and DI column values from the supplied catalogue.
   if col != "I":
      msg_out( "Extracting columns I and DI from {0}...".format(cat) )
      icat = NDG( 1 )
      invoke( "$POLPACK_DIR/polimage in={0} out={1} coldat=I box=1".
              format(cat,icat) )
      dicat = NDG( 1 )
      invoke( "$POLPACK_DIR/polimage in={0} out={1} coldat=DI box=1".
              format(cat,dicat) )
   else:
      icat = vcat
      dicat = dvcat

#  Get the data units string, and map size in pixels.
   invoke( "$KAPPA_DIR/ndftrace ndf={0}".format(vcat) )
   units = get_task_par( "units", "ndftrace" )

   nx = float( get_task_par( "dims(1)", "ndftrace" ) )
   ny = float( get_task_par( "dims(2)", "ndftrace" ) )
   size = math.sqrt( nx*ny )

#  Form the basic SNR map to use when creating the mask.
   msg_out( "Masking the {0} and D{0} maps...".format(col) )
   snr = NDG( 1 )
   invoke( "$KAPPA_DIR/div in1={0} in2={1} out={2}".format(icat,dicat,snr) )

#  Fill the bad pixels in the corners of the map with zeros.
   snr2 = NDG( 1 )
   invoke( "$KAPPA_DIR/nomagic in={0} out={1} repval=0".format(snr,snr2) )

#  Blank out bright features in the filled SNR map.
   snr3 = NDG( 1 )
   invoke( "$KAPPA_DIR/ffclean in={0} out={1} box=7 clip=\[2,2,2\]".
           format( snr2, snr3 ) )

#  Extend the bad source regions slightly by smoothing with a block filter
#  and restricting each resulting good value to positions where all the
#  surrounding input pixels are good.
   mask = NDG( 1 )
   invoke( "$KAPPA_DIR/block in={0} out={1} box=3 wlim=1.0".
           format( snr3, mask ) )

#  Use this mask to mask out source regions in the two catalogue maps.
   back = NDG( 1 )
   invoke( "$KAPPA_DIR/copybad in={0} ref={1} out={2}".format(vcat,mask,back) )
   temp2a = NDG( 1 )
   invoke( "$KAPPA_DIR/copybad in={0} ref={1} out={2}".format(dvcat,mask,temp2a) )

#  If required, smooth the map holding the second error estimate (i.e.
#  the "D<X>" values read from the catalogue).
   if presmooth is not None:
      msg_out( "Pre-smoothing the D{0} map using a Gaussian with FWHM={1} pixels".
               format( col, presmooth ) )
      temp2b = NDG( 1 )
      invoke( "$KAPPA_DIR/mult in1={0} in2={0} out={1}".format(temp2a,temp2b) )
      temp2c = NDG( 1 )
      invoke( "$KAPPA_DIR/gausmooth in={0} out={1} fwhm={2}".
              format(temp2b,temp2c,presmooth) )
      temp2 = NDG( 1 )
      invoke( "$KAPPA_DIR/maths exp=\"'sqrt(ia)'\" ia={0} out={1}".
              format(temp2c,temp2) )
   else:
      temp2 = temp2a

#  Determine the smoothing width to use when forming the first error
#  estimate from the local data variations. We use the smoothing width
#  that gives the lowest RMS residual to the fit between the two noise
#  estimates.
   msg_out( "Determining smoothing width (FWHM) that gives best fit...\n" )
   msg_out( "# FWHM (pixel)	RMS ({0})".format(units) )

#  First square the masked data values (needed for each fwhm).
   temp3 = NDG( 1 )
   invoke( "$KAPPA_DIR/mult in1={0} in2={0} out={1}".format(back,temp3) )

#  Find the RMS using a set of different FWHM values. Remember the FWHM
#  that gives the minimum RMS.
   rms_list = []
   fwhm_list= []
   minrms = 1E30
   for fwhm in range( 0, 21 ):

#  Smooth the squared data values with the current fwhm. Then take the
#  square root to get an RMS map.
      temp4 = NDG( 1 )
      if fwhm > 0:
         invoke( "$KAPPA_DIR/gausmooth in={0} out={1} fwhm={2}".
                 format(temp3,temp4,fwhm) )
      else:
         invoke( "$KAPPA_DIR/ndfcopy in={0} out={1}".format(temp3,temp4) )

      temp5 = NDG( 1 )
      invoke( "$KAPPA_DIR/maths exp=\"'sqrt(ia)'\" ia={0} out={1}".
              format(temp4,temp5) )

#  Find the best fit line for the scatter plot of temp5 (the RMS of the local
#  variations) against temp2 (the errors read from the catalogue).
      invoke( "$KAPPA_DIR/scatter in1={0} in2={1} perc1=\[0,{2}\] "
              "perc2=\[0,{2}\] fit=yes device=!".format( temp5, temp2, perc ),
              buffer=True )
      rms = float( get_task_par( "rms", "scatter" ) )
      slope = float( get_task_par( "slope", "scatter" ) )
      offset = float( get_task_par( "offset", "scatter" ) )
      msg_out("  {0}		{1:.2f}".format(fwhm,rms) )

#  Form lists of the fwhm and rms values.
      rms_list.append( rms )
      fwhm_list.append( fwhm )

#  Find the fwhm with the smallest RMS. Leave the loop as soon as the RMS
#  starts to increase, so long as we have done at least 3 different values.
      if rms < minrms:
         minrms = rms
         minfwhm = fwhm
      else:
         break

#  Fit a quadratic to three points centred on the lowest RMS, and find
#  the FWHM at the maximum.
   fwhm = None
   if minfwhm == 0:
      fwhm = 0.0
      msg_out( "\nNo smoothing produces best fit (FWHM=0)" )

   elif minfwhm < 20:
      a = np.polyfit( fwhm_list[ minfwhm - 1 : minfwhm + 2 ],
                      rms_list[ minfwhm - 1 : minfwhm + 2 ], 2 )
      b = np.poly1d( a )
      if a[0] > 0.0:
         fwhm = -0.5*a[1]/a[0]
         msg_out( "\nBest FWHM is {0:.2f} pixels".format( fwhm ) )

   if fwhm is None:
      raise starutil.InvalidParameterError("Cannot determine the best FWHM")

#  Form the map of the first error estimate using the optimum fwhm.
   temp4 = NDG( 1 )
   if fwhm > 0:
      invoke( "$KAPPA_DIR/gausmooth in={0} out={1} fwhm={2}".
              format(temp3,temp4,fwhm) )
   else:
      invoke( "$KAPPA_DIR/ndfcopy in={0} out={1}".format(temp3,temp4) )

   temp5 = NDG( 1 )
   invoke( "$KAPPA_DIR/maths exp=\"'sqrt(ia)'\" ia={0} out={1}".
           format(temp4,temp5) )

#  Create a basic graphics style file to use when displaying images with
#  the kappa display command. We do not need to see the RA and Dec values
#  round the edges.
   stylefile = NDG.tempfile()
   with open( stylefile, "w" ) as fd:
      fd.write( "NumLab=0\n" )
      fd.write( "TextLab=0\n" )
      fd.write( "MajTickLen=0\n" )
      fd.write( "MinTickLen=0\n" )

#  Include any user-supplied style first.
      if style and style != "def":
         fd.write( "{0}\n".format(style) )

#  Set the graphics device, clear it and divide its area into three pictures.
   invoke( "$KAPPA_DIR/gdset device={0}".format(device), buffer=True )
   invoke( "$KAPPA_DIR/gdclear", buffer=True )
   invoke( "$KAPPA_DIR/picdef mode=a xpic=3 ypic=1 prefix=a outline=no",
           buffer=True )

#  Display the first error estimate map in the first picture.
   invoke( "$KAPPA_DIR/picsel label=a1", buffer=True )
   with open( stylefile, "a" ) as fd:
      fd.write( "title=Errors from local variations of {0}\n".format(col) )
   invoke( "$KAPPA_DIR/display in={0} mode=perc percentiles=\[2,80\] "
           "style=^{1} margin=0.1 badcol=blue4".format(temp5,stylefile), buffer=True )

#  Calculate the largest temp5 (X) value that will be used in the scatter
#  plot (controlled by PERC), and draw a contour at that height over the map.
   invoke( "$KAPPA_DIR/histat ndf={0} percentiles={1}".format(temp5,perc) )
   xright = get_task_par( "perval(1)", "histat" )
   invoke( "$KAPPA_DIR/contour ndf={0} clear=no key=no mode=free heights={1} "
           "style=\"'colour=red'\"".format(temp5,xright))

#  Display the second error estimate map in the second picture.
   invoke( "$KAPPA_DIR/picsel label=a2", buffer=True )
   with open( stylefile, "a" ) as fd:
      fd.write( "title=Errors from D{0} column\n".format(col) )
   invoke( "$KAPPA_DIR/display in={0} mode=cur style=^{1} margin=0.1".
           format(temp2,stylefile), buffer=True )

#  Calculate the largest temp2 (Y) value that will be used in the scatter
#  plot (controlled by PERC), and draw a contour at that height over the map.
   invoke( "$KAPPA_DIR/histat ndf={0} percentiles={1}".format(temp2,perc) )
   ytop = get_task_par( "perval(1)", "histat" )
   stylefile = NDG.tempfile()
   with open( stylefile, "w" ) as fd:
      fd.write("colour(curve)=red\n" )
      if style and style != "def":
         fd.write( "{0}\n".format(style) )
   invoke( "$KAPPA_DIR/contour ndf={0} clear=no key=no mode=free heights={1} "
           "style=^{2}".format(temp2,ytop,stylefile))

#  Display the scatter plot in the third picture. To avoid a tall thin
#  plot, create a square picture inside the third picture.
   invoke( "$KAPPA_DIR/picsel label=a3", buffer=True )
   invoke( "$KAPPA_DIR/picdef mode=cc current=yes aspect=1 outline=no", buffer=True )
   stylefile = NDG.tempfile()
   with open( stylefile, "w" ) as fd:
      fd.write("colour(curve)=red\n" )
      if style and style != "def":
         fd.write( "{0}\n".format(style) )
      fd.write("Label(1)=Errors from local variations in {0} ({1})\n".format(col,units))
      fd.write("Label(2)=Errors from D{0} column ({1})\n".format(col,units))
      fd.write("Title=Errors in {0}\n".format(col) )
   invoke( "$KAPPA_DIR/scatter in1={0} in2={1} fit=yes xleft=0 ybot=0 "
           "xright={2} ytop={3} style=^{4}".
           format( temp5, temp2, xright, ytop, stylefile ), buffer=True )

   slope = float( get_task_par( "slope", "scatter" ) )
   offset = float( get_task_par( "offset", "scatter" ) )
   rms = float( get_task_par( "rms", "scatter" ) )

   msg_out("\nLinear fit to scatter plot:")
   if offset >= 0:
      msg_out("   Y = {0:.2f}*X + {1:.2f}".format(slope,offset) )
   else:
      msg_out("   Y = {0:.2f}*X - {1:.2f}".format(slope,-offset) )
   msg_out("   RMS residual: {0:.2f} {1}\n".format(rms,units))

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

