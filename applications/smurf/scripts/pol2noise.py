#!/usr/bin/env python3

'''
*+
*  Name:
*     POL2NOISE

*  Purpose:
*     Analyse the noise in a POL2 vector catalogue

*  Language:
*     python (2.7 or 3.*)

*  Description:
*     This script manipulates the noise estimates contained within a POL2
*     vector catalogue. Currently two options are available, selected by
*     parameter MODE:
*
*     - "Display" - this displays the noise values stored in a specified
*     column of the catalogue (e.g. "DQ") and compares them to the noise
*     values estimated from the variations of the corresponding value
*     column (e.g. "Q"). This allows the noise values ("DQ" etc) to be
*     verified.
*
*     - "Remodel" - This replaces all the noise values stored in a specified
*     catalogue with values based on smoother models of the background and
*     source noise. This helps to remove outlying noise estimates that are
*     anomalously low or high because of the random nature of the MAPVAR
*     errors created by the pol2map script.

*  Display Mode:
*     The "display" mode compares the error estimates stored in the error
*     columns of the catalogue (e.g. the values in the DI column) with
*     estimates of the errors derived from the value columns (e.g. the
*     I column). This comparison is limited to regions where the true
*     astronomical signal can be assumed to be zero (i.e. the background
*     regions). This mode can only be used with the intensity-like values
*     in the catalogue (e.g. I, Q, U and PI).
*
*     Two different methods are used for comparing the noise values.
*     Ideally they should both show that the error columns stored in the
*     catalogue accurately describe the noise in the data columns.
*     However, in reality they will both give reports that depart from
*     this ideal by differing amounts. Results for both methods are
*     displayed on the graphics device specified by parameter DEVICE.
*     The results from "Method 1" are displayed in the lower row of three
*     pictures on the graphics device, and the results from "Method 2"
*     are displayed in the upper row of three pictures. For convenience,
*     the scatter plot produced by method 1 (top right picture) is
*     overlayed in blue on top of the scatter plot produced by method 1
*     (lower right plot).
*
*     Method 1:
*
*     Firstly, a mask is created that is later used to identify background
*     regions. This is based on the total intensity, I, regardless of the
*     column being checked, since I is usually much brighter than Q, U or
*     PI. The values from catalogue columns "I" and "DI" are extracted into
*     a pair of 2-dimensional maps. A basic SNR map is then formed (I/DI)
*     and the FellWalker algorithm within the Starlink CUPID package (see
*     SUN/255) is used to identify contiguous clumps of pixels with SNR
*     higher than 5 and to extend each such clump down to an SNR of 2.

*     Next, the values from the requested catalogue columns, "<X>" and "D<X>",
*     are extracted into a pair of 2-dimensional maps and masked to remove
*     source regions (i.e. the clumps of high SNR identified by FellWalker).
*
*     The full range of "D<X>" values in the remaining background is divided
*     into a set of bins, and each "<X>" value is then placed into a bin on
*     the basis of its corresponding "D<X>" value. The "<X>" values in each
*     bin should in principle have a mean value of zero since they are
*     all background pixels. The standard deviation of the "<X>" values
*     in each bin is found and plotted against the "D<X>" value associated
*     with the bin (which varies across the map, being larger nearer the
*     edges). Ideally the resulting best fit line should have a slope of
*     unity and an offset of zero, indicating that the noise estimate
*     "D<X>" associated with each bin is a good measure of the standard
*     deviation of the "<X>" values in the bin.
*
*     The masked "<X>" and "D<X>" maps are displayed using a common scaling
*     on the graphics device specified by parameter DEVICE. A scatter plot
*     showing the standard deviation of the "<X>" values in each bin on
*     the vertical axis and the RMS "D<X>" value in each bin on the
*     horizontal axis is also displayed. The slope and offset of the best
*     fitting straight line are displayed on standard output, together with
*     the RMS residual of the fit. The upper data limit included in the
*     scatter plot is displayed as a red contour on the two map.
*
*     The "D<X>" map may optionally be smoothed using a Gaussian kernel
*     before being used - see parameter PRESMOOTH.
*
*     The size of each "D<X>" bin and the data included in the scatter
*     plot can make a significant difference to the final slope and
*     offset. The first bin (lowest D<X>) is centred on the peak of the
*     D<X> histogram. This histogram is usually heavily skewed with a very
*     rapid rise at low D<X> values followed by a long tail to higher D<X>
*     values. The bin width is 1.5 times the FWHM of the histogram peak,
*     as determined solely from the D<X> values below the peak. All bins
*     have equal width, and the highest bin includes the D<X> value
*     corresponding to the value of parameter PERC. Any points below the
*     first bin or above the last bin are excluded from the scatter plot.
*     This binning scheme aims to reduce statistical bias at the low D<X>
*     end, which tends to cause the lowest D<X> points in the scatter plot
*     to be slightly higher than they should be. This bias is caused by
*     there being few points at lower D<X> to balance those with higher
*     D<X> value.
*
*     Method 2:
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

*  Remodel Mode:
*     The "remodel" mode creates an output catalogue holding a copy of the
*     input catalogue, and then calculates new values for all the error
*     columns in the output catalogue. The new I, Q and U error values
*     are first derived from a three component model of the noise in each
*     quantity:
*
*     The "background component" is derived from the exposure time map
*     (obtained using parameter EXPTIME). The background component is equal
*     to "A*(exptime^B)" where A and B are constants determined by doing a
*     linear fit between the log of the noise estimate in the catalogue (DQ,
*     DU or DI) and the log of the exposure time (in practice, B is usually
*     close to -0.5). The fit is limited to background areas in the signal
*     map, but also excludes a thin rim around the edge of the map where
*     the original noise estimates are subject to large inaccuracies. Since
*     the exposure time map is usually very much smoother than the original
*     noise estimates, the background component is also much smoother.
*
*     The "source component" represents the extra noise found in and around
*     compact sources and caused by pointing errors, calibration errors,
*     etc. The background component is first subtracted from the catalogue
*     noise estimates and the residual noise values are then modelled using
*     a collection of Gaussians. This modeling is done using the GaussClumps
*     algorithm provided by the findclumps command in the Starlink CUPID
*     package. The noise residuals are first divided into a number of
*     "islands", each island being a collection of contiguous pixels with
*     noise residual significantly higher than zero (this is done using
*     the FellWalker algorithm in CUPID). The GaussClumps algorithm is
*     then used to model the noise residuals in each island. The resulting
*     model is smoothed lightly using a Gaussian kernel of FWHM 1.2 pixels.
*
*     The "residual component" represents any noise not accounted for
*     by the other two models. The noise residuals are first found by
*     subtracting the other two components from the original catalogue
*     noise estimates. Any strong outlier values are removed and the
*     results are smoothed more heavily using a Gaussian kernel of FWHM
*     4 pixels.
*
*     The final model is the sum of the above three components. The new
*     DI, DQ and DU values are found independently using the above method.
*     The errors for the derived quantities (DPI, DP and DANG) are then
*     found from DQ, DU and DI using the usual error popagation formulae.
*     Finally new P and PI values are found using a specified form of
*     de-biasing (see parameter DEBIASTYPE).
*
*     The results of the re-modelling are displayed on the graphics
*     device specified by parameter DEVICE. A row of four pictures is
*     created for each Stokes parametyer (I, Q and U). From left to
*     right, these are:
*
*     - An image of the original error estimates in the supplied catalogue.
*     - An image of the re-modelled error estimates in the output catalogue.
*     - An image of the residuals between original and re-modelled error
*       estimates.
*     - A scatter plot of re-modelled against original error estimates.
*
*     The images of the original and re-modelled error estimates use the
*     same scaling. The image of the residuals is scaled between the 2nd
*     and 98th percentiles.

*  Usage:
*     pol2noise cat column [mode] [perc] [presmooth] [style] [device]

*  ADAM Parameters:
*     CAT = LITERAL (Read)
*        The input vector catalogue. This should have been created by
*        POL2MAP.
*     COLUMN = LITERAL (Read)
*        The name of the catalogue column to be used if parameter MODE is
*        "Display". Both the named column and the associated error column
*        ("<X>" and "D<X>") must exist in the catalogue. The name must be
*        one of "Q", "U", "I" or "PI".
*     DEBIASTYPE = LOGICAL (Given)
*        Gives the type of bias estimator to use if paremeter MODE is
*        "Remodel", using the nomeclature of Montier at al "Polarization
*        measurements analysis II. Best estimators of polarization fraction
*        and angle" (A&A, 2018):
*          - "AS": The asymptotic estimator. See section 2.3 of Montier
*             et al. This estimator produces bad P and PI values if the
*             squared PI value is less than the variance in PI.
*          - "MAS": The modified asymptotic estimator. See section 2.5 of
*             Montier et al. This estimator does not produces bad P and PI
*             values, even if the squared PI value is less than the
*             variance in PI.
*          - "None": No de-biasing.
*     DEVICE = DEVICE (Read)
*        The graphics workstation to use. The default is the current
*        graphics device as previously set using KAPPA:GDSET. If GDSET
*        has not been used to establish a current graphics device, the
*        user is prompted for a device. Use KAPPA:GDNAMES to see a list
*        of available device names. []
*     EXPTIME = NDF (Read)
*        An NDF that contains an exposure time map for the data in the
*        supplied catalogue. Only used if parameter MODE is "Remodel". For
*        instance, the "iext", "qext" or "uext" map that was created by
*        POL2MAP at the same time as the catalogue could be supplied.
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
*     MODE = LITERAL (Read)
*        The operation to be performed on the input catalogue specified
*        by parameter CAT:
*
*        - "DISPLAY": Verify the noise estimates on a single quantity
*        by comparing them to the local variations of the quantity. See
*        parameters COLUMN, DEVICE, PER, PRESMOOTH.
*
*        - "REMODEL": Replace the noise estimates in the catalogue using
*        a smoother model. See parameters OUT, EXPTIME, DEBIASTYPE.
*
*        ["Display"]
*     MSG_FILTER = LITERAL (Read)
*        Controls the default level of information reported by Starlink
*        atasks invoked within the executing script. The accepted values
*        are the list defined in SUN/104 ("None", "Quiet", "Normal",
*        "Verbose", etc). ["Normal"]
*     OUT = LITERAL (Read)
*        The output FITS vector catalogue. Only used if parameter MODE is
*        "Remodel".
*     PERC = _REAL (Read)
*        The percentile corresponding to the highest "D<X>" value to include
*        in the scatter plot. Only used if parameter MODE is "Display". In
*        the range 20 to 100. A value below 100 causes the edge pixels, which
*        usually have very high variances, to be excluded from the plot. A
*        red contour is displayed over the "D<X>" map indicating the noise
*        level corresponding to the value of PERC. [90]
*     PRESMOOTH = _REAL (Read)
*        Controls initial smoothing of the "D<X>" map in "Method 2". If a
*        value is supplied for PRESMOOTH, then the "D<X>" map read from the
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
*     1-MAY-2020 (DSB):
*        Completely re-wrote the DISPLAY algorithm, based on the new
*        KAPPA:PIXBIN command.
*     29-MAY-2020 (DSB):
*        - Added "Re-modelling" mode.
*        - Changed "Display" mode to use two different independent methods.
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
   params.append(starutil.ParChoice("MODE", ("DISPLAY","REMODEL"),
                                    "Operation to perform", "DISPLAY",
                                    noprompt=True ))
   params.append(starutil.Par0F("PERC", "Upper percentile for scatter plot",
                                default=90.0, noprompt=True, maxval=100.0,
                                minval=20 ))
   params.append(starutil.Par0F("PRESMOOTH", "FWHM (pixels) for pre-smoothing",
                                default=None, noprompt=True ))
   params.append(starutil.ParGrp("STYLE", "Graphics style parameters",
                                 "def", noprompt=True))
   params.append(starutil.Par0S("DEVICE", "Input vector catalogue",
                                default=None, noprompt=True ))
   params.append(starutil.Par0S("OUT", "The output FITS vector catalogue"))
   params.append(starutil.ParNDG("EXPTIME", "An NDF containing an exposure "
                                 "time map" ))
   params.append(starutil.ParChoice("DEBIASTYPE", ("AS","MAS","NONE"),
                                    "Bias estimator to be used" ))
   params.append(starutil.Par0L("RETAIN", "Retain temporary files?", False,
                                 noprompt=True))

#  Initialise the parameters to hold any values supplied on the command
#  line.
   parsys = ParSys( params )

#  Get the input catalogue
   cat = parsys["CAT"].value

#  Get the operation to perform.
   mode = parsys["MODE"].value

#  See if temp files are to be retained.
   retain = parsys["RETAIN"].value

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



#  First deal with "display" mode.
#  -------------------------------
   if mode == "DISPLAY":

#  Get the column name.
      col = parsys["COLUMN"].value.upper()
#      if col not in ["Q","U","I","PI"]:
#         raise starutil.InvalidParameterError("\nCannot use column '{0}' - "
#                              "must be one of Q, U, I or PI".format(col))

#  Get the upper percentile for the "D<X>" values to include.
      perc = parsys["PERC"].value

#  Get the FWHM to use for any pre-smoothing to be applied to the "D<X>"
#  values read from the catalogue.
      presmooth = parsys["PRESMOOTH"].value

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

#  Warn the user if it looks like this is an old catalogue with no
#  negative I values.
      invoke( "$KAPPA_DIR/stats ndf={0}".format(icat) )
      mini = float( get_task_par( "minimum", "stats" ) )
      if mini >= 0.0:
         msg_out( "\nWARNING: it looks like the supplied catalogue may "
                  "have been created using an old (i.e. before 2nd April "
                  "2020) version of Starlink, causing there to be many "
                  "holes in the background regions of the total intensity "
                  "map. This may cause the results of this script to be "
                  "unreliable.\n" )

#  Get the data units string and pixel size (in arc-sec).
      invoke( "$KAPPA_DIR/ndftrace ndf={0}".format(vcat) )
      units = get_task_par( "units", "ndftrace" )
      pixsize = float( get_task_par( "fpixscale(1)", "ndftrace" ) )

#  Form the basic total intensity SNR map to use when creating the mask.
      msg_out( "Masking the {0} and D{0} maps...".format(col) )
      snr = NDG( 1 )
      invoke( "$KAPPA_DIR/div in1={0} in2={1} out={2}".format(icat,dicat,snr) )

#  Omit everything brighter than +/- 3 sigma.
      snr2 = NDG( 1 )
      invoke( "$KAPPA_DIR/thresh in={0} out={1} thrlo=-3 newlo=bad thrhi=3 "
              "newhi=bad".format(snr,snr2) )

#  Get the clipped standard deviation of whatever is left.
      invoke( "$KAPPA_DIR/stats ndf={0} clip=\[2,2,2\]".format(snr2) )
      sigma = float( get_task_par( "sigma", "stats" ) )

#  Use FellWalker to find islands of significant emission in the total
#  intensity SNR map. Each island must contain one or more pixels with
#  SNR above 5 and contains all neighbouring pixels down to an SNR of 2.
      conf_fw = NDG.tempfile()
      fd = open( conf_fw, "w" )
      fd.write("FellWalker.FlatSlope=0\n")
      fd.write("FellWalker.MinDip=1.0E30\n")
      fd.write("FellWalker.Noise=2*RMS\n")
      fd.write("FellWalker.Fwhmbeam={0}\n".format(15/pixsize))
      fd.write("FellWalker.MinPix={0}\n".format(round(4*((12/pixsize)**2))))
      fd.write("FellWalker.MaxJump=1\n")
      fd.write("FellWalker.MaxBad=0.1\n")
      fd.write("FellWalker.MinHeight=5*RMS\n")
      fd.close()
      islands = NDG( 1 )
      invoke("$CUPID_DIR/findclumps in={0} out={1} method=fellwalker "
             "rms={2} outcat=! config=^{3}".
             format( snr, islands, sigma, conf_fw ))

#  No masking if no clumps were found.
      nclumps = int( get_task_par( "nclumps", "findclumps" ) )
      if nclumps == 0:
         back = vcat
         temp2a = dvcat

#  Otherwise use the inverted clump mask to mask out source regions in the two
#  catalogue maps.
      else:
         back = NDG( 1 )
         invoke( "$KAPPA_DIR/copybad in={0} ref={1} out={2} invert=yes".format(vcat,islands,back) )
         temp2a = NDG( 1 )
         invoke( "$KAPPA_DIR/copybad in={0} ref={1} out={2} invert=yes".format(dvcat,islands,temp2a) )

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

#  The size and nature of D<X> bins we use are important. Since there
#  is random noise on the D<X> values, the very lowest D<X> values
#  will constitute the extreme wing of the distribution centred on the
#  lowest "true" D<X> value. Therefore you would expect the corresponding
#  estimate measured from the spread of <X> values to be greater than the
#  corresponding D<X> value. So if the D<X> bin width is very small the
#  very lowest bins will be biased (the <X>-based estimate being biased
#  upwards from the D<X> value). To avoid this bias at small D<X>, the
#  bin width should be at least equal to the uncertainty in the D<X> value.
#  The only way we have to get an estimate of this is to look at the
#  width of the peak of the D<X> distribution. So find the peak position
#  (the mode of the D<X> histogram) and then get the 32nd percentile of
#  the data values below the mode - the difference between them should be
#  about 1 standard deviation (i.e. about 0.42 of the peak FWHM).
      msg_out( "Binning the {0} and D{0} maps...".format(col) )
      invoke( "$KAPPA_DIR/histat ndf={0} percentiles={1} numbin=1000 "
              "method=moments".format(temp2,perc) )
      mode = float( get_task_par( "mode", "histat" ) )
      percval = float( get_task_par( "perval(1)", "histat" ) )

      temp5 = NDG( 1 )
      invoke( "$KAPPA_DIR/thresh in={0} out={1} thrlo=-1E10 newlo=bad "
              "thrhi={2} newhi=bad".format(temp2,temp5,mode) )

      invoke( "$KAPPA_DIR/histat ndf={0} percentiles=32".format(temp5) )
      pval = float( get_task_par( "perval(1)", "histat" ) )
      fwhm = 2.3548*( mode - pval )
      binwidth = 0.25*fwhm

#  All D<X> bins have the same width (given by binwidth). The lowest bin is centred
#  on the mode. The number of bins is determined by the D<X> value corresponding
#  to parameter PERC. First get the bin number corresponding to the "perc"
#  percentile. Points that have D<X> value higher than this value are ignored.
#  Note bin numbers/indices are zero-based.
      topbin = round( (percval - mode)/binwidth )

#  Get the number of bins
      nbin = topbin + 1

#  Create an NDF that holds the integer bin number assigned to each map
#  pixel.
      index0 = NDG( 1 )
      invoke( "$KAPPA_DIR/maths exp=\"'nint((ia-pa)/pb)'\"  ia={0} "
              "out={1} pa={2} pb={3}".format(temp2,index0,mode,binwidth) )
      invoke( "$KAPPA_DIR/settype ndf={0} type=_integer".format(index0) )

#  Threshold this to assign bad bin numbers to bins below index 0 (we use
#  this image later as a mask for the displayed maps).
      index1 = NDG( 1 )
      invoke( "$KAPPA_DIR/thresh in={0} out={1} thrlo=0 newlo=bad "
              "thrhi=1E8 newhi=bad".format( index0, index1 ) )

#  Threshold it again to assign bad bin numbers to bins above index nbin.
      index = NDG( 1 )
      invoke( "$KAPPA_DIR/thresh in={0} out={1} thrlo=0 newlo=bad "
              "thrhi={2} newhi=bad".format( index1, index, topbin ) )

#  Collect the "<X>" values in each bin. The "<X>" values in each bin are
#  stored in a single column of the 2D output NDF. The first pixel axis
#  in the output NDF corresponds to bin number and goes from 1 to "nbin".
      xbin = NDG( 1 )
      invoke( "$KAPPA_DIR/pixbin in={0} out={1} index={2}".format(back,xbin,index))

#  Collapse the columns to get the sigma-clipped standard deviation of the
#  "<X>" values in each bin, and mask it.
      xsigma = NDG( 1 )
      invoke( "$KAPPA_DIR/collapse in={0} out={1} axis=2 estimator=csigma "
              "wlim=0".format(xbin,xsigma) )

#  Likewise collect the "D<X>" values in each bin.
      dxbin = NDG( 1 )
      invoke( "$KAPPA_DIR/pixbin in={0} out={1} index={2}".format(temp2,dxbin,index))

#  Collapse the columns to get the RMS of the "D<X>" values in each bin,
#  and mask it.
      dxrms = NDG( 1 )
      invoke( "$KAPPA_DIR/collapse in={0} out={1} axis=2 estimator=rms "
              "wlim=0".format(dxbin,dxrms) )

#  Mask the images to be displayed to remove pixels below the lowest bin.
      temp2_disp = NDG( 1 )
      invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".
              format(temp2,temp2_disp,index1))
      back_disp = NDG( 1 )
      invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".
              format(back,back_disp,index1))

#  Create a basic graphics style file to use when displaying images with
#  the kappa display command. We do not need to see the RA and Dec values
#  round the edges.
      msg_out( "Plotting...".format(col) )
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
      invoke( "$KAPPA_DIR/picdef mode=a xpic=3 ypic=2 prefix=a outline=no",
              buffer=True )

#  Display the "D<X>" map in the second picture.
      invoke( "$KAPPA_DIR/picsel label=a2", buffer=True )
      with open( stylefile, "a" ) as fd:
         fd.write( "title=Background D{0} values\n".format(col) )
      invoke( "$KAPPA_DIR/display in={0} mode=perc percentiles=\[1,85\] "
              "style=^{1} badcol=blue4 margin=0.1".format(temp2_disp,stylefile), buffer=True )

#  The perc value is the upper limit of D<X>. Draw a contour at that level.
      stylefile2 = NDG.tempfile()
      with open( stylefile2, "w" ) as fd:
         fd.write("colour(curve)=red\n" )
         if style and style != "def":
            fd.write( "{0}\n".format(style) )
      invoke( "$KAPPA_DIR/contour ndf={0} clear=no key=no mode=free heights={1} "
              "style=^{2}".format(temp2,percval,stylefile2))

#  Display the <X> map in the first picture.
      invoke( "$KAPPA_DIR/picsel label=a1", buffer=True )
      with open( stylefile, "a" ) as fd:
         fd.write( "title=Background {0} values\n".format(col) )
      invoke( "$KAPPA_DIR/display in={0} mode=cur style=^{1} margin=0.1 "
              "badcol=blue4".format(back_disp,stylefile), buffer=True )

#  Draw the perc value contour.
      invoke( "$KAPPA_DIR/contour ndf={0} clear=no key=no mode=free heights={1} "
              "style=^{2}".format(temp2,percval,stylefile2))

#  Display the scatter plot in the third picture. To avoid a tall thin
#  plot, create a square picture inside the third picture.
      invoke( "$KAPPA_DIR/picsel label=a3", buffer=True )
      invoke( "$KAPPA_DIR/picdef mode=cc current=yes aspect=1 outline=no", buffer=True )
      stylefile = NDG.tempfile()
      with open( stylefile, "w" ) as fd:
         fd.write("colour(curve)=red\n" )
         if style and style != "def":
            fd.write( "{0}\n".format(style) )
         fd.write("Label(1)=Std. devn. of {0} values in each bin ({1})\n".format(col,units))
         fd.write("Label(2)=RMS D{0} value in each bin ({1})\n".format(col,units))
         fd.write("Title=Errors in {0}\n".format(col) )
      invoke( "$KAPPA_DIR/scatter in1={0} in2={1} fit=yes xleft=0 ybot=0 "
              "perc1=\[0,100\] perc2=\[0,100\] style=^{2}".
              format( xsigma, dxrms, stylefile ), buffer=True )

      slope = float( get_task_par( "slope", "scatter" ) )
      offset = float( get_task_par( "offset", "scatter" ) )
      rms = float( get_task_par( "rms", "scatter" ) )

      msg_out("\nLinear fit to scatter plot:")
      if offset >= 0:
         msg_out("   Y = {0:.2f}*X + {1:.2f}".format(slope,offset) )
      else:
         msg_out("   Y = {0:.2f}*X - {1:.2f}".format(slope,-offset) )
      msg_out("   RMS residual: {0:.2f} {1}\n".format(rms,units))





#  ------------------------
#  Now do it the other way....

#  Get the clipped stats of whatever is left.
      invoke( "$KAPPA_DIR/stats ndf={0} clip=\[3,3,3\]".format(snr2) )

#  Threshold again, this time at three times the clipped sigma value.
      sigma = float( get_task_par( "sigma", "stats" ) )
      mask = NDG( 1 )
      invoke( "$KAPPA_DIR/thresh in={0} out={1} thrlo={2} newlo=bad thrhi={3} "
              "newhi=bad".format(snr,mask,-sigma,sigma) )

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

#  Produce an image that is zero everywhere except for one central pixel,
#  which has value 1.0
      temp33 = NDG( 1 )
      invoke( "$KAPPA_DIR/creframe like={0} mode=fl mean=0 out={1}".format(back,temp33) )
      temp33a = NDG( 1 )
      invoke( "$KAPPA_DIR/chpix in={0} out={1} section=\"'~1,~1'\" newval=1".format(temp33,temp33a) )

#  Find the RMS using a set of different FWHM values. Remember the FWHM
#  that gives the minimum RMS.
      rms_list = []
      fwhm_list= []
      minrms = 1E30
      iter = 0
      nup = 0
      for fwhm in range( 0, 21 ):
         iter += 1

#  Smooth the squared data values with the current fwhm. Then take the
#  square root to get an RMS map. A correction factor is applied because
#  of the bias caused by finding the RMS of a very low number of values
#  ("v2" is the sum of the squared wieghts used to find the weighted mean of
#  the squared values).
         temp4 = NDG( 1 )
         if fwhm > 0:
            invoke( "$KAPPA_DIR/gausmooth in={0} out={1} fwhm={2}".
                    format(temp3,temp4,fwhm) )

            temp4a = NDG( 1 )
            invoke( "$KAPPA_DIR/gausmooth in={0} out={1} fwhm={2}".
                    format(temp33a,temp4a,fwhm) )
            temp4b = NDG( 1 )
            invoke( "$KAPPA_DIR/mult in1={0} in2={0} out={1}".
                    format(temp4a,temp4b) )

            invoke( "$KAPPA_DIR/stats ndf={0}".format(temp4b))
            v2 = float( get_task_par( "total", "stats" ) )
            corfac = 1/math.sqrt( 1.0 - v2 )

         else:
            invoke( "$KAPPA_DIR/ndfcopy in={0} out={1}".format(temp3,temp4) )
            corfac = 1.0

         temp5 = NDG( 1 )
         invoke( "$KAPPA_DIR/maths exp=\"'pa*sqrt(ia)'\" ia={0} out={1} pa={2}".
                 format(temp4,temp5,corfac) )

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

#  Find the fwhm with the smallest RMS. Leave the loop when the RMS has
#  increased 4 times following a minimum.
         if fwhm == 0.0:
            pass
         elif rms < minrms:
            minrms = rms
            minfwhm = fwhm
            nup = 0
         elif nup > 3:
            break
         else:
            nup += 1

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
         else:
            fwhm = minfwhm
         msg_out( "\nBest FWHM is {0:.2f} pixels".format( fwhm ) )

      if fwhm is None:
         raise starutil.InvalidParameterError("\nCannot determine the best FWHM")

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

#  Display the first error estimate map in the first picture.
      invoke( "$KAPPA_DIR/picsel label=a4", buffer=True )
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
      invoke( "$KAPPA_DIR/picsel label=a5", buffer=True )
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
      invoke( "$KAPPA_DIR/picsel label=a6", buffer=True )
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

#  Overlay the second result on the first result.
      invoke( "$KAPPA_DIR/picsel label=a3", buffer=True )
      invoke( "$KAPPA_DIR/scatter in1={0} in2={1} fit=yes xleft=0 ybot=0 "
              "xright={2} ytop={3} style=\"'^{4},colour(markers)=blue,colour(curves)=green'\" clear=no".
              format( temp5, temp2, xright, ytop, stylefile ), buffer=True )




#  Now deal with "remodel" mode.
#  -----------------------------
   else:

#  Get the NDF containing the exposure time map. Report an error if the
#  exp_time map is not present in the SMURF extension of the NDF.
      exptime0 = parsys["EXPTIME"].value
      try:
         invoke( "$KAPPA_DIR/ndfecho ndf={0}.more.smurf.exp_time".
                 format(exptime0) )
         exptime = None
      except starutil.AtaskError:
         raise starutil.InvalidParameterError( "\nNo exp_time map found in "
                                               "{0}".format(exptime0))

#  Get the output catalogue
      outcat = parsys["OUT"].value

#  Get the type of de-biasing
      debiastype = parsys["DEBIASTYPE"].value

#  Set the graphics device, clear it and divide its area into 12 pictures.
      invoke( "$KAPPA_DIR/gdset device={0}".format(device), buffer=True )
      invoke( "$KAPPA_DIR/gdclear", buffer=True )
      invoke( "$KAPPA_DIR/picdef mode=a xpic=4 ypic=3 prefix=a outline=no",
              buffer=True )
      apic = 0

#  Each pass through the following loop generates a new catalogue based
#  on the contents of a previous catalogue. Initially, the "previous
#  catalogue" is the supplied catalogue.
      prevcat = cat

#  Remodel the I, Q and U noise estimates in turn.
      for iqu in ("I", "Q", "U"):
         msg_out( "\nForming new D{0} values...".format(iqu) )


#  First form the background component of the remodeled errors
#  -----------------------------------------------------------
         msg_out( "   Forming background component..." )

#  Extract the current Stokes parameter and the error on the current Stokes
#  parameter from the input catalogue, creating a pair of 2D NDFs.
         vcat = NDG( 1 )
         invoke( "$POLPACK_DIR/polimage in={0} out={1} coldat={2} box=1".
                 format( cat, vcat, iqu ) )
         dvcat = NDG( 1 )
         invoke( "$POLPACK_DIR/polimage in={0} out={1} coldat=D{2} box=1".
                 format( cat, dvcat, iqu ) )

#  Get the data units string and pixel size (in arc-sec).
         invoke( "$KAPPA_DIR/ndftrace ndf={0}".format(dvcat) )
         units = get_task_par( "units", "ndftrace" )
         pixsize = float( get_task_par( "fpixscale(1)", "ndftrace" ) )

#  If this is the first Stokes parameter, align the exposure time map
#  with the catalogue map (since it may use a different pixel size).
         if exptime is None:
            exptime = NDG( 1 )
            invoke( "$KAPPA_DIR/wcsalign in={0}.more.smurf.exp_time out={1} "
                    "ref={2} rebin=yes method=bilin accept".
                    format( exptime0, exptime, vcat ) )

            try:
               junk = NDG( exptime, "*" )
            except starutil.NoNdfError:
               raise starutil.InvalidParameterError( "\nCannot align {0} "
                                     "with the supplied catalogue {1}. "
                                     "Are they for the same field?".
                                     format(exptime0,cat))

#  Set pixels bad in the exp time map if they are less than 5% of the mean
#  exposure time. This clips off a thin rim round the edge of the map.
            invoke( "$KAPPA_DIR/stats ndf={0}".format(exptime) )
            mean = float( get_task_par( "mean", "stats" ) )
            expmask = NDG( 1 )
            invoke( "$KAPPA_DIR/thresh in={0} out={1} thrlo={2} newlo=bad "
                    "thrhi=1E30 newhi=bad".format( exptime, expmask,
                                                   0.05*mean ) )

#  Form the Stokes parameter SNR map.
         snr = NDG( 1 )
         invoke( "$KAPPA_DIR/div in1={0} in2={1} out={2}".format( vcat, dvcat,
                                                                  snr ) )

#  We want a reasonable estimate of the standard deviation in the background
#  regions of the SNR map. In principle the standard deviation should be
#  1.0, but the noise estimate may be wrong so we need to calculate it from
#  the spread of SNR values. Bright sources will bias the answer so first
#  remove values about snr=60.
         snr_cut = NDG( 1 )
         invoke( "$KAPPA_DIR/thresh in={0} out={1} thrlo=-1E10 newlo=bad "
                 "thrhi=60 newhi=bad".format( snr, snr_cut ) )

#  Now get the stats of the remaining values, doing a single 3-sigma clip to
#  remove outliers.
         invoke( "$KAPPA_DIR/stats ndf={0} clip=3".format(snr_cut) )
         sigma = float( get_task_par( "sigma", "stats" ) )

#  Now fill holes in the original SNR map (mainly the blanks map corners)
#  with the value zero, including believable noise. This helps ffclean to
#  work properly round the map edges.
         snr_filled = NDG( 1 )
         invoke( "$KAPPA_DIR/nomagic in={0} out={1} repval=0 sigma={2}".
                 format( snr, snr_filled, sigma ) )

#  Identify compact features that are significantly higher than the noise
#  in the filled SNR map and set them bad.
         snr_cleaned = NDG( 1 )
         invoke( "$KAPPA_DIR/ffclean in={0} out={1} box=7 clip=\[2,2,2\]".
                 format( snr_filled, snr_cleaned ) )

#  Expand the blank areas a bit (the degree of expansion depends on the
#  box and wlim values). This creates a mask that blanks out source
#  regions.
         mask0 = NDG( 1 )
         invoke( "$KAPPA_DIR/block in={0} out={1} box=3 wlim=0.8".
                 format( snr_cleaned, mask0 ) )

#  Combine this mask with the mask that blanks out the bad rim of the map.
         mask = NDG( 1 )
         invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".
                 format( mask0, mask, expmask ) )

#  Use this mask to remove unusable areas from the Stokes error map.
         dvcat_masked = NDG( 1 )
         invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".
                 format( dvcat, dvcat_masked, mask ) )

#  Take the log (base 10) of the above masked Stokes error map and the
#  exposure time map.
         logdvcat = NDG( 1 )
         invoke( "$KAPPA_DIR/logar base=10D0 in={0} out={1}".
                 format( dvcat_masked, logdvcat ) )
         logexptime = NDG( 1 )
         invoke( "$KAPPA_DIR/logar base=10D0 in={0} out={1}".
                 format( exptime, logexptime ) )

#  Do a least squares linear fit between the log maps created above, and
#  get the slope and offset of the fit.
         invoke( "$KAPPA_DIR/normalize in1={0} in2={1} out=! "
                 "pcrange=\[10,90\] device=!".format(logdvcat,logexptime))
         slope = float( get_task_par( "slope", "normalize" ) )
         offset = float( get_task_par( "offset", "normalize" ) )

#  Create the basic background component as a function of the exposure
#  time map.
         dvcat_model = NDG( 1 )
         invoke( "$KAPPA_DIR/maths exp=\"'10**(pa*ia+pb)'\" ia={0} "
                 "pa={1} pb={2} out={3}".
                 format(logexptime,slope,offset,dvcat_model))

#  There often seems to be a remaining systematic offset between the model
#  and the original Stokes error values. Possibly caused by us using a model
#  of the form "A*(exptime^B)", which assumes zero offset. So now remove
#  any offset by fitting a quadratic surface to the model residuals then
#  subtracting the fit from the model.
         resid = NDG( 1 )
         invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".
                 format( dvcat_model, dvcat_masked, resid ) )
         quad = NDG( 1 )
         invoke( "$KAPPA_DIR/surfit in={0} out={1} fittype=poly "
                 "estimator=median order=2 fitclip=\[1,2,3\]".
                 format( resid, quad ) )
         background_comp = NDG( 1 )
         invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".
                 format( dvcat_model, quad, background_comp ) )


#  Now form the source component of the remodeled errors
#  -----------------------------------------------------
         msg_out( "   Forming source component..." )

#  Get an NDF section describing the bounds of the whole NDF.
         invoke( "$KAPPA_DIR/ndftrace ndf={0}".format(dvcat) )
         xlo = get_task_par( "lbound(1)", "ndftrace" )
         xhi = get_task_par( "ubound(1)", "ndftrace" )
         ylo = get_task_par( "lbound(2)", "ndftrace" )
         yhi = get_task_par( "ubound(2)", "ndftrace" )
         sec = "{0}:{1},{2}:{3}".format(xlo,xhi,ylo,yhi)

#  Get the residual noise values left after subtracting the background
#  component from the original noise values, and convert them into
#  relative values (i.e. fractions of the background component value).
#  Mask using the exposure time mask. This removes a thin rim round the
#  edges of the map.
         resids = NDG( 1 )
         invoke( "$KAPPA_DIR/maths exp=\"'(ia-ib)/ib+0*ic'\" ia={0} "
                 "ib={1} ic={2} out={3}". format( dvcat, background_comp,
                                                  expmask, resids ) )

#  Get an estimate of the background noise in the resids map, first
#  masking it to remove the source regions.
         resids2 = NDG(1)
         invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".
                 format(resids,resids2,dvcat_masked) )
         invoke( "$KAPPA_DIR/stats ndf={0} clip=\[2,2,2\]".format(resids2) )
         sigma = float( get_task_par( "sigma", "stats" ) )

#  Remove all background pixels from the original resids map produced by
#  maths above (i.e. before the sources were masked out).
         resids3 = NDG(1)
         invoke( "$KAPPA_DIR/copybad in={0} out={1} invert=yes ref={2}".
                 format(resids,resids3,mask0) )

#  Use FellWalker to find islands of significant emission in the resids
#  map.
         conf_fw = NDG.tempfile()
         fd = open( conf_fw, "w" )
         fd.write("FellWalker.FlatSlope=0\n")
         fd.write("FellWalker.MinDip=1.0E30\n")
         fd.write("FellWalker.Noise=2*RMS\n")
         fd.write("FellWalker.MaxJump=1\n")
         fd.write("FellWalker.MaxBad=0.1\n")
         fd.write("FellWalker.MinHeight=4*RMS\n")
         fd.write("FellWalker.Fwhmbeam={0}\n".format(12/pixsize))
         fd.write("FellWalker.MinPix={0}\n".format(round(4*((12/pixsize)**2))))
         fd.close()
         islands = NDG( 1 )
         invoke("$CUPID_DIR/findclumps in={0} out={1} method=fellwalker "
                "rms={2} outcat=! config=^{3}".
                format( resids3, islands, sigma, conf_fw ))

#  See how many islands were found.
         nisland = int( get_task_par( "nclumps", "findclumps" ) )

#  Create a config file to control the GaussClumps algorithm.
         conf_gc = NDG.tempfile()
         fd = open( conf_gc, "w" )
         fd.write( "GaussClumps.allowedge=1\n" )
         fd.write( "GaussClumps.maxbad=1\n" )
         fd.write( "GaussClumps.rfctol=0.001\n" )
         fd.write( "GaussClumps.sa=2\n" )
         fd.write( "GaussClumps.maxfn=200\n" )
         fd.write( "GaussClumps.fwhmbeam=0.4\n")
         fd.write( "GaussClumps.thresh=0.5\n" )
         fd.write( "GaussClumps.modellim=0.1\n" )
         fd.close()

#  Loop round each island found by fellwalker
         totgauss = 0
         models = []
         for iisland in range(nisland):

#  First expand the boundary of the island a little.
            island_mask = NDG( 1 )
            invoke("$KAPPA_DIR/gausmooth in={0}.more.cupid.clumps\({1}\).model\({2}\) "
                   "out={3} fwhm=3 wlim=0.1".format( islands, iisland+1, sec,
                                                     island_mask ) )
            island = NDG( 1 )
            invoke("$KAPPA_DIR/copybad in={0} ref={1} out={2}".format( resids,
                                                        island_mask, island ) )

#  Now use GaussClumps to produce a model of the resids value in the
#  expanded island, as the sum of a set of Gaussian blobs.  This may fail
#  so do it in a try block.
            try:
               gcmodel = NDG( 1 )
               invoke("$CUPID_DIR/findclumps in={0} method=gauss rms={1} "
                      "outcat=! out={2} config=^{3}".format( island, sigma,
                                                             gcmodel, conf_gc ))

#  If the model contains at least one Gaussian, ensure the model does not go
#  outside the island, and add the model to the list of island models.
               ngauss = int( get_task_par( "nclumps", "findclumps" ) )
               if ngauss > 0:
                  totgauss += ngauss
                  model = NDG( 1 )
                  invoke("$KAPPA_DIR/copybad in={0} ref={1} out={2}".
                         format( gcmodel, island, model ))
                  models.append( model )

#  If GaussClumps fails, ignore the island.
            except starutil.AtaskError:
               pass

#  Paste the Gaussian models for all islands into a single NDF.
         if len(models) > 0:
            msg_out( "      ({0} Gaussian(s) used to model {1} source(s))".format(totgauss,nisland) )
            if len(models) ==  1:
               allmodels = models[ 0 ]
            else:
               allmodels = NDG( 1 )
               invoke("$KAPPA_DIR/paste in={0} out={1} transp=yes".
                      format( NDG( models ), allmodels ))

#  Fill missing pixels with zero
            allmodels_filled = NDG( 1 )
            invoke("$KAPPA_DIR/nomagic in={0} out={1} repval=0".
                   format( allmodels, allmodels_filled ))

#  Apply some light smoothing.
            allmodels_smoothed = NDG( 1 )
            invoke("$KAPPA_DIR/gausmooth in={0} out={1} fwhm=1.2".
                   format( allmodels_filled, allmodels_smoothed ))

#  Remove the normalisation to get the final source component of the model.
            source_comp = NDG( 1 )
            invoke( "$KAPPA_DIR/mult in1={0} in2={1} out={2}".
                    format( allmodels_smoothed, background_comp, source_comp ))

#  Fill the source model with zeros if no sources were found.
         else:
            msg_out( "      (no sources found)" )
            source_comp = NDG( 1 )
            invoke( "$KAPPA_DIR/cmult in={0} scalar=0 out={1}".
                    format( dvcat, source_comp ))


#  Now form the residual component of the remodeled errors
#  -----------------------------------------------------
         msg_out( "   Forming residual component..." )

#  Subtract the background and source components from the original Stokes
#  error estimatres to get the residuals.
         resids = NDG( 1 )
         invoke( "$KAPPA_DIR/maths exp=\"'ia-ib-ic'\" ia={0} "
                 "ib={1} ic={2} out={3}". format( dvcat, background_comp,
                                                  source_comp, resids ) )

#  Blank out the bad rim round the edge of the map.
         resids_masked = NDG( 1 )
         invoke("$KAPPA_DIR/copybad in={0} ref={1} out={2}".
                format( resids, expmask, resids_masked ))

#  Remove outliers.
         resids_cleaned = NDG( 1 )
         invoke( "$KAPPA_DIR/ffclean in={0} out={1} box=7 clip=\[2,2,2\]".
                 format( resids_masked, resids_cleaned ) )

#  Smooth. This is a slightly heavier smoothing, and wlim is set low so that
#  holes are filled in.
         residual_comp = NDG( 1 )
         invoke("$KAPPA_DIR/gausmooth in={0} out={1} fwhm=4 wlim=1E-6".
                format( resids_cleaned, residual_comp ) )



#  Now form the total model and store in the catalogue column
#  ----------------------------------------------------------
         msg_out( "   Updating catalogue D{0} column...".format(iqu) )

         total_model = NDG( 1 )
         invoke( "$KAPPA_DIR/maths exp=\"'max(0,ia+ib+ic)'\" ia={0} "
                 "ib={1} ic={2} out={3}". format( background_comp, source_comp,
                                               residual_comp, total_model ) )

         newcat = NDG.tempfile( ".FIT" )
         invoke( "$POLPACK_DIR/poledit in={0} out={1} mode=ChangeColVals "
                 "ndf={2} col=D{3}".format(prevcat,newcat,total_model,iqu))

#  The next pass through the I/Q/U loop should add its new values into the
#  catalogue created on this pass.
         prevcat = newcat

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

#  Display the original D<X> map in the first picture.
         apic += 1
         invoke( "$KAPPA_DIR/picsel label=a{0}".format(apic), buffer=True )
         with open( stylefile, "a" ) as fd:
            fd.write( "title=Original D{0} values\n".format(iqu) )
         invoke( "$KAPPA_DIR/display in={0} mode=perc percentiles=\[1,85\] "
                 "style=^{1} badcol=blue4 margin=0.05".format(dvcat,stylefile), buffer=True )

#  Display the re-modelled D<X> map in the second picture.
         apic += 1
         invoke( "$KAPPA_DIR/picsel label=a{0}".format(apic), buffer=True )
         with open( stylefile, "a" ) as fd:
            fd.write( "title=Re-modelled D{0} values\n".format(iqu) )
         invoke( "$KAPPA_DIR/display in={0} mode=cur style=^{1} badcol=blue4 "
                 "margin=0.05".format(total_model,stylefile), buffer=True )

#  Display the D<X> residual map in the third picture.
         diff = NDG(1)
         invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".format(dvcat,total_model,diff) )
         apic += 1
         invoke( "$KAPPA_DIR/picsel label=a{0}".format(apic), buffer=True )
         with open( stylefile, "a" ) as fd:
            fd.write( "title=D{0} residuals\n".format(iqu) )
         invoke( "$KAPPA_DIR/display in={0} mode=cur style=^{1} badcol=blue4 "
                 "margin=0.05 mode=perc percentiles=\[2,98\]".format(diff,stylefile), buffer=True )

#  Display a scatter plot of the before and after D<X> map in the fourth picture.
         apic += 1
         invoke( "$KAPPA_DIR/picsel label=a{0}".format(apic), buffer=True )
         invoke( "$KAPPA_DIR/picdef mode=cc current=yes aspect=1 outline=no", buffer=True )
         stylefile = NDG.tempfile()
         with open( stylefile, "w" ) as fd:
            fd.write("colour(curve)=red\n" )
            if style and style != "def":
               fd.write( "{0}\n".format(style) )
            fd.write("Label(1)=Original D{0} values\n".format(iqu))
            fd.write("Label(2)=Re-modelled D{0} values\n".format(iqu))
            fd.write("Title={0} scatter plot ({1})".format(iqu,units) )
         invoke( "$KAPPA_DIR/scatter in1={0} in2={1} fit=yes xleft=0 ybot=0 "
                 "style=^{2} axes=yes".format( dvcat, total_model, stylefile ), buffer=True )

         slope = float( get_task_par( "slope", "scatter" ) )
         offset = float( get_task_par( "offset", "scatter" ) )
         rms = float( get_task_par( "rms", "scatter" ) )

         msg_out("\nLinear fit to scatter plot:")
         if offset >= 0:
            msg_out("   Y = {0:.2f}*X + {1:.2f}".format(slope,offset) )
         else:
            msg_out("   Y = {0:.2f}*X - {1:.2f}".format(slope,-offset) )
         msg_out("   RMS residual: {0:.2f} {1}\n".format(rms,units))


#  Now remodeled errors have beens stored in the catalogue (newcat) for
#  all three Stokes parameters. Update the other columns in the catalogue
#  and create the output catalogue.
      msg_out( "\nUpdating other catalogue columns..." )
      invoke( "$POLPACK_DIR/poledit in={0} out={1} mode=recalc "
              "debiastype={2}".format(newcat,outcat,debiastype))

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

