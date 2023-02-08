#!/usr/bin/env python3

'''
*+
*  Name:
*     pol2cat

*  Purpose:
*     Create a catalogue of Q,U and I values from a set of POL-2 data
*     files, and optionally creates a vector plot.

*  Language:
*     python (2.7 or 3.*)

*  Description:
*     This script runs SMURF:CALCQU on the POL-2 data files specified by
*     parameter IN, to create a set of images holding the mean Q and U
*     values in each bolometer. Each pair of Q and U images contains data
*     for a single sub-array from a short period of time over which the
*     position of each bolometer on the sky does not change significantly.
*
*     For each sub-array, estimates of the background Q and U values at
*     each bolometer are made and subtracted form the original Q and U
*     images. Instrumental polarisation is then removed, based on the
*     total intensity values specified by the IMAP parameter. The
*     parameters of the instrumental polarisation model can be specified
*     via the "IP_..." parameters.
*
*     All the Q images are then mosaiced together into a single Q image,
*     using the total intensity map specified by parameter IREF to define
*     the pixel grid. All the U images are mosaiced together in the same
*     way.
*
*     These Q, U and I images are then converted into a set of polarisation
*     vectors using POLPACK:POLVEC, and are stored in the FITS file
*     specified by parameter CAT.
*
*     Optionally, a map of polarised intensity can be produced. See
*     parameter PI.
*
*     Optionally, a vector plot can then be produced from the catalogue.
*     See parameter PLOT. However, it is usually much more versatile and
*     convenient to examine the final catalogue using TOPCAT, or the
*     polarimetry toolbox in GAIA.
*
*     The script produces several intermediate files that are placed in a
*     newly created directory that is normally deleted before the script
*     exits. The files can be retained for debugging purposes if required
*     by running the script with "retain=yes" on the command line. These
*     temporary files are placed in a directory name "NDG_xxxxx", located
*     within the directory specified by environment variable STAR_TEMP.
*     If STAR_TEMP is not defined, they are placed in the system's
*     temporary directory (e.g. "/tmp").

*  Usage:
*     pol2cat in cat iref pi [plot] [snr] [maxlen] [domain] [pixsize]
*             [config] [device] [nsigma] [extcor] [retain] [qui] [hits]
*             [harmonic] [forcefile] [msg_filter] [ilevel] [glevel] [logfile]

*  ADAM Parameters:
*     CAT = LITERAL (Read)
*        The output FITS vector catalogue.
*     CONFIG = LITERAL (Read)
*        The configuration to use when cleaning the raw data. This should
*        be specified in the same way as the CONFIG parameter for
*        SMURF:MAKEMAP. ["^$STARLINK_DIR/share/smurf/dimmconfig.lis"]
*     DEBIAS = LOGICAL (Given)
*        TRUE if a correction for statistical bias is to be made to
*        percentage polarization and polarized intensity. [FALSE]
*     DEVICE = LITERAL (Read)
*        The name of the graphics device on which to produce the vector
*        plot.  [current graphics device]
*     DOMAIN = LITERAL (Read)
*        The domain for alignment:
*
*        - "FPLANE": Alignment occurs in focal plane coordinates.
*
*        - "SKY": Alignment occurs in celestial sky coordinates.
*
*        ["SKY"]
*     EXTCOR = LOGICAL (Given)
*        If TRUE, the time series data is corrected for extinction using
*        the SMURF:EXTINCTION task before being used to calculate Q and
*        U values (the data are first cleaned using SMURF:SC2CLEAN to
*        remove a linear background from each bolometer time stream). If
*        FALSE, no extinction correction is applied. The value of this
*        parameter is ignored if pre-calculated Q and U values are
*        supplied via the INQU parameter. [TRUE]
     FORCEFLAT = _LOGICAL (Read)
*        Should any remaining low frequency structure be removed from the
*        final background-removed Q and U images? This option is only
*        available if a mask has been suppplied using parameter MASK. If
*        TRUE, each masked Q and U image is smoothed using a 3 pixel
*        Gaussian. The masked areas in the resulting smoothed image are
*        filled in with artifical data using KAPPA:FILLBAD, and the filled
*        image is subtracted form the original (unmasked) image. [FALSE]
*     GLEVEL = LITERAL (Read)
*        Controls the level of information to write to a text log file.
*        Allowed values are as for "ILEVEL". The log file to create is
*        specified via parameter "LOGFILE. In adition, the glevel value
*        can be changed by assigning a new integer value (one of
*        starutil.NONE, starutil.CRITICAL, starutil.PROGRESS,
*        starutil.ATASK or starutil.DEBUG) to the module variable
*        starutil.glevel. ["ATASK"]
*     HARMONIC = _INTEGER (Read)
*        The Q and U values are derived from the fourth harmonic of the
*        half-wave plate rotation. However, to allow investigation of
*        other instrumental effects, it is possible instead to derive
*        equivalent quantities from any specified harmonic. These quantities
*        are calculated in exactly the same way as Q and U, but use the
*        harmonic specified by this parameter. [4]
*     HITS = NDF (Read)
*        If a value is supplied for HITS, a 2D NDF is created holding the
*        number of grid stare positions contributing to each pixel of the
*        Q and U images. [!]
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
*        debugging information. In addition scatter plots showing how each Q
*        and U image compares to the mean Q and U image are displayed at this
*        ILEVEL.
*
*        In adition, the glevel value can be changed by assigning a new
*        integer value (one of starutil.NONE, starutil.CRITICAL,
*        starutil.PROGRESS, starutil.ATASK or starutil.DEBUG) to the module
*        variable starutil.glevel. ["PROGRESS"]
*     IN = NDF (Read)
*        A group of POL-2 time series NDFs. Only used if a null (!) value is
*        supplied for INQU.
*     INQU = NDF (Read)
*        A group of NDFs containing Q and U images calculated by a previous run
*        of SMURF:CALCQU. If not supplied, the IN parameter is used to get input
*        NDFs holding POL-2 time series data. [!]
*     IP = _LOGICAL (Read)
*        Should a correction for instrumental polarisation be applied? The
*        default is True if a total intensity map has been supplied (see
*        parameter IREF) and False otherwise. []
*     IPDATA = NDF (Read)
*        An HDS container file holding the NDFs containing the individual
*        pixel "ps","pf","ang_IP", and "Co" terms in the instrumental
*        polarisation model. This parameter is accessed only if parameter
*        IP is set to TRUE. [$SMURF_DIR/../../share/smurf/ipdata.sdf]
*     IREF = NDF (Read)
*        An optional total intensity map covering the same area. If
*        supplied, this will be used to determine the expected
*        instrumental polarisation, the percentage polarisation, the pixel
*        size and projection for the Q and U images, and will also be used
*        as a background for the plotted vectors. It will also determine
*        the extent of the resulting catalogue and vector plot. If a null
*        value (!) is supplied, then an image of polarised intensity
*        calculated from the supplied POL-2 data will be used instead.
*        This will result in percentage polarisation values being 100%.
*        The extent and WCS of the vector plot and catalogue will then be
*        determined by the supplied POL-2 data files.
*     LOGFILE = LITERAL (Read)
*        The name of the log file to create if GLEVEL is not NONE. The
*        default is "<command>.log", where <command> is the name of the
*        executing script (minus any trailing ".py" suffix), and will be
*        created in the current directory. Any file with the same name is
*        over-written. The script can change the logfile if necessary by
*        assign the new log file path to the module variable
*        "starutil.logfile". Any old log file will be closed befopre the
*        new one is opened. []
*     MASK = NDF (Read)
*        An optional NDF covering the same area of sky as the input data,
*        and in which areas containing significant astronomical flux are
*        set bad. This mask is used to define the regions that are to be
*        omitted from the estimate of the sky background. Without a mask,
*        it is common for there to be significant "bowling" around bright
*        sources in the final background-subtracted Q and U images. Such a
*        bowl turn into a bright "halo" in the polarised intensity image,
*        with the vector orientations rotating through 90 degrees in the
*        halo.
*
*        If a mask is supplied, a further stage of cleaning is performed
*        in which correlations between the unmasked regions within the
*        residual Q and U images are found and removed. In addition,
*        supplying a mask also enables the FORCEFLAT parameter to be used
*        to remove any final low frequency structure in the background
*        regions.
*
*        A suitable mask can often be formed by running pol2cat first
*        without a mask, and then thresholding the resulting polarised
*        intensity image. The thresholded image can then be used as the
*        mask in a second run of pol2cat. ALternatively, a mask can be
*        formed form a scanm map of the region. [!]
*     MAXLEN = _REAL (Read)
*        The maximum length for the plotted vectors, in terms of the
*        quantity specified by parameter PLOT. Only vectors below this
*        length are plotted. If a null (!) value is supplied,  no maximum
*        length is imposed. [!]
*     MSG_FILTER = LITERAL (Read)
*        Controls the default level of information reported by Starlink
*        atasks invoked within the executing script. This default can be
*        over-ridden by including a value for the msg_filter parameter
*        within the command string passed to the "invoke" function. The
*        accepted values are the list defined in SUN/104 ("None", "Quiet",
*        "Normal", "Verbose", etc). ["Normal"]
*     NSIGMA = _REAL (Read)
*        Specifies the strength of the spike removal. Features with a
*        scale size smaller than 3 bolometers are removed by comparison
*        of each pixel value with the local mean. If a pixel value deviates
*        by more than NSIGMA standard deviations from the local mean, then
*        it is set bad. Since the removal of such pixel values will
*        change the local mean, this process is repeated 3 times. This
*        cleaning algorithm is performed by the KAPPA FFCLEAN command with
*        parameter BOX set to 3 and CLIP set to an array of three values
*        each equal to NSIGMA. Using a larger value for NSIGMA will result
*        in fewer pixels being removed. [3.0]
*     PI = NDF (Read)
*        The output NDF in which to return the polarised intensity map.
*        No polarised intensity map will be created if null (!) is supplied.
*        If a value is supplied for parameter IREF, then PI defaults to
*        null. Otherwise, the user is prompted for a value if none was
*        supplied on the command line. []
*     PIXSIZE = _REAL (Read)
*        The pixel size to use when forming the combined Q and U images,
*        in arc-seconds. If null (!) is supplied, the pixel size of the
*        image given by IREF will be used. If no image is given for IREF,
*        then the default pixel size used by CALCQU will be used (i.e.
*        pixel size equals bolometer spacing). [!]
*     PLOT = LITERAL (Read)
*        The quantity that gives the lengths of the plotted vectors:
*
*         - "P": Percentage polarisation.
*
*         - "PI": Polarised intensity.
*
*        If a null(!) value is supplied, no vectors will be plotted. Only
*        the vectors that pass the tests specified by parameter SNR and
*        MAXLEN are plotted. If a value is supplied for parameter IREF, then
*        only vectors that fall within the IREF image will be displayed.
*        The plot is displayed on the device specified by parameter DEVICE. [!]
*     QUI = NDF (Read)
*        If a value is supplied for QUI, the total Q, U and I images that
*        go into the final polarisation vector catalogue will be saved to
*        disk as a set of three 2D NDFs. The three NDFs are stored in a
*        single container file, with path given by QUI. So for instance if
*        QUI is set to "stokes.sdf", the Q, U and I images can be accessed
*        as "stokes.q", "stokes.u" and "stokes.i". [!]
*     REFINE = _LOGICAL (Read)
*        Add extra background removal stages? Only available if a mask is
*        supplied (See parameter MASK). This increases the time taken to
*        run pol2cat hugely.
*     RETAIN = _LOGICAL (Read)
*        Should the temporary directory containing the intermediate files
*        created by this script be retained? If not, it will be deleted
*        before the script exits. If retained, a message will be
*        displayed at the end specifying the path to the directory. [FALSE]
*     SNR = _REAL (Read)
*        The minimum ratio of the polarised intensity to the error on
*        polarised intensity for vectors to be plotted. [3.0]
*     STAREDIR = LITERAL (Read)
*        A directory in which to store separate Q, U, angle and polarised
*        intensity images for each stare position, for each sub-array.
*        The 2D NDFs created have names of the form "ANG_<A>_<I>.sdf",
*        "PI_<A>_<I>.sdf", etc, where <A> is the subarray name ("S8A", etc.)
*        and <I> is an index that counts from zero to one less than the number
*        of stare positions. The Q and U values use focal plane Y as the
*        reference direction, and the angles are in degrees, relative to the
*        focal plane Y axis. The directory is created if it does not exist. [!]

*  Copyright:
*     Copyright (C) 2012-2013 Science & Technology Facilities Council.
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
*     15-OCT-2012 (DSB):
*        Original version
*     16-NOV-2012 (DSB):
*        Use a temporary ADAM directory for each invocation.
*     27-NOV-2012 (DSB):
*        - Ensure the script runs even if no ~/adam/GLOBAL.sdf file exists.
*        - Ensure reference image has units of pW.
*     14-JAN-2013 (DSB):
*        Added parameter INQU.
*     11-MAR-2013 (DSB):
*        - Remove low frequency drift between stare positions in Q and U
*        values for each bolometer.
*        - Relax rejection criteria.
*     19-MAR-2013 (DSB):
*        Combine all Q (and U) images together in a single invocation of
*        makemos (not wcsalign).
*     6-JUN-2013 (DSB):
*        Added parameter STAREDIR.
*     12-JUN-2013 (DSB):
*        Added parameter NSIGMA.
*     28-JUN-2013 (DSB):
*        Added parameter EXTCOR.
*     4-JUL-2013 (DSB):
*        Added parameter HARMONIC.
*     27-AUG-2013 (DSB):
*        Store Q and U images in STAREDIR.
*     4-SEP-2013 (DSB):
*        - Added REFINE and FORCEFLAT parameters.
*        - Added removal of correlated residual Q and U components.
*     12-SEP-2013 (DSB):
*        Added instrumental polarisation correction.
*     13-SEP-2013 (DSB):
*        Do not conserve flux when aligning the Q and U images with the
*        reference image. The Q and U images represent the average Q and
*        U in each pixel, not the sum, and so flux should not be conserved
*        when changing the pixel scale of the Q and U images.
*     16-SEP-2013 (DSB):
*        Remove the background using SC2CLEAN rather than REMSKY before
*        doing th extinction correction.
*     20-SEP-2013 (DSB):
*        Provide some support for masks that are larger than a single subarray.
*     30-APR-2015 (DSB):
*        Remove median background value before mosaicing Q and U images.
*        This reduces the noise in the mosaics in cases where the sky Q/U is
*        varying strongly between grid points.
*     5-MAY-2015 (DSB):
*        - Only import smurfutil if needed. This avoids problems if the mdp
*        and/or pyndf module are not available locally.
*        - Report an error if the mask blanks out out nearly all of a Q or U image.
*        - Report an error if the supplied ref image does not have the requested Domain.
*-
'''

import os
import math
import starutil
from starutil import invoke
from starutil import NDG
from starutil import Parameter
from starutil import ParSys
from starutil import msg_out
from starutil import AtaskError

#  Assume for the moment that we will not be retaining temporary files.
retain = 0

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

   params.append(starutil.ParNDG("IN", "The input POL2 time series NDFs",
                                 starutil.get_task_par("DATA_ARRAY","GLOBAL",
                                                       default=Parameter.UNSET)))

   params.append(starutil.Par0S("CAT", "The output FITS vector catalogue",
                                 "out.FIT"))

   params.append(starutil.ParNDG("IREF", "The reference total flux map", default=None,
                                 help="Enter a null (!) to use an artifical total flux map",
                                 minsize=0, maxsize=1 ))

   params.append(starutil.ParNDG("PI", "The output polarised intensity map",
                                 default=None, exists=False, minsize=0, maxsize=1 ))

   params.append(starutil.ParChoice("PLOT", ["P","PI"], "Quantity to define "
                                 "lengths of plotted vectors", None,
                                 noprompt=True))

   params.append(starutil.Par0F("SNR", "Polarised intensity SNR threshold for "
                                 "plotted vectors", 3.0, maxval=1000.0, minval=0.0,
                                 noprompt=True))

   params.append(starutil.Par0F("MAXLEN", "Maximum vector length to plot", None,
                                 noprompt=True))

   params.append(starutil.ParChoice("DOMAIN", ["SKY","FPLANE"], "Domain for alignment",
                                 "Sky", noprompt=True))

   params.append(starutil.Par0F("PIXSIZE", "Pixel size (arcsec)", None,
                                 maxval=1000, minval=0.01, noprompt=True))

   params.append(starutil.Par0S("CONFIG", "The cleaning config",
                                 "^$STARLINK_DIR/share/smurf/dimmconfig.lis",
                                 noprompt=True))

   params.append(starutil.Par0S("DEVICE", "Device for graphical output",
                                 starutil.get_task_par("GRAPHICS_DEVICE",
                                                       "GLOBAL",default=None),
                                 noprompt=True))

   params.append(starutil.Par0F("NSIGMA", "No. of standard deviations at "
                                "which to clip spikes", 3.0, noprompt=True))

   params.append(starutil.Par0L("EXTCOR", "Perform extinction correction?",
                                True, noprompt=True))

   params.append(starutil.Par0L("RETAIN", "Retain temporary files?", False,
                                 noprompt=True))

   params.append(starutil.Par0S("QUI", "An HDS container file in which to "
                                "store the 2D Q, U and I images",
                                 default=None ))

   params.append(starutil.ParNDG("HITS", "An output NDF holding the hits per pixel",
                                 default=None, exists=False, minsize=0,
                                 maxsize=1, noprompt=True ))

   params.append(starutil.Par0I("HARMONIC", "The harmonic for calculating "
                                "Q and U", 4, minval=1, noprompt=True))

   params.append(starutil.Par0L("FORCEFLAT", "Force Q and U images to be flat?",
                                False, noprompt=True))

   params.append(starutil.Par0L("DEBIAS", "Remove statistical bias from P"
                                "and IP?", False, noprompt=True))

   params.append(starutil.Par0L("REFINE", "Do extra refinements (takes "
                                "lots of time)?", False, noprompt=False))

   params.append(starutil.ParNDG("INQU", "NDFs containing previously calculated Q and U values",
                                 None,noprompt=True))

   params.append(starutil.Par0S("DIAGFILE", "File to recieve diagnostic information",
                                 None, noprompt=True))

   params.append(starutil.ParNDG("MASK", "A 2D image in which source pixels are bad",
                                 default=None, minsize=0, maxsize=1, noprompt=True ))

   params.append(starutil.Par0S("STAREDIR", "Directory for stare position images",
                                 None, noprompt=True))

   params.append(starutil.Par0L("IP", "Do instrumental polarisation correction?",
                                True, noprompt=True))

   params.append(starutil.Par0S("IPDATA", "A file  containing pixel IP data",
                                "$SMURF_DIR/../../share/smurf/ipdata",noprompt=True,))

#  Initialise the parameters to hold any values supplied on the command
#  line.
   parsys = ParSys( params )

#  It's a good idea to get parameter values early if possible, in case
#  the user goes off for a coffee whilst the script is running and does not
#  see a later parameter propmpt or error...

#  See if pre-calculated Q and U values have been supplied on the command
#  line. If so, we use these in preference to any raw time-series files
#  specified via parameter "IN".
   inqu = parsys["INQU"].value

#  Get the raw POL-2 data files. They should be supplied as the first item on
#  the command line, in the form of a Starlink "group expression" (i.e.
#  the same way they are supplied to other SMURF commands such as makemap).
#  Quote the string so that it can be used as command line argument when
#  running an atask from the shell.
   if inqu is None:
      indata = parsys["IN"].value
   else:
      indata = None

#  See if the indata files are raw time series files, or files created by
#  a previous run of CALCQU containing Q and U estimates.

#  Get the source mask
   mask = parsys["MASK"].value

#  Get the alignment domain.
   domain = parsys["DOMAIN"].value

#  Get the image to use for the total flux (I) map.
   iref = parsys["IREF"].value

#  If a value was supplied for IREF, indicate that parameter PI should not
#  be prompted for. The default value (None) will then be used if no value
#  has been supplied on the command line.
   if iref:
      parsys["PI"].noprompt = True

#  Ensure the I image has the expected units (pW).
      invoke( "$KAPPA_DIR/ndftrace ndf={0} quiet".format(iref) )
      iunits = starutil.get_task_par( "UNITS", "ndftrace" )
      if iunits != "pW":
         raise starutil.InvalidParameterError("Reference image ({0}) has "
                    "incorrect units '{1} - must be 'pW'.".format(iref,iunits))

#  Ensure it is 2-dimensional (remove a degenerate 3rd pixel axis if necessary)
      ndim = starutil.get_task_par( "NDIM", "ndftrace" )
      if ndim == 3:
         if starutil.get_task_par( "DIMS(3)", "ndftrace" ) == 1:
            newref = NDG(1)
            invoke( "$KAPPA_DIR/ndfcopy in={0} out={1} trim=yes".
                    format(iref,newref) )
            iref = newref
         else:
            raise starutil.InvalidParameterError("Reference image ({0}) is "
                    "a cube!It must be a 2-d image.'.".format(iref))
      elif ndim != 2:
         raise starutil.InvalidParameterError("Reference image ({0}) is "
                 "{1} dimensional - it must be a 2-d image.'.".format(iref,ndim))

#  Ensure the I image has the requested domain.
      icur = int( starutil.get_task_par( "CURRENT", "ndftrace" ))
      try:
         invoke( "$KAPPA_DIR/wcsframe ndf={0} frame={1}".format(iref,domain) )
         invoke( "$KAPPA_DIR/wcsframe ndf={0} frame={1}".format(iref,icur) )
      except AtaskError:
         raise starutil.InvalidParameterError("\nThe reference image ({0}) "
                    "has no {1} Frame in its WCS component. Try again with "
                    "no reference image or use a different Domain.\n".
                    format(iref,domain))

#  We do not yet have any cut-outs of the IREF image.
   icuts = None

#  Now get the PI value to use.
   pimap = parsys["PI"].value

#  Now get the QUI value to use.
   qui = parsys["QUI"].value

#  Now get the HITS value to use.
   hitsmap = parsys["HITS"].value

#  Now get the harmonic to use
   harmonic = parsys["HARMONIC"].value

#  Get the output catalogue now to avoid a long wait before the user gets
#  prompted for it.
   outcat = parsys["CAT"].value

#  Get the clipping limit and create a string to use for the FFCLEAN CLIP
#  parameter.
   nsigma = parsys["NSIGMA"].value
   clip = "{0},{0},{0}".format(nsigma)

#  Get the pixel size to use. If no pixel size is supplied we use the pixel
#  size of the total intensity map if supplied, or of the Q and U maps
#  created by SMURF:CALCQU otherwise.
   pixsize = parsys["PIXSIZE"].value
   ref = None

#  Get the cleaning config.
   config = parsys["CONFIG"].value

#  Get the graphics device for graphical output. Normalisation plots will
#  only be produced if at least one of ILEVEL and GLEVEL is DEBUG.
   device = parsys["DEVICE"].value
   if device is not None:
      device = starutil.shell_quote( device )
      if starutil.ilevel >= starutil.DEBUG or starutil.glevel >= starutil.DEBUG:
         ndevice = device
      else:
         ndevice = "!"
   else:
      device = "!"
      ndevice = "!"

#  Get the quantity to use as the vector lengths (could be "None")
   plot = parsys["PLOT"].value

#  If any vectors are to be plotted, get the SNR limit for the plotted
#  vectors.
   if plot is not None:
      snr = parsys["SNR"].value
      maxlen = parsys["MAXLEN"].value

#  See if temp files are to be retained.
   retain = parsys["RETAIN"].value

#  See if backgrounds refinements are to be performed.
   refine = parsys["REFINE"].value
   if refine:
      if mask is None:
         raise starutil.InvalidParameterError("REFINE is True but no mask "
                                              "was supplied.")

#  Only import smurfutil if needed, since importing smurfutil requires
#  pyndf and mdp modules to be installed.
      import smurfutil

#  See if the Q and U backgrounds are to be force flat by smoothing, and
#  then removing the low frequency structure.
   forceflat = parsys["FORCEFLAT"].value
   if forceflat and not refine:
      raise starutil.InvalidParameterError("FORCEFLAT is True but REFINE "
                                           "is False.".format(inqu))

#  See if statistical debiasing is to be performed.
   debias = parsys["DEBIAS"].value

#  See if instrumental polarisation correction is to be done, and get the
#  associated constants to use.
   if iref:
      parsys["IP"].default = True
   else:
      parsys["IP"].default = False

   doip = parsys["IP"].value

   if doip:
      if iref:
         ipdata = parsys["IPDATA"].value
         if ipdata.endswith('.sdf'):
           ipdata = ipdata[:-4]
      else:
         raise starutil.InvalidParameterError("Cannot correct for instrumental"
             " polarisation since no total intensity map was supplied (IREF)")

#  Open a new file to receive diagnostic info
   diagfile = parsys["DIAGFILE"].value
   if diagfile is not None:
      diagfd = open( diagfile, "w" )
      diagfd.write("# QU block array chunk slope offset\n")
   else:
      diagfd = None

#  If required, ensure the stare data directory exists.
   staredir = parsys["STAREDIR"].value
   if staredir is not None:
      if not os.path.exists( staredir ):
         os.makedirs( staredir )

#  If Q and U values were supplied, use them:
   if inqu is not None:
      msg_out( "Using pre-calculating Q and U values...")
      qcont = inqu.filter( "'Q\d'" )
      if qcont is None:
         raise starutil.InvalidParameterError("Supplied QU files ({0}) "
                     "do not contain any Q data.".format(inqu))
      else:
         qcont.comment = "qcont"

      ucont = inqu.filter( "'U\d'" )
      if ucont is None:
         raise starutil.InvalidParameterError("Supplied QU files ({0}) "
                     "do not contain any U data.".format(inqu))
      else:
         ucont.comment = "ucont"

#  Otherwise create a set of Q images and a set of U images. These are put
#  into the HDS container files "q_TMP.sdf" and "u_TMP.sdf". Each image
#  contains Q or U values derived from a short section of raw data during
#  which each bolometer moves less than half a pixel.
   else:

#  First remove a sky background and perform extinction correction if
#  requested. We use SC2CLEAN to remove the sky rather than REMSKY. This
#  is because REMSKY removes a background from eahc time slice in turn,
#  which removes most of the polarised signal. On the other hand, SC2CLEAN
#  removes a linear baseline from each bolometer time stream, which leaves
#  the time-variation unchanges in each bolometer. SC2CLEAN only creates
#  output files for science files, not flats, etc, so we need to filter
#  out any NDF paths that do not exist after SC2CLEAN.
      if parsys["EXTCOR"].value:
         nosky = NDG(indata)
         msg_out( "Removing a background from the time series data...")
         invoke("$SMURF_DIR/sc2clean in={0} out={1} config={2}".
                 format(indata,nosky,starutil.shell_quote( config ) ) )
         nosky = nosky.filter()

         noext = NDG( nosky )
         msg_out( "Performing extinction correction on the time series data...")
         invoke("$SMURF_DIR/extinction in={0} out={1} tausrc=auto "
                "method=adaptive csotau=! hasskyrem=yes".format(nosky,noext) )
         if config and config != "def":
            config += ",doclean=0"
         else:
            config = "doclean=0"

#  Use the supplied input data from now on if no extinction correction is
#  required.
      else:
         noext = indata

#  The following call to SMURF:CALCQU creates two HDS container files -
#  one holding a set of Q NDFs and the other holding a set of U NDFs. Create
#  these container files in the NDG temporary directory.
      qcont = NDG(1)
      qcont.comment = "qcont"
      ucont = NDG(1)
      ucont.comment = "ucont"

      msg_out( "Calculating Q and U values for each bolometer...")
      invoke("$SMURF_DIR/calcqu in={0} config=\"{1}\" outq={2} outu={3} fix=yes "
             "harmonic={4} lsqfit=no".format(noext,starutil.shell_quote(config),
                                       qcont,ucont,harmonic) )

#  The next stuff we do independently for each subarray.
   qmaps = []
   umaps = []
   for a in ('S4A','S4B','S4C','S4D','S8A','S8B','S8C','S8D'):

#  Get NDG object that contains the Q and U maps for the current subarray.
      qarray = qcont.filter(a)
      uarray = ucont.filter(a)

#  If any data was found for the current subarray...
      if qarray is not None and uarray is not None:

#  Checking if IP correction is set for 450 um data. If so, throw an error
         if (doip) and (a in ['S4A','S4B','S4C','S4D']):
            raise starutil.InvalidParameterError("No IP correction exists for the"
                   " 450 um data.")

#  Remove spikes from the Q images for the current subarray. The cleaned NDFs
#  are written to temporary NDFs specified by the new NDG object "qff", which
#  inherit its size from the existing group "qarray"".
         msg_out( "Removing spikes from {0} bolometer Q values...".format(a))
         qff = NDG(qarray)
         qff.comment = "qff"
         invoke( "$KAPPA_DIR/ffclean in={0} out={1} genvar=yes box=3 clip=\[{2}\]"
                 .format(qarray,qff,clip) )

#  Create a set of Q images in which source pixels are blanked out.
         if mask is not None:
            msg_out( "Blanking source pixels in {0} Q values...".format(a))
            method = "mean"
            qmasked = NDG(qff)
            tmask = NDG(1)
            for (qin,qout) in zip( qff, qmasked ):
               invoke( "$KAPPA_DIR/wcsalign in={0} out={1} ref={2} lbnd=! "
                       "method=near rebin=no".format(mask,tmask,qin) )
               invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".
                       format(qin,qout,tmask) )
         else:
            method = "median"
            qmasked = qff

#  Remove the median value from each Q image. This is important if the sky
#  brightness changes a lot between grid points, resulting in big changes
#  in background Q and U values (due to instrumental polarisation). Without
#  it, the Q images may be so spaced out that the median of them (as created
#  by makemos) is just a copy of the grid point with the middle sky brightness
#  level.
         qbk = NDG(qff)
         for mm in range(len( qff )):
            qm = qmasked[ mm ];
            invoke( "$KAPPA_DIR/stats ndf={0} order quiet".format(qm) )
            medval = starutil.get_task_par( "median", "stats" )

            ngood = starutil.get_task_par( "numgood", "stats" )
            if ngood < 100:
               text = "\n\nOnly {0} good Q values remain for grid point {1}. ".format(ngood,mm+1)
               text += "Have you set NSIGMA (={0}) too low? ".format(nsigma)
               if mask:
                  text += "Or maybe the blanked out source regions in your mask ({0}) are too large.\n".format(mask)
               raise starutil.InvalidParameterError( text )

            qb = qbk[ mm ];
            invoke( "$KAPPA_DIR/csub in={0} scalar={1} out={2}".format(qm,medval,qb) )

#  There seems to be a tendency for each bolometer to have its own fixed
#  bias in Q and U. We now try to remove these biases by removing the Q and
#  U values that are common to each image (as opposed to astronomical Q/U
#  variations, which are  fixed on the sky and so will vary from image to
#  image as the focal plane is moved on the sky). First we find the median Q
#  value in each bolometer by averaging the Q images, aligned in PIXEL (i.e.
#  bolometer) coords. The median Q value per bolometer is put in qcom.sdf.
         msg_out( "Removing background Q level from {0} bolometers...".format(a))
         qcom = NDG(1)
         qcom.comment = "qcom"
         invoke( "$CCDPACK_DIR/makemos method={2} in={0} out={1}".format(qbk,qcom,method) )

#  We simply assume that the fixed bolometer Q bias is linearly related to
#  the mean Q value per bolometer. Astronomical sources will affect this
#  mean to a small extent, which is why we have the option to mask them out.
#  To find the gradient and offset of the linear relationship for each Q image,
#  we use kappa:normalize to do a least squares fit between each Q image and
#  the mean Q signal.
         qnm = NDG(qff)
         qnm.comment = "qnm"

         for mm in range(len( qff )):
            qin = qff[ mm ];
            qout = qnm[ mm ];
            qm = qmasked[ mm ];

            invoke( "$KAPPA_DIR/normalize in1={0} in2={1} out={2} device={3}".
                    format(qcom,qm,qout,ndevice), buffer=True )

#  If required, store the slope and offset in the diagnostics file.
            if diagfd is not None:
               slope = starutil.get_task_par( "slope", "normalize" )
               offset = starutil.get_task_par( "offset", "normalize" )
               ichunk = starutil.get_fits_header( qin, "POLCHUNK" )
               iblock = starutil.get_fits_header( qin, "POLBLOCK" )
               diagfd.write("Q {0} {1} {2} {3} {4}\n".format(iblock,a,ichunk,slope,offset))

#  Now substract the normalized mean Q signal from each Q image.
         qsub = NDG(qff)
         qsub.comment = "qsub"
         invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".
                 format(qff,qnm,qsub) )

#  And then remove smaller spikes that have now become visible in the
#  Q images as a result of subtracting off the bolometer biases.
         qffb = NDG(qff)
         qffb.comment = "qffb"
         invoke( "$KAPPA_DIR/ffclean in={0} out={1} genvar=yes box=3  clip=\[{2}\]".
                 format(qsub,qffb,clip) )

#  Remove the low frequency drift that seems to exist in the Q values for
#  each bolometer from stare position to stare position. Paste the cleaned,
#  background-subtracted Q images into a cube.
         qcube = NDG(1)
         invoke( "$KAPPA_DIR/paste in={0} out={1} shift=\[0,0,1\]".
                 format(qffb,qcube) )

#  Blank out the source pixel in the cube.
         if mask is not None:
            qcubem = NDG(1)
            qcube2 = NDG(1)
            invoke( "$KAPPA_DIR/paste in={0} out={1} shift=\[0,0,1\]".
                    format(qmasked,qcubem) )
            invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".
                    format(qcube,qcube2,qcubem) )
         else:
            qcube2 = qcube

#  Smooth the cube along the Z axis (the stare position axis). Currently
#  use a simple top-hat median filter, but maybe try Gaussian? Use median to
#  avoid the source being smeared out, and so causing rings in the residuals
#  created below.
         qcubebx2 = NDG(1)
         invoke( "$KAPPA_DIR/block in={0} out={1} estimator=median wlim=0.1 box=\[1,1,5\]".
                 format(qcube2,qcubebx2) )

#  Smooth again using a small mean filter to get better noise statistics.
         qcubebx = NDG(1)
         invoke( "$KAPPA_DIR/block in={0} out={1} estimator=mean wlim=0.1 box=\[1,1,3\]".
                 format(qcubebx2,qcubebx) )

#  Find the residuals after removal of the smoothed cube.
         qres = NDG(1)
         invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".
                 format(qcube,qcubebx,qres) )

#  Split the cube of residuals up into separate 2D planes, and copy the
#  WCS back. Could do with a new KAPPA command to do this.
         qsm = NDG(qffb)
         islice = 0
         for qq in qsm:
            islice += 1
            invoke( "$KAPPA_DIR/ndfcopy in={0}\(,,{1}\) out={2} trim=yes".
                    format(qres,islice,qq) )
            invoke( "$KAPPA_DIR/wcscopy ndf={0} like={1} ok".
                    format(qq,qffb[islice-1]) )


#  If required, look for correlated components in the residuals and remove
#  them.
         if refine:
            msg_out( "Removing correlated background components from {0} "
                     "bolometer Q values...".format(a) )
            qsm = smurfutil.remove_corr( qsm, qmasked )

#  If required, force the background to be flat my removing any remaining
#  low frequency structure in the unmasked regions.
            if forceflat:
               msg_out( "Forcing flat backgrounds in {0} bolometer Q values...".format(a) )
               qsm = smurfutil.force_flat( qsm, qmasked )

#  Remove smaller spikes from the Q images and estimate variances.
         msg_out( "Removing smaller spikes from {0} bolometer Q values...".format(a))
         qff2 = NDG(qsm)
         invoke( "$KAPPA_DIR/ffclean in={0} out={1} genvar=yes box=3 clip=\[{2}\]"
                 .format(qsm,qff2,clip) )

#  Now do the same for the U images.
         msg_out( "Removing spikes from {0} bolometer U values...".format(a))
         uff = NDG(uarray)
         uff.comment = "uff"
         invoke( "$KAPPA_DIR/ffclean in={0} out={1} genvar=yes box=3 clip=\[{2}\]"
                 .format(uarray,uff,clip) )

         if mask is not None:
            msg_out( "Blanking source pixels in {0} U values...".format(a))
            method = "mean"
            umasked = NDG(uff)
            for (uin,uout) in zip( uff, umasked ):
               invoke( "$KAPPA_DIR/wcsalign in={0} out={1} ref={2} lbnd=! "
                       "method=near rebin=no".format(mask,tmask,uin) )
               invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".
                       format(uin,uout,tmask) )
         else:
            method = "median"
            umasked = uff

         ubk = NDG(uff)
         for mm in range(len( uff )):
            um = umasked[ mm ];
            invoke( "$KAPPA_DIR/stats ndf={0} order quiet".format(um) )
            medval = starutil.get_task_par( "median", "stats" )

            ngood = starutil.get_task_par( "numgood", "stats" )
            if ngood < 100:
               text = "Only {0} good U values remain for grid point {1}.".format(ngood,mm+1)
               text += "Have you set NSIGMA (={0}) too low? ".format(nsigma)
               if mask:
                  text += "Or maybe the source regions in your mask ({0}) are too large.".format(mask)
               raise starutil.InvalidParameterError( text )

            ub = ubk[ mm ];
            invoke( "$KAPPA_DIR/csub in={0} scalar={1} out={2}".format(um,medval,ub) )

         msg_out( "Removing background U level from {0} bolometers...".format(a))
         ucom = NDG(1)
         invoke( "$CCDPACK_DIR/makemos method={2} in={0} out={1}".format(ubk,ucom,method) )

         unm = NDG(uff)
         unm.comment = "unm"

         for mm in range(len( uff )):
            uin = uff[ mm ];
            uout = unm[ mm ];
            um = umasked[ mm ];

            invoke( "$KAPPA_DIR/normalize in1={0} in2={1} out={2} device={3}".
                    format(ucom,um,uout,ndevice), buffer=True )

            if diagfd is not None:
               slope = starutil.get_task_par( "slope", "normalize" )
               offset = starutil.get_task_par( "offset", "normalize" )
               ichunk = starutil.get_fits_header( uin, "POLCHUNK" )
               iblock = starutil.get_fits_header( uin, "POLBLOCK" )
               diagfd.write("U {0} {1} {2} {3} {4}\n".format(iblock,a,ichunk,slope,offset))

         usub = NDG(uff)
         usub.comment = "usub"
         invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".
                 format(uff,unm,usub) )
         uffb = NDG(uff)
         uffb.comment = "uffb"
         invoke( "$KAPPA_DIR/ffclean in={0} out={1} genvar=yes box=3 clip=\[{2}\]".
                 format(usub,uffb,clip) )

         ucube = NDG(1)
         invoke( "$KAPPA_DIR/paste in={0} out={1} shift=\[0,0,1\]".
                 format(uffb,ucube) )

         if mask is not None:
            ucubem = NDG(1)
            ucube2 = NDG(1)
            invoke( "$KAPPA_DIR/paste in={0} out={1} shift=\[0,0,1\]".
                    format(umasked,ucubem) )
            invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".
                    format(ucube,ucube2,ucubem) )
         else:
            ucube2 = ucube

         ucubebx2 = NDG(1)
         invoke( "$KAPPA_DIR/block in={0} out={1} estimator=median wlim=0.1 box=\[1,1,5\]".
                 format(ucube2,ucubebx2) )
         ucubebx = NDG(1)
         invoke( "$KAPPA_DIR/block in={0} out={1} estimator=mean wlim=0.1 box=\[1,1,3\]".
                 format(ucubebx2,ucubebx) )
         ures = NDG(1)
         invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".
                 format(ucube,ucubebx,ures) )
         usm = NDG(uffb)
         islice = 0
         for uu in usm:
            islice += 1
            invoke( "$KAPPA_DIR/ndfcopy in={0}\(,,{1}\) out={2} trim=yes".
                    format(ures,islice,uu) )
            invoke( "$KAPPA_DIR/wcscopy ndf={0} like={1} ok".
                    format(uu,uffb[islice-1]) )

         if refine:
            msg_out( "Removing correlated background components from {0} "
                     "bolometer U values...".format(a) )
            usm = smurfutil.remove_corr( usm, umasked )

            if forceflat:
               msg_out( "Forcing flat backgrounds in {0} bolometer U values...".format(a) )
               usm = smurfutil.force_flat( usm, umasked )

         uff2 = NDG(usm)
         invoke( "$KAPPA_DIR/ffclean in={0} out={1} genvar=yes box=3 clip=\[{2}\]"
                 .format(usm,uff2,clip) )

#  Ensure all images have the required current WCS Frame (as indicated by
#  the DOMAIN parameter).
         invoke( "$KAPPA_DIR/wcsframe ndf={0} frame={1}".format(qff2,domain) )
         invoke( "$KAPPA_DIR/wcsframe ndf={0} frame={1}".format(uff2,domain) )

#  If required, dump the angles and polarised intensities for each stare
#  position.
         if staredir is not None:
            for mm in range(len( uff2 )):
               qin = qff2[ mm ];
               uin = uff2[ mm ];
               out = "{0}/ANG_{1}_{2}".format(staredir,a,mm)
               invoke( "$KAPPA_DIR/maths exp='0.5*atan2(iu,iq)*57.29578' "
                       "iu={0} iq={1} out={2}".format(uin,qin,out))
               out = "{0}/PI_{1}_{2}".format(staredir,a,mm)
               invoke( "$KAPPA_DIR/maths exp='sqrt(iu**2+iq**2)' iu={0} "
                       "iq={1} out={2}".format(uin,qin,out))
               out = "{0}/Q_{1}_{2}".format(staredir,a,mm)
               invoke( "$KAPPA_DIR/ndfcopy in={0} out={1}".format(qin,out))
               out = "{0}/U_{1}_{2}".format(staredir,a,mm)
               invoke( "$KAPPA_DIR/ndfcopy in={0} out={1}".format(uin,out))

#  The reference map defines the output pixel grid - the origin, pixel size,
#  sky projection, etc (but not the pixel bounds) - of the final Q, U and I
#  maps. If we do not yet have such a reference map, create one now.
         if not ref:
            msg_out( "Creating reference map...")

#  We use a copy of the total flux reference map, if one was supplied, and the
#  first Q map otherwise.
            ref = NDG( 1 )
            if iref:
               invoke( "$KAPPA_DIR/ndfcopy in={0} out={1} trim=yes".format(iref,ref) )
            else:
               invoke( "$KAPPA_DIR/ndfcopy in={0} out={1} trim=yes".format(qff2[0],ref) )

#  We add a WCS Frame with Domain "POLANAL" to define the polarimetric
#  reference direction (we choose the pixel Y axis). POLPACK expects this
#  Frame to be present and uses the first axis as the reference direction
#  (positive rotation is in the sense of rotation from the first to the
#  second axis in the POLANAL Frame). We take a copy of the PIXEL Frame
#  and then rotate it by 90 degs so that the first axis is parallel to the
#  original pixel Y axis. First remove any pre-existing POLANAL Frame.
            invoke( "$KAPPA_DIR/wcsremove ndf={0} frames=POLANAL".format(ref) )
            invoke( "$KAPPA_DIR/wcsadd ndf={0} frame=PIXEL domain=POLANAL maptype=linear attrs=! tr=\[0,0,1,0,-1,0\]".format(ref) )
            invoke( "$KAPPA_DIR/wcsframe ndf={0} frame={1}".format(ref,domain) )

#  If a specific pixel size was requested, squash or stretch the
#  reference map so that it has the requested pixel size.
            if pixsize:
               tmp = NDG(1)
               invoke( "$KAPPA_DIR/sqorst in={0} out={1} mode=pixelscale piscale=\'{2}\,{2}\'".
                       format(ref,tmp,pixscale) )
               ref = tmp

#  Correct the background-subtracted Q and U for the instrumental
#  polarisation, whilst they are still referred to the focal plane Y axis.
#  We can only do this if we have a total intenbsity map.
         if doip:
            msg_out( "Correcting all {0} Q and U values for instrumental "
                     "polarisation...".format(a) )

#  The number of stare positions (usually 25).
            nstare = len(qff2)

#  If not yet done, create a set of cut-outs from the supplied total
#  intensity map that are aligned in pixel coords with each of the 25
#  Q and U images. Also, if a mask was supplied, mask these cut-out by
#  setting background regions to zero.
            if icuts is None:

#  If a mask was supplied, align it with the iref image, and then use it
#  to create a copy of iref in which all non-source pixels are bad, then
#  convert bad values to zero.
               if mask:
                  maska = NDG( 1 )
                  invoke( "$KAPPA_DIR/wcsalign in={0} out={1} ref={2} "
                          "method=near rebin=no accept".format(mask,maska,iref) )
                  iref_tmp = NDG( 1 )
                  invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2} "
                          "invert=yes".format(iref,iref_tmp,maska) )
                  invoke( "$KAPPA_DIR/erase object={0}.variance ok=yes".format(iref_tmp) )
                  iref_masked = NDG( 1 )
                  invoke( "$KAPPA_DIR/nomagic in={0} out={1} repval=0".
                          format(iref_tmp,iref_masked) )
               else:
                  iref_masked = iref

#  Create the required aligned cut-outs of this masked iref image.
            icuts = NDG( nstare )
            for (ipref,icut) in zip( qff2, icuts ):
               invoke( "$KAPPA_DIR/wcsalign in={0} out={1} ref={2} "
                       "method=bilin rebin=no accept". format(iref_masked,icut,ipref) )

#  Get the elevation (in degs) at the centre of the observation.
            elev = float( invoke("$KAPPA_DIR/fitsmod ndf={0} edit=print keyword=ELSTART".
                                 format( qff2[ nstare/2 ] )))

#  Get the instrumental polarisation parameters.
            ipC0 = "{0}.{1}.c0".format(ipdata,a)
            ipP0 = "{0}.{1}.p0".format(ipdata,a)
            ipP1 = "{0}.{1}.p1".format(ipdata,a)
            ipANGC = "{0}.{1}.angc".format(ipdata,a)

#  Calculate the factors by which to multiply the total intensity image
#  to get the Q and U corrections.
            qfac = NDG( nstare )
            ufac = NDG( nstare )

            el_temp = NDG( nstare )
            el_temp2 = NDG( nstare )
            invoke( "$KAPPA_DIR/cadd in={0} scalar={1} out={2}".format(ipC0,elev,el_temp))
            invoke( "$KAPPA_DIR/add in1={0} in2={1} out={2}".format(el_temp,ipANGC,el_temp2))
            invoke( "$KAPPA_DIR/maths exp='ia*COSD(2*ib)+ic*COSD(2*id)' ia={0} ib={1} ic={2} id={3} out={4}".format(ipP0,ipANGC,ipP1,el_temp2,qfac))
            invoke( "$KAPPA_DIR/maths exp='ia*SIND(2*ib)+ic*SIND(2*id)' ia={0} ib={1} ic={2} id={3} out={4}".format(ipP0,ipANGC,ipP1,el_temp2,ufac))

            #invoke( "$KAPPA_DIR/maths exp='ia*COSD(2*ib)+ic*COSD(2*(id+ie+pa))' ia={0} ib={1} ic={2} id={3} ie={4} pa={5} out={6}".
            #        format(ipP0,ipANGC,ipP1,ipANGC,ipC0,elev,qfac))
            #invoke( "$KAPPA_DIR/maths exp='ia*SIND(2*ib)+ic*SIND(2*(id+ie+pa))' ia={0} ib={1} ic={2} id={3} ie={4} pa={5} out={6}"
            #        .format(ipP0,ipANGC,ipP1,ipANGC,ipC0,elev,ufac))

#  Correct the Q and U images.
            qip = NDG( nstare )
            uip = NDG( nstare )
            invoke( "$KAPPA_DIR/maths exp='ia-ib*ic' ia={0} ib={1} ic={2} out={3}".
                    format(qff2,icuts,qfac,qip))
            invoke( "$KAPPA_DIR/maths exp='ia-ib*ic' ia={0} ib={1} ic={2} out={3}".
                    format(uff2,icuts,ufac,uip))
         else:
            qip = qff2
            uip = uff2

#  Modify the values in each pair of Q and U images so that they refer to
#  the common reference direction (i.e. the ref image pixel Y axis).
         msg_out( "Rotating all {0} Q and U reference directions to be parallel...".format(a) )
         qrot = NDG(qip)
         urot = NDG(uip)
         for (qin,uin,qout,uout) in zip( qip, uip, qrot, urot ):
            invoke( "$POLPACK_DIR/polrotref qin={0} uin={1} like={2} qout={3} uout={4} ".
                    format(qin,uin,ref,qout,uout) )

#  Record the NDG objects specifying the cleaned and rotated Q and U maps for
#  this subarray.
         qmaps.append( qrot )
         umaps.append( urot )

#  All sub-arrays are done. Now combine the Q images for all sub-arrays
#  together into a single image. First form a new NDG containing all the
#  individual sub-array Q maps, and another one holding the name of the
#  total Q map.
   msg_out( "Combining all Q and U images for all sub-arrays...")
   qmaps_all = NDG( qmaps )
   qaligned = NDG( qmaps_all )
   invoke( "$KAPPA_DIR/wcsalign method=bilin rebin=yes conserve=no in={0} "
           "ref={1} out={2} lbnd=!".format(qmaps_all,ref,qaligned) )
   qaligned = qaligned.filter() #  Remove any NDFs that could not be created by wcsalign
   qtotal = NDG( 1 )
   invoke( "$CCDPACK_DIR/makemos method=broad  in={0} out={1}".format(qaligned,qtotal) )

#  Do the same for U.
   umaps_all = NDG( umaps )
   ualigned = NDG( umaps_all )
   invoke( "$KAPPA_DIR/wcsalign method=bilin rebin=yes conserve=no in={0} ref={1} "
           "out={2} lbnd=!".format(umaps_all,ref,ualigned) )
   ualigned = ualigned.filter() #  Remove any NDFs that could not be created by wcsalign
   utotal = NDG( 1 )
   invoke( "$CCDPACK_DIR/makemos method=broad in={0} out={1}".format(ualigned,utotal) )

#  Create the hits map if required.
   if hitsmap:

#  Fill the aligned Q maps with the value 1.0 at all good pixels
      wone = NDG( qaligned )
      invoke( "$KAPPA_DIR/maths exp='0*ia+1' ia={0} out={1}".format(qaligned,wone))

#  Put them into a cube.
      wcube = NDG( 1 )
      invoke( "$KAPPA_DIR/paste in={0} shift=\[0,0,1\] out={1}".format(wone,wcube))

#  Collapse the cube to get the sum at each pixel.
      invoke( "$KAPPA_DIR/collapse in={0} out={1} axis=3 wlim=0 estimator=sum".format(wcube,hitsmap) )

#  If no total intensity map was supplied, generate an artificial I image that
#  is just equal to the polarised intensity image. This is needed because
#  polpack:polvec uses the I value to normalise the Q and U values prior to
#  calculating the polarised intensity and angle.
   if iref is None:
      iref = NDG(1)
      msg_out( "Generating an artificial total intensity image...")
      invoke( "$KAPPA_DIR/maths exp='sqrt(ia**2+ib**2)' ia={0} ib={1} out={2}".format(qtotal,utotal,iref))

#  If a total intensity map was supplied, and we are not using the
#  default pixel size (i.e. the pixel size in the total intensity map),
#  aligned the total intensity map with the reference image.
   elif pixsize:
      tmp = NDG( 1 )
      invoke( "$KAPPA_DIR/wcsalign in={0} out={1} ref={2} method=bilin "
              "rebin=no lbnd=!".format(iref,tmp,ref) )
      iref = tmp

#  Ensure the Q U and I images all have the same bounds, equal to the
#  overlap region between them. To get the overlap region, use MATHS to
#  add them together. Then use ndfcopy to produce the sections from each,
#  which match the overlap area.
   tmp = NDG( 1 )
   invoke( "$KAPPA_DIR/maths exp='ia+ib+ic' ia={0} ib={1} ic={2} out={3}".format(qtotal,utotal,iref,tmp) )
   qtrim = NDG( 1 )
   invoke( "$KAPPA_DIR/ndfcopy in={0} like={1} out={2}".format(qtotal,tmp,qtrim) )
   utrim = NDG( 1 )
   invoke( "$KAPPA_DIR/ndfcopy in={0} like={1} out={2}".format(utotal,tmp,utrim) )
   itrim = NDG( 1 )
   invoke( "$KAPPA_DIR/ndfcopy in={0} like={1} out={2}".format(iref,tmp,itrim) )

#  If required, save the Q, U and I images.
   if qui is not None:
      invoke( "$KAPPA_DIR/ndfcopy in={0} out={1}.Q".format(qtrim,qui) )
      invoke( "$KAPPA_DIR/ndfcopy in={0} out={1}.U".format(utrim,qui) )
      invoke( "$KAPPA_DIR/ndfcopy in={0} out={1}.I".format(itrim,qui) )

#  The polarisation vectors are calculated by the polpack:polvec command,
#  which requires the input Stokes vectors in the form of a 3D cube. Paste
#  the 2-dimensional Q, U and I images into a 3D cube.
   planes = NDG( [qtrim,utrim,itrim] )
   cube = NDG( 1 )
   invoke( "$KAPPA_DIR/paste in={0} shift=\[0,0,1\] out={1}".format(planes,cube))

#  Check that the cube has a POLANAL frame, as required by POLPACK.
   try:
      invoke( "$KAPPA_DIR/wcsframe ndf={0} frame=POLANAL".format(cube) )

#  If it does not, see if it has a "POLANAL-" Frame (kappa:paste can
#  cause this by appending "-" to the end of the domain name to account for
#  the extra added 3rd axis).
   except AtaskError:
      invoke( "$KAPPA_DIR/wcsframe ndf={0} frame=POLANAL-".format(cube) )

#  We only arrive here if the POLANAL- frame was found, so rename it to POLANAL
      invoke( "$KAPPA_DIR/wcsattrib ndf={0} mode=set name=domain newval=POLANAL".format(cube) )

#  Re-instate the required current Frame
   invoke( "$KAPPA_DIR/wcsframe ndf={0} frame={1}".format(cube,domain) )

#  POLPACK needs to know the order of I, Q and U in the 3D cube. Store
#  this information in the POLPACK enstension within "cube.sdf".
   invoke( "$POLPACK_DIR/polext in={0} stokes=qui".format(cube) )

#  Create a FITS catalogue containing the polarisation vectors.
   command = "$POLPACK_DIR/polvec {0} cat={1} debias={2}".format(cube,outcat,debias)
   if pimap:
      command = "{0} ip={1}".format(command,pimap)
      msg_out( "Creating the output catalogue {0} and polarised intensity map {1}...".format(outcat,pimap) )
   else:
      msg_out( "Creating the output catalogue: {0}...".format(outcat) )
   msg = invoke( command )
   msg_out( "\n{0}\n".format(msg) )

#  If required, produce a vector plot.
   if plot is not None:
      msg_out( "Plotting the '{0}' vectors ...".format(plot) )

#  Select vectors where the signal to noise ratio for the Polarised
#  Intentisy value is more than 3. The catalogue is stored in the NDG
#  temporary directory.
      exp = "pi>{0}*dpi".format(snr)
      if maxlen is not None:
         exp += "&{0}<{1}".format(plot,maxlen)
      selcat = "{0}/selcat".format(NDG.tempdir)
      invoke( "$CURSA_DIR/catselect catin={0} catout={1} norejcat seltyp=e expr='{2}'".format(outcat,selcat,exp))

#  If a total intensity reference was supplied, display it as a background
#  image, and plot the vectors over the top.
      if iref:
         invoke( "$KAPPA_DIR/lutable mapping=linear coltab=grey device={0}".format(device), buffer=True)
         invoke( "$KAPPA_DIR/display in={0}'(,,3)' mode=perc percentiles=\[1,99\] badcol=black device={1}".format(cube,device), buffer=True)
         invoke( "$POLPACK_DIR/polplot cat={0} clear=no axes=no colmag={1} key=yes style='colour=red' device={2}".format(selcat,plot,device), buffer=True)

#  Otherwise, just plot the vectors.
      else:
         invoke( "$POLPACK_DIR/polplot cat={0} colmag={1} key=yes style=def device={2}".format(selcat,plot,device), buffer=True)

#  Remove temporary files.
   cleanup()

#  close any diagnostics file
   if diagfd is not None:
      diagfd.close()

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

