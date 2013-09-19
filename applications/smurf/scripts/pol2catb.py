#!/usr/bin/env python

'''
*+
*  Name:
*     pol2catb

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
*     each bolometer are made and are used to correct the original Q and U
*     images for varying bolometer gain and phase lag. The resulting flat
*     background are then removed from the corrected Q and U images.
*
*     Instrumental polarisation is then removed, based on the total
*     intensity values specified by the IMAP parameter. The parameters
*     of the instrumental polarisation model can be specified via the
*     "IP_..." parameters.
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

*     This script requires the pyndf package (available from
*     github.com/timj/starlink-pyndf) and the Modular toolkit for
*     Data Processing" (MDP) package ("pip install MDP").

*  Usage:
*     pol2catb in cat iref pi [plot] [snr] [maxlen] [domain] [pixsize]
*              [config] [device] [extcor] [retain] [qui] [hits]
*              [harmonic] [forcefile] [msg_filter] [ilevel] [glevel] [logfile]

*  Parameters:
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
*     IP_P0 = _REAL (Read)
*        The "p0" term in the instrumental polarisation model. [0.0025]
*     IP_P1 = _REAL (Read)
*        The "p1" term in the instrumental polarisation model. [0.01]
*     IP_ANGLE_C = _REAL (Read)
*        The "angle_c" term in the instrumental polarisation model, in
*        degrees. [-53.0]
*     IP_C_0 = _REAL (Read)
*        The "C_0" term in the instrumental polarisation model, in
*        degrees. [90.0]
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
*     NPCA = _INTEGER (Read)
*        The number of PCA components to use to model the background.

*        The default is 6 if a mask has been supplied (see parameter
*        MASK), and zero otherwise. []
*     NKEEP = _INTEGER (Read)
*        This controls the algorithm that chooses which bolometers to
*        retain and which to reject as too noisey. It specifies the
*        target number of bolometers that should be retained in each
*        subarray. The least noisey "NKEEP" bolometers are retained and
*        the rest are set bad. In some cases it may be necessary to reject
*        more than the specified number. The supplied number should be
*        less than 1280. [700]
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
*     11-SEP-2013 (DSB):
*        Original version based on pol2cat.
*     12-SEP-2013 (DSB):
*        Added instrumental polarisation correction.
*     13-SEP-2013 (DSB):
*        Do not conserve flux when aligning the Q and U images with the
*        reference image. The Q and U images represent the average Q and
*        U in each pixel, not the sum, and so flux should not be conserved
*        when changing the pixel scale of the Q and U images.
*     16-SEP-2013 (DSB):
*        Remove the background using SC2CLEAN rather than REMSKY before
*        doing th eextinction correction.
*     20-SEP-2013 (DSB):
*        - Provide some support for masks that are larger than a single subarray.
*        - Replace the (unused) NSIGMA parameter with NKEEP. This
*        controls how many bolometers are rejected.
*-
'''

import os
import math
import starutil
import smurfutil
from starutil import invoke
from starutil import NDG
from starutil import Parameter
from starutil import ParSys
from starutil import msg_out

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

   params.append(starutil.Par0I("NKEEP", "No. of bolometers to retain in "
                                 "each subarray", 700, noprompt=True))

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

   params.append(starutil.Par0L("DEBIAS", "Remove statistical bias from P"
                                "and IP?", False, noprompt=True))

   params.append(starutil.ParNDG("INQU", "NDFs containing previously calculated Q and U values",
                                 None,noprompt=True))

   params.append(starutil.Par0S("DIAGFILE", "File to recieve diagnostic information",
                                 None, noprompt=True))

   params.append(starutil.ParNDG("MASK", "A 2D image in which source pixels are bad",
                                 default=None, minsize=0, maxsize=1, noprompt=True ))

   params.append(starutil.Par0S("STAREDIR", "Directory for stare position images",
                                 None, noprompt=True))

   params.append(starutil.Par0I("NPCA", "The number of PCA components to use "
                 "when modelling the background", 6, minval=0, noprompt=True))

   params.append(starutil.Par0L("IP", "Do instrumental polarisation correction?",
                                True, noprompt=True))

   params.append(starutil.Par0F("IP_P0", "The p0 constant for the IP correction",
                                0.0025, minval=0, noprompt=True))

   params.append(starutil.Par0F("IP_P1", "The p1 constant for the IP correction",
                                0.01, minval=0, noprompt=True))

   params.append(starutil.Par0F("IP_ANGLE_C", "The Angle_C constant for the IP correction",
                                -53.0, noprompt=True))

   params.append(starutil.Par0F("IP_C_0", "The C_0 constant for the IP correction",
                                90.0, noprompt=True))

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
   if inqu == None:
      indata = parsys["IN"].value
   else:
      indata = None

#  See if the indata files are raw time series files, or files created by
#  a previous run of CALCQU containing Q and U estimates.

#  Get the source mask
   mask = parsys["MASK"].value

#  Get the image to use for the total flux (I) map.
   iref = parsys["IREF"].value

#  If a value was supplied for IREF, indicate that parameter PI should not
#  be prompted for. The default value (None) will then be used if no value
#  has been supplied on the command line.
   if iref:
      parsys["PI"].noprompt = True

#  Ensure the I image has the expected units (pW).
      invoke( "$KAPPA_DIR/ndftrace {0} quiet".format(iref) )
      iunits = starutil.get_task_par( "UNITS", "ndftrace" )
      if iunits != "pW":
         raise starutil.InvalidParameterError("Reference image ({0}) has "
                    "incorrect units '{1} - must be 'pW'.".format(iref,iunits))

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

#  Now get the number of PCA components to use when modelling the
#  background. Set the default to zero if no mask was supplied.
   if mask == None:
      parsys["NPCA"].default = 0
   npca = parsys["NPCA"].value

#  Get the output catalogue now to avoid a long wait before the user gets
#  prompted for it.
   outcat = parsys["CAT"].value

#  Get the alignment domain.
   domain = parsys["DOMAIN"].value

#  Get the target number of bolometers to use per subarray.
   nkeep = parsys["NKEEP"].value

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
   if device != None:
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
   if plot != None:
      snr = parsys["SNR"].value
      maxlen = parsys["MAXLEN"].value

#  See if temp files are to be retained.
   retain = parsys["RETAIN"].value

#  See statistical debiasing is to be performed.
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
         ip_p0 = parsys["IP_P0"].value
         ip_p1 = parsys["IP_P1"].value
         ip_angle_c = parsys["IP_ANGLE_C"].value
         ip_c_0 = parsys["IP_C_0"].value
      else:
         raise starutil.InvalidParameterError("Cannot correct for instrumental"
             " polarisation since no total intensity map was supplied (IREF)")

#  Open a new file to receive diagnostic info
   diagfile = parsys["DIAGFILE"].value
   if diagfile != None:
      diagfd = open( diagfile, "w" )
      diagfd.write("# QU block array chunk slope offset\n")
   else:
      diagfd = None

#  If required, ensure the stare data directory exists.
   staredir = parsys["STAREDIR"].value
   if staredir != None:
      if not os.path.exists( staredir ):
         os.makedirs( staredir )

#  If Q and U values were supplied, use them:
   if inqu != None:
      msg_out( "Using pre-calculating Q and U values...")
      qcont = inqu.filter( "'Q\d'" )
      if qcont == None:
         raise starutil.InvalidParameterError("Supplied QU files ({0}) "
                     "do not contain any Q data.".format(inqu))
      else:
         qcont.comment = "qcont"

      ucont = inqu.filter( "'U\d'" )
      if ucont == None:
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
         config += ",doclean=0"

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
      invoke("$SMURF_DIR/calcqu in={0} config=\"{1}\" outq={2} outu={3} "
             "harmonic={4} fix".format(noext,starutil.shell_quote(config),
                                       qcont,ucont,harmonic) )

#  The next stuff we do independently for each subarray.
   qmaps = []
   umaps = []
   for a in ('S4A','S4B','S4C','S4D','S8A','S8B','S8C','S8D'):

#  Get NDG object that contains the Q and U maps for the current subarray.
      qarray = qcont.filter(a)
      uarray = ucont.filter(a)

#  If any data was found for the current subarray...
      if qarray != None and uarray != None:

#  Save the number of stare positions.
         npos = len( qarray )

#  Most operations in this script are performed on cubes formed by
#  stacking together all the individual stare positions in pixel
#  coordinates. If a 2D mask image was supplied, create a cube that
#  can be used to mask out source pixels in such cubes. The same mask can
#  be used for both Q and U since Q and U arrays are always aligned in
#  pixel coords. We loop over all stare positions, and for each one we
#  align the supplied 2D mask image to the Q array, cutting the resulting
#  aligned mask down to the size of the Q array. We then paste all the
#  aligned 2D mask images into a single 3D cube.
         if mask != None:
            msg_out( "Forming a 3D mask for {0} values...".format(a))
            mplanes = NDG(qarray)
            for (din,mplane) in zip( qarray, mplanes ):
               invoke( "$KAPPA_DIR/wcsalign in={0} out={1} ref={2} method=near "
                       "rebin=no accept".format(mask,mplane,din) )

            mcube = NDG(1)
            invoke( "$KAPPA_DIR/paste in={0} out={1} shift=\[0,0,1\]".
                     format(mplanes,mcube) )

#  Form a cube by stacking the individual 2D Q arrays.
         qtmp = NDG(1)
         invoke( "$KAPPA_DIR/paste in={0} out={1} shift=\[0,0,1\]".
                  format(qarray,qtmp) )

#  Form a cube by stacking the individual 2D U arrays.
         utmp = NDG(1)
         invoke( "$KAPPA_DIR/paste in={0} out={1} shift=\[0,0,1\]".
                  format(uarray,utmp) )

#  The first stage of cleaning is to look for bolometers (="pixels") that
#  have unusually large ranges in the above Q and U cubes. So first collapse
#  the above cubes into a pair of 2D maps by taking the maximum value in
#  each z-column in the cube.
         msg_out( "Removing {0} bolometers with high Q and U ranges values...".format(a))
         qmax = NDG(1)
         invoke( "$KAPPA_DIR/collapse in={0} out={1} estimator=max axis=3".
                  format(qtmp,qmax) )
         umax = NDG(1)
         invoke( "$KAPPA_DIR/collapse in={0} out={1} estimator=max axis=3".
                  format(utmp,umax) )

#  Now collapse them again finding the minimum value in each z-column.
         qmin = NDG(1)
         invoke( "$KAPPA_DIR/collapse in={0} out={1} estimator=min axis=3".
                  format(qtmp,qmin) )
         umin = NDG(1)
         invoke( "$KAPPA_DIR/collapse in={0} out={1} estimator=min axis=3".
                  format(utmp,umin) )

#  Take the difference between the max and min to find the range of Q and U
#  values in each z-column.
         qrange = NDG(1)
         invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".format(qmax,qmin,qrange) )
         urange = NDG(1)
         invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".format(umax,umin,urange) )

#  We keep the best "nkeep" bolometers (i.e. the ones with the smallest
#  ranges). We first need to convert "nkeep" to a percentage of the remaining
#  valid bolometers.
         invoke( "$KAPPA_DIR/stats {0} quiet".format(qrange) )
         ngood = starutil.get_task_par( "numgood", "stats" )
         if ngood > nkeep:
            perc = float( nkeep )/float( ngood )

#  Get the corresponding percentile value.
            invoke( "$KAPPA_DIR/stats {0} order=yes percentiles={1} quiet".format(qrange,perc) )
            thresh = starutil.get_task_par( "perval(1)", "stats" )

#  Set bad all bolometers above this percentile value.
            qrancln = NDG(1)
            invoke( "$KAPPA_DIR/thresh in={0} out={1} thrlo=-1 newlo=bad "
                    "thrhi={2} newhi=bad".format(qrange,qrancln,thresh) )
         else:
            qrancln = qrange

#  Do the same for U.
         invoke( "$KAPPA_DIR/stats {0} quiet".format(urange) )
         ngood = starutil.get_task_par( "numgood", "stats" )
         if ngood > nkeep:
            perc = float( nkeep )/float( ngood )

#  Get the corresponding percentile value.
            invoke( "$KAPPA_DIR/stats {0} order=yes percentiles={1} quiet".format(urange,perc) )
            thresh = starutil.get_task_par( "perval(1)", "stats" )

#  Set bad all bolometers above this percentile value.
            urancln = NDG(1)
            invoke( "$KAPPA_DIR/thresh in={0} out={1} thrlo=-1 newlo=bad "
                    "thrhi={2} newhi=bad".format(urange,urancln,thresh) )
         else:
            urancln = urange

#  We can only use bolometers that are good in both Q *and* U. So
#  multiply the cleaned maps together to get a 2d mask of bad bolometers.
         badmask = NDG(1)
         invoke( "$KAPPA_DIR/mult in1={0} in2={1} out={2}".format(qrancln,urancln,badmask) )

#  Extrude this 2D mask into a 3D cube with one plane for each stare position.
         badcube = NDG(1)
         invoke( "$KAPPA_DIR/manic in={0} out={1} axes=\[1,2,0\] lbound=1 ubound={2}".
                 format(badmask,badcube,npos) )

#  Copy the bad pixels from this "bad-bolo" mask (as opposed to the "source"
#  mask supplied by the user) into the Q and U cubes, thus blanking out
#  bad bolometers.
         qcube = NDG(1)
         invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".
                 format(qtmp,qcube,badcube) )
         ucube = NDG(1)
         invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".
                 format(utmp,ucube,badcube) )

#  We assume 1) that the Q and U values are dominated by the sky (polarised by
#  the wind shield etc), 2) that the true Q and U should therefore be (almost)
#  the same for all bolometers at all stare positions, and 3) the fact they are
#  *not* the same but change both with position and with time, indicates that
#  the gain and time lag of each bolometer can change slowly over the duration
#  of the observation. So we now transform the raw Q and U values into
#  polarised intensity and angle so that we can get direct measures of the gain
#  and delay (time lag) in each bolometer at each stare position. The
#  central elevation of each stare position will be slightly different, and
#  so the angle of the wind shield relative to the focal plane will change,
#  causing real changes in Q and U that are independent of the bolometer
#  characteristics. But these changes are tiny compared to the change in
#  phase that we see between stare positions and so we ignore them for the
#  moment.
         msg_out( "Converting Q and U to polar coords...".format(a))
         gcube = NDG(1)
         invoke( "$KAPPA_DIR/maths exp='sqrt(ia**2+ib**2)' ia={0} ib={1} out={2}".
                 format(qcube,ucube,gcube) )
         dcube1 = NDG(1)
         invoke( "$KAPPA_DIR/maths exp='atan2(ia,ib)' ia={0} ib={1} out={2}".
                 format(qcube,ucube,dcube1) )

#  The delay values are produced by the atan2 function, which always
#  creates values in the range +/- PI. This means there can be a sudden
#  jump from delay values of +PI to -PI. Such discontinuities will upset
#  our attempts to find smooth functions to model the delay. To avoid
#  this, we offset the delay values to a mean of zero, and then ensure
#  that no values are outside the range of +/- PI.
         invoke( "$KAPPA_DIR/stats {0} clip=\[3,3,3\]".format(dcube1) )
         mean = starutil.get_task_par( "mean", "stats" )
         dcube2 = NDG(1)
         invoke( "$KAPPA_DIR/csub in={0} scalar={1} out={2}".format(dcube1,mean,dcube2) )
         dcube = NDG(1)
         invoke( "$KAPPA_DIR/maths exp=\"'qif((ia<(-pi)),ia+2*pi,qif((ia>pi),ia-2*pi,ia))'\" "
                 "ia={0} pi={1} out={2}".format(dcube2,math.pi,dcube) )

#  We now attempt to produce models of the gain and delay, as a function
#  of time and bolometer position. We model each one in the same way, on
#  the basis that the
         gmodel = NDG(1)
         dmodel = NDG(1)
         for (incube,model) in zip( (gcube,dcube), (gmodel,dmodel) ):
            if incube == gcube:
               msg_out( "Modelling the gain of each {0} bolometer...".format(a))
            else:
               msg_out( "Modelling the phase lag of each {0} bolometer...".format(a))

#  The source regions will bias these models, so we first mask out the
#  source if a mask was supplied.
            if mask != None:
               masked = NDG(1)
               invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".format(incube,masked,mcube) )
            else:
               masked = incube

#  If the mask is very large, it may blank out a whole plane or the vast
#  majority of a plane, leaving very few if any samples to fit. To avoid
#  this we create another copy of the Q/U cube in which masked pixels
#  have a very much higher variance than the non-masked pixels. This high
#  variance will give low weight to the masked pixels, but will allow a
#  result to be calculated even if all pixels are masked. So find the assign a
#  variance of 1.0 to all non-masked pixels, and a variance of 100.0 to all
#  masked pixels.
            if mask != None:
               tmp1 = NDG(1)
               invoke( "$KAPPA_DIR/maths exp=\"'qif((ia==<bad>),100,1)'\" ia={0} out={1}".
                       format(masked,tmp1) )
               wmasked = NDG(1)
               invoke( "$KAPPA_DIR/ndfcopy in={0} out={1}".format(incube,wmasked) )
               invoke( "$KAPPA_DIR/setvar ndf={0} comp=data from={1}".format(wmasked,tmp1) )
            else:
               wmasked = incube

#  All stare positions (i.e. planes in the cube) tend to have very
#  similar structure. So our first guess at the model is given by the mean
#  of all planes (excluding source regions), and is time-invariant.
            model1 = NDG(1)
            invoke( "$KAPPA_DIR/collapse in={0} out={1} estimator=mean wlim=0 axis=3".
                     format(masked,model1) )

#  We improve the model by allowing each individual plane to be a scaled
#  and shifted version of this mean image. Use kappa:normalize to do this.
#  Normalize can loop over rows or columns in a 2D image, but it cannot
#  loop over planes in a cube, so we first reshape the cube into a 2D
#  image in which each row is a vectorised form of one plane in the cube,
#  each containing 1280 values (i.e. 32x40 bolometers). We also reshape
#  the current model to be a 2D image with only a single row.
            flatwmasked = NDG(1)
            invoke( "$KAPPA_DIR/reshape in={0} out={1} shape=\[1280,{2}\]".
                    format(wmasked,flatwmasked,npos) )
            flatmodel = NDG(1)
            invoke( "$KAPPA_DIR/reshape in={0} out={1} shape=\[1280,1\]".
                    format(model1,flatmodel) )

#  Now use normalize. For each row in flatwmasked (i.e. each stare
#  position), it fits the model to the masked data, and stores the scaled
#  and shifted model in the output.
            flatnew = NDG(1)
            invoke( "$KAPPA_DIR/normalize in1={0} in2={1} out={2} loop=yes device={3}".
                    format(flatmodel,flatwmasked,flatnew,ndevice), buffer=True )

#  Reshape the 2D new model back into a 3D cube. Each plane in this cube
#  will now contain a scaled and shifted form of the original model that
#  is a good fit to the masked data.
            model2 = NDG(1)
            invoke( "$KAPPA_DIR/reshape in={0} out={1} shape=\[32,40,{2}\]".
                    format(flatnew,model2,npos) )

#  We now form the residuals between the data and current model, and look
#  for further regularities that we can model in the residuals.
            res1 = NDG(1)
            invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".format(incube,model2,res1 ))

#  Again we mask the residuals in order to avoid the new model components
#  being biassed by the source. Also set up values for mfittrend
#  parameters appropriate for the type of fit that will be used.
            if mask != None:
               rmasked = NDG(1)
               invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".format(res1,rmasked,mcube) )
               pars = "fittype=spline knots=3"
            else:
               rmasked = res1
               pars = "fittype=poly order=1"

#  For each bolometer, fit a 1D smooth curve to the masked residuals. The
#  type of curve used depends on whether we have a mask. If the source
#  has not been masked out, we use a straight line so that the effect of
#  the source on the fit will hopefully be small. If the source has been
#  masked out we can afford to use a more flexible curve, so we use a
#  bi-cubic spline with three knots is used as the curve. Note, mfittrend
#  with PROPBAD=NO leaves zeros in the fit if the fit failed, so we need
#  to check for this. But we cannot just replace all zeros with bad values
#  since there may be some genuine zero values in the fit. So we look for
#  z-columns in the cube that are entirely filled with zeros, and just set
#  those columns bad.
            tmod = NDG(1)
            invoke( "$KAPPA_DIR/mfittrend in={0} out={1} {2} axis=3 rmsclip=4 subtract=no "
                    "ranges=! propbad=no ".format(rmasked,tmod,pars) )
            tmax = NDG(1)
            invoke( "$KAPPA_DIR/collapse in={0} out={1} axis=3 estimator=max".format(tmod,tmax) )
            tmin = NDG(1)
            invoke( "$KAPPA_DIR/collapse in={0} out={1} axis=3 estimator=min".format(tmod,tmin) )
            msk2d = NDG(1)
            invoke( "$KAPPA_DIR/maths exp=\"'qif(((ia==0).and.(ib==0)),<bad>,1)'\" ia={0} ib={1} "
                    "out={2}".format(tmax,tmin,msk2d) )
            msk3d = NDG(1)
            invoke( "$KAPPA_DIR/manic in={0} out={1} axes=\[1,2,0\] lbound=1 ubound={2}".
                    format(msk2d,msk3d,npos) )
            model3 = NDG(1)
            invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".format(tmod,model3,msk3d) )

#  The above fit was made to the residuals left after subtracting the
#  previous model ("model2") from the data. So to form the total model,
#  we add the fit onto "model2".
            model4 = NDG( 1 )
            invoke( "$KAPPA_DIR/add in1={0} in2={1} out={2}".format(model2,model3,model4) )

#  If required, use PCA to model the remaining residuals. We look for
#  spatial structures that are present (at the same pixel coordinates) in
#  several of the residual planes. Such features cannot be astronomical
#  since they are fixed in pixel space (i.e. do not move around the array
#  as a source fixed on the sky would do). We identify such structures
#  using Principal Component Analysis (PCA).
            if npca > 0:
               msg_out( "   Preparing for PCA...")

#  Get the residuals between the original data and the new model, and mask them again.
               res2 = NDG(1)
               invoke( "$KAPPA_DIR/sub in1={0} in2={1} out={2}".format(incube,model4,res2 ))
               if mask != None:
                  rmasked = NDG(1)
                  invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".format(res2,rmasked,mcube) )
                  pars = "fittype=spline knots=5"
               else:
                  rmasked = res2
                  pars = "fittype=poly order=1"

#  Use PCA to identify the correlated features that appear in multiple
#  planes, and return them in tmp3.
               tmp3 = smurfutil.pca( rmasked, npca )

#  Add the new model component onto the previous model.
               model5 = NDG(1)
               invoke( "$KAPPA_DIR/add in1={0} in2={1} out={2}".format(model4,tmp3,model5) )
            else:
               model5 = model4

#  We now have the complete model. Normalise the gain model to a mean
#  value of 1.0 and the delay model to a mean of zero.
            invoke( "$KAPPA_DIR/stats {0}".format(model5) )
            mean = starutil.get_task_par( "mean", "stats" )
            tmp = NDG( 1 )
            if incube == gcube:
               invoke( "$KAPPA_DIR/cdiv in={0} scalar={1} out={2}".format(model5,mean,tmp) )
            else:
               invoke( "$KAPPA_DIR/csub in={0} scalar={1} out={2}".format(model5,mean,tmp) )

# Also need to fill any bad values. This is important if the mask is so
# large that an entire plane is masked out, in which case it will have no
# gain/delay values. Bad values in the final map will be inherited form the
# Q and U values, not the gain and delay values.
            invoke( "$KAPPA_DIR/fillbad in={1} variance=no out={0} size=10 niter=10".format(model,tmp) )

#  Having determined models for the gain and time delay for each bolometer
#  at each time slice, use these models to correct the raw Q and U cubes.
#  The resulting cubes should be flat in the background regions.
         msg_out( "Correcting {0} Q and U using the gain and phase lag models".format(a) )
         qcor = NDG(1)
         invoke( "$KAPPA_DIR/maths exp='(iq*cos(id)-iu*sin(id))/ig' iq={0} iu={1} "
                  "ig={2} id={3} out={4}".format(qcube,ucube,gmodel,dmodel,qcor) )
         ucor = NDG(1)
         invoke( "$KAPPA_DIR/maths exp='(iu*cos(id)+iq*sin(id))/ig' iq={0} iu={1} "
                  "ig={2} id={3} out={4}".format(qcube,ucube,gmodel,dmodel,ucor) )


#  Clean smaller blemishes now that the background is (hopefully) flat.
#  First erase variance, as they are not reliable, upset ffclean, and will
#  be replace later by more reliable variances.
         msg_out( "Removing small blemishes from {0} Q and U images".format(a) )
         invoke( "$KAPPA_DIR/erase {0}.variance report=no ok=yes".format(qcor) )
         qff = NDG( 1 )
         invoke( "$KAPPA_DIR/ffclean in={0} out={1} axes=\[1,2\] box=3 "
                 "clip=\[5,5,5\]".format(qcor,qff) )

         invoke( "$KAPPA_DIR/erase {0}.variance report=no ok=yes".format(ucor) )
         uff = NDG( 1 )
         invoke( "$KAPPA_DIR/ffclean in={0} out={1} axes=\[1,2\] box=3 "
                 "clip=\[5,5,5\]".format(ucor,uff) )

#  This correction should have flattened the background, but it will not
#  have removed the background. So now we find the median value in each cube
#  and remove it. Mask the images first if possible.
         msg_out( "Removing sky backgrounds for {0}".format(a) )
         if mask != None:
            qm = NDG( 1 )
            um = NDG( 1 )
            invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".format(qff,qm,mcube) )
            invoke( "$KAPPA_DIR/copybad in={0} out={1} ref={2}".format(uff,um,mcube) )
         else:
            qm = qff
            um = uff

         invoke( "$KAPPA_DIR/stats {0} order=true".format(qm) )
         median = starutil.get_task_par( "median", "stats" )
         qsub = NDG( 1 )
         invoke( "$KAPPA_DIR/csub in={0} scalar={1} out={2}".format(qff,median,qsub) )

         invoke( "$KAPPA_DIR/stats {0} order=true".format(um) )
         median = starutil.get_task_par( "median", "stats" )
         usub = NDG( 1 )
         invoke( "$KAPPA_DIR/csub in={0} scalar={1} out={2}".format(uff,median,usub) )

#  The rest of this script operates on separate 2D images rather than 3D
#  cubes, so extract all the planes of the cubes into serparate 2D images
#  and give them their correct WCS.
         msg_out("Extracting {0} Q and U planes to separate 2D images".format(a) )
         qoff = NDG( npos )
         uoff = NDG( npos )
         for i in range(0,npos):
            invoke( "$KAPPA_DIR/ndfcopy in={0}\(,,{1}\) out={2} trim=yes".
                    format(qsub,i+1,qoff[i]) )
            invoke( "$KAPPA_DIR/wcscopy ndf={0} like={1} ok=yes".
                    format(qoff[i],qarray[i]) )
            invoke( "$KAPPA_DIR/ndfcopy in={0}\(,,{1}\) out={2} trim=yes".
                    format(usub,i+1,uoff[i]) )
            invoke( "$KAPPA_DIR/wcscopy ndf={0} like={1} ok=yes".
                    format(uoff[i],uarray[i]) )

#  Ensure all images have the required current WCS Frame (as indicated by
#  the DOMAIN parameter).
         invoke( "$KAPPA_DIR/wcsframe ndf={0} frame={1}".format(qoff,domain) )
         invoke( "$KAPPA_DIR/wcsframe ndf={0} frame={1}".format(uoff,domain) )

#  If required, dump the angles and polarised intensities for each stare
#  position.
         if staredir != None:
            for mm in range(len( uoff )):
               qin = qoff[ mm ];
               uin = uoff[ mm ];
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
               invoke( "$KAPPA_DIR/ndfcopy in={0} out={1} trim=yes".format(qoff[0],ref) )

#  We add a WCS Frame with Domain "POLANAL" to define the polarimetric reference
#  direction (we choose the pixel Y axis). POLPACK expects this Frame to be present
#  and uses the first axis as the reference direction (positive rotation is
#  in the sense of rotation from the first to the second axis in the POLANAL
#  Frame). We take a copy of the PIXEL Frame and then rotate it by 90 degs
#  so that the first axis is parallel to the original pixel Y axis. First
#  remove any pre-existing POLANAL Frame.
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

#  If not yet done, create a set of cut-outs from the supplied total
#  intensity map that are aligned in pixel coords with each of the 25
#  Q and U images. Also, if a mask was supplied, mask these cut-out by
#  setting background regions to zero.
            if icuts == None:

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
                  invoke( "$KAPPA_DIR/erase {0}.variance report=no ok=yes".format(iref_tmp) )
                  iref_masked = NDG( 1 )
                  invoke( "$KAPPA_DIR/nomagic in={0} out={1} repval=0".
                          format(iref_tmp,iref_masked) )
               else:
                  iref_masked = iref

#  Create the required aligned cut-outs of this masked iref image.
            icuts = NDG( npos )
            for (ipref,icut) in zip( qoff, icuts ):
               invoke( "$KAPPA_DIR/wcsalign in={0} out={1} ref={2} rebin=no "
                       "method=bilin accept". format(iref_masked,icut,ipref) )

#  Get the elevation (in degs) at the centre of the observation.
            elev = float( invoke("$KAPPA_DIR/fitsmod {0} edit=print keyword=ELSTART".
                                 format( qoff[ npos/2 ] )))

#  Calculate the factors by which to multiply the total intensity image
#  to get the Q and U corrections.
            qfac = ip_p0*math.cos(math.radians(2*ip_angle_c)) + ip_p1*math.cos(math.radians(2*(ip_angle_c+elev+ip_c_0)))
            ufac = ip_p0*math.sin(math.radians(2*ip_angle_c)) + ip_p1*math.sin(math.radians(2*(ip_angle_c+elev+ip_c_0)))

#  Correct the Q and U images.
            qip = NDG( npos )
            uip = NDG( npos )
            invoke( "$KAPPA_DIR/maths exp='ia-ib*pa' ia={0} ib={1} pa={2} out={3}".
                    format(qoff,icuts,qfac,qip))
            invoke( "$KAPPA_DIR/maths exp='ia-ib*pa' ia={0} ib={1} pa={2} out={3}".
                    format(uoff,icuts,ufac,uip))
         else:
            qip = qoff
            uip = uoff

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
#  total Q map. Variances for the mosaic are generated from the spread of
#  input values at each output pixel.
   msg_out( "Combining all Q and U images for all sub-arrays...")
   qmaps_all = NDG( qmaps )
   qaligned = NDG( qmaps_all )
   invoke( "$KAPPA_DIR/wcsalign method=gauss params=\[0,1.2\] rebin=yes in={0} ref={1} "
           "out={2} lbnd=! conserve=no".format(qmaps_all,ref,qaligned) )
   qaligned = qaligned.filter() #  Remove any NDFs that could not be created by wcsalign
   qtotal = NDG( 1 )
   invoke( "$CCDPACK_DIR/makemos method=broad genvar=yes in={0} out={1}".format(qaligned,qtotal) )

#  Do the same for U.
   umaps_all = NDG( umaps )
   ualigned = NDG( umaps_all )
   invoke( "$KAPPA_DIR/wcsalign method=gauss params=\[0,1.2\] rebin=yes in={0} ref={1} "
           "out={2} lbnd=! conserve=no".format(umaps_all,ref,ualigned) )
   ualigned = ualigned.filter() #  Remove any NDFs that could not be created by wcsalign
   utotal = NDG( 1 )
   invoke( "$CCDPACK_DIR/makemos method=broad genvar=yes in={0} out={1}".format(ualigned,utotal) )






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
   if iref == None:
      iref = NDG(1)
      msg_out( "Generating an artificial total intensity image...")
      invoke( "$KAPPA_DIR/maths exp='sqrt(ia**2+ib**2)' ia={0} ib={1} out={2}".format(qtotal,utotal,iref))

#  If a total intensity map was supplied, and we are not using the
#  default pixel size (i.e. the pixel size in the total intensity map),
#  aligned the total intensity map with the reference image.
   elif pixsize:
      tmp = NDG( 1 )
      invoke( "$KAPPA_DIR/wcsalign in={0} out={1} ref={2} method=gauss "
              "params=\[0,1\] rebin=yes conserve=no lbnd=!".format(iref,tmp,ref) )
      iref = tmp

#  Ensure the Q U and I images all have the same bounds, equal to the
#  overlap region between them. To get the overlap region, use MATHS to
#  add them together. Then use ndfcopy to produce the sections from each,
#  which match the overlap area.
   tmp = NDG( 1 )
   invoke( "$KAPPA_DIR/maths exp='ia+ib+ic' ia={0} ib={1} ic={2} out={3}".format(qtotal,utotal,iref,tmp) )
   qtrim = NDG( 1 )
   invoke( "$KAPPA_DIR/ndfcopy {0} like={1} out={2}".format(qtotal,tmp,qtrim) )
   utrim = NDG( 1 )
   invoke( "$KAPPA_DIR/ndfcopy {0} like={1} out={2}".format(utotal,tmp,utrim) )
   itrim = NDG( 1 )
   invoke( "$KAPPA_DIR/ndfcopy {0} like={1} out={2}".format(iref,tmp,itrim) )

#  If required, save the Q, U and I images.
   if qui != None:
      invoke( "$KAPPA_DIR/ndfcopy {0} out={1}.Q".format(qtrim,qui) )
      invoke( "$KAPPA_DIR/ndfcopy {0} out={1}.U".format(utrim,qui) )
      invoke( "$KAPPA_DIR/ndfcopy {0} out={1}.I".format(itrim,qui) )

#  The polarisation vectors are calculated by the polpack:polvec command,
#  which requires the input Stokes vectors in the form of a 3D cube. Paste
#  the 2-dimensional Q, U and I images into a 3D cube.
   planes = NDG( [qtrim,utrim,itrim] )
   cube = NDG( 1 )
   invoke( "$KAPPA_DIR/paste in={0} shift=\[0,0,1\] out={1}".format(planes,cube))

#  Check that the cube has a POLANAL frame, as required by POLPACK.
   try:
      invoke( "$KAPPA_DIR/wcsframe {0} POLANAL".format(cube) )

#  If it does not, see if it has a "POLANAL-" Frame (kappa:paste can
#  cause this by appending "-" to the end of the domain name to account for
#  the extra added 3rd axis).
   except AtaskError:
      invoke( "$KAPPA_DIR/wcsframe {0} POLANAL-".format(cube) )

#  We only arrive here if the POLANAL- frame was found, so rename it to POLANAL
      invoke( "$KAPPA_DIR/wcsattrib {0} set domain POLANAL".format(cube) )

#  Re-instate the required current Frame
   invoke( "$KAPPA_DIR/wcsframe {0} {1}".format(cube,domain) )

#  POLPACK needs to know the order of I, Q and U in the 3D cube. Store
#  this information in the POLPACK enstension within "cube.sdf".
   invoke( "$POLPACK_DIR/polext {0} stokes=qui".format(cube) )

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
   if plot != None:
      msg_out( "Plotting the '{0}' vectors ...".format(plot) )

#  Select vectors where the signal to noise ratio for the Polarised
#  Intentisy value is more than 3. The catalogue is stored in the NDG
#  temporary directory.
      exp = "pi>{0}*dpi".format(snr)
      if maxlen != None:
         exp += "&{0}<{1}".format(plot,maxlen)
      selcat = "{0}/selcat".format(NDG.tempdir)
      invoke( "$CURSA_DIR/catselect catin={0} catout={1} norejcat seltyp=e expr='{2}'".format(outcat,selcat,exp))

#  If a total intensity reference was supplied, display it as a background
#  image, and plot the vectors over the top.
      if iref:
         invoke( "$KAPPA_DIR/lutable mapping=linear coltab=grey device={0}".format(device), buffer=True)
         invoke( "$KAPPA_DIR/display {0}'(,,3)' mode=perc percentiles=\[1,99\] badcol=black device={1}".format(cube,device), buffer=True)
         invoke( "$POLPACK_DIR/polplot {0} clear=no axes=no colmag={1} key=yes style='colour=red' device={2}".format(selcat,plot,device), buffer=True)

#  Otherwise, just plot the vectors.
      else:
         invoke( "$POLPACK_DIR/polplot {0} colmag={1} key=yes style=def device={2}".format(selcat,plot,device), buffer=True)

#  Remove temporary files.
   cleanup()

#  close any diagnostics file
   if diagfd != None:
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

