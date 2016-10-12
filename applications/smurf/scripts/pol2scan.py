#!/usr/bin/env python

'''
*+
*  Name:
*     POL2SCAN

*  Purpose:
*     Create Q and U maps from a set of POL-2 "spin&scan" data
*     files.

*  Language:
*     python (2.7 or 3.*)

*  Description:
*     This script runs SMURF:CALCQU on any raw POL-2 data files specified
*     by parameter IN, to create a set of down-sampled time series files
*     holding Q and U values in each bolometer. These are placed in the
*     directory specified by parameter QUDIR. These time series, together
*     with any Q,U time series files specified directly by parameter IN, are
*     then converted into Q and U maps using SMURF:MAKEMAP. A separate pair
*     of Q and U maps is created for each observation present in the supplied
*     data. These maps, together with any Q,U maps specified by parameter IN,
*     are then coadded to form a final pair of Q and U maps. In addition, a
*     range of information derived from each pair of maps included in the
*     coadd is displayed on the screen, and may also be written to an output
*     text file (see parameter OBSTABLE).
*
*     The final Q and U maps, together with the supplied total intensity
*     reference map (see parameter IPREF) are then used to create a
*     vector catalogue and polarised intensity map (see parameters CAT,
*     PI and DEBIAS).
*
*     Correction for instrumental polarisation is made only if a value
*     is supplied for parameter IPREF.
*
*     By default, the Q, U, I and PI catalogue values, together with the
*     maps specified by parameters "Q", "U" and "PI", are in units of
*     Jy/beam (see parameter Jy).

*  Usage:
*     pol2scan in q u [cat] [ipref] [config] [pixsize] [qudir] [mapdir]
*              [obstable] [retain] [msg_filter] [ilevel] [glevel] [logfile]

*  ADAM Parameters:
*     ALIGN = LOGICAL (Read)
*        If TRUE, and if a non-null value is supplied for parameter REF,
*        then corrections to the telescope pointing are determined and
*        applied when creating the Q and U maps for each observation. To
*        determine this pointing correction for an observation, a total
*        intensity map is created from the raw POL2 data for the observation
*        using the supplied reference map (see parameter REF) to specify
*        the pixel grid. Due to pointing errors, this total intensity map
*        may not be aligned accurately with the reference map (errors of up
*        to 8 arc-seconds have been seen). The positional shift required to
*        minimise the residuals between the shifted total intensity map and
*        the reference map is found, and used to correct the pointing when
*        creating the Q and U map. Note, this option slows down the operation
*        of pol2scan as it requires an extra invocation of smurf:makemap
*        for each observation, to create the total intensity maps (which
*        is not required if ALIGN is FALSE). [TRUE]
*     CAT = LITERAL (Read)
*        The output FITS vector catalogue. No catalogue is created if
*        null (!) is supplied. [!]
*     CONFIG = LITERAL (Read)
*        The MAKEMAP configuration parameter values to use. If a null
*        value (!) or "def" is supplied, the following defaults will be
*        used:
*
*        ast.zero_snr=3
*        ast.zero_snrlo=2
*        maptol=0.05
*        modelorder=(pca,ext,ast,noi)
*        noisecliphigh=3
*        numiter=-20
*        pca.pcathresh=4
*        spikebox=10
*        spikethresh=5
*
*        If a configuration is supplied, it is used in place of the above
*        default configurations. In either case, the following values are
*        always appended to the end of the used config (whether external
*        or defaulted):
*
*        flagslow = 0.01
*        downsampscale = 0
*        noi.usevar=1
*     DEBIAS = LOGICAL (Given)
*        TRUE if a correction for statistical bias is to be made to
*        percentage polarization and polarized intensity in the output
*        vector catalogue specified by parameter CAT and the polarised
*        intensity map specified by parameter PI. [FALSE]
*     GLEVEL = LITERAL (Read)
*        Controls the level of information to write to a text log file.
*        Allowed values are as for "ILEVEL". The log file to create is
*        specified via parameter "LOGFILE. In adition, the glevel value
*        can be changed by assigning a new integer value (one of
*        starutil.NONE, starutil.CRITICAL, starutil.PROGRESS,
*        starutil.ATASK or starutil.DEBUG) to the module variable
*        starutil.glevel. ["ATASK"]
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
*        A group of input files. Each specified file must be one of the
*        following types:
*        - a raw POL-2 data file
*        - a time-series file holding Stokes Q, U or I values
*        - a two-dimensional map holding Stokes Q or U values (Q and U
*        maps must be in units of pW).
*        Any combination of the above types can be supplied.
*     IPBEAMFIX = _LOGICAL (Read)
*        Should the supplied total intensity reference image (parameter
*        IPREF) be modified so that its beam shape matches the expected
*        IP beam shape at the elevation of each supplied POL2 observation,
*        before doing IP correction? This is currently an experimental
*        feature. [FALSE]
*     IPFCF = _REAL (Read)
*        The FCF that should be used to convert the supplied IP REF map
*        to pW. This parameter is only used if the supplied IPREF map is
*        not already in units of pW, and if the FCF is not stored in the
*        FITS extension of the map. The suggested default is the standard
*        FCF for the band concerned (450 or 840). Just press return at
*        the prompt to use this default, or enter a new value if the
*        suggested value is not the FCF that was actually used to create
*        the map. []
*     IPREF = NDF (Read)
*        A 2D NDF holding a map of total intensity within the sky area
*        covered by the input POL2 data, in units of pW, mJy/beam, Jy/beam,
*        mJy/arcsec**2, Jy/arcsec**2 ("^" may be used in place of "**").
*        If supplied, the returned Q and U maps will be corrected for
*        instrumental polarisation, based on the total intensity values in
*        IPREF. The supplied IPREF map need not be pre-aligned with the
*        output Q and U maps - it will be resampled as necessary using a
*        transformation derived from its WCS information. The total intensity
*        values in this map are also used to calculate the percentage
*        polarisation values stored in the output vector catalogue
*        specified by parameter CAT. [!]
*     JY = _LOGICAL (Read)
*        If TRUE, the output catalogue, and the output maps specified by
*        parameters "Q", "U" and "PI", will be in units of Jy/beam. Otherwise
*        they will be in units of pW (in this case, the I values in the output
*        catalogue will be scaled to take account of the different FCFs
*        for POL-2 and non-POL-2 observations). [True]
*     LOGFILE = LITERAL (Read)
*        The name of the log file to create if GLEVEL is not NONE. The
*        default is "<command>.log", where <command> is the name of the
*        executing script (minus any trailing ".py" suffix), and will be
*        created in the current directory. Any file with the same name is
*        over-written. The script can change the logfile if necessary by
*        assign the new log file path to the module variable
*        "starutil.logfile". Any old log file will be closed befopre the
*        new one is opened. []
*     MAPDIR = LITTERAL (Read)
*        The name of a directory in which to put the Q and U maps made from
*        each individual observation supplied via "IN", before coadding
*        them (the QMAP and UMAP parameters specify the finaled coadded
*        Q and U maps). If null is supplied, they are placed in the same
*        temporary directory as all the other intermediate files and so
*        will be deleted when the scrip exists (unless parameter RETAIN
*        is set TRUE). [!]
*     MSG_FILTER = LITERAL (Read)
*        Controls the default level of information reported by Starlink
*        atasks invoked within the executing script. This default can be
*        over-ridden by including a value for the msg_filter parameter
*        within the command string passed to the "invoke" function. The
*        accepted values are the list defined in SUN/104 ("None", "Quiet",
*        "Normal", "Verbose", etc). ["Normal"]
*     NORTH = LITERAL (Read)
*        Specifies the celestial coordinate system to use as the reference
*        direction in any newly created Q and U time series files. For
*        instance if NORTH="AZEL", then they use the elevation axis as the
*        reference direction, and if "ICRS" is supplied, they use the ICRS
*        Declination axis. If "TRACKING" is supplied, they use north in the
*        tracking system - what ever that may be. ["TRACKING"]
*     OBSTABLE = LITERAL (Read)
*        The path of a new text file to create, to which will be written
*        statistics describined the Q and U maps for each individual
*        observation present in the list of files specified by parameter IN.
*        No file is created if null (!) is supplied. The values are written
*        in the form of a TOPCAT "ascii" table, with one row for each
*        observation. The columns are:
*         - UT: UT date of observation
*         - OBS: Observation number
*         - SUBSCAN: The first subscan included in the map
*         - WVM: The mean of the starting and ending WVM tau values
*         - NEFD_Q: The measured NEFD in the Q map (mJy.sec^(0.5))
*         - NEFD_U: The measured NEFD in the U map (mJy.sec^(0.5))
*         - NEFD_EXP: The expected NEFD based on WVM and elevation (mJy.sec^(0.5))
*         - TIME: The elapsed time of the data included in the maps (s)
*         - SIZE_Q: The total area of the source regions in the Q map (square arc-mins)
*         - SIZE_U: The total area of the source regions in the U map (square arc-mins)
*         - RMS_Q: The RMS Q value within the source regions (pW)
*         - RMS_U: The RMS U value within the source regions (pW)
*         - NBOLO_Q: Number of bolometers contributing to Q map
*         - NBOLO_U: Number of bolometers contributing to U map
*         - DX: Pointing correction in azimuth (arc-sec)
*         - DY: Pointing correction in elevation (arc-sec)
*        The last two columns (DX and DY) are only created if parameter
*        ALIGN is TRUE. [!]
*     PI = NDF (Read)
*        An output NDF in which to return the polarised intensity map.
*        No polarised intensity map will be created if null (!) is
*        supplied. [!]
*     PIXSIZE = _REAL (Read)
*        Pixel dimensions in the output Q and U maps, in arcsec. The default
*        is 4 arc-sec for 850 um data and 2 arc-sec for 450 um data. []
*     Q = NDF (Read)
*        The output NDF in which to return the total Q intensity map including
*        all supplied observations.
*     QUDIR = LITTERAL (Read)
*        The name of a directory in which to put the Q and U time series
*        generated by SMURF:CALCQU. If null (!) is supplied, they are placed
*        in the same temporary direcory as all the other intermediate files. [!]
*     U = NDF (Read)
*        The output NDF in which to return the total U intensity map including
*        all supplied observations.
*     REF = NDF (Read)
*        An optional map defining the pixel grid for the output maps. If
*        no value is specified for REF on the command line, it defaults
*        to the value supplied for parameter IPREF. See also parameter
*        ALIGN. []
*     RETAIN = _LOGICAL (Read)
*        Should the temporary directory containing the intermediate files
*        created by this script be retained? If not, it will be deleted
*        before the script exits. If retained, a message will be
*        displayed at the end specifying the path to the directory. [FALSE]

*  Copyright:
*     Copyright (C) 2015, 2016 East Asian Observatory.
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
*     20-MAY-2015 (DSB):
*        Original version
*     21-SEP-2015 (DSB):
*        Update default config to use PCA instead of COM/GAI.
*     28-SEP-2015 (DSB):
*        - Add azel pointing correction for old data.
*        - Allow default config values to be used as the defaults even
*          if an external config is supplied.
*     29-SEP-2015 (DSB):
*        - Add parameter PIXSIZE.
*     8-OCT-2015 (DSB):
*        Only correct azel pointing error for data between 20150606 and
*        20150930.
*     26-NOV-2015 (DSB):
*        Add parameters CAT, PI and DEBIAS.
*     3-MAR-2016 (DSB):
*        - Rename parameter IREF as IPREF.
*        - Add parameters REF and ALIGN.
*        - Remove parameters QREF and UREF.
*     6-MAY-2016 (DSB):
*        Do not add default values for optional parameters to a supplied
*        config.
*     12-MAY-2016 (DSB):
*        - Improve pointing correction by using CORLIMIT option when
*        running kappa:align2d.
*        - Use the Epoch of the POL2 observation rather than the Epoch of
*        the IP reference map when determining the expected IP beam shape.
*     26-MAY-2016 (DSB):
*        Do not use the AST mask when determining the pointing correction if
*        the AST mask contains very few pixels.
*     1-SEP-2016 (DSB):
*        Allow IPREF maps in units of mJy/beam as well as pW.
*     13-SEP-2016 (DSB):
*        - Allow the IN parameter to be used to specify raw data, Q/U
*        time-streams or Q/U maps.
*        - Ignore duplicated input files specified within IN.
*     15-SEP-2016 (DSB):
*        - Store pointing corrections in FITS extensions of Q and U maps.
*     16-SEP-2016 (DSB):
*        Mosaic per-osevration EXP_TIME and WEIGHTS extension NDFs and
*        store in final out Q and U maps.
*     20-SEP-2016 (DSB):
*        Report an error if any of the input maps has no quality array.
*     22-SEP-2016 (DSB):
*        - Take account of the difference in POL2 and non-POL2 FCFs when
*        calculating percentage polarisation values.
*        - Add parameter JY.
*     29-SEP-2016 (DSB):
*        Relax the requirment for a "far away" object. An object is far
*        away (and thus unusable) if its map has a pixel bound greater
*        than 10000. This assumes that the required source is at the
*        pixel origin.
*     12-OCT-2016 (DSB):
*        - Report a warning if any raw data sub-scan are omitted from the
*        list of input files.
*        - Report an error if any supplied input Q or U maps are in units
*        other than pW.
'''

import os
import math
import shutil
import starutil
from starutil import invoke
from starutil import NDG
from starutil import Parameter
from starutil import ParSys
from starutil import msg_out
from starutil import AtaskError
from starutil import get_fits_header
from starutil import get_task_par

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



#  A function to calculate the following threee statistics from the
#  supplied Q or U map:
#     1 - The estimated noise level at the map centre in pW (based on the
#         NDF Variance array). Returned as -1.0 if the map appears to be
#         for a different source.
#     2 - The source size in square arc-seconds.
#     3 - The RMS value of the source in pW.
#  The source pixels are defined by the AST mask created by makemap and
#  stored in the Quality array of the supplied NDF. The
def calc_stats(ndf):

#  Get info about the supplied NDF.
   invoke("$KAPPA_DIR/ndftrace ndf={0}".format(ndf))
   pixsize = float( get_task_par( "fpixscale(1)", "ndftrace" ))
   lbnd1 = int( get_task_par( "lbound(1)", "ndftrace" ) )
   ubnd1 = int( get_task_par( "ubound(1)", "ndftrace" ) )
   lbnd2 = int( get_task_par( "lbound(2)", "ndftrace" ) )
   ubnd2 = int( get_task_par( "ubound(2)", "ndftrace" ) )

#  Can't do this if no quality.
   if not get_task_par( "quality", "ndftrace"):
      raise starutil.InvalidParameterError("Supplied map {0} has no "
               "Quality array.".format( ndf ) )

#  We want the noise at the centre of the map because we want it to be
#  comparable to the noise used in the expected NEFD calculations. But it
#  is hard to calculate the noise at the centre because of the presence of
#  potentially bright sources. So instead we calculate it in the background
#  region, and then scale it down assuming that the noise goes as 1/root(exptime).
#  To do this we measure the mean exptime in the background and the mean
#  exptime in the source and base the scaling on their ratio. The background
#  region used is defined as the outside of the AST mask, excluding a rim
#  around the edge defined by exptime being lower than the mean. First get
#  the mean exp_time value, then create a mask by the exp_time below the
#  mean with bad values.
   invoke("$KAPPA_DIR/stats ndf={0}.more.smurf.exp_time".format(ndf))
   mean_exp_time = get_task_par( "mean", "stats" )
   edgemask = NDG(1)
   invoke("$KAPPA_DIR/thresh in={0}.more.smurf.exp_time out={1} "
          "thrlo={2} newlo=bad thrhi=1E10 newhi=bad".
          format(ndf,edgemask,mean_exp_time))

#  Note the pixel coords at the maximum exp_time value.
   xcen = get_task_par( "maxpos(1)", "stats" )
   ycen = get_task_par( "maxpos(2)", "stats" )

#  Create another mask that is bad inside the source area defined by the
#  AST mask (created by makemap), and unity everywhere else. First set
#  the bad bits so that any flagged (i.e. background) pixels are treated as
#  bad. Since POL2 DR only uses AST flagging (not FLT or COM), this is OK.
#  Only the source pixels remain good after this.
   invoke("$KAPPA_DIR/setbb ndf={0} bb=255".format(ndf))

#  Find the count, mean value and standard deviation of the source pixels.
   invoke("$KAPPA_DIR/stats ndf={0}".format(ndf))
   source_size = get_task_par( "numgood", "stats" )
   if source_size > 1 :
      mean = get_task_par( "mean", "stats" )
      sigma = get_task_par( "sigma", "stats" )
      source_rms = math.sqrt( mean*mean + sigma*sigma )
   else:
      source_rms = "null"

#  Convert the source size from pixels to square arc-seconds.
   source_size *= (pixsize*pixsize)

#  If any of the pixel bounds look to be an unreasonably large way from
#  the pixel origin, it probably means that the observation was for some
#  other field other than the one covered by the reference IP map.
   if ( abs(lbnd1) > 10000 or abs(lbnd2) > 10000 or
        abs(ubnd1) > 10000 or abs(ubnd2) > 10000 ):
      bad = True
   else:
      bad= False

#  Create a map that is unity where ever the Q or U map is now bad, and is bad
#  where ever the Q or U map is not bad (i.e. the source region). Also include
#  the earlier mask that excludes the edge of the observation.
   fullmask = NDG(1)
   invoke( "$KAPPA_DIR/maths exp=\"'qif(((ia==<bad>).and.(ib!=<bad>)),1,<bad>)'\" "
           "ia={0} ib={1} out={2}".format(ndf,edgemask,fullmask))

#  Reset the bad bits mask in the map to its original state (zero). This
#  brings back the original source region pixel values.
   invoke("$KAPPA_DIR/setbb ndf={0} bb=0".format(ndf))

#  Also reset the bad bits mask in the fullmask created above (it will have
#  been inherited from "ndf").
   invoke("$KAPPA_DIR/setbb ndf={0} bb=0".format(fullmask))

#  Multiply the Q or U map by the fullmask, and find the mean variance value
#  for the remaining pixels (the background pixels). Take it's square
#  root to get the noise in the background region.
   background = NDG(1)
   invoke( "$KAPPA_DIR/mult in1={0} in2={1} out={2}".
           format(ndf,fullmask,background))

   invoke("$KAPPA_DIR/stats ndf={0} comp=var".format(background))
   noise = math.sqrt( float( get_task_par( "mean", "stats" )))

#  Now we need to reduce this noise value so that it represents an
#  estimate of the noise in the centre of the map. Find the mean exp_time
#  in the background region used above. First apply the mask to the
#  exp_time array, and then find the stats of the remaining values.
   masked_exptime = NDG(1)
   invoke("$KAPPA_DIR/mult in1={0}.more.smurf.exp_time in2={1} out={2}".
          format(ndf,fullmask,masked_exptime))
   invoke("$KAPPA_DIR/stats ndf={0}".format(masked_exptime))
   back_exptime = get_task_par( "mean", "stats" )

#  Now find the mean exp_time within 3 arc-mins of the centre. The centre
#  is defined as the point where the exp_time value is highest. We know
#  this position in pixel coords, so define the circle in pixel coords. It's
#  good enough...
   if not bad:
      ardfile = NDG.tempfile()
      fd = open(ardfile,"w")
      fd.write("COFRAME(PIXEL)\n")
      fd.write("CIRCLE({0},{1},{2})\n".format(xcen,ycen,(3*60)/pixsize))
      fd.close()
      invoke("$KAPPA_DIR/aperadd ndf={0}.more.smurf.exp_time ardfile={1}".
             format(ndf,ardfile))
      centre_exptime = get_task_par( "mean", "aperadd" )

#  Correct the returned noise value.
      noise *= math.sqrt(back_exptime/centre_exptime)
   else:
      noise = -1.0

   return ( noise, source_size, source_rms )








#  Main entry...
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

   params.append(starutil.ParNDG("IN", "The input POL2 data",
                                 get_task_par("DATA_ARRAY","GLOBAL",
                                              default=Parameter.UNSET)))

   params.append(starutil.ParNDG("Q", "The total output Q intensity map",
                                 default=None, exists=False, minsize=1,
                                 maxsize=1 ))

   params.append(starutil.ParNDG("U", "The total output Q intensity map",
                                 default=None, exists=False, minsize=1,
                                 maxsize=1 ))

   params.append(starutil.Par0S("CAT", "The output FITS vector catalogue",
                                 default=None, noprompt=True))

   params.append(starutil.ParNDG("IPREF", "Reference map defining IP correction",
                                 default=None, noprompt=True, minsize=0, maxsize=1 ))

   params.append(starutil.Par0S("CONFIG", "Map-maker tuning parameters",
                                "def", noprompt=True))

   params.append(starutil.Par0F("PIXSIZE", "Pixel size (arcsec)", None,
                                 maxval=1000, minval=0.01, noprompt=True))

   params.append(starutil.Par0S("QUDIR", "Directory in which to save new "
                                "Q/U time series", None, noprompt=True))

   params.append(starutil.Par0S("MAPDIR", "Directory in which to save new "
                                "Q/U maps before they are co-added", None,
                                noprompt=True))

   params.append(starutil.Par0S("OBSTABLE", "Output text file holding "
                                "info about individual observations", None,
                                noprompt=True))

   params.append(starutil.Par0L("IPBEAMFIX", "Convolve the IPREF map to "
                                "the expected IP beam shape?", False,
                                noprompt=True))

   params.append(starutil.Par0L("DEBIAS", "Remove statistical bias from P"
                                "and IP?", False, noprompt=True))

   params.append(starutil.ParNDG("PI", "The output polarised intensity map",
                                 default=None, noprompt=True, exists=False,
                                 minsize=0, maxsize=1 ))

   params.append(starutil.Par0L("ALIGN", "Determine pointing corrections?", True,
                                 noprompt=True))

   params.append(starutil.Par0L("RETAIN", "Retain temporary files?", False,
                                 noprompt=True))

   params.append(starutil.ParNDG("REF", "Reference map defining the pixel grid", default=None,
                                 noprompt=True, minsize=0, maxsize=1 ))

   params.append(starutil.ParChoice( "NORTH", ("TRACKING","FK5","ICRS","AZEL",
                                     "GALACTIC","GAPPT","FK4","FK4-NO-E",
                                     "ECLIPTIC"), "Celestial system to "
                                     "use as reference direction", "TRACKING",
                                     noprompt=True ))

   params.append(starutil.Par0F("IPFCF", "FCF needed to convert IPREF map to pW"))

   params.append(starutil.Par0L("Jy", "Should outputs be converted from pW to Jy/beam?",
                 True, noprompt=True))


#  Initialise the parameters to hold any values supplied on the command
#  line.
   parsys = ParSys( params )

#  It's a good idea to get parameter values early if possible, in case
#  the user goes off for a coffee whilst the script is running and does not
#  see a later parameter propmpt or error...

#  Get the input POL-2 data files. They should be supplied as the first item on
#  the command line, in the form of a Starlink "group expression" (i.e.
#  the same way they are supplied to other SMURF commands such as makemap).
   indata = parsys["IN"].value

#  Now get the Q and U output maps.
   qmap = parsys["Q"].value
   umap = parsys["U"].value

#  Get the output catalogue.
   outcat = parsys["CAT"].value

#  See if statistical debiasing is to be performed.
   debias = parsys["DEBIAS"].value

#  See if the beam is to be corrected in the IPREF map.
   ipbeamfix = parsys["IPBEAMFIX"].value

#  Now get the PI value to use.
   pimap = parsys["PI"].value

#  The user-supplied makemap config, and pixel size.
   config = parsys["CONFIG"].value
   pixsize = parsys["PIXSIZE"].value
   if pixsize:
      pixsize_par = "pixsize={0}".format(pixsize)
   else:
      pixsize_par = ""

#  See if temp files are to be retained.
   retain = parsys["RETAIN"].value

#  Get the reference maps
   ipref = parsys["IPREF"].value
   if not ipref:
      ipref = "!"
      parsys["REF"].default = None
   else:
      parsys["REF"].default = ipref

#  If the IPREF map is in units of mJy/beam, convert it to pW using the FCF
#  in the "FCF" FITS header if available, or the standard FCF for the
#  wavelength otherwise.
      invoke("$KAPPA_DIR/ndftrace ndf={0} quiet".format(ipref) )
      units = get_task_par( "UNITS", "ndftrace" ).replace(" ", "")
      if units != "pW":

         try:
            filter = int( float( get_fits_header( ipref, "FILTER", True )))
         except starutil.NoValueError:
            filter = 850
            msg_out( "No value found for FITS header 'FILTER' in {0} - assuming 850".format(ipref))

         if filter != 450 and filter != 850:
            raise starutil.InvalidParameterError("Invalid FILTER header value "
                   "'{0} found in {1}.".format( filter, ipref ) )

         if units == "mJy/beam":
            if filter == 450:
               fcf = 491000.0
            else:
               fcf = 537000.0

         elif units == "Jy/beam":
            if filter == 450:
               fcf = 491.0
            else:
               fcf = 537.0

         elif units == "mJy/arcsec**2" or units == "mJy/arcsec^2" :
            if filter == 450:
               fcf = 4710
            else:
               fcf = 2340

         elif units == "Jy/arcsec**2" or units == "Jy/arcsec^2" :
            if filter == 450:
               fcf = 4.71
            else:
               fcf = 2.34

         else:
            raise starutil.InvalidParameterError("IPREF map {0} has unsupported units {1}".
                                                 format(ipref, units) )

         fcfhead = get_fits_header( ipref, "FCF" )
         if fcfhead != None:
            fcfhead = float( fcfhead )
            ratio = fcfhead/fcf
            if ratio < 0.5 or ratio > 2.0:
               msg_out("WARNING: IPREF map {0} has units {1} but the FCF header is {2} "
                       "- which looks wrong (the expected FCF is {3}).".
                       format(ipref, units, fcfhead, fcf) )
            fcf = fcfhead

         parsys["IPFCF"].default = fcf
         fcf = parsys["IPFCF"].value

         msg_out( "Converting IPREF map ({0}) from {1} to pW using FCF={2}...".
                  format(ipref,units,fcf))
         iprefpw = NDG(1)
         invoke("$KAPPA_DIR/cdiv in={0} scalar={1} out={2}".format(ipref,fcf,iprefpw) )
         ipref=iprefpw

#  See if we should convert pW to Jy/beam.
   jy = parsys["JY"].value

#  Determine the waveband and get the corresponding FCF values with and
#  without POL2 in the beam.
   try:
      filter = int( float( starutil.get_fits_header( indata[0], "FILTER", True )))
   except NoValueError:
      filter = 850
      msg_out( "No value found for FITS header 'FILTER' in {0} - assuming 850".format(indata[0]))

   if filter == 450:
      fcf1 = 962.0
      fcf2 = 491.0
   elif filter == 850:
      fcf1 = 725.0
      fcf2 = 537.0
   else:
      raise starutil.InvalidParameterError("Invalid FILTER header value "
             "'{0} found in {1}.".format( filter, indata[0] ) )


#  The reference map that defines the required pixel grid in the Q/U maps.
   ref = parsys["REF"].value
   if not ref:
      ref = "!"
      align = False

#  See if we should determine pointing corrections.
   else:
      align = parsys["ALIGN"].value

#  See where to put new Q and U maps for individual observations, and
#  ensure the directory exists.
   mapdir =  parsys["MAPDIR"].value
   if not mapdir:
      mapdir = NDG.tempdir
   elif not os.path.exists(mapdir):
      os.makedirs(mapdir)

#  See where to put new Q and U time series, and ensure the directory exists.
   qudir =  parsys["QUDIR"].value
   if not qudir:
      qudir = NDG.tempdir
   elif not os.path.exists(qudir):
      os.makedirs(qudir)

#  See if a table holding info about individual observations is to be
#  created.
   obstable =  parsys["OBSTABLE"].value

#  Get the reference direction.
   north = parsys["NORTH"].value












#  Classify each input data file as raw, QU time-series or QU map. Create
#  three separate text files containing all input NDFs of each type (plus
#  a fourth holing non-POL2 data). Also, create another text file
#  containing a list of ony missing raw sub-scan files.
   junks = NDG.tempfile()
   inraws = NDG.tempfile()
   inquis = NDG.tempfile()
   inmaps = NDG.tempfile()
   rawinfo = NDG.tempfile()
   missing = NDG.tempfile()
   invoke("$SMURF_DIR/pol2check in={0} quiet=yes junkfile={1} mapfile={2} "
          "rawfile={3} stokesfile={4} rawinfo={5} missing={6}".
          format(indata,junks,inmaps,inraws,inquis,rawinfo,missing))

#  Warn about any non-POL2 input data files that are being ignored.
   if get_task_par( "JUNKFOUND", "pol2check" ):
      msg_out( " ")
      msg_out( "WARNING: The following inappropriate input data files are "
               "being ignored: " )
      with open( junks ) as f:
         msg_out( f.read() )
      msg_out( " ")

#  Warn about any missing raw data scub-scans.
   if os.path.isfile( missing ):
      msg_out( " ")
      msg_out( "WARNING: The raw data files for the following sub-scans seem "
               "to be missing from the supplied list of input files: " )
      with open( missing ) as f:
         msg_out( f.read() )
      msg_out( " ")

#  Initialise the list of all Stokes time-series files to be processed by
#  makemap so that it holds any Stokes time-series files supplied by
#  parameter IN.
   allquis = NDG.tempfile()
   if get_task_par( "STOKESFOUND", "pol2check" ):
      shutil.copyfile( inquis, allquis )

#  Initialise the list of all Stokes maps to be included in the final Q
#  and U maps so that it holds any maps supplied by parameter IN. Check
#  that any supplied maps are in units of pW.
   allmaps = NDG.tempfile()
   if get_task_par( "MAPFOUND", "pol2check" ):
      shutil.copyfile( inmaps, allmaps )

      with open(allmaps) as infile:
         lines = infile.readlines()
      paths = [line.strip() for line in lines]
      for path in paths:
         invoke("$KAPPA_DIR/ndftrace ndf={0} quiet".format(path) )
         units = get_task_par( "UNITS", "ndftrace" ).replace(" ", "")
         if units != "pW":
            raise starutil.InvalidParameterError("All supplied Q and U "
                 "maps must be in units of 'pW', but '{0}' has units '{1}'.".
                 format(path,units))
   else:
      open( allmaps, 'a').close()



#  If any raw analysed intensity files were supplied, use smurf:calcqu to
#  convert them into Stokes paramater time-series files.
   if get_task_par( "RAWFOUND", "pol2check" ):
      msg_out( "Calculating Q, U and I time streams from raw analysed intensity data...")

#  Get a dict in which each key is an observation identifier of the form
#  <UT>_<OBS>, and each value is a list of raw data files for the observation.
      with open(inraws) as infile:
         lines = infile.readlines()
      paths = [line.strip() for line in lines]

      with open(rawinfo) as infile:
         lines = infile.readlines()
      infos = [line.strip() for line in lines]

      rawlist = {}
      for (path,id) in zip( paths, infos ):
         if id in rawlist:
            if path not in rawlist[id]:
               rawlist[id].append( path )
         else:
            rawlist[id] = [ path ]

#  Run calcqu separately on each observation.
      nobs = len(rawlist)
      iobs = 0
      for id in rawlist:
         iobs += 1
         msg_out("   {0}/{1}: Processing {2} raw data files from observation {3} ... ".
                 format(iobs,nobs,len(rawlist[ id ]), id ) )

#  Create an NDG object holding the raw POL2 files for the current
#  observation.
         rawdata = NDG( rawlist[ id ] )

#  Use CALCQU to create the new Q and U time streams from the supplied
#  analysed intensity time streams. Put them in the QUDIR directory.
         new_q = NDG.tempfile()
         new_u = NDG.tempfile()
         new_i = NDG.tempfile()
         try:
            invoke("$SMURF_DIR/calcqu in={0} lsqfit=yes config=def outq={1}/\*_QT "
                   "outu={1}/\*_UT outi={1}/\*_IT fix=yes north={2} outfilesi={3} "
                   "outfilesq={4} outfilesu={5}".
                   format( rawdata, qudir, north, new_i, new_q, new_u ) )

#  Append the new Stokes parameter time series files created above to the
#  list of all Stokes parameter time series files.
            with open(allquis, 'a') as outfile:
               for fname in ( new_q, new_u, new_i ):
                   if os.path.isfile( fname ):
                       with open(fname) as infile:
                          outfile.write(infile.read())

         except starutil.AtaskError as err:
            msg_out( err )
            msg_out( "\nAn error occurred within CALCQU. The above observation will be ignored.\nContinuing to process any remaining observations...\n" )

#  Dictionaries holding the stats for the Q and U maps made from each chunk.
   wvm = {}
   elapsed_time = {}
   nbolo_used_q = {}
   nbolo_used_u = {}
   nefd_q = {}
   nefd_u = {}
   nefd_expected = {}
   source_size_q = {}
   source_rms_q = {}
   noise_u = {}
   source_size_u = {}
   source_rms_u = {}
   pointing_dx = {}
   pointing_dy = {}

#  If we have some  time-series files to process...
   if os.path.isfile(allquis):

#  Create a text file holding information about all the Stokes time-series
#  files to be processed. For each one, get the Stokes parameter (Q, U or I)
#  and a key that is unique for the chunk of data, of the form
#  "<UT>_<OBS>_<SUBSCAN>".
      stokesinfo = NDG.tempfile()
      quindg = NDG("^{0}".format(allquis) )
      invoke("$SMURF_DIR/pol2check in={0} quiet=yes stokesinfo={1}".
             format(quindg,stokesinfo))

#  Set up three dicts - one each for Q, U and I. Each key is as described
#  above. Each value is a list of paths for NDFs holding data with the same
#  key and the same Stokes parameter (Q, U or I).
      with open(allquis) as infile:
         lines = infile.readlines()
      paths = [line.strip() for line in lines]

      with open(stokesinfo) as infile:
         lines = infile.readlines()
      infos = [line.strip() for line in lines]

      ilist = {}
      qlist = {}
      ulist = {}
      for (path,info) in zip( paths, infos ):
         (stokes,id) = info.split()
         if stokes == "Q":
            if id in qlist:
               if path not in qlist[id]:
                  qlist[id].append( path )
            else:
               qlist[id] = [ path ]

         elif stokes == "U":
            if id in ulist:
               if path not in ulist[id]:
                  ulist[id].append( path )
            else:
               ulist[id] = [ path ]

         else:
            if id in ilist:
               if path not in ilist[id]:
                  ilist[id].append( path )
            else:
               ilist[id] = [ path ]

#  Create a config file to use with makemap.
      conf = NDG.tempfile()
      fd = open(conf,"w")

#  If a non-default config was supplied, use it.
      if config and config != "def":
         fd.write("{0}\n".format(config))

#  Otherwise, use the default values.
      else:
         fd.write("ast.zero_snr=3\n")
         fd.write("ast.zero_snrlo=2\n")
         fd.write("maptol=0.05\n")
         fd.write("modelorder=(pca,ext,ast,noi)\n")
         fd.write("noisecliphigh=3\n")
         fd.write("numiter=-20\n")
         fd.write("pca.pcathresh=4\n")
         fd.write("spikebox=10\n")
         fd.write("spikethresh=5\n")

#  Now put in values that are absolutely required by this script. These
#  over-write any values in the user-supplied config.
      fd.write("noi.usevar=1\n")
      fd.write("flagslow=0.01\n")
      fd.write("downsampscale=0\n")
      fd.close()

      if len(qlist) == 1:
         msg_out( "Only one observation supplied" )

#  Dictionaries holding the Q and U maps for each observation chunk.
      qmaps = {}
      umaps = {}

#  Loop over all Q time series files. Each separate observation will
#  usually have one Q time series file (although there may be more if the
#  observation was split into two or more discontiguous chunks). We form
#  Q and U maps separately for each observation chunk present in the supplied
#  list of input raw data.
      for key in qlist:

#  Get the Q, U and I time stream files for the current observation chunk.
         qsdf = NDG( qlist[ key ] )

         if key in ulist:
            usdf = NDG( ulist[ key ] )
         else:
            usdf = None

         if ilist and ( key in ilist ):
            isdf = NDG( ilist[ key ] )
         else:
            isdf = None

#  Check that the corresponding U and I time series files exist.
         if usdf and (not ilist or isdf):
            msg_out("\n>>>>   Making Q and U maps from {0}...\n".format(key) )

#  AZ/EL pointing correction, for data between 20150606 and 20150930.
            ut = int(get_fits_header( qsdf[0], "UTDATE", True ))
            if ut >= 20150606 and ut <= 20150929:
               pntfile = NDG.tempfile()
               fd = open(pntfile,"w")
               fd.write("# system=azel\n")
               fd.write("# tai dlon dlat\n")
               fd.write("54000 32.1 27.4\n")
               fd.write("56000 32.1 27.4\n")
               fd.close()
            else:
               pntfile = "!"

#  If we are applying pointing correction to each observation, we create an
#  I map and determine any extra pointing corrections that are needed to
#  bring the I map into alignment with the reference map.
            if align and isdf:

#  Issue a warning if we have no I time-stream files.
               msg_out( "Making a map from the I time series...")

#  Create a config file to use when creating the I map. The I map needs an
#  FLT model because  it is derived from the background level in the time
#  stream data which can drift, rather then the modulation amplitude which
#  is insensitive to drift. Also mask out edge regions more aggresively since
#  we do not want spurious structures round the edges to upset the alignment
#  process in kappa:align2d.
               iconf = NDG.tempfile()
               if not os.path.isfile(iconf):
                  fd = open(iconf,"w")
                  fd.write("^$STARLINK_DIR/share/smurf/dimmconfig_jsa_generic.lis\n")
                  fd.write("hitslimit=1\n")
                  fd.write("maptol=0.1\n")
                  fd.write("flagslow=0.01\n")
                  fd.write("downsampscale=0\n")
                  fd.write("noi.usevar=1\n")
                  fd.close()

#  Create the I map.
               imap = NDG( 1 )
               try:
                  invoke("$SMURF_DIR/makemap in={0} config=^{1} out={2} ref={3} pointing={4}".
                        format(isdf,iconf,imap,ref,pntfile))
               except starutil.AtaskError as err:
                  imap = None
                  pointing_dx[key] = "null"
                  pointing_dy[key] = "null"
                  msg_out( "\nWARNING: No I map could be created for the "
                           "current observation or chunk of observation. "
                           "Therefore the final Q and U maps cannot be "
                           "corrected for pointing errors.\n" )

#  See what translations (in pixels) are needed to align the imap with
#  the reference map. The determination of the shift is more accurate if
#  we first mask out background areas. Use the AST mask to define source
#  pixels, but only if the mask contains a reasonable number of pixels
#  (very faint sources will have very small or non-existant AST masks).
               if imap:
                  invoke("$KAPPA_DIR/showqual ndf={0}".format(imap))
                  if get_task_par( "QNAMES(1)", "showqual" ) == "AST":
                     bb = 1
                  elif get_task_par( "QNAMES(2)", "showqual" ) == "AST":
                     bb = 2
                  elif get_task_par( "QNAMES(3)", "showqual" ) == "AST":
                     bb = 4
                  else:
                     bb = 0

                  if bb > 0:
                     invoke("$KAPPA_DIR/setbb ndf={0} bb={1}".format(imap,bb))

#  Clear badbits to use the whole map if the above masking results in too
#  few pixels.
                     invoke("$KAPPA_DIR/stats ndf={0}".format(imap))
                     nused = float( get_task_par( "numgood", "stats" ) )
                     if nused < 400:
                        invoke("$KAPPA_DIR/setbb ndf={0} bb=0".format(imap))

#  Find the pixel shift that aligns features in this masked, trimmed I map with
#  corresponding features in the reference map.
                  try:
                     invoke("$KAPPA_DIR/align2d ref={0} out=! in={1} form=3 corlimit=0.7".
                            format(ref,imap))
                     dx = float( get_task_par( "TR(1)", "align2d" ) )
                     dy = float( get_task_par( "TR(4)", "align2d" ) )
                  except starutil.AtaskError:
                     dx = 1E6
                     dy = 1E6

#  If the shifts are suspiciously high, we do not believe them. In which
#  case we cannot do pointing ocorrection when creating the Q and U maps.
                  if abs(dx) > 5 or abs(dy) > 5:
                     pointing_dx[key] = "null"
                     pointing_dy[key] = "null"
                     msg_out( "\nWARNING: The I map created from the POL2 data cannot be aligned "
                              "with the supplied IP reference map. Therefore the final "
                              "Q and U maps cannot be corrected for pointing errors.\n" )

#  Otherwise, convert the offset in pixels to (longitude,latitude) offsets
#  in the sky system of the reference map, in arc-seconds....
                  else:

#  Strip the wavelength axis off the total intensity map created above.
                     imap2d = NDG( 1 )
                     invoke("$KAPPA_DIR/ndfcopy in={0} out={1} trim=yes".format(imap,imap2d))

#  Get the pixel coords at the centre of the total intensity map.
                     invoke("$KAPPA_DIR/ndftrace ndf={0}".format(imap2d))
                     lbndx = float( get_task_par( "LBOUND(1)", "ndftrace" ) )
                     lbndy = float( get_task_par( "LBOUND(2)", "ndftrace" ) )
                     ubndx = float( get_task_par( "UBOUND(1)", "ndftrace" ) )
                     ubndy = float( get_task_par( "UBOUND(2)", "ndftrace" ) )
                     cenx = 0.5*( lbndx + ubndx )
                     ceny = 0.5*( lbndy + ubndy )

#  Convert to SKY coords, in radians. Use ATOOLS rather than pyast in
#  order to avoid the need for people to install pyast. Also, ATOOLS
#  integrates with NDFs more easily than pyast.
                     (cena,cenb) = invoke("$ATOOLS_DIR/asttran2 this={0} forward=yes "
                                          "xin={1} yin={2}".format( imap2d,cenx,ceny)).split()
                     cena = float( cena )
                     cenb = float( cenb )

#  Add on the pixel offsets, and convert to SKY coords, in radians.
                     offx = cenx + dx
                     offy = ceny + dy
                     (offa,offb) = invoke("$ATOOLS_DIR/asttran2 this={0} forward=yes "
                                          "xin={1} yin={2}".format( imap2d,offx,offy)).split()
                     offa = float( offa )
                     offb = float( offb )

#   Now find the arc-distance parallel to the longitude axis, between the central
#   and offset positions, and convert from radians to arc-seconds.
                     dx = invoke("$ATOOLS_DIR/astdistance this={0}, point1=\[{1},{2}\] "
                                 "point2=\[{3},{4}\]".format(imap2d,cena,cenb,offa,cenb))
                     dx = 3600.0*math.degrees( float( dx ) )

#  The value returned by astDistance is always positive. Adjust the sign
#  of dx so that it goes the right way.
                     da = offa - cena
                     while da > math.pi:
                        da -= math.pi
                     while da < -math.pi:
                        da += math.pi
                     if da < 0.0:
                        dx = -dx

#  Now find the arc-distance parallel to the latitude axis, between the central
#  and offset positions, and convert from radians to arc-seconds.
                     dy = invoke("$ATOOLS_DIR/astdistance this={0}, point1=\[{1},{2}\] "
                                 "point2=\[{3},{4}\]".format(imap2d,cena,cenb,cena,offb))
                     dy = 3600.0*math.degrees( float( dy ) )

#  The value returned by astDistance is always positive. Adjust the sign
#  of dx so that it goes the right way.
                     db = offb - cenb
                     if db < 0.0:
                        dy = -dy

#  Create the pointing correction file to use with subsequent makemap
#  calls. If a file is already in use (because of the data being old)
#  append the new pointing correction to the end of the file, preceeded
#  by an "end-of-table" Marker (two minus signs). Makemap will then apply
#  both correction.
                     msg_out( "Using pointing corrections of ({0},{1}) arc-seconds".format(dx,dy) )
                     if pntfile == "!":
                        pntfile = NDG.tempfile()
                        fd = open(pntfile,"w")
                     else:
                        fd = open(pntfile,"a")
                        fd.write("--\n")

                     fd.write("# system=tracking\n")
                     fd.write("# tai dlon dlat\n")
                     fd.write("54000 {0} {1}\n".format(dx,dy))
                     fd.write("56000 {0} {1}\n".format(dx,dy))
                     fd.close()

#  Store the pointing corrections for inclusion in the obstable.
                     pointing_dx[key] = dx
                     pointing_dy[key] = dy


#  Convolve the supplied ip reference map to give it a beam that matches
#  the expected IP beam at the elevation of the supplied data.
            if ipbeamfix and ipref != "!":

#  Get the azmimuth and elevation of the POL2 data.
               el1 = float( get_fits_header( qsdf[0], "ELSTART" ) )
               el2 = float( get_fits_header( qsdf[0], "ELEND" ) )
               el = 0.5*( el1 + el2 )
               az1 = float( get_fits_header( qsdf[0], "AZSTART" ) )
               az2 = float( get_fits_header( qsdf[0], "AZEND" ) )
               az = 0.5*( az1 + az2 )

               msg_out( "Convolving the I reference map to match the expected IP beam shape at elevation {0} degs...".format(el))

#  Get the pixel size in the input total intensity map.
               invoke("$KAPPA_DIR/ndftrace ndf={0} quiet".format(ipref) )
               ipixsize = float( get_task_par( "fpixscale(1)", "ndftrace" ) )

#  Generate an NDF holding the canonical total-intensity beam (circular).
#  Parameters are: pg=shape exponent, pf=FWHM in arc-sec, pp=pixel size
#  in arc-sec.
               ibeam = NDG(1)
               invoke("$KAPPA_DIR/maths exp=\"'exp(-0.69315*((4*((xa+0.5)**2+(xb+0.5)**2)/(fa*fa))**(pg/2)))'\" "
                      "fa=\"'pf/pp'\" lbound=\[-15,-15\] ubound=\[15,15\] type=_double "
                      "pf=14 pg=1.984 pp={1} out={0}".format(ibeam,ipixsize) )

#  Get the parameters of the expected polarised-intensity beam, at the
#  elevation of the data. FWHM values are in arc-seconds. Area is in square
#  arc-seconds. Orientation is in degrees from the elevation axis towards
#  the azimuth axis.
               fwhm1 = 14.6914727284 + 0.0421973549002*el - 9.70079974113e-05*el*el
               fwhm2 = 15.245386229 - 0.115624437578*el + 0.000763994058326*el*el
               area = fwhm1*fwhm2
               gamma = 4.65996074835 - 0.0340987643291*area + 0.000115483045339*area*area
               orient = 118.639637086 - 0.472915017742*az +  0.00140620919736*az*az

#  Convert the array of TCS_TAI values within the JCMTSTATE extension of
#  the first Q time series into an NDF, and then get the average TCS_TAI
#  value (the epoch as an MJD).
               tcstai = NDG(1)
               invoke("$HDSTOOLS_DIR/hcreate type=image inp={0}".format(tcstai))
               invoke("$HDSTOOLS_DIR/hcopy in={0}.more.jcmtstate.tcs_tai out={1}.data_array".
                      format(qsdf[0],tcstai))
               invoke("$KAPPA_DIR/stats ndf={0}".format(tcstai))
               epoch = float( get_task_par( "mean", "stats" ) )

#  Get the angle from the Y pixel axis to the elevation axis in the supplied
#  IP reference image. Positive rotation is from X pixel axis to Y pixel axis.
#  We need to set the WCS to AZEL offsets to do this, but we first need to
#  set the Epoch in the image to that of the POL2 observation, in order to
#  ensure that the AZEL axes are approriate to the time the POL2 dta was
#  taken rather than the time of the image. And we to do this without
#  adjusting the WCS Mappings. Take a copy to avoid changing the original.
               junk = NDG(1)
               invoke("$KAPPA_DIR/ndfcopy in={0} out={1} trim=yes".format(ipref,junk))
               invoke("$KAPPA_DIR/wcsattrib ndf={0} mode=set name=epoch newval=\"'MJD {1}'\" remap=no".format(junk,epoch))
               invoke("$KAPPA_DIR/wcsattrib ndf={0} mode=set name=skyrefis newval=origin".format(junk))
               invoke("$KAPPA_DIR/wcsattrib ndf={0} mode=set name=system newval=azel".format(junk))

               junk2 = NDG(1)
               invoke("$KAPPA_DIR/rotate in={0} out={1} angle=!".format(junk,junk2))
               angrot = float( get_task_par( "angleused", "rotate" ) )

#  Convert the major axis orientation from sky coords to pixel coords.
               orient = angrot - orient

#  Convert FWHM values to pixels.
               fwhm1 = fwhm1/ipixsize
               fwhm2 = fwhm2/ipixsize

#  Generate an NDF holding the expected polarised-intensity IP beam, at the
#  elevation of the data. This image has the same WCS axis orientation as the
#  supplied total intensity image.
               ipbeam = NDG(1)
               invoke("$KAPPA_DIR/maths exp=\"'exp(-0.69315*((4*((fx/px)**2+(fy/py)**2))**(pg/2)))'\" "
                      "fx=\"'-(xa+0.5)*sind(po)+(xb+0.5)*cosd(po)'\" fy=\"'-(xa+0.5)*cosd(po)-(xb+0.5)*sind(po)'\" "
                      "lbound=\[-15,-15\] ubound=\[15,15\] type=_double po={4} "
                      "px={1} py={2} pg={3} out={0}".format(ipbeam,fwhm1,fwhm2,gamma,orient) )

#  Deconvolve the IP beam using the total intensity beam as the PSF. This
#  gives the required smoothing kernel.
               tmp1 = NDG(1)
               invoke("$KAPPA_DIR/wiener in={0} pmodel=1 pnoise=1E-5 psf={1} xcentre=0 ycentre=0 "
                      "out={2}".format(ipbeam,ibeam,tmp1))

#  The results seem to have less ringing if the kernel is apodised. Use
#  a Gaussian of FWHM 30 arc-seconds as the apodising function.
               tmp2 = NDG(1)
               invoke("$KAPPA_DIR/maths exp=\"'ia*exp(-((xa+0.5)**2+(xb+0.5)**2)/fa)'\" "
                      "fa=\"'30/pa'\" pa={1} ia={0} out={2}".format(tmp1,ipixsize,tmp2))

#  Ensure the kernal has a total data value of unity. This means the
#  input and output maps will have the same normalisation.
               invoke("$KAPPA_DIR/stats ndf={0}".format(tmp2))
               total = get_task_par( "total", "stats" )
               bkernel = NDG(1)
               invoke("$KAPPA_DIR/cdiv in={0} scalar={1} out={2}".format(tmp2,total,bkernel))

#  Convolve the supplied total intensity map using this kernel. The
#  output map should have a beam similar to the expected IP beam.
               iprefbeam = NDG(1)
               invoke("$KAPPA_DIR/convolve in={0} psf={1} xcentre=0 ycentre=0 "
                      "out={2}".format(ipref,bkernel,iprefbeam))
               ipref = iprefbeam

#  Make maps from the Q and U time series. Ensure that we ignore both Q and U
#  if either map cannot be made. Also ensure we continue to process any remaining
#  observation chunks if the current chunk fails.
            qmaps[key] = NDG("{0}/{1}_qmap".format(mapdir,key), False)
            msg_out( "Making a map from the Q time series...")
            try:
               invoke("$SMURF_DIR/makemap in={0} config=^{1} out={2} ref={3} pointing={4} "
                      "ipref={5} {6}".format(qsdf,conf,qmaps[key],ref,pntfile,ipref,pixsize_par))

               if ref == "!":
                  ref = qmaps[key]

               umaps[key] = NDG("{0}/{1}_umap".format(mapdir,key), False)
               msg_out( "Making a map from the U time series..." )
               try:
                  invoke("$SMURF_DIR/makemap in={0} config=^{1} out={2} ref={3} pointing={4} "
                         "ipref={5} {6}".format(usdf,conf,umaps[key],ref,pntfile,ipref,pixsize_par))

#  Append these new Q and U maps to the text file listing all Stokes maps to
#  be included in the final Q and U maps.
                  with open(allmaps, 'a') as infile:
                     infile.write( "{0}\n".format( qmaps[key] ) )
                     infile.write( "{0}\n".format( umaps[key] ) )

#  Store the pointing corrections (if any) as FITS headers within the maps.
                  if key in pointing_dx and pointing_dx[key] != "null":
                     sym = invoke("$KAPPA_DIR/wcsattrib ndf={0} mode=get name='Symbol(1)'".
                                  format(qmaps[key]))
                     invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=POINT_DX "
                            "edit=w value={1} comment=\"'{2} pointing correction [arcsec]'\""
                            " position=! mode=interface".format(qmaps[key],pointing_dx[key],sym))
                     invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=POINT_DX "
                            "edit=w value={1} comment=\"'{2} pointing correction [arcsec]'\""
                            " position=! mode=interface".format(umaps[key],pointing_dx[key],sym))

                  if key in pointing_dy and pointing_dy[key] != "null":
                     sym = invoke("$KAPPA_DIR/wcsattrib ndf={0} mode=get name='Symbol(2)'".
                                  format(qmaps[key]))
                     invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=POINT_DY "
                            "edit=w value={1} comment=\"'{2} pointing correction [arcsec]'\""
                            " position=! mode=interface".format(qmaps[key],pointing_dy[key],sym))
                     invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=POINT_DY "
                            "edit=w value={1} comment=\"'{2} pointing correction [arcsec]'\""
                            " position=! mode=interface".format(umaps[key],pointing_dy[key],sym))

               except starutil.AtaskError:
                  if ref == qmaps[key]:
                     ref = "!"
                  del umaps[key]
                  del qmaps[key]

            except starutil.AtaskError:
               del qmaps[key]



#  Create a text file holding information about all the Stokes maps to be
#  processed. For each one, get the Stokes parameter (Q, U or I)
#  and a key that is unique for the chunk of data, of the form
#  "<UT>_<OBS>_<SUBSCAN>".
   mapinfo = NDG.tempfile()
   mapndg = NDG("^{0}".format(allmaps) )
   invoke("$SMURF_DIR/pol2check in={0} quiet=yes mapinfo={1}".
          format(mapndg,mapinfo))

#  Set up two dicts - one for Q and one for U. Each key is as described
#  above. Each value is the path to an NDF holding a map with the same
#  key and the same Stokes parameter (Q, U or I).
   with open(allmaps) as infile:
      lines = infile.readlines()
   paths = [line.strip() for line in lines]

   with open(mapinfo) as infile:
      lines = infile.readlines()
   infos = [line.strip() for line in lines]

   qmaps = {}
   umaps = {}
   for (path,info) in zip( paths, infos ):
      (stokes,id) = info.split()
      if stokes == "Q":
         qmaps[id] = path

      elif stokes == "U":
         umaps[id] = path

#  Loop over all Q maps.
   badkeys = []
   for key in qmaps:

#  Get the Q and U maps for the current observation chunk.
      qsdf = NDG( qmaps[ key ] )

      if key in umaps:
         usdf = NDG( umaps[ key ] )
      else:
         usdf = None

      msg_out("\n>>>>   Analysing Q and U maps from {0}...\n".format(key) )

#  Store useful info about this pair of Q and U maps.
#  FITS headers...
      wvm[key] = 0.5*( float( get_fits_header( qmaps[key], "WVMTAUST" ) )
                       + float( get_fits_header( qmaps[key], "WVMTAUEN" ) ) )
      elapsed_time[key] = float( get_fits_header( qmaps[key], "ELAPTIME" ) )
      nbolo_used_q[key] = float( get_fits_header( qmaps[key], "NBOLOEFF" ) )
      nbolo_used_u[key] = float( get_fits_header( umaps[key], "NBOLOEFF" ) )
      obj = get_fits_header( qmaps[key], "OBJECT" )

      if key not in pointing_dx:
         pointing_dx[key] = get_fits_header( qmaps[key], "POINT_DX" )
         if pointing_dx[key] == None:
            pointing_dx[key] = "null"
      if key not in pointing_dy:
         pointing_dy[key] = get_fits_header( qmaps[key], "POINT_DY" )
         if pointing_dy[key] == None:
            pointing_dy[key] = "null"

#  Calculate the expected NEFD. See:
#     www.eaobservatory.org/jcmt/instrumentation/continuum/scuba-2/pol-2/
#     www.eaobservatory.org/jcmt/instrumentation/continuum/scuba-2/calibration/
      elevation = 0.5*( float( get_fits_header( qmaps[key], "ELSTART" ) )
                       + float( get_fits_header( qmaps[key], "ELEND" ) ) )
      band = float( get_fits_header( qmaps[key], "FILTER" ) )
      if band == 450:
         transmission = math.exp(-26*(wvm[key]-0.01196)/math.sin(math.radians(elevation)))
         nefd_expected[key] = 981.5/transmission - 87.3
         fcf = 962000
         c = 0.045
      else:
         transmission = math.exp(-4.6*(wvm[key]-0.00435)/math.sin(math.radians(elevation)))
         nefd_expected[key] = 310/transmission - 26
         fcf = 725000
         c = 0.165

#  Background noise, source size, mean source value...
      (noise_q, source_size_q[key], source_rms_q[key] ) = calc_stats( qmaps[key] )
      (noise_u, source_size_u[key], source_rms_u[key] ) = calc_stats( umaps[key] )
      if noise_q >= 0.0 and noise_u >= 0.0:

#  Calculate the NEFDs based on the measured noises.
         nefd_q[key] = fcf*noise_q*math.sqrt(elapsed_time[key]*c)
         nefd_u[key] = fcf*noise_u*math.sqrt(elapsed_time[key]*c)

#  Display all this info.
         msg_out( " " )
         msg_out( "  Object = {0}".format(obj))
         msg_out( "  WVM tau = {0}".format(wvm[key]))
         msg_out( "  Measured NEFD in Q = {0} mJy.sec^(0.5)".format(nefd_q[key]))
         msg_out( "  Measured NEFD in U = {0} mJy.sec^(0.5)".format(nefd_u[key]))
         msg_out( "  Expected NEFD = {0} mJy.sec^(0.5)".format(nefd_expected[key]))
         msg_out( "  Elapsed observation time = {0} sec".format(elapsed_time[key]))
         msg_out( "  Number of bolometers contributing to Q map = {0}".format(nbolo_used_q[key]))
         msg_out( "  Number of bolometers contributing to U map = {0}".format(nbolo_used_u[key]))
         msg_out( "  Source area in Q = {0} arc-sec^(2)".format(source_size_q[key]))
         msg_out( "  Source area in U = {0} arc-sec^(2)".format(source_size_u[key]))
         if source_rms_q[key] != "null":
            msg_out( "  RMS of Q within source area = {0} pW".format(source_rms_q[key]))
         else:
            msg_out( "  RMS of Q within source area = <undefined>")

         if source_rms_u[key] != "null":
            msg_out( "  RMS of U within source area = {0} pW".format(source_rms_u[key]))
         else:
            msg_out( "  RMS of U within source area = <undefined>")

         if key in pointing_dx and pointing_dx[key] != "null" and key in pointing_dy and pointing_dy[key] != "null":
            msg_out( "  Pointing correction = ( {0}, {1} ) arc-sec".
                     format(pointing_dx[key],pointing_dy[key]))

      else:
         msg_out( "  This observation appears to be for a far away object ({0}) "
                  "and will not be included in the coadd.".format(obj))
         badkeys.append( key )

#  Remove any bad maps so they are not included in the coadd.
   for key in badkeys:
      del qmaps[key]
      del umaps[key]

#  Check some good maps remain to be processed.
   if len(qmaps) == 0 or len(umaps) == 0:
      raise starutil.InvalidParameterError("Do usable maps remains to be processed.")

#  If required, dump the stats for the individual observations to a text
#  file, formatted in topcat "ascii" format.
   msg_out( " " )
   if obstable:
      msg_out( "Writing stats for individual observations to output text file {0}".format(obstable))
      fd = open(obstable,"w")
      fd.write("#\n")
      fd.write("# UT - UT date of observation\n")
      fd.write("# OBS - Observation number\n")
      fd.write("# SUBSCAN - The first subscan included in the map\n")
      fd.write("# WVM - The mean of the starting and ending WVM tau values\n")
      fd.write("# NEFD_Q - The measured NEFD in the Q map (mJy.sec^(0.5))\n")
      fd.write("# NEFD_U - The measured NEFD in the U map (mJy.sec^(0.5))\n")
      fd.write("# NEFD_EXP - The expected NEFD based on WVM and elevation (mJy.sec^(0.5))\n")
      fd.write("# TIME - The elapsed time of the data included in the maps (s)\n")
      fd.write("# SIZE_Q - The total area of the source regions in the Q map (square arc-mins)\n")
      fd.write("# SIZE_U - The total area of the source regions in the U map (square arc-mins)\n")
      fd.write("# RMS_Q - The RMS Q value within the source regions (pW)\n")
      fd.write("# RMS_U - The RMS U value within the source regions (pW)\n")
      fd.write("# NBOLO_Q - Number of bolometers contributing to Q map\n")
      fd.write("# NBOLO_U - Number of bolometers contributing to U map\n")

      if len( pointing_dx ) > 0:
         fd.write("# DX - Pointing correction in tracking longitude (arc-sec))\n")
         fd.write("# DY - Pointing correction in tracking latitude (arc-sec))\n")

      fd.write("#\n")
      fd.write("# UT OBS SUBSCAN WVM NEFD_Q NEFD_U NEFD_EXP TIME SIZE_Q SIZE_U RMS_Q RMS_U NBOLO_Q NBOLO_U")
      if len( pointing_dx ) > 0:
         fd.write(" DX DY")
      fd.write("\n")

      for key in umaps:
         ( ut, obs, subscan ) = key.split("_")
         fd.write("{0} {1} {2} {3} {4} {5} {6} {7} {8} {9} {10} {11} {12} {13}".
                  format( ut, obs, subscan, wvm[key],
                          nefd_q[key], nefd_u[key], nefd_expected[key],
                          elapsed_time[key],
                          source_size_q[key], source_size_u[key],
                          source_rms_q[key], source_rms_q[key],
                          nbolo_used_q[key], nbolo_used_u[key] ))

         if len( pointing_dx ) > 0:
            fd.write(" {0} {1}".format( pointing_dx[key], pointing_dy[key] ))

         fd.write("\n")
      fd.close()

#  All observation chunks have now been mapped. If we have only one
#  observation just copy it to the output maps. If we will be converting
#  to Jy/beam, we need to use intermediate NDFs for the mosaics. Otherwise,
#  we can put the mosaics into their final destinations.
   if jy:
      qmos = NDG(1)
      umos = NDG(1)
   else:
      qmos = qmap
      umos = umap

   if len(qmaps) == 1:
      key = list(qmaps)[0]
      invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(qmaps[key],qmos))
      invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(umaps[key],umos))

#  If we have more than one observation, coadd them. Also coadd the
#  extension NDFs (EXP_TIMES and WEIGHTS), but without normalisation so
#  that the coadd is the sum rather than the mean of the inputs.
   elif len(qmaps) > 1:
      msg_out("Coadding Q and U maps from all observations")
      allmaps = NDG( qmaps.values() )
      invoke("$KAPPA_DIR/wcsmosaic in={0} out={1} lbnd=! ref=! "
             "conserve=no method=bilin variance=yes".format(allmaps,qmos))

      invoke("$KAPPA_DIR/erase object={0}.more.smurf.exp_time ok=yes".format(qmos))
      invoke("$KAPPA_DIR/wcsmosaic in={{{0}}}.more.smurf.exp_time lbnd=! ref=! "
             "out={1}.more.smurf.exp_time conserve=no method=bilin norm=no "
             "variance=no".format(allmaps,qmos))

      invoke("$KAPPA_DIR/erase object={0}.more.smurf.weights ok=yes".format(qmos))
      invoke("$KAPPA_DIR/wcsmosaic in={{{0}}}.more.smurf.weights lbnd=! ref=! "
             "out={1}.more.smurf.weights conserve=no method=bilin norm=no "
             "variance=no".format(allmaps,qmos))

      allmaps = NDG( umaps.values() )
      invoke("$KAPPA_DIR/wcsmosaic in={0} out={1} lbnd=! ref=! "
             "conserve=no method=bilin variance=yes".format(allmaps,umos))

      invoke("$KAPPA_DIR/erase object={0}.more.smurf.exp_time ok=yes".format(umos))
      invoke("$KAPPA_DIR/wcsmosaic in={{{0}}}.more.smurf.exp_time lbnd=! ref=! "
             "out={1}.more.smurf.exp_time conserve=no method=bilin norm=no "
             "variance=no".format(allmaps,umos))

      invoke("$KAPPA_DIR/erase object={0}.more.smurf.weights ok=yes".format(umos))
      invoke("$KAPPA_DIR/wcsmosaic in={{{0}}}.more.smurf.weights lbnd=! ref=! "
             "out={1}.more.smurf.weights conserve=no method=bilin norm=no "
             "variance=no".format(allmaps,umos))

#  If output PI and I values are in Jy, convert the Q, U and I maps to Jy
#  and store in their final destination.
   if jy:
      invoke( "$KAPPA_DIR/cmult in={0} scalar={1} out={2}".format(qmos,fcf1,qmap))
      invoke( "$KAPPA_DIR/setunits ndf={0} units=Jy/beam".format(qmap))

      invoke( "$KAPPA_DIR/cmult in={0} scalar={1} out={2}".format(umos,fcf1,umap ))
      invoke( "$KAPPA_DIR/setunits ndf={0} units=Jy/beam".format(umap))

      if ipref != "!":
         imap = NDG(1)
         invoke( "$KAPPA_DIR/cmult in={0} scalar={1} out={2}".format(ipref,fcf2,imap))
         invoke( "$KAPPA_DIR/setunits ndf={0} units=Jy/beam".format(imap))
      else:
         imap = None

#  If output values are in pW, scale the IPREF map to take account of the
#  difference in FCF with and without POL2 in the beam.
   else:
      if ipref != "!":
         imap = NDG(1)
         invoke( "$KAPPA_DIR/cmult in={0} scalar={1} out={2}".format( ipref, fcf2/fcf1, imap ))
      else:
         imap = None

#  Create the polarised intensity map if required.
   if pimap:
      msg_out( "Generating an polarised intensity image...")
      if debias:
         invoke( "$KAPPA_DIR/maths exp=\"'sign(sqrt(abs(fa)),fa)'\" "
                 "fa=\"'ia**2+ib**2-(va+vb)/2'\" ia={0} ib={1} out={2}".
                 format(qmap,umap,pimap))
      else:
         invoke( "$KAPPA_DIR/maths exp=\"'sqrt(ia**2+ib**2)'\" ia={0} "
                 "ib={1} out={2}".format(qmap,umap,pimap))

# The rest we only do if an output catalogue is reqired.
   if outcat:

#  If no total intensity map was supplied, generate an artificial I image that
#  is just equal to the polarised intensity image. This is needed because
#  polpack:polvec uses the I value to normalise the Q and U values prior to
#  calculating the polarised intensity and angle.
      if imap == None:
         if pimap:
            imap = pimap
         else:
            imap = NDG(1)
            msg_out( "Generating an artificial total intensity image...")
            if debias:
               invoke( "$KAPPA_DIR/maths exp=\"'sign(sqrt(abs(fa)),fa)'\" "
                       "fa=\"'ia**2+ib**2-(va+vb)/2'\" ia={0} ib={1} out={2}".
                       format(qmap,umap,imap))
            else:
               invoke( "$KAPPA_DIR/maths exp=\"'sqrt(ia**2+ib**2)'\" ia={0} "
                       "ib={1} out={2}".format(qmap,umap,imap))

#  Ensure the Q U and I images all have the same bounds, equal to the
#  overlap region between them. To get the overlap region, use MATHS to
#  add them together. Then use ndfcopy to produce the sections from each,
#  which match the overlap area.
      tmp = NDG( 1 )
      invoke( "$KAPPA_DIR/maths exp=\"'ia+ib+ic'\" ia={0} ib={1} ic={2} out={3}".
              format(qmap,umap,imap,tmp) )
      qtrim = NDG( 1 )
      invoke( "$KAPPA_DIR/ndfcopy in={0} like={1} out={2}".format(qmap,tmp,qtrim) )
      utrim = NDG( 1 )
      invoke( "$KAPPA_DIR/ndfcopy in={0} like={1} out={2}".format(umap,tmp,utrim) )
      itrim = NDG( 1 )
      invoke( "$KAPPA_DIR/ndfcopy in={0} like={1} out={2}".format(imap,tmp,itrim) )

#  The polarisation vectors are calculated by the polpack:polvec command,
#  which requires the input Stokes vectors in the form of a 3D cube. Paste
#  the 2-dimensional Q, U and I images into a 3D cube.
      planes = NDG( [qtrim,utrim,itrim] )
      cube = NDG( 1 )
      invoke( "$KAPPA_DIR/paste in={0} shift=\[0,0,1\] out={1}".format(planes,cube))

#  The cube will have a 3D "POLANAL-SPECTRUM" WCS Frame, but POLVEC
#  requires a 2D POLANAL Frame. So use wcsframe to create the 2D Frame
#  from the 3D Frame, then delete the 3D Frame.
      invoke( "$KAPPA_DIR/wcsframe ndf={0} frame=POLANAL".format(cube) )
      invoke( "$KAPPA_DIR/wcsremove ndf={0} frame=POLANAL-SPECTRUM".format(cube) )

#  Re-instate SKY as the current Frame
      invoke( "$KAPPA_DIR/wcsframe ndf={0} frame=SKY".format(cube) )

#  POLPACK needs to know the order of I, Q and U in the 3D cube. Store
#  this information in the POLPACK enstension within "cube.sdf".
      invoke( "$POLPACK_DIR/polext in={0} stokes=qui".format(cube) )

#  Create a FITS catalogue containing the polarisation vectors.
      msg_out( "Creating the output catalogue: '{0}'...".format(outcat) )
      msg = invoke( "$POLPACK_DIR/polvec {0} cat={1} debias={2}".format(cube,outcat,debias) )
      msg_out( "\n{0}\n".format(msg) )

#  Remove temporary files.
   cleanup()

#  If an StarUtilError of any kind occurred, display the message but hide the
#  python traceback. To see the trace back, uncomment "raise" instead.
except starutil.StarUtilError as err:
#  raise
   print( err )
   print( "See the end of the log file ({0}) for further details.".format(starutil.logfile) )
   cleanup()

# This is to trap control-C etc, so that we can clean up temp files.
except:
   cleanup()
   raise





