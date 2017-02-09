#!/usr/bin/env python

'''
*+
*  Name:
*     POL2MAP

*  Purpose:
*     Create Q, U and I maps from a group of POL-2 "spin&scan" data files.

*  Description:
*     This script creates maps (Q, U and I) and a vector catalogue from a
*     set of POL-2 observation. New observations can be added into the map
*     without the need to re-process previously processed observations.
*     The output maps are all in units of pW.
*
*     Note, with the default configuration this script can take up to
*     an hour to run for each observation on a typical SCUBA-2-capabale
*     computer.
*
*     Masking of models within makemap (AST, etc) can be based either on the
*     SNR of the map created as the end of each iteration, or on an external
*     map, or on a fixed circle centred on the origin - see parameter MASK.

*  Usage:
*     pol2map in iout qout uout [cat] [config] [pixsize] [qudir] [mapdir]
*             [mask] [ipcor] [ipref] [reuse] [ref] [north] [reffcf]
*             [debias] [retain] [msg_filter] [ilevel] [glevel] [logfile]

*  ADAM Parameters:
*     CAT = LITERAL (Read)
*        The output FITS vector catalogue. No catalogue is created if
*        null (!) is supplied. Note - currently, the Q, U  and PI values
*        in this catalogue will be in units of pW. [!]
*     CONFIG = LITERAL (Read)
*        Extra parameter values to include in the MAKEMAP configuration.
*        The supplied configuration is applied on top of the following
*        set of parameters:
*
*           ^$STARLINK_DIR/share/smurf/.dimmconfig_pol2.lis
*           numiter = -100
*           modelorder=(com,gai,pca,ext,flt,ast,noi)
*
*           maptol = 0.05
*           maptol_mask = <undef>
*           maptol_mean = 0
*           maptol_box = 60
*           maptol_hits = 1
*
*           ast.mapspike_freeze = 5
*           pca.pcathresh = 0.04
*           pca.pcathresh_freeze = 0.1
*           pca.zero_niter = 0.2
*           com.zero_niter = 0.2
*           flt.zero_niter = 0.2
*
*        Additional parameters are also set, depending on the value of
*        parameter MASK. If MASK is set to "AUTO", the following
*        parameters are added to the above default config:
*
*           ast.skip = 10
*           ast.zero_snr = 3
*           ast.zero_snrlo = 2
*           ast.zero_freeze = 0.2
*
*           pca.pcathresh = 0.1
*           pca.zero_snr = 5
*           pca.zero_snrlo = 3
*           pca.zero_freeze = -1
*
*           com.zero_snr = 5
*           com.zero_snrlo = 3
*           com.zero_freeze = -1
*
*           flt.zero_snr = 5
*           flt.zero_snrlo = 3
*           flt.zero_freeze = -1
*
*        If MASK is set to "CIRCLE", the following parameters are added
*        to the above default config:
*
*           ast.zero_circle = 0.0083  (degrees, i.e. 30 arc-seconds)
*           pca.zero_circle = 0.0038
*           com.zero_circle = 0.0083
*           flt.zero_circle = 0.0083
*
*        If MASK is set to the name of an NDF, this script creates fixed
*        masks from the NDF, and the following parameters are added
*        to the above default config:
*
*           ast.zero_mask = ref
*           pca.zero_mask = mask2
*           com.zero_mask = mask2
*           flt.zero_mask = mask2
*
*        The above "ref" mask consists of clumps of pixel with SNR greater
*        than 3, extended down to an SNR level of 2. The "mask2" mask
*        consists of clumps of pixel with SNR greater than 5, extended
*        down to an SNR level of 3.
*
*        If a configuration is supplied using parameter CONFIG, values
*        supplied for any of the above parameters will over-write the
*        values specified above. In addition, the following mandatory
*        values are always appended to the end of the used configuration:
*
*        flagslow = 0.01
*        downsampscale = 0
*        noi.usevar=1
*
*        If null (!) or "def" is supplied, the above set of default
*        configuration parameters are used without change. ["def"]
*     DEBIAS = LOGICAL (Given)
*        TRUE if a correction for statistical bias is to be made to
*        percentage polarization and polarized intensity in the output
*        vector catalogue specified by parameter CAT. [FALSE]
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
*        debugging information.
*
*        In adition, the glevel value can be changed by assigning a new
*        integer value (one of starutil.NONE, starutil.CRITICAL,
*        starutil.PROGRESS, starutil.ATASK or starutil.DEBUG) to the module
*        variable starutil.glevel. ["PROGRESS"]
*     IN = NDF (Read)
*        A group of input files. Each specified file must be one of the
*        following types:
*
*        - a raw POL-2 data file. Any supplied raw POL-2 data files will
*        be converted into time-series Q,U and I files using SMURF:CALCQU
*        and placed in the directory specified by parameter QUDIR. These
*        will then be converted into maps using SMURF:MAKEMAP, and placed
*        in the directory specified by parameter MAPDIR.
*
*        - a time-series file holding Stokes Q, U or I values. Any supplied
*        time-series files will be converted into individual maps (one for
*        each file) using SMURF:MAKEMAP, and placed in the directory
*        specified by parameter MAPDIR. These maps are created only for
*        the required Stokes parameters - as indiciated by parameters
*        IOUT, QOUT and UOUT.
*
*        - a two-dimensional map holding Stokes Q, U or I values. Any
*        maps must be in units of pW. The final output I map is created by
*        coadding any supplied I maps with the I maps created by this script.
*        These coadded maps are created only for the required Stokes
*        parameters - as indiciated by parameters IOUT, QOUT and UOUT.
*
*        Any combination of the above types can be supplied. Note, if
*        parameter REUSE is TRUE, then any required output files that
*        already exist in the directory specified by parameter MAPDIR
*        are re-used rather than being re-created from the corresponding
*        input data.
*     IOUT = NDF (Write)
*        The output NDF in which to return the total intensity (I) map
*        including all supplied observations. This will be in units of pW.
*        Supply null (!) if the I map is not to be retained on exit. In
*        this case, the I map will not be created unless it is needed for
*        IP correction (see parameters IPCOR and IPREF).
*     IPCOR = _LOGICAL NDF (Read)
*        If TRUE, then IP correction is used when creating Q and U maps,
*        based on the values in the total intensity map specified by
*        parameter IPREF. If FALSE, then no IP correction is performed. [TRUE]
*     IPREF = NDF (Read)
*        The total intensity map to be used for IP correction. Only
*        accessed if parameter IPCOR is set TRUE. If null (!) is supplied
*        for IPREF, the map supplied for parameter REF is used. The map must
*        be in units of pW. [!]
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
*        The name of a directory in which to put the Q, U an I maps made
*        from each individual observation supplied via "IN", before
*        coadding them. If
*        null is supplied, the new maps are placed in the same temporary
*        directory as all the other intermediate files and so will be
*        deleted when the script exists (unless parameter RETAIN is set
*        TRUE). Note, these maps are always in units of pW. Each one will
*        contain FITS headers specifying the pointing corrections needed
*        to align the map with the reference map. [!]
*     MASK = LITERAL (Read)
*        Specifies the type of masking to be used within makemap (the
*        same type of masking is used to create all three maps - I, Q
*        and U):
*
*        - "AUTO": makemap uses automatically generated masks based
*        on the SNR map at the end of each iteration. The SNR levels
*        used are specified by the "xxx.ZERO_SNR" and "xxx.ZERO_SNRLO"
*        configuration parameters (see parameter CONFIG).
*
*        - "CIRCLE": makemap uses a fixed circular mask of radius 60
*        arc-seconds centred on the expected source position.
*
*        - Any other value is assumed to be the path to an NDF holding
*        a map from which "external" AST and PCA masks should be
*        determined. It can be in any units, but must have a Variance
*        component, and must be aligned in pixel coordinates with the
*        reference map (parameter REF). The masks created from this map
*        are generated by finding all clumps of contiguous pixels above
*        an SNR limit, and then extending these clumps down to a lower
*        SNR limit. For the AST model, the upper and lower SNR limits
*        are of 3.0 and 2.0. For the PCA mask, the limits are 5.0 and
*        3.0. ["AUTO"]
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
*     PIXSIZE = _REAL (Read)
*        Pixel dimensions in the output I maps, in arcsec. The default
*        is 4 arc-sec for 850 um data and 2 arc-sec for 450 um data. []
*     QOUT = NDF (Write)
*        The output NDF in which to return the Q map including all supplied
*        observations. This will be in units of pW. Supply null (!) if no Q
*        map is required.
*     QUDIR = LITTERAL (Read)
*        The name of a directory in which to put the Q, U and I time series
*        generated by SMURF:CALCQU, prior to generating maps from them. If
*        null (!) is supplied, they are placed in the same temporary directory
*        as all the other intermediate files and so will be deleted when the
*        script exists (unless parameter RETAIN is set TRUE). [!]
*     REF = NDF (Read)
*        An optional map defining the pixel grid for the output maps,
*        and which is used to determien pointing corrections. If null
*        (!) is supplied, then the map (if any) specified by parameter
*        MASK is used. See also parameter REFFCF. [!]
*     REFFCF = _REAL (Read)
*        The FCF that should be used to convert the supplied REF map
*        to pW. This parameter is only used if the supplied REF map is
*        not already in units of pW. The default is the FCF value stored
*        in the FITS extension of the map, or the standard FCF for the
*        band concerned (450 or 840) if there is no FCF value in the FITS
*        header. Specify a new value on the pol2map command line if the
*        default value described above is inappropriate. []
*     REUSE = _LOGICAL (Read)
*        If TRUE, then any output maps or time-treams that already exist
*        (for instance, created by a previous run of this script) are re-used
*        rather than being re-created from the corresponding input files.
*        If FALSE, any previously created output maps or time-streams are
*        ignored and new ones are created from the corresponding input
*        files. [TRUE]
*     RETAIN = _LOGICAL (Read)
*        Should the temporary directory containing the intermediate files
*        created by this script be retained? If not, it will be deleted
*        before the script exits. If retained, a message will be
*        displayed at the end specifying the path to the directory. [FALSE]
*     UOUT = NDF (Write)
*        The output NDF in which to return the U map including all supplied
*        observations. This will be in units of pW. Supply null (!) if no U
*        map is required.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
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
*     25-JAN-2017 (DSB):
*        Original version
'''

import os
import math
import shutil
import starutil
import numpy as np
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

   params.append(starutil.ParNDG("IOUT", "The output total intensity map",
                                 default=None, exists=False, minsize=0,
                                 maxsize=1 ))

   params.append(starutil.ParNDG("QOUT", "The output Q map",
                                 default=None, exists=False, minsize=0,
                                 maxsize=1 ))

   params.append(starutil.ParNDG("UOUT", "The output U map",
                                 default=None, exists=False, minsize=0,
                                 maxsize=1 ))

   params.append(starutil.Par0S("CAT", "The output FITS vector catalogue",
                                 default=None, noprompt=True))

   params.append(starutil.Par0S("CONFIG", "Map-maker tuning parameters",
                                "def", noprompt=True))

   params.append(starutil.Par0F("PIXSIZE", "Pixel size (arcsec)", None,
                                 maxval=1000, minval=0.01, noprompt=True))

   params.append(starutil.Par0S("QUDIR", "Directory in which to save new "
                                "Q, U and I time series", None, noprompt=True))

   params.append(starutil.Par0S("MAPDIR", "Directory in which to save new "
                                "I maps before they are co-added", None,
                                noprompt=True))

   params.append(starutil.Par0S("MASK", "Type of masking to use in makemap",
                                "AUTO", noprompt=True ))

   params.append(starutil.Par0L("IPCOR", "Perform IP correction?", True,
                                 noprompt=True))

   params.append(starutil.ParNDG("IPREF", "The total intensity map to use "
                                 "for IP correction", default=None,
                                 noprompt=True, minsize=0, maxsize=1 ))

   params.append(starutil.Par0L("REUSE", "Re-use existing time-streams and maps?", True,
                                 noprompt=True))

   params.append(starutil.ParNDG("REF", "Reference map defining the pixel grid", default=None,
                                 noprompt=True, minsize=0, maxsize=1 ))

   params.append(starutil.ParChoice( "NORTH", ("TRACKING","FK5","ICRS","AZEL",
                                     "GALACTIC","GAPPT","FK4","FK4-NO-E",
                                     "ECLIPTIC"), "Celestial system to "
                                     "use as reference direction", "TRACKING",
                                     noprompt=True ))

   params.append(starutil.Par0F("REFFCF",
                                 "FCF needed to convert REF map to pW",
                                 None, noprompt=True))

   params.append(starutil.Par0L("DEBIAS", "Remove statistical bias from P"
                                "and PI?", False, noprompt=True))

   params.append(starutil.Par0L("RETAIN", "Retain temporary files?", False,
                                 noprompt=True))



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

#  Now get the I, Q and U output maps.
   imap = parsys["IOUT"].value
   qmap = parsys["QOUT"].value
   umap = parsys["UOUT"].value

#  Get the output catalogue.
   outcat = parsys["CAT"].value

#  If a catalogue is required, we need to create all three maps, so
#  ensure this is the case (use temporary files for any that have not
#  been requested by the user).
   if outcat:
      if not imap:
         imap = NDG( 1 )
      if not qmap:
         qmap = NDG( 1 )
      if not umap:
         umap = NDG( 1 )

#  See if statistical debiasing is to be performed.
   debias = parsys["DEBIAS"].value

#  The user-supplied makemap config, and pixel size.
   config = parsys["CONFIG"].value
   pixsize = parsys["PIXSIZE"].value
   if pixsize:
      pixsize_par = "pixsize={0}".format(pixsize)
   else:
      pixsize_par = ""

#  See if temp files are to be retained.
   retain = parsys["RETAIN"].value

#  See if existing output files are to be re-used. If not, they are
#  re-created from the corresponding input files.
   reuse = parsys["REUSE"].value

#  Get the type of masking to use. If a map is supplied, assume external
#  masking.
   mask = parsys["MASK"].value
   try:
      maskmap = NDG(mask)
      automask = False
      circlemask = False
      msg_out("Masking will be based on {0}.".format(maskmap))

   except starutil.NoNdfError:
      if "AUTO".startswith(mask.upper()):
         automask = True
         circlemask = False
         maskmap = None
      elif "CIRCLE".startswith(mask.upper()):
         automask = False
         circlemask = True
         maskmap = None
      else:
         raise starutil.InvalidParameterError("Invalid value {0} for "
                  "parameter MASK - no such NDF can be found.".
                  format(mask))

#  Get the reference map
   ref = parsys["REF"].value
   if not ref:
      if maskmap:
         ref = maskmap
      else:
         ref = "!"

   else:

#  If the REF map is in units of mJy/beam, convert it to pW using the FCF
#  in the "FCF" FITS header if available, or the standard FCF for the
#  wavelength otherwise.
      invoke("$KAPPA_DIR/ndftrace ndf={0} quiet".format(ref) )
      ref_units = get_task_par( "UNITS", "ndftrace" ).replace(" ", "")
      if ref_units != "pW":

         try:
            filter = int( float( get_fits_header( ref, "FILTER", True )))
         except starutil.NoValueError:
            filter = 850
            msg_out( "No value found for FITS header 'FILTER' in {0} - assuming 850".format(ref))

         if filter != 450 and filter != 850:
            raise starutil.InvalidParameterError("Invalid FILTER header value "
                   "'{0} found in {1}.".format( filter, ref ) )

         if ref_units == "mJy/beam":
            if filter == 450:
               fcf = 491000.0
            else:
               fcf = 537000.0

         elif ref_units == "Jy/beam":
            if filter == 450:
               fcf = 491.0
            else:
               fcf = 537.0

         elif ref_units == "mJy/arcsec**2" or ref_units == "mJy/arcsec^2" :
            if filter == 450:
               fcf = 4710
            else:
               fcf = 2340

         elif ref_units == "Jy/arcsec**2" or ref_units == "Jy/arcsec^2" :
            if filter == 450:
               fcf = 4.71
            else:
               fcf = 2.34

         else:
            raise starutil.InvalidParameterError("REF map {0} has unsupported units {1}".
                                                 format(ref, ref_units) )

         fcfhead = get_fits_header( ref, "FCF" )
         if fcfhead is not None:
            fcfhead = float( fcfhead )
            ratio = fcfhead/fcf
            if ratio < 0.5 or ratio > 2.0:
               msg_out("WARNING: REF map {0} has units {1} but the FCF header is {2} "
                       "- which looks wrong (the expected FCF is {3}).".
                       format(ref, ref_units, fcfhead, fcf) )
            fcf = fcfhead

         parsys["REFFCF"].default = fcf
         ref_fcf = parsys["REFFCF"].value

         msg_out( "Converting REF map ({0}) from {1} to pW using FCF={2}...".
                  format(ref,ref_units,ref_fcf))
         refpw = NDG(1)
         invoke("$KAPPA_DIR/cdiv in={0} scalar={1} out={2}".format(ref,ref_fcf,refpw) )
         ref = refpw

#  If IP correction is to be performed, get the map to be used to define
#  the IP correction.
   if parsys["IPCOR"].value:
      ipref = parsys["IPREF"].value
      if not ipref:
         if not ref:
            raise starutil.InvalidParameterError("IP correction requested "
                                        "but no IP reference map supplied.")
         ipref = ref
      else:
         invoke("$KAPPA_DIR/ndftrace ndf={0} quiet".format(ipref) )
         units = get_task_par( "UNITS", "ndftrace" ).replace(" ", "")
         if units != "pW":
            raise starutil.InvalidParameterError("IP reference map {0} is"
                 " has units {1} - units must be pW".format(ipref,units))

      ip = "ipref={0}".format(ipref)
   else:
      ip = "ipref=!"

#  See where to put new Q, U and I maps for individual observations, and
#  ensure the directory exists.
   mapdir =  parsys["MAPDIR"].value
   if not mapdir:
      mapdir = NDG.tempdir
   elif not os.path.exists(mapdir):
      os.makedirs(mapdir)

#  See where to put new Q, U and I time series, and ensure the directory
#  exists.
   qudir =  parsys["QUDIR"].value
   if not qudir:
      qudir = NDG.tempdir
   elif not os.path.exists(qudir):
      os.makedirs(qudir)

#  Get the reference direction.
   north = parsys["NORTH"].value







#  -----------  CLASSIFY THE INPUT DATA FILES ------------------------


#  Classify each input data file as raw, QUI time-series or QUI map. Create
#  three separate text files containing all input NDFs of each type (plus
#  a fourth holing non-POL2 data). Also, create another text file
#  containing a list of any missing raw sub-scan files.
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

#  Initialise the list of all individual Stokes maps to be included in the
#  final coadded maps so that it holds any maps supplied by parameter IN.
#  Check that any supplied maps are in units of pW.
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
            raise starutil.InvalidParameterError("All supplied "
                 "maps must be in units of 'pW', but '{0}' has units '{1}'.".
                 format(path,units))
   else:
      open( allmaps, 'a').close()








#  -----------  CREATE STOKES TIME SERIES FROM RAW DATA ------------------------


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

#  Use CALCQU to create the new Q, U and I time streams from the supplied
#  analysed intensity time streams. Put them in the QUDIR directory.
         new_q = NDG.tempfile()
         new_u = NDG.tempfile()
         new_i = NDG.tempfile()
         try:

#  If REUSE is TRUE and old Q, U and I time-streams exists, re-use them.
            try:
               if reuse:
                  qts = NDG("{0}/\*_QT".format(qudir, True)
                  uts = NDG("{0}/\*_UT".format(qudir, True)
                  its = NDG("{0}/\*_IT".format(qudir, True)

                  nq = len( qts )
                  nu = len( uts )
                  ni = len( its )
                  if nq == nu and nq == ni:
                     msg_out("   Re-using previously created Q, U and I "
                             "time-streams for observation {0}".format(id))

                     with open(new_q) as outfile:
                        for ndf in qts:
                          outfile.write(ndf)

                     with open(new_u) as outfile:
                        for ndf in uts:
                          outfile.write(ndf)

                     with open(new_i) as outfile:
                        for ndf in its:
                          outfile.write(ndf)
                  else:
                     raise starutil.NoNdfError("Ignoring pre-existing data")
               else:
                  raise starutil.NoNdfError("Ignoring pre-existing data")

#  Otherwise create new time-streams.
            except starutil.NoNdfError:
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








#  -----------  CREATE INDIVIDUAL MAPS FROM STOKES TIME SERIES DATA ---------


#  If we have some Stokes parameter time-series files to process...
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

#  Store the default set of config parameters in the config file.
      fd.write("^$STARLINK_DIR/share/smurf/.dimmconfig_pol2.lis\n")
      fd.write("numiter = -100\n")
      fd.write("modelorder = (com,gai,pca,ext,flt,ast,noi)\n")

      fd.write("maptol = 0.05\n")
      fd.write("maptol_mask = <undef>\n")
      fd.write("maptol_mean = 0\n")
      fd.write("maptol_box = 60\n")
      fd.write("maptol_hits = 1\n")

      fd.write("pca.pcathresh = 0.04\n")
      fd.write("pca.pcathresh_freeze=0.1\n")
      fd.write("ast.mapspike_freeze = 5\n")
      fd.write("pca.zero_niter = 0.2\n")
      fd.write("com.zero_niter = 0.2\n")
      fd.write("flt.zero_niter = 0.2\n")

#  Some depend on the masking type.
      if automask:
         fd.write("ast.skip = 10\n")
         fd.write("ast.zero_snr = 3\n")
         fd.write("ast.zero_snrlo = 2\n")
         fd.write("ast.zero_freeze = 0.2\n")

         fd.write("pca.pcathresh = 0.1\n")
         fd.write("pca.zero_snr = 5\n")
         fd.write("pca.zero_snrlo = 3\n")
         fd.write("pca.zero_freeze = -1\n")

         fd.write("com.zero_snr = 5\n")
         fd.write("com.zero_snrlo = 3\n")
         fd.write("com.zero_freeze = -1\n")

         fd.write("flt.zero_snr = 5\n")
         fd.write("flt.zero_snrlo = 3\n")
         fd.write("flt.zero_freeze = -1\n")

      elif circlemask:
         fd.write("ast.zero_circle = (0.0083)\n")
         fd.write("pca.zero_circle = (0.0083)\n")
         fd.write("com.zero_circle = (0.0083)\n")
         fd.write("flt.zero_circle = (0.0083)\n")

      else:
         fd.write("ast.zero_mask = ref\n")
         fd.write("pca.zero_mask = mask2\n")
         fd.write("com.zero_mask = mask2\n")
         fd.write("flt.zero_mask = mask2\n")

#  If the user supplied extra config parameters, append them to the
#  config file.
      if config and config != "def":
         fd.write("{0}\n".format(config))

#  Now put in values that are absolutely required by this script. These
#  over-write any values in the user-supplied config.
      fd.write("noi.usevar=1\n")
      fd.write("flagslow=0.01\n")
      fd.write("downsampscale=0\n")
      fd.close()

#  If required, generate the AST and PCA masks from the supplied MASK
#  map.
      if maskmap:
         snr = NDG(1)
         invoke("$KAPPA_DIR/makesnr in={0} out={1} minvar=0".format(maskmap,snr))

#  Very strong sources such as Orion A can create masks in which there
#  are insufficient background pixels to allow future invocations of
#  makemap to succeed. We therefore loop round raising the SNR limits for
#  the mask until no more than 20% of the originally good pixels are
#  designated as source pixels.
         invoke("$KAPPA_DIR/stats ndf={0}".format(snr))
         ngood = float( get_task_par( "numgood", "stats" ) )
         maxgood = ngood / 5

         noise = 2
         minheight = 3
         aconf = NDG.tempfile()
         astmask = NDG(1)

         while True:
            fd = open(aconf,"w")
            fd.write("FellWalker.FlatSlope=0\n")
            fd.write("FellWalker.MinDip=1.0E30\n")
            fd.write("FellWalker.Noise={0}\n".format(noise))
            fd.write("FellWalker.MinHeight={0}\n".format(minheight))
            fd.close()
            invoke("$CUPID_DIR/findclumps in={0} method=fellwalker rms=1 "
                   "outcat=! out={1} config=^{2}".format(snr,astmask,aconf))

            invoke("$KAPPA_DIR/stats ndf={0}".format(astmask))
            ngood = float( get_task_par( "numgood", "stats" ) )
            if ngood < maxgood:
               break
            else:
               if noise == minheight:
                  minheight *= 1.2
               noise = minheight

#  The source regions within the PCA mask need to be smaller than in the
#  AST mask. Make sure it uses no more than 10% of the original good
#  pixels.
         maxgood = ngood / 2

         noise = 3
         minheight = 5
         pconf = NDG.tempfile()
         pcamask = NDG(1)

         while True:
            fd = open(pconf,"w")
            fd.write("FellWalker.FlatSlope=0\n")
            fd.write("FellWalker.MinDip=1.0E30\n")
            fd.write("FellWalker.Noise={0}\n".format(noise))
            fd.write("FellWalker.MinHeight={0}\n".format(minheight))
            fd.close()
            invoke("$CUPID_DIR/findclumps in={0} method=fellwalker rms=1 "
                   "outcat=! out={1} config=^{2}".format(snr,pcamask,pconf))

            invoke("$KAPPA_DIR/stats ndf={0}".format(pcamask))
            ngood = float( get_task_par( "numgood", "stats" ) )
            if ngood < maxgood:
               break
            else:
               if noise == minheight:
                  minheight *= 1.2
               noise = minheight


#  Loop over each Stokes parameter, creating maps from each observation
#  if reqired.
      for qui in ('I', 'Q', 'U'):

#  Pass on to the next parameter if we are not creating a map for the
#  current parameter. Also create dictionaries holding the I, Q or U map
#  for each observation chunk.
         if qui == 'I':
            if imap:
               imaps = {}
               qui_maps = imaps
               qui_list = ilist
            else:
               continue

         elif qui == 'Q':
            if qmap:
               qmaps = {}
               qui_maps = qmaps
               qui_list = qlist
            else:
               continue

         else:
            if umap:
               umaps = {}
               qui_maps = umaps
               qui_list = ulist
            else:
               continue

#  Loop over all the time series files for the current Stokes parameter. Each
#  separate observation will usually have one time series file (although
#  there may be more if the observation was split into two or more discontiguous
#  chunks). We form a map for each observation chunk present in the supplied
#  list of input raw data.
         for key in qui_list:

#  Get the Stokes time stream files for the current observation chunk.
            isdf = NDG( qui_list[ key ] )
            msg_out("\n>>>>   Making {1} map from {0}...\n".format(key,qui) )

#  AZ/EL pointing correction, for data between 20150606 and 20150930.
            ut = int(get_fits_header( isdf[0], "UTDATE", True ))
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

#  If an auto-masked I map from a previous run exists for the current
#  observation, see if it has pointing corrections recorded in its FITS
#  header. If so, we use them when creating the new map.
            try:
               hmap = NDG("{0}/{1}_imap".format(mapdir,key))
               dx = get_fits_header( hmap, "POINT_DX" )
               dy = get_fits_header( hmap, "POINT_DY" )
            except starutil.NoNdfError:
               dx = None
               dy = None

#  Create the pointing correction file to use when running makemap. If
#  a file is already in use (because of the data being old) append the
#  new pointing correction to the end of the file, preceeded by an
#  "end-of-table" Marker (two minus signs). Makemap will then apply
#  both correction.
            if dx is not None and dy is not None:
               dx = float( dx )
               dy = float( dy )
               msg_out( "   Using pre-calculated pointing corrections of ({0},{1}) arc-seconds".format(dx,dy) )
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
               calculate_pointing = False
            else:
               calculate_pointing = True

#  Make a map from the time series. Ensure we continue to process any remaining
#  observation chunks if the current chunk fails.
            try:

#  Get the path to the map.
               if automask:
                  mapname = "{0}/{1}_{2}map".format(mapdir,key,qui.lower())
               else:
                  mapname = "{0}/{1}_{2}map".format(mapdir,key,qui)

#  If REUSE is TRUE and an old map exists, re-use it.
               try:
                  if reuse:
                     qui_maps[key] = NDG(mapname, True)
                     msg_out("   Re-using previously created map {0}".format(qui_maps[key]))
                  else:
                     raise starutil.NoNdfError("Ignoring pre-existing data")

#  Otherwise create a new map.  The call signature for makemap depends on
#  whether an external mask is being supplied or not.
               except starutil.NoNdfError:
                  qui_maps[key] = NDG(mapname, False)
                  if not maskmap:
                     invoke("$SMURF_DIR/makemap in={0} config=^{1} out={2} ref={3} pointing={4} "
                         "{5} {6}".format(isdf,conf,qui_maps[key],ref,pntfile,pixsize_par,ip))
                  else:
                     invoke("$SMURF_DIR/makemap in={0} config=^{1} out={2} ref={3} pointing={4} "
                         "{5} {6} mask2={7}".format(isdf,conf,qui_maps[key],astmask,pntfile,
                                                    pixsize_par,ip,pcamask))

#  If no ref map was supplied, use the first map for the first observation as
#  the ref map so that all maps are aligned.
               if ref == "!":
                  ref = qui_maps[key]

#  Append this map to the text file listing all maps to be included in the final
#  coadds.
               with open(allmaps, 'a') as infile:
                  infile.write( "{0}\n".format( qui_maps[key] ) )

#  If the pointing correction is not already known, and we have just
#  created an I map, see what translations (in pixels) are needed to align
#  the new I map with the reference map. The determination of the shift is
#  more accurate if we first mask out background areas. Use the AST mask to
#  define source pixels, but only if the mask contains a reasonable number
#  of pixels (very faint sources will have very small or non-existant AST
#  masks).
               if calculate_pointing and qui == 'I':
                  invoke("$KAPPA_DIR/showqual ndf={0}".format(qui_maps[key]))
                  if get_task_par( "QNAMES(1)", "showqual" ) == "AST":
                     bb = 1
                  elif get_task_par( "QNAMES(2)", "showqual" ) == "AST":
                     bb = 2
                  elif get_task_par( "QNAMES(3)", "showqual" ) == "AST":
                     bb = 4
                  else:
                     bb = 0

                  if bb > 0:
                     invoke("$KAPPA_DIR/setbb ndf={0} bb={1}".format(qui_maps[key],bb))

#  Clear badbits to use the whole map if the above masking results in too
#  few pixels.
                  invoke("$KAPPA_DIR/stats ndf={0}".format(qui_maps[key]))
                  nused = float( get_task_par( "numgood", "stats" ) )
                  if nused < 400:
                     invoke("$KAPPA_DIR/setbb ndf={0} bb=0".format(qui_maps[key]))

#  Find the pixel shift that aligns features in this masked, trimmed I map with
#  corresponding features in the reference map.
                  try:
                     invoke("$KAPPA_DIR/align2d ref={0} out=! in={1} form=3 "
                            "corlimit=0.7 rebin=no method=sincsinc params=\[0,2\]".
                            format(ref,qui_maps[key]))
                     dx = float( get_task_par( "TR(1)", "align2d" ) )
                     dy = float( get_task_par( "TR(4)", "align2d" ) )

#  If align2d failed, use silly dx,dy values to ensure it is flagged by
#  the following code.
                  except starutil.AtaskError:
                     dx = 1E6
                     dy = 1E6

#  Reset the bad-bits mask.
                  if bb > 0:
                     invoke("$KAPPA_DIR/setbb ndf={0} bb=0".format(qui_maps[key]))

#  If the shifts are suspiciously high, we do not believe them. In which
#  case we cannot do pointing ocorrection when creating the Q and U maps.
                  if abs(dx) > 5 or abs(dy) > 5:
                     pointing_dx = "null"
                     pointing_dy = "null"
                     dx = None
                     dy = None
                     msg_out( "\nWARNING: The I map created from the POL2 data cannot be aligned "
                              "with the supplied reference map.\n" )

#  Otherwise, convert the offset in pixels to (longitude,latitude) offsets
#  in the sky system of the reference map, in arc-seconds....
                  else:

#  Strip the wavelength axis off the total intensity map created above.
                     imap2d = NDG( 1 )
                     invoke("$KAPPA_DIR/ndfcopy in={0} out={1} trim=yes".format(qui_maps[key],imap2d))

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
                     msg_out( "Storing pointing corrections of ({0},{1}) "
                              "arc-seconds for future use".format(dx,dy) )

#  Store the pointing corrections as FITS headers within the map. Do this whether
#  they were calculated here or inherited from an earlier map.
               if dx is not None and dy is not None:
                  sym = invoke("$KAPPA_DIR/wcsattrib ndf={0} mode=get name='Symbol(1)'".
                                     format(qui_maps[key]))
                  invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=POINT_DX "
                         "edit=w value={1} comment=\"'{2} pointing correction [arcsec]'\""
                         " position=! mode=interface".format(qui_maps[key],dx,sym))

                  sym = invoke("$KAPPA_DIR/wcsattrib ndf={0} mode=get name='Symbol(2)'".
                               format(qui_maps[key]))
                  invoke("$KAPPA_DIR/fitsmod ndf={0} keyword=POINT_DY "
                         "edit=w value={1} comment=\"'{2} pointing correction [arcsec]'\""
                         " position=! mode=interface".format(qui_maps[key],dy,sym))

            except starutil.AtaskError:
               msg_out("WARNING: makemap failed - could not produce a {1} map "
                       "for observation chunk {0}".format(key,qui) )
               if ref == qui_maps[key]:
                  ref = "!"
               del qui_maps[key]









#  -----------  CREATE THE COADDED MAPS ------------------------


#  Create a text file holding information about all the Stokes maps. For each one,
#  get the Stokes parameter (Q, U or I) and a key that is unique for the chunk of
#  data, of the form "<UT>_<OBS>_<SUBSCAN>".
   mapinfo = NDG.tempfile()
   mapndg = NDG("^{0}".format(allmaps) )
   invoke("$SMURF_DIR/pol2check in={0} quiet=yes mapinfo={1}".
          format(mapndg,mapinfo))

   if not get_task_par( "MAPFOUND", "pol2check" ):
      raise starutil.InvalidParameterError("No usable maps remains to be processed.")

#  Set up a dict. Each key is as described above. Each value is path to an
#  NDF holding a map with the same key and I, Q or U Stokes parameter.
   with open(allmaps) as infile:
      lines = infile.readlines()
   paths = [line.strip() for line in lines]

   with open(mapinfo) as infile:
      lines = infile.readlines()
   infos = [line.strip() for line in lines]

   imaps = {}
   qmaps = {}
   umaps = {}
   for (path,info) in zip( paths, infos ):
      (stokes,id) = info.split()
      if stokes == "I":
         imaps[id] = path
      elif stokes == "Q":
         qmaps[id] = path
      else:
         umaps[id] = path

      if "pol" not in get_fits_header( NDG(path), "INBEAM" ):
         raise starutil.InvalidParameterError("One of the {0} maps ({1}) "
                                          "was not created from POL2 data".
                                          format(stokes,path))

#  Loop over each Stokes parameter, creating a coadd for each required
#  parameter.
   for qui in ('I', 'Q', 'U'):

#  Pass on to the next parameter if we are not creating a map for the
#  current parameter. Also create dictionaries holding the I, Q or U map
#  for each observation chunk.
      if qui == 'I':
         if imap:
            qui_maps = imaps
            coadd = imap
         else:
            continue

      elif qui == 'Q':
         if qmap:
            qui_maps = qmaps
            coadd = qmap
         else:
            continue

      else:
         if umap:
            qui_maps = umaps
            coadd = umap
         else:
            continue

#  Check some good maps remain to be processed.
      if len(qui_maps) == 0:
         raise starutil.InvalidParameterError("No usable {0} maps remains "
                                              "to be coadded.".format(qui))

#  If we have only one observation just copy it to the output maps.
      if len(qui_maps) == 1:
         key = list(qui_maps)[0]
         invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(qui_maps[key],coadd))

#  If we have more than one observation, coadd them. Also coadd the
#  extension NDFs (EXP_TIMES and WEIGHTS), but without normalisation so
#  that the coadd is the sum rather than the mean of the inputs.
      elif len(qui_maps) > 1:

         msg_out("Coadding {0} maps from all observations:".format(qui))
         allmaps = NDG( list( qui_maps.values() ) )
         invoke("$CCDPACK_DIR/makemos in={0} out={1} method=mean".format(allmaps,coadd))

         invoke("$KAPPA_DIR/erase object={0}.more.smurf.exp_time ok=yes".format(coadd))
         invoke("$KAPPA_DIR/wcsmosaic in={{{0}}}.more.smurf.exp_time lbnd=! ref=! "
                "out={1}.more.smurf.exp_time conserve=no method=bilin norm=no "
                "variance=no".format(allmaps,coadd))

         invoke("$KAPPA_DIR/erase object={0}.more.smurf.weights ok=yes".format(coadd))
         invoke("$KAPPA_DIR/wcsmosaic in={{{0}}}.more.smurf.weights lbnd=! ref=! "
                "out={1}.more.smurf.weights conserve=no method=bilin norm=no "
                "variance=no".format(allmaps,coadd))





#  -----------  CREATE VECTOR CATALOGUE ------------------------


# The rest we only do if an output catalogue is reqired.
   if outcat:

#  We need I, Q and U maps to create a catalogue.
      if imap and qmap and umap:

#  Ensure the Q, U and I images all have the same bounds, equal to the
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
         msg = invoke( "$POLPACK_DIR/polvec {0} cat={1} debias={2} "
                       "radec=yes refupdate=no".format(cube,outcat,debias) )
         msg_out( "\n{0}\n".format(msg) )



#  -----------  TIDY UP ------------------------


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





