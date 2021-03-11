#!/usr/bin/env python

'''
*+
*  Name:
*     SKYLOOP

*  Purpose:
*     Create a map using the "inside-out" algorithm.

*  Language:
*     python (2.7 or 3.*)

*  Description:
*     This script makes a map from specified raw time-series data using
*     the algorithm described at
*     http://pipelinesandarchives.blogspot.co.uk/2012/10/inside-out-map-making.html.
*     It runs SMURF:MAKEMAP multiple times, performing a single iteration of
*     the Dynamic Iterative Map-Maker algorithm on each invocation,
*     including data from all chunks. Each map created by MAKEMAP is used
*     as the initial sky estimate for the next invocation. MAKEMAP subtracts
*     this initial sky estimate from the time-series data before starting
*     the first (and only) iteration, and then adds the initial sky estimate
*     back on at the end prior to creating the output map.
*
*     The script produces several intermediate files: a set of cleaned
*     time-series files that may be 2 to 3 times the size of the entire
*     set of raw data files included in the map, and a 2D map for every
*     iteration. These files are placed in a newly created directory that
*     is normally deleted before the script exits. The files can be retained
*     for debugging purposes if required by running the script with
*     "retain=yes" on the command line.
*
*     The temporary files are placed in a directory name "NDG_xxxxx",
*     located within the directory specified by environment variable
*     STAR_TEMP. If STAR_TEMP is not defined, they are placed in the system's
*     temporary directory (e.g. "/tmp").

*  Usage:
*     skyloop in out niter pixsize config [itermap] [ref] [mask2] [mask3]
*             [extra] [ipref] [retain] [msg_filter] [ilevel] [glevel]
*             [logfile] [restart]

*  ADAM Parameters:
*     CHUNKWGT = _LOGICAL (Read)
*        Controls the weight used for each chunk. If False, then the
*        weights are determined by the value of the CHUNKWGT FITS header
*        in each input time-stream data file (unit weight is used for any
*        data file that has no CHUNKWGT FITS header). If True, the weight
*        for each chunk is based on the normalised map change for each
*        chunk calculated on the previous iteration - the weight for a
*        chunk is the ratio of the mean of all the chunk map changes to
*        the chunk's own map change. Weights above 1.0 are limited to 1.0.
*        [False]
*     CONFIG = LITERAL (Read)
*        The MAKEMAP configuration parameter values to use. Additions
*        will be made as follows:
*
*        - First iteration:
*           ---
*           numiter=1
*           noi.export=1
*           exportNDF=(lut,ext,res,qua)
*           noexportsetbad=1
*           exportclean=1
*           ast.zero_notlast = 0
*           flt.zero_notlast = 0
*           com.zero_notlast = 0
*           pca.zero_notlast = 0
*           itermap=0
*           shortmap=0
*           bolomap=0
*           flagmap=<undef>
*           sampcube=0
*           diag.append=0
*           ---
*        - Subsequent iterations:
*           ---
*           numiter=1
*           noi.import=1
*           exportNDF=(res,qua)
*           doclean=0
*           importsky=ref
*           importlut=1
*           ext.import=1
*           ast.zero_notlast = 0
*           flt.zero_notlast = 0
*           com.zero_notlast = 0
*           pca.zero_notlast = 0
*           ast.zero_mask0 = <undef>
*           flt.zero_mask0 = <undef>
*           com.zero_mask0 = <undef>
*           pca.zero_mask0 = <undef>
*           flt.notfirst = 0
*           pca.notfirst = 0
*           pln.notfirst = 0
*           smo.notfirst = 0
*           itermap=0
*           shortmap=0
*           bolomap=0
*           flagmap=<undef>
*           sampcube=0
*           diag.append=1
*           downsampscale=0
*           downsampfreq=0
*           fakemap=<undef>
*           ---
*        - Last iteration:
*           ---
*           numiter=1
*           noi.import=1
*           doclean=0
*           importsky=ref
*           importlut=1
*           ext.import=1
*           ast.zero_notlast = 1
*           flt.zero_notlast = 1
*           com.zero_notlast = 1
*           pca.zero_notlast = 1
*           flt.notfirst = 0
*           pca.notfirst = 0
*           pln.notfirst = 0
*           smo.notfirst = 0
*           itermap=0
*           shortmap=<value specified by parameter CONFIG>
*           bolomap=<value specified by parameter CONFIG>
*           flagmap=<undef>
*           sampcube=0
*           diag.append=1
*           downsampscale=0
*           downsampfreq=0
*           fakemap=<undef>
*           ---
*     EXTRA = LITERAL (Read)
*        A string holding any extra command line options to be passed to
*        MAKEMAP (all invocations). [!]
*     GLEVEL = LITERAL (Read)
*        Controls the level of information to write to a text log file.
*        Allowed values are as for "ILEVEL". The log file to create is
*        specified via parameter "LOGFILE. ["ATASK"]
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
*     IN = NDF (Read)
*        The group of time series NDFs to include in the output map.
*     INITIALSKY = NDF (Read)
*        An NDF holding an initial guess at the final map. This should
*        contain any a priori expectations of what the final map should look
*        like. It is used to define the starting point for the iterative map-making
*        algorithm, in place of the usual flat map full of zeros. The data units
*        in the supplied NDF must be "pW". If an NDF is supplied, it is also used
*        to define the WCS and pixel bounds of the output map (any NDF supplied
*        for parameter REF is then ignored). [!]
*     IPREF = NDF (Read)
*        An existing NDF that is to be used to define the correction
*        to be made for instrumental polarisation (IP). It is only
*        accessed if the input data contains POL2 Q or U time-series
*        values, as created by SMURF:CALCQU. No IP correction is made
*        if a null (!) value is supplied. If a non-null value is supplied,
*        it should be an NDF that holds the total intensity (in pW)
*        within the area of sky covered by the output map. The supplied
*        NDF need not be pre-aligned with the output map - the WCS
*        information in the NDF will be used to aligned them. For each Q
*        or U value in the input time-streams, the corresponding total
*        intensity (I) value is found by sampling the supplied IPREF map
*        at the sky position of the Q/U value. This I value is multipled
*        by a factor that depends on elevation and focal plane position,
*        to get the IP correction. These Q and U corrections are
*        rotated so that they use the same reference direction as the input
*        Q/U data, corrected for extinction, and are then subtracted from
*        the input Q or U value before going on to make a map from the
*        corrected values. [!]
*     ITERMAP = NDF (Write)
*        A 3D NDF to create holding the maps from all iterations. [!]
*     LOGFILE = LITERAL (Read)
*        The name of the log file to create if GLEVEL is not NONE. The
*        default is "<command>.log", where <command> is the name of the
*        executing script (minus any trailing ".py" suffix), and will be
*        created in the current directory. Any file with the same name is
*        over-written. []
*     NITER = _INTEGER (Read)
*        The number of iterations to perform. A positive value specifies
*        a fixed number of iterations to perform. A negative value
*        indicates that iterations should continue until the normalized
*        change in the map between iterations is less than the value of
*        the "maptol" parameter in the configuration supplied by
*        parameter CONFIG (a maptol value of 0.05 is used if CONFIG does
*        not specify maptol). If a value of zero is supplied for NITER,
*        the value used will be read from the "numiter" parameter in the
*        configuration. [0]
*     MASK2 = NDF (Read)
*        An existing NDF that can be used to specify a second external mask
*        for use with either the AST, PCA, FLT or COM model. See configuration
*        parameters AST.ZERO_MASK, PCA.ZERO_MASK, FLT.ZERO_MASK and
*        COM.ZERO_MASK. Note, it is assumed that this image is aligned in
*        pixel coordinate with the output map. [!]
*     MASK3 = NDF (Read)
*        An existing NDF that can be used to specify a third external mask
*        for use with either the AST, PCA, FLT or COM model. See configuration
*        parameters AST.ZERO_MASK, PCA.ZERO_MASK, FLT.ZERO_MASK and
*        COM.ZERO_MASK. Note, it is assumed that this image is aligned in
*        pixel coordinate with the output map. [!]
*     MSG_FILTER = LITERAL (Read)
*        Controls the default level of information reported by Starlink
*        atasks invoked within the executing script. The accepted values
*        are the list defined in SUN/104 ("None", "Quiet", "Normal",
*        "Verbose", etc). ["Normal"]
*     OBSDIR = LITERAL (Read)
*        The name of a directory in which to put maps made from the
*        individual observations. These are generated on the final
*        iteration. If null is supplied, individual observation maps will
*        not be created. Each map is stored in a file with name
*        <UT>_<OBS>.sdf. If a single observation is split into multiple
*        chunks, the first chunk will use the above naming scheme but the
*        second and subsequent chunks will have names of the form
*        <UT>_<OBS>_<CHUNK>.sdf. [!]
*     OUT = NDF (Write)
*        The NDF holding the output map.
*     PIXSIZE = _REAL (Read)
*        Pixel dimensions in the output image, in arcsec. The same value
*        will be used for both axes. The default depends on the wavelength
*        of the input data. []
*     REF = NDF (Read)
*        An existing NDF that is to be used to define the output grid.
*        If supplied, the output grid will be aligned with the supplied
*        reference NDF. The reference can be either 2D or 3D and the spatial
*        frame will be extracted. If a null (!) value is supplied then the
*        output grid is determined by parameters REFLON, REFLAT, etc.
*        In addition, this NDF can be used to mask the AST, PCA, FLT or COM
*        model. See configuration parameters AST.ZERO_MASK, PCA.ZERO_MASK,
*        FLT.ZERO_MASK and COM.ZERO_MASK.
*
*        This parameter is only accessed if a null value is supplied for
*        INITIALSKY. Otherwise, the NDF supplied as the initial sky is
*        used to define the output grid.
*
*        On the second and subsequent invocations of MAKEMAP, any
*        supplied REF image is replaced by the map created by the previous
*        invocation of MAKEMAP. [!]
*     RESTART = LITERAL (Read)
*        If a value is assigned to this parameter, it should be the path
*        to a directory containing the intermediate files created by a
*        previous run of SKYLOOP. If supplied, execution of skyloop will
*        restart from the point where the previous run finished. This is
*        useful for continuing runs that have been interupted accidentally.
*        The path to the intermediate files can be found by examining the
*        log file created by the previous run. [!]
*     RETAIN = _LOGICAL (Read)
*        Should the temporary directory containing the intermediate files
*        created by this script be retained? If not, it will be deleted
*        before the script exits. If retained, a message will be
*        displayed at the end specifying the path to the directory. [FALSE]

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     24-OCT-2012 (DSB):
*        Original version
*     16-JAN-2013 (DSB):
*        - Only include any supplied REF value in the makemap comamnd line
*        on the first iteration.
*        - Record quality info in the final map.
*     9-JAN-2013 (DSB):
*        Add support for diagnostics.
*     28-MAR-2013 (DSB):
*        Added parameter RESTART.
*     1-JUL-2013 (DSB):
*        Do not export cleaned data on the first iteration if the supplied
*        data has already been cleaned. Instead, re-use the supplied data on
*        subsequent iterations.
*     10-JUL-2013 (DSB):
*        Add support for ast.skip parameter.
*     9-SEP-2013 (DSB):
*        Add support for "..._last" parameters.
*     27-SEP-2013 (DSB):
*        Changed from using noi.calcfirst=1 to noi.calcfirst=0. The NOI
*        model is now calculated after the first iteration is completed,
*        and is exported for use on subsequent iterations. Previously, it
*        was calculated afresh on every iteration, before the iteration
*        commenced. This means that it is now possible for skyloop to
*        recognise and use the noi.box_size parameter. But first results
*        suggest that the noise values are less stable without calcfirst
*        set, causing some bolometers to be given inappropriately small
*        variances, and thus be over-emphasised in the final map,
*        resulting in visible bolometer tracks. If this is a problem, add
*        "noi.calcfirst=1" to your config., and remove "noi.box_size".
*     9-DEC-2013 (DSB):
*        - Fix nasty bug which caused the raw (i.e. uncleaned) data to be
*        used on every iteration, even though "doclean=0" was used on the
*        second and subsequent iteration, thus causing the map to be
*        formed from uncleaned data.
*        - Ensure only one iteration is used on the second and subsequent
*        invocations of makemap, even if ast.skip is non-zero.
*     8-JAN-2014 (DSB):
*        - Fix bug that caused NOI model to be ignored on all iterations.
*        - Update quality flags in cleaned data after each invocation of makemap.
*        - Cache LUT model values.
*     14-JAN-2014 (DSB):
*        Ensure same map bounds are used on every invocation of makemap.
*     14-FEB-2014 (DSB):
*        Ensure downsampling occurs only on the first invocation of makemap.
*     4-MAR-2014 (DSB):
*        Do not update quality flags at the end of each iteration.
*     14-MAY-2014 (DSB):
*        Abort if ast.skip is negative.
*     11-JUN-2015 (DSB):
*        Only add on any fakemap on the first iteration.
*     19-MAY-2016 (DSB):
*        Aded parameter IPREF. IP correction is performed only on the
*        first iteration, as the effects of IP correction are incorporated
*        into the cleaned data created by the first iteration, and passed
*        on to subsequent iterations.
*     10-NOV-2016 (DSB):
*        Revert the 4-MAR-2014 change, so that the quality array in the
*        cleaned time series data is again updated by each call to
*        makemap. The 4-MAR-2014 change was made because the time-series
*        quality data was mangled by being reduced to 8 bits in
*        smf_qual_unmap, and consequently was useless for later iterations.
*        But this means that spike flags are lost. Now, the new exportqbits
*        config parameter is used to tell smf_qual_unmap to export the
*        SPIKE flag. We also export the flags set by the initial cleaning
*        process (BADDA,BADBOL,DCJUMP,STAT,NOISE,PAD), so that they are
*        available to later iterations. Since this is only 7 bit, they can
*        be exported without loss and so can be re-used on subsequent
*        iterations. Things like COM and RING are recalculated from
*        scratch on each iteration and so do not need to be exported.
*     28-NOV-2016 (DSB):
*        Take account of the value of ast.skip when determining whether to
*        freeze (xxx.zero_freeze) or apply (xxx.zero_niter) masks, etc.
*     15-JAN-2018 (DSB):
*        Added parameter OBSDIR.
*     18-JAN-2018 (DSB):
*        Modify PCA-related config parameters in the same way that
*        FLT-related parametrers are modified.
*     23-JAN-2018 (DSB):
*        Handle ZERO_NITER values between 0.0 and 1.0.
*     14-MAR-2018 (DSB):
*        Put the temporary NDFs created by makemap (models, cleaned data,
*        etc) in the temporary directory rather than in the current
*        directory.
*     9-MAR-2020 (DSB):
*        Allow shortmaps and bolomaps to be created. If they are requested
*        in the config supplied via parameter CONFIG, then they are created on
*        the final iteration.
*     26-NOV-2020 (DSB):
*        Add parameter INITIALSKY.
*     5-MAR-2021 (DSB):
*        Support config parameter XXX.ZERO_MASK0
*     10-MAR-2021 (DSB):
*        Support config parameter AST.MAPSPIKE_FREEZE
*     11-MAR-2021 (DSB):
*        - Ensure the quality masks are switched off in the output map created
*        by the first invocation of makemap. Otherwise, they can affect the
*        normalised map change calculated after the second invocation of makemap.
*        - Correct handling of XXX.ZERO_MASK0.
*        - Support COM.FREEZE_FLAGS values that are specified as a maptol value 
*        (i.e. fractional values between zero and one).
*-
'''


import glob
import os
import re
import shutil
import starutil
import sys
from starutil import invoke
from starutil import NDG
from starutil import msg_out


#  Assume for the moment that we will not be retaining temporary files.
retain = 0

#  A function that converts a string to an int, handling cases where the
#  string ends with ".0" (the python intrinsic "int" function moans in
#  such cases).
def myint( text ):
   return int( float( text ) + 0.5 )

#  A function to clean up before exiting. Delete all temporary NDFs etc,
#  unless the script's RETAIN parameter indicates that they are to be
#  retained. Also delete the script's temporary ADAM directory.
def cleanup():
   global retain, new_ext_ndfs, new_lut_ndfs, new_noi_ndfs
   try:
      if retain:
         msg_out( "Retaining EXT, LUT and NOI models and all "
                  "temporary files in {0}".format(NDG.tempdir))
      else:
         NDG.cleanup()
      starutil.ParSys.cleanup()
   except Exception:
      pass

#  Catch any exception so that we can always clean up, even if control-C
#  is pressed.
try:

#  Declare the script parameters. Their positions in this list define
#  their expected position on the script command line. They can also be
#  specified by keyword on the command line. If no value is supplied on
#  the command line, the  user is prompted for a value when the parameter
#  value is first accessed within this script. The parameters "MSG_FILTER",
#  "ILEVEL", "GLEVEL" and "LOGFILE" are added automatically by the ParSys
#  constructor.
   params = []

   params.append(starutil.ParNDG("IN", "The input time series NDFs",
                                  starutil.get_task_par("DATA_ARRAY",
                                                        "GLOBAL",
                                                    default=starutil.Parameter.UNSET)))

   params.append(starutil.ParNDG("OUT", "The output map", default=None,
                                  exists=False, minsize=0, maxsize=1 ))

   params.append(starutil.Par0I("NITER", "No. of iterations to perform",
                                0, noprompt=True))

   params.append(starutil.Par0F("PIXSIZE", "Pixel size (arcsec)", None,
                                 maxval=1000, minval=0.01))

   params.append(starutil.ParGrp("CONFIG", "Map-maker tuning parameters",
                                "^$STARLINK_DIR/share/smurf/dimmconfig.lis"))

   params.append(starutil.ParNDG("ITERMAP", "Output cube holding itermaps",
                                 default=None, exists=False, minsize=0,
                                 maxsize=1, noprompt=True ))

   params.append(starutil.ParNDG("REF", "The reference NDF", default=None,
                                 minsize=0, maxsize=1, noprompt=True ))

   params.append(starutil.ParNDG("MASK2", "The second mask NDF", default=None,
                                 minsize=0, maxsize=1, noprompt=True ))

   params.append(starutil.ParNDG("MASK3", "The third mask NDF", default=None,
                                 minsize=0, maxsize=1, noprompt=True ))

   params.append(starutil.Par0S("EXTRA", "Extra command-line options for MAKEMAP",
                                default=None, noprompt=True ))

   params.append(starutil.ParNDG("IPREF", "Total intensity map for IP "
                                 "correction of POL2 data", default=None,
                                 minsize=0, maxsize=1, noprompt=True ))

   params.append(starutil.Par0L("CHUNKWGT", "Use separate weights for each chunk?",
                                False, noprompt=True))

   params.append(starutil.Par0L("RETAIN", "Retain temporary files?", False,
                                 noprompt=True))

   params.append(starutil.Par0S("RESTART", "Directory holding data from an interrupted run of skyloop",
                                default=None, noprompt=True ))

   params.append(starutil.Par0S("OBSDIR", "Directory in which to save maps "
                                "made from individual observations", None,
                                noprompt=True))

   params.append(starutil.ParNDG("INITIALSKY", "An a priori guess at final map",
                                 default=None, minsize=0, maxsize=1, noprompt=True ))

#  Initialise the parameters to hold any values supplied on the command
#  line. This automatically adds definitions for the additional parameters
#  "MSG_FILTER", "ILEVEL", "GLEVEL" and "LOGFILE".
   parsys = starutil.ParSys( params )

#  It's a good idea to get parameter values early if possible, in case
#  the user goes off for a coffee whilst the script is running and does not
#  see a later parameter prompt or error.
   restart = parsys["RESTART"].value
   if restart is None:
      retain = parsys["RETAIN"].value
   else:
      retain = True
      NDG.tempdir = restart
      NDG.overwrite = True
      msg_out( "Re-starting using data in {0}".format(restart) )

   indata = parsys["IN"].value
   outdata = parsys["OUT"].value
   niter = parsys["NITER"].value
   pixsize = parsys["PIXSIZE"].value
   config = parsys["CONFIG"].value
   mask2 = parsys["MASK2"].value
   mask3 = parsys["MASK3"].value
   extra = parsys["EXTRA"].value
   ipref = parsys["IPREF"].value
   itermap = parsys["ITERMAP"].value
   obsdir =  parsys["OBSDIR"].value
   chunkwgt =  parsys["CHUNKWGT"].value

#  If no initial sky is supplied, get a reference NDF.
   initsky = parsys["INITIALSKY"].value
   if initsky is None:
      ref = parsys["REF"].value

#  Otherwise, use it as the reference NDF and check it has units of "pW".
#  Also ensure the map created by makemap has the same pixel bounds as the
#  initial sky.
   else:
      invoke("$KAPPA_DIR/ndftrace ndf={0} quiet=yes".format(initsky) )
      units = starutil.get_task_par( "UNITS", "ndftrace" ).replace(" ", "")
      if units != "pW":
         raise starutil.InvalidParameterError("Initial sky map {0} has "
                       "units '{1}' - units must be 'pW'".format(initsky,units))
      lx = starutil.get_task_par( "lbound(1)", "ndftrace" )
      ly = starutil.get_task_par( "lbound(2)", "ndftrace" )
      ux = starutil.get_task_par( "ubound(1)", "ndftrace" )
      uy = starutil.get_task_par( "ubound(2)", "ndftrace" )

#  The supplied initial sky map may have been created by makemap or
#  skyloop, in which case it may contain components that would affect the
#  behaviour of makemap now. For instance, the initial sky map may
#  contain a quality component containing an AST mask. If this were
#  passed on to makemap unchanged, it would cause makemap to use the AST
#  mask in the supplied initial sky map, rather than the AST mask implied by
#  the config parameters. The same sort of thing applies to the Variance
#  component and any NDFs contained in the SMURF extension. So take a
#  copy of the supplied initial sky NDF and delete all these components.
      tsky = NDG( 1 )
      invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(initsky,tsky) )
      invoke("$KAPPA_DIR/erase object={0}.Variance ok=yes report=no".format(tsky) )
      invoke("$KAPPA_DIR/erase object={0}.Quality ok=yes report=no".format(tsky) )
      invoke("$KAPPA_DIR/erase object={0}.more.smurf ok=yes report=no".format(tsky) )
      initsky = tsky
      ref = initsky

#  See if we are using pre-cleaned data, in which case there is no need
#  to export the cleaned data on the first iteration. Note we need to
#  convert the string returned by "invoke" to an int explicitly, otherwise
#  the equality is never satisfied and we end up assuming that the raw
#  data has been precleaned, even if it hasn't been precleaned.
   if myint( invoke( "$KAPPA_DIR/configecho name=doclean config={0} "
              "defaults=$SMURF_DIR/smurf_makemap.def "
              "select=\"\'450=0,850=1\'\" defval=1".format(config))) == 1:
      precleaned = False
   else:
      precleaned = True

#  If requested, use numiter from the config file. Arbitrarily choose 850 um
#  values for the waveband-specific parameters, but these are not actually used.
   if niter == 0:
      niter = myint( invoke( "$KAPPA_DIR/configecho name=numiter config={0} "
                           "defaults=$SMURF_DIR/smurf_makemap.def "
                           "select=\"\'450=0,850=1\'\" defval=5".format(config)))

#  If iterating to convergence, get the maximum allowed normalised map
#  change between iterations, and set the number of iterations positive.
   if niter < 0:
      niter = -niter
      maptol = float( invoke( "$KAPPA_DIR/configecho name=maptol config={0} "
                           "defaults=$SMURF_DIR/smurf_makemap.def "
                           "select=\"\'450=0,850=1\'\" defval=0.05".format(config)))
   else:
      maptol = 0

   converged = False

#  Determine the value of the (AST,COM,PCA,FLT).ZERO_NITER, ZERO_NOTLAST and
#  ZERO_FREEZE parameters in the supplied config. We need to ensure that
#  appropriate changes are made to the values of these on each invocation
#  of makemap.
   zero_niter = {}
   zero_notlast = {}
   zero_freeze = {}
   zero_mask0 = {}
   for model in ["ast", "com", "flt", "pca"]:
      zero_niter[model] = float( invoke( "$KAPPA_DIR/configecho name={0}.zero_niter config={1} "
                                         "defaults=$SMURF_DIR/smurf_makemap.def "
                                         "select=\"\'450=0,850=1\'\"".format(model,config)))
      zero_notlast[model] = myint( invoke( "$KAPPA_DIR/configecho name={0}.zero_notlast config={1} "
                                       "defaults=$SMURF_DIR/smurf_makemap.def "
                                       "select=\"\'450=0,850=1\'\"".format(model,config)))
      zero_freeze[model] = myint( invoke( "$KAPPA_DIR/configecho name={0}.zero_freeze config={1} "
                                       "defaults=$SMURF_DIR/smurf_makemap.def "
                                       "select=\"\'450=0,850=1\'\"".format(model,config)))
      zero_mask0[model] = invoke( "$KAPPA_DIR/configecho name={0}.zero_mask0 config={1} "
                                  "defaults=$SMURF_DIR/smurf_makemap.def "
                                  "select=\"\'450=0,850=1\'\"".format(model,config))

#  Similarly, we need to record ast.mapspike_freeze, com.freeze_flags and
#  flt.ring_freeze.
   ast_mapspike_freeze = float( invoke( "$KAPPA_DIR/configecho name=ast.mapspike_freeze config={0} "
                                   "defaults=$SMURF_DIR/smurf_makemap.def "
                                   "select=\"\'450=0,850=1\'\"".format(config)))
   com_freeze_flags = float( invoke( "$KAPPA_DIR/configecho name=com.freeze_flags config={0} "
                                   "defaults=$SMURF_DIR/smurf_makemap.def "
                                   "select=\"\'450=0,850=1\'\"".format(config)))
   flt_ring_freeze = myint( invoke( "$KAPPA_DIR/configecho name=flt.ring_freeze config={0} "
                                   "defaults=$SMURF_DIR/smurf_makemap.def "
                                   "select=\"\'450=0,850=1\'\"".format(config)))

#  Save parameter values to be used on the last iteration (-1.0 if unset)
   filt_edge_largescale_last = float( invoke( "$KAPPA_DIR/configecho "
                                      "name=flt.filt_edge_largescale_last config={0} "
                                      "defaults=$SMURF_DIR/smurf_makemap.def "
                                      "select=\"\'450=0,850=1\'\" defval=-1.0".format(config)))
   filt_edge_smallscale_last = float( invoke( "$KAPPA_DIR/configecho "
                                      "name=flt.filt_edge_smallscale_last config={0} "
                                      "defaults=$SMURF_DIR/smurf_makemap.def "
                                      "select=\"\'450=0,850=1\'\" defval=-1.0".format(config)))
   filt_edgehigh_last = float( invoke( "$KAPPA_DIR/configecho "
                               "name=flt.filt_edgehigh_last config={0} "
                               "defaults=$SMURF_DIR/smurf_makemap.def "
                               "select=\"\'450=0,850=1\'\" defval=-1.0".format(config)))
   filt_edgelow_last = float( invoke( "$KAPPA_DIR/configecho "
                               "name=flt.filt_edgelow_last config={0} "
                               "defaults=$SMURF_DIR/smurf_makemap.def "
                               "select=\"\'450=0,850=1\'\" defval=-1.0".format(config)))
   flt_whiten_last = myint( invoke( "$KAPPA_DIR/configecho "
                               "name=flt.whiten_last config={0} "
                               "defaults=$SMURF_DIR/smurf_makemap.def "
                               "select=\"\'450=0,850=1\'\" defval=-1".format(config)))
   com_perarray_last = myint( invoke( "$KAPPA_DIR/configecho "
                               "name=com.perarray_last config={0} "
                               "defaults=$SMURF_DIR/smurf_makemap.def "
                               "select=\"\'450=0,850=1\'\" defval=-1".format(config)))

#  Get the number of iterations for which no AST model should be used.
   ast_skip = myint( invoke( "$KAPPA_DIR/configecho name=ast.skip config={0} "
                           "defaults=$SMURF_DIR/smurf_makemap.def "
                           "select=\"\'450=0,850=1\'\"".format(config)))
   if ast_skip < 0 :
      msg_out("\nThe ast.skip parameter is set to {0} in the supplied "
              "config. skyloop does not handle negative ast.skip "
              "values. Use makemap instead (there is no benefit "
              "in a skyloop-style algorithm since no AST model "
              "is used).".format(ast_skip))
      cleanup()
      sys.exit()

#  See if low frequency changes are to be removed from the map on each
#  iteration.
   ast_filt_diff = float( invoke( "$KAPPA_DIR/configecho "
                               "name=ast.filt_diff config={0} "
                               "defaults=$SMURF_DIR/smurf_makemap.def "
                               "select=\"\'450=0,850=1\'\" defval=0.0".format(config)))

#  See if shortmaps are to be created on the final iteration.
   shortmap = float( invoke( "$KAPPA_DIR/configecho name=shortmap config={0} "
                             "defaults=$SMURF_DIR/smurf_makemap.def "
                             "select=\"\'450=0,850=1\'\" defval=0.0".format(config)))

#  See if bolomaps are to be created on the final iteration.
   bolomap = myint( invoke( "$KAPPA_DIR/configecho name=bolomap config={0} "
                            "defaults=$SMURF_DIR/smurf_makemap.def "
                            "select=\"\'450=0,850=1\'\" defval=0.0".format(config)))

#  Create a directory in which makemap should store the dumped models, cleaned data,
#  etc.
   dumpdir = NDG.subdir()

#  Find the number of iterations to perform on the initial invocation of
#  makemap.
   niter0 = 1 + ast_skip
   if niter0 > niter:
      niter0 = niter

#  Adjust config parameters that count iterations excluding the initial
#  "skip" iterations. The "iter" variable below starts at 1 for the first
#  skipped iteration, so we need to include the skip iterations in these
#  counts.
   if ast_skip > 0:
      for model in ["ast", "com", "flt", "pca"]:
         if zero_niter[model] >= 1.0:
            zero_niter[model] += ast_skip;

         if zero_freeze[model] > 0:
            zero_freeze[model] += ast_skip;

      if com_freeze_flags >= 1.0:
         com_freeze_flags += ast_skip

      if flt_ring_freeze > 0:
         flt_ring_freeze += ast_skip

      if ast_mapspike_freeze >= 1.0:
         ast_mapspike_freeze += ast_skip

#  On the first invocation of makemap, we use the raw data files specified
#  by the IN parameter to create an initial estimate of the sky. We also
#  save the cleaned time series data, and the EXT, LUT and NOI models (if we
#  are doing more than one iteration), for use on subsequent iterations (this
#  speeds them up a bit). First create a text file holding a suitably modified
#  set of configuration parameters. This file is put in the NDG temp
#  directory (which is where we store all temp files).
   conf0 = os.path.join(NDG.tempdir,"conf0") # Full path to new config file
   fd = open(conf0,"w")       # Open the new config file.
   fd.write("{0}\n".format(config)) # Inherit the supplied config parameter values.
   fd.write("numiter={0}\n".format(niter0))    # MAKEMAP should do only one
                              # iteration (plus any skipped iterations).
   fd.write("itermap=0\n")    # Itermaps don't make sense
   fd.write("bolomap=0\n")    # Bolomaps only make sense on the last iteration
   fd.write("shortmap=0\n")   # Shortmaps only make sense on the last iteration
   fd.write("flagmap=<undef>\n")# Flagmaps don't make sense
   fd.write("sampcube=0\n")   # Sampcubes don't make sense
   fd.write("dumpdir={0}\n".format(dumpdir)) # Where to dump models, cleaned data etc

   if niter > 1:
      fd.write("noi.export=1\n") # Export the NOI model. This forces the
                                 # NOI model to be created and exported after
                                 # the first iteration has completed.
      fd.write("exportNDF=(lut,ext,res,qua)\n")# Save the EXT, LUT model values to
                                 # avoid re-calculation on each invocation of makemap.
                                 # Also need QUA to update time-series flags
      fd.write("exportqbits=(BADBOL,BADDA,DCJUMP,STAT,NOISE,PAD,SPIKE)\n")# We only
                                 # export quality flags that are not recalculated on
                                 # each iteration (we must have fewer than 8 bits)
      fd.write("noexportsetbad=1\n")# Export good EXT values for bad bolometers
      if not precleaned:
         fd.write("exportclean=1\n")  # Likewise save the cleaned time-series data.

   fd.write("ast.zero_notlast = 0\n") # Masking is normally not performed
   fd.write("flt.zero_notlast = 0\n") # on the last iteration. But the first
   fd.write("com.zero_notlast = 0\n") # iteration is also the last iteration
   fd.write("pca.zero_notlast = 0\n") # iteration is also the last iteration
                              # in our case, so force any enabled
                              # masking to be performed on the last iteration.
   fd.write("diag.append = 0\n") # Ensure a new diagnostics file is started

   fd.write("flt.filt_edge_largescale_last=<undef>\n") # Ensure these parameter
   fd.write("flt.filt_edge_smallscale_last=<undef>\n") # are only used on the
   fd.write("flt.filt_edgehigh_last=<undef>\n")        # final iteration. We
   fd.write("flt.filt_edgelow_last=<undef>\n")         # reset them here in
   fd.write("flt.whiten_last=<undef>\n")               # case they are set in
   fd.write("com.perarray_last=<undef>\n")             # the supplied config.
   if precleaned:
      fd.write("downsampscale = 0\n") # Cleaned data will have been downsampled already.
      fd.write("downsampfreq = 0\n")

   if initsky:
      fd.write("importsky = ref\n")   # If an initial sky was supplied use it.

   fd.close()                 # Close the config file.

#  Get the name of a temporary NDF that can be used to store the first
#  iteration map. This NDF is put in the NDG temp directory. If we are
#  only doing one iteration, used the supplied output NDF name.
   if niter == 1:
      newmap = outdata
   else:
      newmap = NDG(1)
   prevmap = None

#  Start a list of these maps in case we are creating an output itermap cube.
   maps = []
   maps.append(newmap)

#  If we are restarting, check if the NDF already exists and is readable.
#  If so, we do not re-create it.
   msg_out( "Iteration 1...")
   gotit = False
   if restart is not None:
      try:
         invoke("$KAPPA_DIR/ndftrace ndf={0} quiet=yes".format(newmap))
         msg_out( "Re-using existing map {0}".format(newmap) )
         gotit = True

#  Get the pixel index bounds of the map.
         lx = starutil.get_task_par( "lbound(1)", "ndftrace" )
         ly = starutil.get_task_par( "lbound(2)", "ndftrace" )
         ux = starutil.get_task_par( "ubound(1)", "ndftrace" )
         uy = starutil.get_task_par( "ubound(2)", "ndftrace" )

      except Exception:
         pass

#  If required, construct the text of the makemap command and invoke it.
   if not gotit:
      cmd = "$SMURF_DIR/makemap in={0} out={1} method=iter config='^{2}'".format(indata,newmap,conf0)
      if pixsize:
         cmd += " pixsize={0}".format(pixsize)
      if ref:
         cmd += " ref={0}".format(ref)
      if mask2:
         cmd += " mask2={0}".format(mask2)
      if mask3:
         cmd += " mask3={0}".format(mask3)
      if extra:
         cmd += " "+extra
      if ipref and not precleaned:
         cmd += " ipref={0}".format(ipref)
      if initsky:
         cmd += " lbnd=\[{0},{1}\] ubnd=\[{2},{3}\]".format(lx,ly,ux,uy)
      invoke(cmd)

#  Ensure all quality masks are off (makemap will have left them on
#  because of ast.zero_notlast being set to 0 above). This is needed
#  as otherwise, the quality mask will affect the normalised map change
#  calculated after the next invocation of makemap.
      invoke("$KAPPA_DIR/setbb ndf={0} bb=0".format(newmap) )

#  If a separate mask was specified for the first iteration using config
#  param "xxx.zero_mask0" then we need to remove the corresponding
#  quality name from the map created above so that the mask created on the
#  first iteration will not get re-used on subsequent iterations.
      qnames = ""
      for model in ["ast", "com", "flt", "pca"]:
         if zero_mask0[model] != "<***>":
            qnames += "{0},".format(model)
      if qnames != "":
         invoke("$KAPPA_DIR/remqual ndf={0} qnames=\"'{1}'\" clear=yes".format(newmap,qnames[:-1]) )

#  Get the pixel index bounds of the map (we already know these if an
#  initial sky was supplied).
      if not initsky:
         lx = starutil.get_task_par( "lbound(1)", "makemap" )
         ly = starutil.get_task_par( "lbound(2)", "makemap" )
         ux = starutil.get_task_par( "ubound(1)", "makemap" )
         uy = starutil.get_task_par( "ubound(2)", "makemap" )

#  Get the paths to the cleaned files. Also get the paths to the
#  files holding the quality flags at the end of each invocation of
#  makemap.
   if niter > 1:
      if not precleaned:
         cleaned = NDG( os.path.join( dumpdir, "*_con_res_cln.sdf"))
         qua = NDG( cleaned, "*|_cln||" )
      else:
         cleaned = indata
         qua = None

#  Now do the second and subsequent iterations. These use the cleaned
#  time-series data created by the first iteration as their time-series
#  input, and use the output map from the previous iteration as their
#  initial guess at the sky. First create a map holding things to add
#  to the config for subsequent invocations.
      add = {}
      add["exportNDF"] = "(res,qua)" # Prevent EXT or LUT model being exported.
      add["exportclean"] = 0   # Prevent cleaned time-series data being exported.
      add["doclean"] = 0       # Do not clean the supplied data (it has
      add["importsky"] = "ref" # Get the initial sky estimate from the REF parameter.
      add["importlut"] = 1     # Import the LUT model created by the first iteration.
      add["ext.import"] = 1    # Import the EXT model created by the first iteration.
      add["flt.notfirst"] = 0  # Ensure we use FLT on 2nd and subsequent invocations
      add["pca.notfirst"] = 0  # Ensure we use PCA on 2nd and subsequent invocations
      add["pln.notfirst"] = 0  # Ensure we use PLN on 2nd and subsequent invocations
      add["smo.notfirst"] = 0  # Ensure we use SMO on 2nd and subsequent invocations
      add["diag.append"] = 1   # Ensure we append diagnostics to the file
                               # created on the first iteration.
      add["ast.skip"] = 0      # Ensure we do not skip any more AST models
      add["noi.import"] = 1    # Use the NOI model created by iteration 1
      add["noi.export"] = 0    # No need to export the NOI model again
      if ast_skip > 0:
         add["numiter"] = 1    # First invocation used (1+ast_skip) iterations
      add["downsampscale"] = 0 # Iter. 1 did any required downsampling. Later iters
      add["downsampfreq"] = 0  # must not further downsampling because the cache files
                               # are only appropriate for the origin downsampling.
      add["fakemap"] = "<undef>" # Iter. 1 added any required fakemap.
      for model in ["ast", "com", "flt", "pca"]:
         add["{0}.zero_mask0".format(model)] = "<undef>"

#  Now create the config, inheriting the config from the first invocation.
      iconf = 1
      confname = os.path.join(NDG.tempdir,"conf1")
      fd = open(confname,"w")
      fd.write("^{0}\n".format(conf0))# Inherit the first iteration config.
      for key in add:
         fd.write("{0}={1}\n".format( key, add[key] ))
      fd.close()

#  Indicate we do not need to create a new config file yet.
      newcon = 0

#  Get the name of an NDF in which to store the normalized map change
#  after each iteration.
      mapchange = NDG(1)

#  Now do the second (assuming none have been skipped) and subsequent
#  iterations.
      if ast_skip > 0:
         msg_out( "Skipping {0} iterations since ast.skip is set to {0}".format(ast_skip))

      meanchange = 1E10
      iter = niter0 + 1
      while iter <= niter:
         msg_out( "Iteration {0}...".format(iter))

#  On this iteration we will want to use the output map from the previous
#  iteration as the initial guess at the sky. So copy the new map name over
#  to the "prevmap" variable.
         prevmap = newmap

#  When "zero_niter" invocations have been performed, switch off zero
#  masking (so long as zero_niter > 0).  Do this for AST, PCA, COM and FLT
#  models.
         for model in ["ast", "com", "flt", "pca"]:
            if zero_niter[model] > 0 and (
               ( zero_niter[model] < 1.0 and meanchange <= zero_niter[model] ) or
               ( zero_niter[model] >= 1.0 and iter > zero_niter[model] ) ):
               zero_niter[model] = 0
               add[ model+".zero_niter" ] = -1
               newcon = 1

#  When "zero_freeze" invocations have been performed, switch freeze the
#  mask (so long as zero_freeze > 0).  Do this for AST, PCA, COM and FLT
#  models.
         for model in ["ast", "com", "flt", "pca"]:
            if zero_freeze[model] > 0 and iter > zero_freeze[model] + 1:
               zero_freeze[model] = 0
               add[ model+".zero_freeze" ] = -1
               newcon = 1

#  When "com_freeze_flags" invocations have been performed, freeze the
#  COM flags (so long as com_freeze_flags > 0).
         if com_freeze_flags > 0 and (
            ( com_freeze_flags < 1.0 and meanchange <= com_freeze_flags ) or
            ( com_freeze_flags >= 1.0 and iter > com_freeze_flags ) ):
            com_freeze_flags = 0
            add[ "com.freeze_flags" ] = -1
            newcon = 1

#  When "flt_ring_freeze" invocations have been performed, freeze the
#  ringing flags (so long as flt_ring_freeze > 0).
         if flt_ring_freeze > 0 and iter > flt_ring_freeze + 1:
            flt_ring_freeze = 0
            add[ "flt.ring_freeze" ] = -1
            newcon = 1

#  When "ast_mapspike_freeze" invocations have been performed, switch off the
#  map spike detection algorithm.
         if ast_mapspike_freeze > 0 and (
            ( ast_mapspike_freeze < 1.0 and meanchange <= ast_mapspike_freeze ) or
            ( ast_mapspike_freeze >= 1.0 and iter > ast_mapspike_freeze ) ):
            ast_mapspike_freeze = 0
            add[ "ast.mapspike" ] = 0
            newcon = 1

#  If this is the last iteration, put the output map in the NDF specified
#  by the script's "OUT" parameter. If required, change the config to
#  indicate that maps made from individual chunks should be created.
         if iter == niter:
            newmap = outdata
            if obsdir is not None:
               add["itermap"] = -2
               newcon = 1
               itermaps = os.path.join(NDG.tempdir,"chunkmaps")
            else:
               itermaps = None

#  Also, if this is the last iteration, create a modified configuration file
#  that supresses masking (unless the xxx.zero_notlast value in the
#  supplied config indicates otherwise).
            for model in ["ast", "com", "flt", "pca"]:
               if zero_notlast[model] != 0:
                  add["ast.zero_notlast"] = 1
                  newcon = 1

#  Also, if this is the last iteration, do not remove low frequency
#  changes from the map.
            if ast_filt_diff != 0.0:
               add["ast.filt_diff"] = 0.0
               newcon = 1

#  Also, if this is the last iteration, create shortmaps if requested in
#  the user-supplied config.
            if shortmap != 0.0:
               add["shortmap"] = shortmap
               newcon = 1

#  Also, if this is the last iteration, create bolomaps if requested in
#  the user-supplied config.
            if bolomap != 0:
               add["bolomap"] = bolomap
               newcon = 1

#  Also override the normal values for parameters that have a
#  corresponding "_last" value.
            if filt_edge_largescale_last != -1.0:
               add["flt.filt_edge_largescale_last"] = filt_edge_largescale_last
               newcon = 1

            if filt_edge_smallscale_last != -1.0:
               add["flt.filt_edge_smallscale_last"] = filt_edge_smallscale_last
               newcon = 1

            if filt_edgehigh_last != -1.0:
               add["flt.filt_edgehigh_last"] = filt_edgehigh_last
               newcon = 1

            if filt_edgelow_last != -1.0:
               add["flt.filt_edgelow_last"] = filt_edgelow_last
               newcon = 1

            if flt_whiten_last != -1:
               add["flt.whiten_last"] = flt_whiten_last
               newcon = 1

            if com_perarray_last != -1:
               add["com.perarray_last"] = com_perarray_last
               newcon = 1

#  No need to export quality flags on the last iteration.
            add["exportNDF"] = 0

#  If this is not the last iteration, get the name of a temporary NDF that
#  can be used to store the current iteration's map. This NDF is put in
#  the NDG temp directory.
         else:
            newmap = NDG(1)
            itermaps = None

#  Determine the weight to use for each chunk on the next iteration. Only
#  do this after 3 iterations have been done.
         if chunkwgt and iter > 3:
            chunkchange = []
            ichunk = 0
            while  True:
               ichunk += 1
               try:
                  chunkchange.append( starutil.get_task_par( "chunkchange({0})".format(ichunk),
                                                             "makemap" ))
               except starutil.StarUtilError:
                  break
            nchunk = len(chunkchange)
            meancc = sum(chunkchange)/nchunk

            text = "("
            for ichunk in range(nchunk):
               wgt = meancc/chunkchange[ichunk]
               wgt = 0.001*int(wgt*1000.0)
               if wgt > 1:
                  wgt = 1

               if ichunk > 0:
                  text += ","
               text += str( wgt )

            text += ")"
            msg_out("chunkweight = {0}".format(text))
            add["chunkweight"] = text
            newcon = 1

#  If required, create a new config file.
         if newcon:
            newcon = 0
            iconf += 1
            confname = os.path.join(NDG.tempdir,"conf{0}".format(iconf))
            fd = open(confname,"w")
            fd.write("^{0}\n".format(conf0))# Inherit the first iteration config.
            for key in add:
               fd.write("{0}={1}\n".format( key, add[key] ))
            fd.close()

#  Update the quality flags in the cleaned time-series data to be the
#  same as the flags exported at the end of the previous iteration.
         if qua is not None:
            invoke( "$KAPPA_DIR/setqual ndf={0} like={1}".format(cleaned,qua) )

#  See if the output NDF already exists.
         gotit = False
         if restart is not None:
            try:
               invoke("$KAPPA_DIR/ndftrace ndf={0} quiet=yes".format(newmap))
               msg_out( "Re-using existing map {0}".format(newmap) )
               gotit = True
            except Exception:
               pass

#  If required, construct the text of the makemap command and invoke it. We
#  specify the map from the previous iteration as the REF image. Since we are
#  re-using the LUT model from the first invocation, we need to ensure that
#  the maps bounds never change (as they may because of new data being
#  flagged for instance). So specify them explicitly when running makemap.
         if not gotit:
            cmd = "$SMURF_DIR/makemap in={0} out={1} method=iter config='^{2}' ref={3} lbnd=\[{4},{5}\] ubnd=\[{6},{7}\]".format(cleaned,newmap,confname,prevmap,lx,ly,ux,uy)
            if pixsize:
               cmd += " pixsize={0}".format(pixsize)
            if mask2:
               cmd += " mask2={0}".format(mask2)
            if mask3:
               cmd += " mask3={0}".format(mask3)
            if itermaps:
               cmd += " itermaps={0}".format(itermaps)
            if extra:
               cmd += " "+extra
            invoke(cmd)

#  The quality array in the new map will not be of much use since it will
#  have been created on the basis of maps made from individual chunks, rather
#  than the total coadded map. This will causes the following estimation
#  of the normalised change to be wrong. So we copy the quality mask from
#  the previous map to the new map, and use that instead (this mask was
#  created when the previous map was read into makemap). This also helps
#  if the mask is frozen by one of the xxx.zero_freeze config parameters.
            if prevmap is not None:
               try:
                  invoke("$KAPPA_DIR/setqual ndf={0} like={1}".format(newmap,prevmap) )
               except starutil.StarUtilError as err:
                  pass

#  If required, get the mean normalised map change, and see if it has
#  dropped below maptol. If so, we must do one further iteration to
#  ensure that the masking is not visible in the final map.
         invoke("$KAPPA_DIR/setbb ndf={0} bb=1".format(newmap) )
         invoke("$KAPPA_DIR/maths exp=\"'abs(ia-ib)/sqrt(va)'\" ia={0} "
                "ib={1} out={2}".format(newmap,prevmap,mapchange))
         invoke("$KAPPA_DIR/setbb ndf={0} bb=0".format(newmap) )
         invoke("$KAPPA_DIR/stats ndf={0} clip=\\[3,3,3\\] quiet".format(mapchange))
         meanchange = starutil.get_task_par( "mean", "stats" )
         if maptol > 0.0 and converged == False:
            msg_out( "Normalised mean change in map = {0} (maptol="
                     "{1})".format(meanchange,maptol) )
            if meanchange <= maptol:
               msg_out( "Converged! But we need to do one more iteration..." )
               converged = True
               niter = iter + 1
         else:
            msg_out( "Normalised mean change in map = {0}".format(meanchange) )


#  Append the output map name to the list of maps to be included in any
#  itermap cube.
         maps.append(newmap)

#  Copy any chunk maps into the specified directory, creating it first
#  if required.
         lobs = ""
         lut = ""
         if itermaps:
            if not os.path.exists(obsdir):
               os.makedirs(obsdir)

            obsmaps = []
            for chunkmap in NDG(itermaps):
               obsid=starutil.get_fits_header( chunkmap, "OBSIDSS" )
               m1 = re.search( 'CH(\d+)I', chunkmap )
               m2 = re.search( 'scuba2_(\d+)_(\d+)T', obsid )
               if m1 and m2:
                  ichunk = int( m1.group(1) )
                  obs = int( m2.group(1) )
                  ut = int( m2.group(2) )
                  if obs != lobs or ut != lut:
                     ich0 = ichunk

                  if ichunk == ich0:
                     thischunk = "{0}/{1}_{2}.sdf".format(obsdir,ut,obs)
                  else:
                     thischunk = "{0}/{1}_{2}_chunk{3}.sdf".format(obsdir,ut,obs,ichunk-ich0)
                  invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(chunkmap,thischunk))

                  obsmaps.append(thischunk)

                  lobs = obs
                  lut = ut
               else:
                  raise starutil.InvalidParameterError("Could not identify "
                                          "chunk map '{}'.".format(chunkmap))

#  Add a SKYLOOP history record to each chunk map.
            NDG(obsmaps).histadd()

#  Update the NDF from which new quality info is to be read.
         if qua:
            qua = NDG( cleaned, "*_con_res" )

#  Increment the iteration number
         iter += 1

#  Report convergence failure.
   if maptol > 0.0 and not converged:
        msg_out("Map did not converge.")

#  Now we have done all iterations, create the output itermap cube if
#  required.
   if itermap and niter > 1:
      msg_out( "Creating output itermap cube {0}...".format(itermap) )
      inputs = NDG( maps )
      invoke("$KAPPA_DIR/paste in={0} out={1} shift=\[0,0,1\]".format(inputs,itermap) )

#  Remove temporary files.
   cleanup()


#  If an StarUtilError of any kind occurred, display the message but hide the
#  python traceback. To see the trace back, uncomment "raise" instead.
except Exception as err:
#  raise
   print( err )
   print( "\n\nskyloop ended prematurely so intermediate files are being retained in {0}.".format(NDG.tempdir) )
   print( "It may be possible to re-started skyloop using the RESTART parameter.\n" )

# This is to trap control-C etc, so that we can clean up temp files.
except:
   print( "\n\nskyloop ended prematurely so intermediate files are being retained in {0}.".format(NDG.tempdir) )
   print( "It may be possible to re-started skyloop using the RESTART parameter.\n" )
   raise

