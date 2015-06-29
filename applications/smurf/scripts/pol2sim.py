#!/usr/bin/env python

'''
*+
*  Name:
*     pol2sim

*  Purpose:
*     Create simulated POL2 data from known I, Q and U maps

*  Language:
*     python (2.7 or 3.*)

*  Description:
*     This script creates simulated POL2 time-series data, using a
*     supplied real POL2 observation as a template. The maps containing
*     the artificial I, Q and U values at each point on the sky can be
*     created automatically, or can be supplied by the user (see
*     parameters ARTI, ARTQ and ARTU).
*
*     Various factors are taken into account when creating the artificial
*     data:
*
*     - A  time-varying unpolarised sky brightness is added onto the
*       I values amapled from the supplied ARTI map. The sky brightness
*       is common to all bolometers, and is derived from the time-series
*       data for a a real scan map (see parameter INCOM). For some
*       unknown rason, the common-mode for real POL2 data seems to be much
*       flatter than the common-mode for real non-POL2 data. For this
*       reason, there is an option to flatten the common-mode before using
*       it in the simulation (see parameter CFACTOR).
*     - Instrumental polarisation is included. Some fraction of the total
*       intensity falling on each bolometer is converted into Q and U
*       (see parameters IPMAX, IPMIN and IPTHETA). This fraction varies
*       from boloemeter to bolometer, and is fixed in bolometer coordinates.
*       (i.e. each sub-array sees the same instrumental polarisation -
*       this needs exapnding at some point to allow for different IPs for
*       each sub-array).
*     - 2, 4 and 16 Hz signals proportional to the total intensity (including
*       sky brightness) in included in the simulated POL2 data.
*     - Each bolometer has a separate gain, which is allowed to vary over
*       time (like the GAI model used by smurf:makemap). The GAI values
*       are derived from the template POL2 data supplied for parameter
*       IN. The extent to which the GAI values vary with time is controlled
*       by parameter GFACTOR
*     - Random Gaussian noise is added to the returned time-stream data.
*
*     NOTE - this script takes a very long time to run !!

*  Usage:
*     pol2sim in out newart arti artq artu

*  Parameters:
*     AMP2 = _DOUBLE (Read)
*        Controls the amplitude of the 2 Hz signal. It gives the amplitude
*        of the 2 Hz signal as a fraction of the total intensity. See
*        also "PHASE2". [0.0001]
*     AMP4 = _DOUBLE (Read)
*        Controls the amplitude of the 4 Hz signal. It gives the amplitude
*        of the 4 Hz signal as a fraction of the total intensity. See
*        also "PHASE4". [0.00027]
*     AMP16 = _DOUBLE (Read)
*        Controls the amplitude of the 16 Hz signal. It gives the amplitude
*        of the 16 Hz signal as a fraction of the total intensity. See
*        also "PHASE16". [0.00005]
*     ARTI = NDF (Read or write)
*        A 2D NDF holding the artificial total intensity map from which the
*        returned time-stream data is derived. If the NEWART parameter
*        is True, then a new artificial I map is created and stored in
*        a new NDF with name specified by ARTI. If NEWART is False, then
*        ARTI should specify an existing NDF on entry, which is used as
*        the artificial I map.
*     ARTQ = NDF (Read or write)
*        A 2D NDF holding the artificial Q map from which the returned
*        time-stream data is derived. If the NEWART parameter is True, then
*        a new artificial Q map is created and stored in a new NDF with name
*        specified by ARTQ. If NEWART is False, then ARTQ should specify an
*        existing NDF on entry, which is used as the artificial Q map.
*     ARTU = NDF (Read or write)
*        A 2D NDF holding the artificial U map from which the returned
*        time-stream data is derived. If the NEWART parameter is True, then
*        a new artificial U map is created and stored in a new NDF with name
*        specified by ARTU. If NEWART is False, then ARTU should specify an
*        existing NDF on entry, which is used as the artificial U map.
*     CFACTOR = _DOUBLE (Read)
*        A factor by which to expand the COM model values derived from
*        the supplied INCOM data. The expansion is centred on the mean
*        value. Real POL2 data seems to have a much flatter common-mode
*        than real non-POL2 data, so the default flattens the common-mode
*        to some extent. [0.2]
*     GFACTOR = _DOUBLE (Read)
*        A factor by which to expand the GAI model values derived from
*        the supplied time-series data. The expansion is centred on the
*        value 1.0. No GAI model is used if GFACTOR is zero. [1.0]
*     GLEVEL = LITERAL (Read)
*        Controls the level of information to write to a text log file.
*        Allowed values are as for "ILEVEL". The log file to create is
*        specified via parameter "LOGFILE. In adition, the glevel value
*        can be changed by assigning a new integer value (one of
*        starutil.NONE, starutil.CRITICAL, starutil.PROGRESS,
*        starutil.ATASK or starutil.DEBUG) to the module variable
*        starutil.glevel. ["ATASK"]
*     IFWHM = _DOUBLE (Read)
*        FWHM of Gaussian source for new artificial total instensity map,
*        in pixels. [8]
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
*        A group of POL2 time series NDFs. Only used if a null (!) value is
*        supplied for INQU.
*     INCOM = NDF (Read)
*        A group of non-POL2 time series NDFs that are used to define
*        the common-mode (i.e. the sky brightness) to include in the
*        simulated POL2 data. The number of NDFs supplied for INCOM must
*        equal the number supplied for IN. Each INCOM file must be at least
*        as long (in time) as the corresponding IN file. If null (!) is
*        supplied, no common-mode is included in the simulated POL2 data.
*     IPEAK = _DOUBLE (Read)
*        Peak intensity for new artificial total instensity map, in pW. [0.08]
*     IPMIN = _DOUBLE (Read)
*        The minimum instrumental polarisation within the focal plane,
*        expressed as a fraction. The IP varies linearly across each
*        array from IPMIN to IPMAX. The IP is fixed in focal plane
*        coordinates over all stare positions. [0.0004]
*     IPMAX = _DOUBLE (Read)
*        The maximum instrumental polarisation within the focal plane,
*        expressed as a fraction. The IP varies linearly across each
*        array from IPMIN to IPMAX. The IP is fixed in focal plane
*        coordinates over all stare positions. [0.0008]
*     IPTHETA = _DOUBLE (Read)
*        The angle from the focal plane Y axis to the instrumental
*        polarisation vectors, in degrees. [15]
*     LOGFILE = LITERAL (Read)
*        The name of the log file to create if GLEVEL is not NONE. The
*        default is "<command>.log", where <command> is the name of the
*        executing script (minus any trailing ".py" suffix), and will be
*        created in the current directory. Any file with the same name is
*        over-written. The script can change the logfile if necessary by
*        assign the new log file path to the module variable
*        "starutil.logfile". Any old log file will be closed befopre the
*        new one is opened. []
*     MSG_FILTER = LITERAL (Read)
*        Controls the default level of information reported by Starlink
*        atasks invoked within the executing script. This default can be
*        over-ridden by including a value for the msg_filter parameter
*        within the command string passed to the "invoke" function. The
*        accepted values are the list defined in SUN/104 ("None", "Quiet",
*        "Normal", "Verbose", etc). ["Normal"]
*     NEWART = _LOGICAL (Read)
*        Indicates if new artificial I, Q and U maps should be created.
*        These are the maps from which the returned time-stream data are
*        derived.
*
*        If NEWART is False, then existing 2D NDFs holding the artificial
*        I, Q and U data to be used should be specified via parameter ARTI,
*        ARTQ and ARTU. These maps should have WCS that is consistent with
*        the supplied template time-stream data (parameter IN). The data
*        values are assumed to be in units of "pW". The Y pixel axis is
*        assumed to be the polarimetric reference direction.
*
*        If NEWART is True, then new artificial I, Q and U data is
*        created representing a single Gaussian source centred at pixel
*        coord (0,0), with peak total intensity given by parameter IPEAK
*        and width given by parameter IFWHM. The polarisation vectors are
*        tangential, centred on the source. The fractional polarisation is
*        constant at the value given by POL. The Y pixel axis is reference
*        direction (suitable POLANAL Frames are included in the WCS to
*        indicate this, as required by POLPACK).
*     OUT = NDF (Write)
*        A group of output NDFs to hold the simulated POL2 time series
*        data. Equal in number to the files in "IN".
*     PHASE2 = _DOUBLE (Read)
*        The phase offset to apply to the 2 Hz signal specified via
*        parameter AMP2, in degrees. [0.0]
*     PHASE4 = _DOUBLE (Read)
*        The phase offset to apply to the 4 Hz signal specified via
*        parameter AMP4, in degrees. [-30.0]
*     PHASE16 = _DOUBLE (Read)
*        The phase offset to apply to the 16 Hz signal specified via
*        parameter AMP16, in degrees. [0.0]
*     POL = _DOUBLE (Read)
*        The fractional polarisation for new artificial Q and U maps.
*        [0.05]
*     RESTART = LITERAL (Read)
*        If a value is assigned to this parameter, it should be the path
*        to a directory containing the intermediate files created by a
*        previous run of POL2SIM (it is necessry to run POL2SIM with
*        RETAIN=YES otherwise the directory is deleted after POL2SIM
*        terminates). If supplied, any files which can be re-used from
*        the supplied directory are re-used, thus speeding things up.
*        The path to the intermediate files can be found by examining the
*        log file created by the previous run. [!]
*     RETAIN = _LOGICAL (Read)
*        Should the temporary directory containing the intermediate files
*        created by this script be retained? If not, it will be deleted
*        before the script exits. If retained, a message will be
*        displayed at the end specifying the path to the directory. [FALSE]
*     SIGMA = _DOUBLE (Read)
*        Gaussian noise level (in pW) to add to the final data.

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory
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
*     4-MAY-2015 (DSB):
*        Original version
*-
'''

import os
import re
import math
import starutil
from starutil import invoke
from starutil import NDG
from starutil import Parameter
from starutil import ParSys
from starutil import msg_out
from starutil import UsageError

#  Assume for the moment that we will not be retaining temporary files.
retain = 0

#  Assume for the moment that we will not be re-using old temporary files.
restart = None

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

#  Save the paths to the NDFs that form a specified NDG, within a file with given
#  label, so that the NDG can be identified on any subequent restarted runs.
def savendg( label, ndg ):
   global retain
   if retain:
      fpath = os.path.join(NDG.tempdir,"{0}.grp".format(label))
      if os.path.exists(fpath):
         raise UsageError("\n\nThe directory {0} already has a group "
                          "called {1}".format(NDG.tempdir,label) )
      fd = open( fpath, "w" )
      for ndf in ndg:
         fd.write("{0}\n".format(ndf))
      fd.close()

#  Create a new NDG from the names in a previously stored file.
def loadndg( label, report=False ):
   fpath = os.path.join(NDG.tempdir,"{0}.grp".format(label))
   if os.path.exists(fpath):
      return NDG( "^{0}".format(fpath) )
   elif report:
      raise UsageError("\n\nThe directory {0} does not contain a list of "
                       "the '{1}' group of NDFs".format(NDG.tempdir,label) )
   else:
      return None


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

   params.append(starutil.ParNDG("IN", "Template POL2 time series NDFs",
                                 starutil.get_task_par("DATA_ARRAY","GLOBAL",
                                                       default=Parameter.UNSET)))
   params.append(starutil.Par0S("OUT", "Output simulated POL2 data"))
   params.append(starutil.Par0L("NEWART", "Create new artificial I, Q and U maps?" ))
   params.append(starutil.ParNDG("ARTI", "Artificial I map", maxsize=1 ))
   params.append(starutil.ParNDG("ARTQ", "Artificial Q map", maxsize=1 ))
   params.append(starutil.ParNDG("ARTU", "Artificial U map", maxsize=1 ))
   params.append(starutil.ParNDG("INCOM", "Non-POL2 data files to define COM",
                                 None ))
   params.append(starutil.Par0S("RESTART", "Restart using old files?", None,
                                 noprompt=True))
   params.append(starutil.Par0L("RETAIN", "Retain temporary files?", False,
                                 noprompt=True))
   params.append(starutil.Par0F("IPEAK", "Peak total instensity in "
                                "artificial I map (pW)", 0.08, True ))
   params.append(starutil.Par0F("IFWHM", "Width of source in artificial I "
                                "map (pixels)", 8, True ))
   params.append(starutil.Par0F("POL", "Fractional polarisation in "
                                "artificial Q/U maps", 0.05, True ))
   params.append(starutil.Par0F("IPMAX", "Maximum fractional instrumental "
                                "polarisation", 0.0008, True ))
   params.append(starutil.Par0F("IPMIN", "Minimum fractional instrumental "
                                "polarisation", 0.0004, True ))
   params.append(starutil.Par0F("IPTHETA", "Angle for for instrumental "
                                "polarisation (deg.s)", 15.0, True ))
   params.append(starutil.Par0F("GFACTOR", "Expansion factor for GAI values",
                                1.0, True ))
   params.append(starutil.Par0F("CFACTOR", "Expansion factor for COM values",
                                0.2, True ))
   params.append(starutil.Par0F("AMP2", "Amplitude for 2Hz signal",
                                0.0001, True ))
   params.append(starutil.Par0F("AMP4", "Amplitude for 4Hz signal",
                                0.00027, True ))
   params.append(starutil.Par0F("AMP16", "Amplitude for 16Hz signal",
                                0.00005, True ))
   params.append(starutil.Par0F("PHASE2", "Phase for 2Hz signal (deg.s)",
                                0.0, True ))
   params.append(starutil.Par0F("PHASE4", "Phase for 4Hz signal (deg.s)",
                                -30.0, True ))
   params.append(starutil.Par0F("PHASE16", "Phase for 16Hz signal (deg.s)",
                                0.0, True ))
   params.append(starutil.Par0F("SIGMA", "Noise level in pW", 0.004, True ))

#  Initialise the parameters to hold any values supplied on the command
#  line.
   parsys = ParSys( params )

#  It's a good idea to get parameter values early if possible, in case
#  the user goes off for a coffee whilst the script is running and does not
#  see a later parameter propmpt or error...

#  Get the template POL2 data files. They should be supplied as the first
#  item on the command line, in the form of a Starlink "group expression"
#  (i.e.the same way they are supplied to other SMURF commands such as
#  makemap). Quote the string so that it can be used as command line
#  argument when running an atask from the shell.
   indata = parsys["IN"].value

#  Output files.
   gexp = parsys["OUT"].value
   outdata = NDG( indata, gexp )

#  Common mode files.
   incom = parsys["INCOM"].value
   if incom:
      if len(incom) < len(indata):
         raise UsageError("\n\nInsufficient files supplied for INCOM - "
                          "must be at least {0}.".format(len(indata)))

#  Do not use more com files for each sub-array than are needed.
      remlist = []
      for subarr in ( "s8a", "s8b", "s8c", "s8d", "s4a", "s4b", "s4c", "s4d" ):
         nin = 0
         for ndf in indata:
            if subarr in ndf:
               nin += 1

         ncom = 0
         for ndf in incom:
            if subarr in ndf:
               ncom += 1
               if ncom > nin:
                  remlist.append( ndf )

      msg_out("Ignoring {0} surplus files in INCOM".format(len(remlist) ))
      for ndf in remlist:
        incom.remove( ndf )

#  See if new artificial I, Q and U maps are to be created.
   newart = parsys["NEWART"].value

#  If not, set the ART parameters to indicate that the specified NDFs
#  must already exist.
   if not newart:
      parsys["ARTI"].exists = True
      parsys["ARTQ"].exists = True
      parsys["ARTU"].exists = True
   else:
      parsys["ARTI"].exists = False
      parsys["ARTQ"].exists = False
      parsys["ARTU"].exists = False

#  Get the ART maps.
   iart = parsys["ARTI"].value
   qart = parsys["ARTQ"].value
   uart = parsys["ARTU"].value

#  Other required params
   amp2 = parsys["AMP2"].value
   phase2 = parsys["PHASE2"].value
   amp4 = parsys["AMP4"].value
   phase4 = parsys["PHASE4"].value
   amp16 = parsys["AMP16"].value
   phase16 = parsys["PHASE16"].value
   sigma = parsys["SIGMA"].value

#  See if old temp files are to be re-used.
   restart = parsys["RESTART"].value
   if restart == None:
      retain = parsys["RETAIN"].value
      savendg( "IN", indata )

   else:
      retain = True
      NDG.tempdir = restart
      if not os.path.isdir(restart):
         raise UsageError("\n\nThe directory specified by parameter RESTART ({0}) "
                          "does not exist".format(restart) )
      fred = loadndg( "IN", True )
      if indata != fred:
         raise UsageError("\n\nThe directory specified by parameter RESTART ({0}) "
                          "refers to different time-series data".format(restart) )
      msg_out( "Re-using data in {0}".format(restart) )

#  Initialise the starlink random number seed to a known value so that
#  results are repeatable.
   os.environ["STAR_SEED"] = "65"

#  Flat field the supplied template data
   ff = loadndg( "FF" )
   if not ff:
      ff = NDG(indata)
      msg_out( "Flatfielding template data...")
      invoke("$SMURF_DIR/flatfield in={0} out={1}".format(indata,ff) )
      ff = ff.filter()
      savendg( "FF", ff  )
   else:
      msg_out( "Re-using old flatfielded template data...")

#  If required, create new artificial I, Q and U maps.
   if newart:
      msg_out( "Creating new artificial I, Q and U maps...")

#  Get the parameters defining the artificial data
      ipeak = parsys["IPEAK"].value
      ifwhm = parsys["IFWHM"].value
      pol = parsys["POL"].value

#  Determine the spatial extent of the data on the sky.
      invoke("$SMURF_DIR/makemap in={0} out=! config=def".format(ff))
      reflon = starutil.get_task_par( "reflon", "makemap" )
      reflat = starutil.get_task_par( "reflat", "makemap" )
      pixsize = starutil.get_task_par( "pixsize", "makemap" )
      lx = int( starutil.get_task_par( "lbnd(1)", "makemap" ))
      ly = int( starutil.get_task_par( "lbnd(2)", "makemap" ))
      ux = int( starutil.get_task_par( "ubnd(1)", "makemap" ))
      uy = int( starutil.get_task_par( "ubnd(2)", "makemap" ))

#  Expand them by some safety margin.
      delta = ( ux - lx )/10
      lx -= delta
      ux += delta
      delta = ( uy - ly )/10
      ly -= delta
      uy += delta

#  Generate Q,U and I for a single gaussian bump.  Tangential vectors
#  centred on the bump, with increasing percentage polarisation at larger
#  radii. Y pixel axis is reference direction. First create the I map.
      if ipeak != 0.0:
         invoke("$KAPPA_DIR/maths exp='pa*exp(-pc*(xa**2+xb**2)/(pf**2))' "
                "pa={0} pc=1.66511 pf={1} type=_double lbound=\[{2},{3}\] "
                "ubound=\[{4},{5}\] out={6}".format(ipeak,ifwhm,lx,ly,ux,uy,iart))

#  Now create the fractional polarisation map.
         fp = NDG(1)
         invoke("$KAPPA_DIR/maths exp='ia*0+pa' ia={0} out={1} pa={2}".
                format(iart,fp,pol) )

#  Polarised intensity.
         pi = NDG(1)
         invoke( "$KAPPA_DIR/mult in1={0} in2={1} out={2}".format(iart,fp,pi) )

#  Get q and u values that give radial vectors.
         theta = NDG(1)
         invoke( "$KAPPA_DIR/maths exp=\"'ia*0+atan2(-xa,xb)'\" ia={0} out={1}".
                 format(iart,theta) )
         invoke("$KAPPA_DIR/maths exp='-ia*cos(2*ib)' ia={0} ib={1} out={2}".format(pi,theta,qart) )
         invoke("$KAPPA_DIR/maths exp='-ia*sin(2*ib)' ia={0} ib={1} out={2}".format(pi,theta,uart) )

#  Fill I, Q and U images with zeros if IPEAK is zero.
      else:
         invoke("$KAPPA_DIR/creframe mode=fl mean=0 lbound=\[{0},{1}\] "
                "ubound=\[{2},{3}\] out={4}".format(lx,ly,ux,uy,iart))
         invoke("$KAPPA_DIR/creframe mode=fl mean=0 lbound=\[{0},{1}\] "
                "ubound=\[{2},{3}\] out={4}".format(lx,ly,ux,uy,qart))
         invoke("$KAPPA_DIR/creframe mode=fl mean=0 lbound=\[{0},{1}\] "
                "ubound=\[{2},{3}\] out={4}".format(lx,ly,ux,uy,uart))

#  Put WCS into the artificial images, indicating that the pol. ref.
#  direction is the pixel Y axis.
      invoke("$KAPPA_DIR/setsky ndf={0} projtype=TAN positions=! coords='eq(j2000)' "
             "refcode=pixel pixelref=\[0,0\] lon=\"'{1}'\" lat=\"'{2}'\" "
             "pixelsize='{3}s' orient=0 epoch=2015".
             format(iart,reflon.replace(":"," "),reflat.replace(":",""),pixsize) )
      invoke("$KAPPA_DIR/wcscopy ndf={0} like={1}".format(qart,iart) )
      invoke("$KAPPA_DIR/wcscopy ndf={0} like={1}".format(uart,iart) )

   else:
      msg_out( "Using supplied artificial I, Q and U maps...")

#  Ensure the artificial maps have a defined polarimetric reference
#  direction parallel to the pixel Y axis, and ensure they are in units
#  of pW.
   invoke("$POLPACK_DIR/polext in={0} angrot=90".format(iart) )
   invoke("$POLPACK_DIR/polext in={0} angrot=90".format(qart) )
   invoke("$POLPACK_DIR/polext in={0} angrot=90".format(uart) )
   invoke("$KAPPA_DIR/setunits ndf={0} units=pW".format(iart) )
   invoke("$KAPPA_DIR/setunits ndf={0} units=pW".format(qart) )
   invoke("$KAPPA_DIR/setunits ndf={0} units=pW".format(uart) )

#  If required, create an artificial common-mode (i.e. unpolarised emission
#  from the sky) for each sub-scan/grid-point.
   if incom:

#  First flat-field all the INCOM files.
      cff = loadndg( "CFF" )
      if not cff:
         cff = NDG(incom)
         msg_out( "Flatfielding common-mode data...")
         invoke("$SMURF_DIR/flatfield in={0} out={1}".format(incom,cff) )
         cff = cff.filter()
         savendg( "CFF", cff  )
      else:
         msg_out( "Re-using old flatfielded common-mode data...")

#  Process each sub-scan separately as they may have different lengths.
      cfactor = parsys["CFACTOR"].value
      com = loadndg( "COM" )
      if not com:
         msg_out( "Creating new artificial common-mode signals...")

         comlen = 0
         totcom = NDG(1)
         lbnd = []
         ubnd = []

         tcom = NDG(ff)
         for icom in range(len( tcom )):
            msg_out( "   sub-scan {0}/{1}".format(icom+1,len(tcom)))
            this_ff = ff[ icom ]
            this_cff = cff[ icom ]
            this_com = tcom[ icom ]

#  Get the number of time slices in the current flat-fielded time-series.
            invoke("$KAPPA_DIR/ndftrace ndf={0}".format(this_ff) )
            ns = int( starutil.get_task_par( "dims(3)", "ndftrace" ))

#  Check the INCOM file is at least as long
            invoke("$KAPPA_DIR/ndftrace ndf={0}".format(this_cff) )
            cns = int( starutil.get_task_par( "dims(3)", "ndftrace" ))
            if cns < ns:
               raise UsageError("\n\nAn INCOM file has been found which is "
                                "shorter than the correspoinding IN file "
                                "({0} < {1}).".format(cns,ns) )

#  Reshape each flat-fielded INCOM file to a 2D array in which axis
#  1 is bolometer and axis 2 is time-slice.
            n2d = NDG(1)
            invoke("$KAPPA_DIR/reshape in={0}\(,,:{2}\) out={1} shape=\[1280,{2}\]".format(this_cff,n2d,ns) )

#  Collapse this 2D array along pixel axis 2 (time) to get the mean value
#  in each bolometer.
            n1dmean = NDG(1)
            invoke("$KAPPA_DIR/collapse in={0} estimator=mean axis=2 out={1} wlim=0".
                   format(n2d,n1dmean))

#  Expand it out again to full 2d, and subtract it off the original to
#  get a version in which each bolometer has a mean value of zero.
            n2dmean = NDG(1)
            invoke("$KAPPA_DIR/manic in={0} axes=\[1,0\] lbound=1 ubound={1} out={2}".
                   format(n1dmean,ns,n2dmean))

            n2dres = NDG(1)
            invoke("$KAPPA_DIR/sub in1={0} in2={1} out={2}".format(n2d,n2dmean,n2dres))

#  Now collapse these residuals along the bolometer axis to get the
#  mean-subtracted common mode. Use a median estimator. The above removal
#  of the mean was necessary so that the resulting common mode is not
#  determined just by the bolometers with the middle background levels.
            tmpcom = NDG(1)
            invoke("$KAPPA_DIR/collapse in={0} estimator=median axis=1 out={1} wlim=0".
                   format(n2dres,tmpcom))

#  Smooth it a bit.
            smocom = NDG(1)
            invoke("$KAPPA_DIR/gausmooth in={0} fwhm=3 out={1}".format(tmpcom,smocom))

#  Attenuate the variations in the COM signal, since real POL2 data seems to
#  have a much flatter common mode than real scan data.
            attcom = NDG(1)
            invoke("$KAPPA_DIR/cmult in={0} scalar={1} out={2}".format(smocom,cfactor,attcom))

#  Get the amplitude of the oscillations in each bolometer of the template
#  POL2 data (=sqrt(2) times the standard deviation). The mean of these
#  amplitude is used to determine the mean sky brightness to add back onto
#  the zero-mean common-mode which is currently in "attcom". We choose mean
#  sky value to give a 4Hz signal with amplitude equal to the mean range.
            sigff = NDG(1)
            invoke("$KAPPA_DIR/collapse in={0} estimator=sigma axis=3 out={1} wlim=0".
                   format(this_ff,sigff))
            invoke("$KAPPA_DIR/stats ndf={0}".format(sigff))
            means = float( starutil.get_task_par( "mean", "stats" ) )
            if amp4 > 0.0:
               mean_com = 1.4142*means/amp4
            else:
               mean_com = means

#  Add this mean value onto the common mode.
            invoke("$KAPPA_DIR/cadd in={0} scalar={1} out={2}".format(attcom,mean_com,this_com))

#  Append the current COM file to the end of the total COM file.
            lbnd.append( comlen + 1 )
            if comlen > 0:
               temp = NDG(1)
               invoke("$KAPPA_DIR/paste in={0} p1={1} shift={2} out={3}".
                      format(totcom,this_com,comlen,temp))
               invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(temp,totcom))
            else:
               invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(this_com,totcom))

            comlen += ns
            ubnd.append( comlen )

#  Now attempt to ensure that there are no jumps between common mode
#  files. First reshape the total com signal into a 3D array as required
#  by fixsteps.
         com3d = NDG(1)
         invoke("$KAPPA_DIR/reshape in={0} shape=\[1,1,{1}\] out={2}".
                format(totcom,comlen,com3d))

#  Now fix the steps.
         com3d_fixed = NDG(1)
         invoke("$SMURF_DIR/fixsteps in={0} out={1} meanshift=no".
                format(com3d,com3d_fixed))

#  Reshape it back to 1-dimension.
         com_fixed = NDG(1)
         invoke("$KAPPA_DIR/reshape in={0} shape={1} out={2}".
                format(com3d_fixed,comlen,com_fixed))

#  Now split the corrected COM signal up into individual files.
         com = NDG(ff)
         for icom in range(len( com )):
            invoke("$KAPPA_DIR/ndfcopy in={0}\({1}:{2}\) out={3}".
                   format(com_fixed,lbnd[icom],ubnd[icom],com[icom]))

         savendg( "COM", com  )

      else:
         msg_out( "Re-using old artificial common-mode signals...")
   else:
      com = "!"




# Generate instrumental normalised Q and U images - 0.07% IP (on average -
# there is a gradient in X). This value, together with the amp4 and phase4
# values used by unmakemap below, produce time-streams that look similar
# to the real thing. The iP is at 15 degrees to the fixed analyser.
   ipqu = loadndg( "IPQU" )
   if not ipqu:
      msg_out( "Creating instrumental polarisation values...")
      ipqu = NDG( 2 )

#  Get the parameters defining the instrumental polarisation
      ipmax = parsys["IPMAX"].value
      ipmin = parsys["IPMIN"].value
      iptheta = parsys["IPTHETA"].value

#  Create a map of the fractional instrumental polarisation across each
#  array. Note, all subarrays are given the same IP pattern.
      ipi = NDG(1)
      invoke("$KAPPA_DIR/maths exp='xa*(pa-pb)/31+pb+xb*0' type=_REAL "
             "pa={0} pb={1} lbound=\[0,0\] ubound=\[31,39\] out={2}".
             format(ipmax,ipmin,ipi))

#  Create the corresponding normalised Q and U maps.
      invoke("$KAPPA_DIR/maths exp='ip*cosd(2*pt)' ip={0} pt={1} out={2}".
             format(ipi,iptheta,ipqu[0] ))
      invoke("$KAPPA_DIR/maths exp='ip*sind(2*pt)' ip={0} pt={1} out={2}".
             format(ipi,iptheta,ipqu[1] ))

      savendg( "IPQU", ipqu  )
   else:
      msg_out( "Re-using old instrumental polarisation values...")

#  Create GAI values from the supplied template data.
   gfactor = parsys["GFACTOR"].value
   if gfactor != 0.0:
      gai = loadndg( "GAI" )
      if not gai:
         msg_out( "Creating new artificial GAI models...")
         gai = NDG(ff)
         m2 = NDG(ff)

         for igai in range(len( gai )):
            msg_out( "   sub-scan {0}/{1}".format(igai+1,len(gai)))
            this_ff = ff[ igai ]
            this_m2 = m2[ igai ]

#  Get the number of time slices in the current flat-fielded time-series.
            invoke("$KAPPA_DIR/ndftrace ndf={0}".format(this_ff) )
            ns = int( starutil.get_task_par( "dims(3)", "ndftrace" ))

#  Reshape each flat-fielded IN file to a 2D array in which axis
#  1 is bolometer and axis 2 is time-slice.
            n2d = NDG(1)
            invoke("$KAPPA_DIR/reshape in={0} out={1} shape=\[1280,{2}\]".format(this_ff,n2d,ns) )

#  Collapse this 2D array along pixel axis 2 (time) to get the mean value
#  in each bolometer.
            n1dmean = NDG(1)
            invoke("$KAPPA_DIR/collapse in={0} estimator=mean axis=2 out={1} wlim=0".
                   format(n2d,n1dmean))

#  Expand it out again to full 2d, and subtract it off the original to
#  get a version in which each bolometer has a mean value of zero.
            n2dmean = NDG(1)
            invoke("$KAPPA_DIR/manic in={0} axes=\[1,0\] lbound=1 ubound={1} out={2}".
                   format(n1dmean,ns,n2dmean))

            n2dres = NDG(1)
            invoke("$KAPPA_DIR/sub in1={0} in2={1} out={2}".format(n2d,n2dmean,n2dres))

#  Now collapse these residuals along the bolometer axis to get the
#  mean-subtracted common mode. Use a median estimator. The above removal
#  of the mean was necessary so that the resulting common mode is not
#  determined just by the bolometers with the middle background levels.
            tmpcom = NDG(1)
            invoke("$KAPPA_DIR/collapse in={0} estimator=median axis=1 out={1} wlim=0".
                   format(n2dres,tmpcom))

#  We need the time axis to be axis 2 when running NORMALIZE below, so
#  permute the axes.
            permcom = NDG(1)
            invoke("$KAPPA_DIR/permaxes in={0}\(,1\) perm=\[2,1\] out={1}".
                   format(tmpcom,permcom))

#  USe NORMALIZE to fit each individual bolometer to the above
#  common-mode, returning NDFs holding the scale and correlation
#  coefficients for all bolometers.
            slp = NDG(1)
            corr = NDG(1)
            invoke("$KAPPA_DIR/normalize in1={0} in2={1} loop=yes quiet device=! out=! "
                   "outslope={2} outcorr={3}".format(permcom,n2d,slp,corr))

#  Blank out bolometers that are poorly correlated to the common-mode.
            m1 = NDG(1)
            invoke("$KAPPA_DIR/thresh in={0} thrlo=0.96 newlo=bad thrhi=10 "
                   "newhi=bad out={1} quiet".format(corr,m1))

#  Reshape the slopes into a 2d array, and give it the correct origin
            invoke("$KAPPA_DIR/reshape in={0} shape=\[32,40\] out={1}".
                   format(m1,this_m2))
            invoke("$KAPPA_DIR/setorigin ndf={0} origin=\[0,0\]".format(this_m2))

#  Remove spikes.
         m3 = NDG(m2)
         invoke("$KAPPA_DIR/ffclean in={0} out={1} box=5 clip=\[3,3,3\] ".
                format(m2,m3))

#  Smooth and fill holes
         invoke("$KAPPA_DIR/gausmooth in={0} fwhm=3 wlim=1E-6 out={1}".
                format(m3,gai))
         savendg( "GAI", gai )
      else:
         msg_out( "Re-using old artificial GAI models...")

#  Amplify the GAI values.
      if gfactor != 1.0:
         msg_out( "Scaling GAI values by {0}".format(gfactor) )
         fgai = NDG( gai )
         invoke("$KAPPA_DIR/maths exp='pa*(ia-1.0)+1.0' ia={0} pa={1} out={2}".
                format(gai,gfactor,fgai))
      else:
         fgai = gai
   else:
      fgai = "!"

#  Finally, create the simulated POL2 time-streams.
   msg_out( "Generating simulated POL2 time-stream data..." )
   invoke("$SMURF_DIR/unmakemap in={0} qin={1} uin={2} ref={3} "
          "sigma={4} out={5} interp=sincsinc params=\[0,3\] com={6} "
          "instq={7} instu={8} amp4={9} phase4={10} "
          "amp2={11} phase2={12} amp16={13} phase16={14} gai={15}".
          format(iart,qart,uart,ff,sigma,outdata,com,ipqu[0],
                 ipqu[1],amp4,phase4,amp2,phase2,amp16,phase16,fgai) )

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

