#!/usr/bin/env python

'''
*+
*  Name:
*     CONFIGMELD

*  Purpose:
*     Compare two MAKEMAP configs using a visual file comparison tool.

*  Language:
*     python (2.7 or 3.*)

*  Description:
*     This script uses a visual file comparison tool such as "meld", to
*     display two sets of configuration parameters, highlighting the
*     differences between them. Each config may be supplied directly, as
*     is done when running MAKEMAP, or can be read from the History
*     component of an NDF that was created by MAKEMAP.

*  Usage:
*     configmeld config1 config2 waveband defaults tool

*  ADAM Parameters:
*     CONFIG1 = LITERAL (Read)
*        The first configuration. This can be a normal config such as is
*        supplied for the CONFIG parameter of MAKEMAP, or an NDF created
*        by MAKEMAP.
*     CONFIG2 = LITERAL (Read)
*        The first configuration. This can be a normal config such as is
*        supplied for the CONFIG parameter of MAKEMAP, or an NDF created
*        by MAKEMAP. If a value is supplied for PARAM, then CONFIG2
*        defaults to null (!). []
*     WAVEBAND = LITERAL (Read)
*        This parameter is not used if either CONFIG1 or CONFIG2 is
*        an NDF created by MAKEMAP. It should be one of "450" or "850". It
*        specifies which value should be displayed for configuration
*        parameters that have separate values for 450 and 850 um. If either
*        CONFIG1 or CONFIG2 is an NDF created by MAKEMAP, then the
*        wavebands to use are determined from the headers in the NDFs.
*     DEFAULTS = _LOGICAL (Read)
*        If TRUE, then each supplied configuration (CONFIG1 and CONFIG2)
*        is extended to include default values are any MAKEMAP parameters
*        that it does not specify. These defaults are read from file
*        "$SMURF_DIR/smurf_makemap.def". [TRUE]
*     PARAM = LITERAL (Read)
*        If supplied, then the value used for the specified parameter
*        is displayed on standard output, and no visual comparison is
*        displayed. Separate values are displayed for CONFIG1 and (if
*        supplied) CONFIG2. [!]
*     TOOL = LITERAL (Read)
*        Gives the name of the file comparison tool to use. The named
*        command should be available on the current PATH. It should take
*        the names of two files to compare as command line arguments. If
*        null (!) is supplied, the first available tool in the following
*        list is used:
*
*        - meld: www.meldmerge.org
*        - opendiff: developer.apple.com
*        - diffmerge: www.sourcegear.com/diffmerge
*        - kdiff3: kdiff3.sourceforge.net
*        - tkdiff: sourceforge.net/projects/tkdiff
*        - diffuse: diffuse.sourceforge.net
*
*        [!]

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
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
*     13-FEB-2013 (DSB):
*        Original version
*     14-FEB-2013 (DSB):
*        Allow selection of waveband-specific values without needing also
*        to display default values.
*     19-MAR-2013 (DSB):
*        Allow the value of a single parameter to be listed.

*-
'''


import starutil
import os
import subprocess
from starutil import invoke
from starutil import msg_out
from starutil import ParSys

#  A function to clean up before exiting. Delete the text files holding
#  the config listings. Also tell ParSys to delete the temporary ADAM
#  directory.
def cleanup():
   try:
      os.remove( "config1.tmp" )
      os.remove( "config2.tmp" )
      ParSys.cleanup()
   except:
      pass

#  Catch any exception so that we can always clean up, even if control-C
#  is pressed.
try:

#  Only display critical messages on the scren, and nothing at all in the
#  log file.
   starutil.glevel = starutil.NONE
   starutil.ilevel = starutil.CRITICAL

#  Declare the script parameters. Their positions in this list define
#  their expected position on the script command line. They can also be
#  specified by keyword on the command line. No validation of default
#  values or values supplied on the command line is performed until the
#  parameter value is first accessed within the script, at which time the
#  user is prompted for a value if necessary. The parameters "MSG_FILTER",
#  "ILEVEL", "GLEVEL" and "LOGFILE" are added automatically by the ParSys
#  constructor.
   params = []
   params.append(starutil.Par0S("CONFIG1", "The first config" ))
   params.append(starutil.Par0S("CONFIG2", "The second config" ))
   params.append(starutil.ParChoice("WAVEBAND", ("450","850"),
                                    "The waveband to display - 450 or 850",
                                    "850" ))
   params.append(starutil.Par0L("DEFAULTS", "Include defaults for missing "
                                "values?", True, True ))
   params.append(starutil.Par0S("TOOL", "The file comparison tool to use",
                                None, True ))
   params.append(starutil.Par0S("PARAM", "A single config parameter to display",
                                None, True ))

#  Initialise the parameters to hold any values supplied on the command
#  line.
   parsys = ParSys( params )

#  Are we displaying a single parameter?
   param = parsys["PARAM"].value

#  If not, get the comparison tool to use.
   if param == None:
      tool = parsys["TOOL"].value
      if tool == None:
         for trytool in ( "meld", "opendiff", "diffmerge", "kdiff3", "tkdiff", "diffuse" ):
            if starutil.which( trytool ) != None:
               tool = trytool
               break
         if tool == None:
            print( "\n!! Could not find a usable file comparison tool")
            os._exit(1)
      else:
         if starutil.which( tool ) == None:
            print( "\n!! Could not find the requested file comparison tool: '{0}'.".format(tool) )
            os._exit(1)

#  If a parameter name was specified, then config2 defaults to null
#  unless it was supplied on the command line.
   else:
      parsys["CONFIG2"].default = None
      parsys["CONFIG2"].noprompt = True

#  Note the path to the defaults file, if required.
   defs = parsys["DEFAULTS"].value
   if parsys["DEFAULTS"].value:
      defs = "$SMURF_DIR/smurf_makemap.def"
   else:
      defs = "!"

#  Get the first config string.
   config1 = parsys["CONFIG1"].value
   if config1 == None:
      print( "\n!! No value supplied for CONFIG1" )
      os._exit(1)

#  To be a group expression, it must contain at least one of the
#  following characters: ^,= (NDFs are not allowed any of these).
   gexp_chars = set( '^=,' )
   if any( (c in gexp_chars) for c in config1 ):
      isndf1 = False
   else:
      isndf1 = True

#  If it is an NDF, attempt to get the SUBARRAY fits header, and
#  determine the waveband. If no SUBARRAY header is found, look for
#  "s4a", etc, in the NDF's provenance info.
   if isndf1:
      try:
         subarray = starutil.get_fits_header( config1, "SUBARRAY" )
         if subarray == None:
            text = starutil.invoke( "$KAPPA_DIR/provshow {0}".format(config1) )
            if "s4a" in text or "s4b" in text or "s4c" in text or "s4d" in text:
               subarray = "s4"
            elif "s8a" in text or "s8b" in text or "s8c" in text or "s8d" in text:
               subarray = "s8"
            else:
               subarray = None
      except:
         print( "\n!! It looks like NDF '{0}' either does not exist or is "
                "corrupt.".format(config1) )
         os._exit(1)

   if isndf1:
      if subarray == None:
         msg_out("Cannot determine the SCUBA-2 waveband for NDF '{0}' "
                 "- was it really created by MAKEMAP?".format(config1), starutil.CRITICAL )
         waveband1 = None
      elif subarray[1:2] == "4":
         waveband1 = "450"
      elif subarray[1:2] == "8":
         waveband1 = "850"
      else:
         raise starutil.InvalidParameterError("Unexpected value '{0}' found "
                  "for SUBARRAY FITS Header in {1}.".format(subarray,config1))
   else:
      waveband1 = None

#  Get the second config string.
   config2 = parsys["CONFIG2"].value
   if config2 == None:
      if param == None:
         print( "\n!! No value supplied for CONFIG2" )
         os._exit(1)
      waveband2 = waveband1

#  See if is a group expression or NDF.
   else:
      if any( (c in gexp_chars) for c in config2 ):
         isndf2 = False
      else:
         isndf2 = True

#  If it is an NDF, attempt to get the SUBARRAY fits header, and
#  determine the waveband.
      if isndf2:
         try:
            subarray = starutil.get_fits_header( config2, "SUBARRAY" )
            isndf2 = True
            if subarray == None:
               text = starutil.invoke( "$KAPPA_DIR/provshow {0}".format(config2) )
               if "s4a" in text or "s4b" in text or "s4c" in text or "s4d" in text:
                  subarray = "s4"
               elif "s8a" in text or "s8b" in text or "s8c" in text or "s8d" in text:
                  subarray = "s8"
               else:
                  subarray = None
         except:
            print( "\n!! It looks like NDF '{0}' either does not exist or is "
                   "corrupt.".format(config2) )
            os._exit(1)

      if isndf2:
         if subarray == None:
            msg_out("Cannot determine the SCUBA-2 waveband for NDF '{0}' "
                    "- was it really created by MAKEMAP?".format(config2), starutil.CRITICAL )
            waveband2 = None
         elif subarray[1:2] == "4":
            waveband2 = "450"
         elif subarray[1:2] == "8":
            waveband2 = "850"
         else:
            raise starutil.InvalidParameterError("Unexpected value '{0}' found "
                     "for SUBARRAY FITS Header in {1}.".format(subarray,config2))
      else:
         waveband2 = None

#  If config1 is an ndf but config2 is not, use the waveband from config1.
   if waveband1 != None and waveband2 == None:
      waveband2 = waveband1

#  If config2 is an ndf but config1 is not, use the waveband from config2.
   elif waveband1 == None and waveband2 != None:
      waveband1 = waveband2

#  If neither are ndfs, use the WAVEBAND parameter value for both.
   elif waveband1 == None and waveband2 == None:
      waveband2 = parsys["WAVEBAND"].value
      waveband1 = waveband2

#  If both are ndfs, but for different wavebands, issue a critical warning.
   elif waveband1 != waveband2:
      msg_out( "\n  Supplied NDFs are for different wavebands", starutil.CRITICAL )

#  Tell the user what wavebands are being used.
   if waveband1 == waveband2:
      msg_out( "\n  Showing {0} um values".format(waveband1),
            starutil.CRITICAL)
   else:
      msg_out( "\n  Showing {0} um values from {1}".format(waveband1,config1),
               starutil.CRITICAL)
      msg_out( "\n  Showing {0} um values from {1}".format(waveband2,config2),
               starutil.CRITICAL)

#  Get the waveband selection specifier for config1.
   if waveband1 == "450":
      select = starutil.shell_quote( '"850=0,450=1"' )
   else:
      select = starutil.shell_quote( '"850=1,450=0"' )

#  Deal with cases where we are comparing two whole configs...
   if param == None:

#  List the configuration parameters in alphabetical order to a text file.
      if isndf1:
         conf1 = invoke("$KAPPA_DIR/configecho ndf={0} application=makemap "
                        "config=! name=! sort=yes defaults={1} select={2}".
                        format(config1,defs,select) )
      else:
         config1 = starutil.shell_quote( config1 )
         conf1 = invoke("$KAPPA_DIR/configecho ndf=! application=makemap "
                        "config={0} name=! sort=yes defaults={1} select={2}".
                        format(config1,defs,select) )

#  Write the config parameters to a disk file.
      fd = open( "config1.tmp", "w" )
      fd.write( conf1 )
      fd.close()

#  Do the same with the second configuration.
      if waveband2 == "450":
         select = starutil.shell_quote( '"850=0,450=1"' )
      else:
         select = starutil.shell_quote( '"850=1,450=0"' )

      if isndf2:
         conf2 = invoke("$KAPPA_DIR/configecho ndf={0} application=makemap "
                        "config=! name=! sort=yes defaults={1} select={2}".
                        format(config2,defs,select) )
      else:
         config2 = starutil.shell_quote( config2 )
         conf2 = invoke("$KAPPA_DIR/configecho ndf=! application=makemap "
                        "config={0} name=! sort=yes defaults={1} select={2}".
                        format(config2,defs,select) )

      fd = open( "config2.tmp", "w" )
      fd.write( conf2 )
      fd.close()

#  Invoke the specified file comparison tool to view the two config files.
      subprocess.call( [tool, "config1.tmp", "config2.tmp"], stdout=open(os.devnull),
                                                             stderr=subprocess.STDOUT )

#  Remove the temporary config text files.
      cleanup()

#  Deal with cases where we are displaying a single parameter...
   else:

#  Get the value of the requested parameter fro config1.
      if isndf1:
         value1 = invoke("$KAPPA_DIR/configecho ndf={0} application=makemap "
                        "config=! name={3} defaults={1} select={2}".
                        format(config1,defs,select,param) )
      else:
         config1 = starutil.shell_quote( config1 )
         value1 = invoke("$KAPPA_DIR/configecho ndf=! application=makemap "
                        "config={0} name={3} defaults={1} select={2}".
                        format(config1,defs,select,param) )

#  Get the value of the requested parameter fro config2 (if supplied).
      if config2 != None:
         if isndf2:
            value2 = invoke("$KAPPA_DIR/configecho ndf={0} application=makemap "
                           "config=! name={3} defaults={1} select={2}".
                           format(config2,defs,select,param) )
         else:
            config2 = starutil.shell_quote( config2 )
            value2 = invoke("$KAPPA_DIR/configecho ndf=! application=makemap "
                           "config={0} name={3} defaults={1} select={2}".
                           format(config2,defs,select,param) )
      else:
         value2 = None

#  Display them.
      if value2 == None:
         msg_out( "\n  {0} = {1}".format(param,value1), starutil.CRITICAL)
      else:
         msg_out( "\n  {0} = {1} (config1)".format(param,value1), starutil.CRITICAL)
         msg_out( "  {0} = {1} (config2)".format(param,value2), starutil.CRITICAL)

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

