#!/usr/bin/env python

'''
*+
*  Name:
*     CONFIGMELD

*  Purpose:
*     Compare two MAKEMAP configs using the unix "meld" tool

*  Language:
*     python (2.7 or 3.*)

*  Description:
*     This script use the unix meld tool to display two sets of
*     configuration parameters, highlighting the differences between
*     them. Each config may be supplied directly, as is done when running
*     MAKEMAP, or can be read from the History component of an NDF that
*     was created by MAKEMAP.

*  Usage:
*     configmeld config1 config2 defaults

*  Parameters:
*     CONFIG1 = LITERAL (Read)
*        The first configuration. This can be a normal config such as is
*        supplied for the CONFIG parameter of MAKEMAP, or an NDF created
*        by MAKEMAP.
*     CONFIG2 = LITERAL (Read)
*        The first configuration. This can be a normal config such as is
*        supplied for the CONFIG parameter of MAKEMAP, or an NDF created
*        by MAKEMAP.
*     DEFAULTS = LITERAL (Read)
*        This should be one of "450", "850" or "!". It specifies whether
*        default values approriate for 450 or 850 um data are to be supplied
*        for any MAKEMAP config parameters that are not specified in CONFIG1
*        or CONFIG2. If null (!) is supplied, no defaults are used, and the
*        displayed list includes only those parameter for which values
*        are defined by the supplied configurations. [!]

*  Notes:
*     - The unix "meld" command must be installed and available on the
*     unix PATH.

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

*-
'''


import starutil
from starutil import invoke
from starutil import ParSys
from subprocess import call

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
   params.append(starutil.Par0S("CONFIG1", "The first config" ))
   params.append(starutil.Par0S("CONFIG2", "The second config" ))
   params.append(starutil.Par0S("DEFAULTS", "The defaults to be used - "
                                "450 or 850", None, True ))

#  Initialise the parameters to hold any values supplied on the command
#  line.
   parsys = ParSys( params )

#  Get the defaults to use.
   defs = parsys["DEFAULTS"].value
   if defs == None:
      select = "!"
      defs = "!"
   elif defs == "850":
      select = "850=1,450=0"
      defs = "$SMURF_DIR/smurf_makemap.def"
   elif defs == "450":
      select = starutil.shell_quote( "'850=0,450=1'" )
      defs = "$SMURF_DIR/smurf_makemap.def"
   else:
      raise starutil.InvalidParameterError("Bad value for parameter DEFAULTS "
                                           "({0}) - must be 450 or 850.".format(defs))

#  Get the first config string.
   config1 = parsys["CONFIG1"].value

#  Attempt to list the configuration parameters assuming it is an NDF. If this
#  fails, try again using it as a standard textual configuration specification.
   try:
      conf1 = invoke("$KAPPA_DIR/configecho ndf={0} application=makemap "
                     "config=! name=! sort=yes defaults={1} select={2}".
                     format(config1,defs,select) )
   except:
      config1 = starutil.shell_quote( config1 )
      conf1 = invoke("$KAPPA_DIR/configecho ndf=! application=makemap "
                     "config={0} name=! sort=yes defaults={1} select={2}".
                     format(config1,defs,select) )

#  Write the config parameters to a disk file.
   fd = open( "config1", "w" )
   fd.write( conf1 )
   fd.close()

#  Do the same with the second configuration.
   config2 = parsys["CONFIG2"].value
   try:
      conf2 = invoke("$KAPPA_DIR/configecho ndf={0} application=makemap "
                     "config=! name=! sort=yes defaults={1} select={2}".
                     format(config2,defs,select) )
   except:
      config2 = starutil.shell_quote( config2 )
      conf2 = invoke("$KAPPA_DIR/configecho ndf=! application=makemap "
                     "config={0} name=! sort=yes defaults={1} select={2}".
                     format(config2,defs,select) )
   fd = open( "config2", "w" )
   fd.write( conf2 )
   fd.close()

#  Invoke meld to view the two config files.
   call( ["meld", "config1", "config2"] )

#  If an StarUtilError of any kind occurred, display the message but hide the
#  python traceback. To see the trace back, uncomment "raise" instead.
except starutil.StarUtilError as err:
#  raise
   print( err )


