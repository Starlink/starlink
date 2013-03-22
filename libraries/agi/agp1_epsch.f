      SUBROUTINE AGP1_EPSCH( DEVNAM, STATUS )
*+
*  Name:
*     AGP1_EPSCH

*  Purpose:
*     Do any required merging of EPS files.

*  Invocation:
*     CALL AGP1_EPSCH( STATUS )

*  Description:

*  Arguments:
*     DEVNAM = CHARACTER * ( * ) (Given and Returned)
*        The native PGPLOT device specification. If it is an EPS device,
*        it may be modified on exit to specify a different output file.
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)

*  History:
*     20-MAR-2013 (DSB):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'AGI_PAR'
      INCLUDE 'agp/AGP_CONST'

*  Global Variables:
      INCLUDE 'agi_eps'

*  Arguments Given and Returned:
      CHARACTER DEVNAM*(*)

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER PFILE*(AGP__SZUSP)
      CHARACTER PTYPE*(AGP__SZPTY)
      INTEGER IAT
      INTEGER ISEP
*.

*  Check status on entry
      IF( STATUS .NE. SAI__OK ) RETURN

*  Ensure the values in common are clear to begin with.
      OLDEPS = ' '
      NEWEPS = ' '
      CLREPS = .FALSE.

*  Split the pgplot device name into file and type. Use a default file
*  name of "pgplot.ps" if no file is specified. Only proceed if the
*  device name looks like a pgplot device name.
      ISEP = INDEX( DEVNAM, '/' )
      IF( ISEP .NE. 0 ) THEN
         PTYPE = DEVNAM( ISEP + 1: )
         IF( ISEP .GT. 1 ) THEN
            PFILE = DEVNAM( : ISEP - 1 )
         ELSE
            PFILE = 'pgplot.ps'
         END IF

*  Is the device one ot the AGI "accumulating" postscript pseudo-devices?
*  If so get the corresponding statndard PGPLOT device name.
         IF( PTYPE .EQ. 'AVPS' .OR.
     :       PTYPE .EQ. 'APS' .OR.
     :       PTYPE .EQ. 'AVCPS' .OR.
     :       PTYPE .EQ. 'ACPS' ) THEN

            PTYPE( 1 : 1 ) = ' '
            CALL CHR_LDBLK( PTYPE )

*  Yes. So we record the name of the old file in common, and choose a new
*  temporary file name to include in the PGPLOT device name. This
*  prevents the old file being over-written by PGPLOT.
            OLDEPS = PFILE

            NEWEPS = ' '
            IAT = 0
            CALL CHR_APPND( AGI__APSPX, NEWEPS, IAT )
            CALL CHR_APPND( PFILE, NEWEPS, IAT )

            DEVNAM = ' '

            IAT = 0
            CALL CHR_APPND( NEWEPS, DEVNAM, IAT )
            CALL CHR_APPND( '/', DEVNAM, IAT )
            CALL CHR_APPND( PTYPE, DEVNAM, IAT )

         END IF
      END IF

      END
