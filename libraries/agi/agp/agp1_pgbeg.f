      SUBROUTINE AGP1_PGBEG( PARAM, WKNAME, STATUS )
*+
*  Name:
*     AGP1_PGBEG
*
*  Purpose:
*     Ensure PGPLOT is open for plotting on the specified device.
*
*  Invocation:
*     CALL AGP1_PGBEG( PARAM, WKNAME, STATUS )
*
*  Description:
*     This routine does nothing if PGPLOT is already active.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name associated with the graphics device. May be
*        blank. Only used for error messages.
*     WKNAME = CHARACTER * ( * ) (Given)
*        Workstation name
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*
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
*     DSB: David S. Berry (STARLINK)
*
*  History:
*     1-NOV-2001 (DSB):
*        Original version
*     14-JAN-2013 (DSB):
*        Release the error context.
*     20-MAR-2013 (DSB):
*        Added merging of EPS output files.
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'AGP_CONST'
      INCLUDE 'AGI_ERR'

*  Global Variables :
      INCLUDE 'AGP_COM'

*  Arguments Given :
      CHARACTER PARAM*(*)
      CHARACTER WKNAME*(*)

*  Status :
      INTEGER STATUS

*  Local variables :
      INTEGER PGBEG
      INTEGER CF, CL, LENSTR, ISTAT
      CHARACTER DEVNAM*64
      CHARACTER STRING*40
      CHARACTER AGINAM*(AGP__SZANM)
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Do nothing if PGPLOT is already open
      CALL PGQINF( 'STATE', STRING, LENSTR )
      IF( STRING( :LENSTR ) .EQ. 'CLOSED' ) THEN

*  Indicate no device is currently opened.
         AGP_CRAWN = ' '

*  Find the native PGPLOT device specification for the supplied
*  workstation name.
         CALL AGP1_TRANS( PARAM, WKNAME, DEVNAM, AGINAM, STATUS )

*  Set flags in common to indicate if postscript files need to
*  be merged when the device is closed.
         CALL AGP1_EPSCH( DEVNAM, STATUS )

*   Mark a new error context to trap errors from PGBEG
         CALL ERR_MARK

*   Append the /APPEND option to the device name
         CALL CHR_FANDL( DEVNAM, CF, CL )
         CALL CHR_APPND( '/APPEND', DEVNAM, CL )

*   Now open the device for PGPLOT, using the /APPEND option to
*   prevent the screen clearing
         ISTAT = PGBEG( 0, DEVNAM, 1, 1 )

*   Annul any errors caused by the /APPEND option in PGBEG.
*   Devices that cannot support /APPEND report an error but continue.
         IF ( ISTAT .EQ. 1 ) THEN
            CALL ERR_STAT( STATUS )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*  Success... save the AGI name of the currently opened PGPLOT device.
            AGP_CRAWN = AGINAM

         ELSE
            STATUS = AGI__WKNOP
            CALL ERR_REP( 'AGP1_PGBEG_WKNOP',
     :                    'Problems opening workstation', STATUS )
         END IF

*   Release the error context.
         CALL ERR_RLSE

      END IF

      END
