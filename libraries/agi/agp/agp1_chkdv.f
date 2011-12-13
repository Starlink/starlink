      SUBROUTINE AGP1_CHKDV( AGINAM, STATUS )
*+
*  Name:
*     AGP1_CHKDV

*  Purpose:
*     Check the current PGPLOT device has a specified AGI type.

*  Invocation:
*     CALL AGP1_CHKDV( AGINAM, STATUS )

*  Description:
*     Reports an error if the AGI worksdtation name for the
*     currently opened PGPLOT device is different to the specified AGI
*     device type. This routine allows for the fact that there may be
*     more than one AGI device type for a given PGPLOT device type.

*  Arguments:
*     AGINAM = CHARACTER*(*) (Given)
*        The required AGI device type.
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     16-NOV-2001 (DSB):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'AGP_CONST'
      INCLUDE 'AGI_ERR'

*  Global Variables:
      INCLUDE 'AGP_COM'

*  Arguments Given:
      CHARACTER AGINAM*(*)

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AGP1_INIT          ! Initializes AGP common blocks

*  Local Variables:
      CHARACTER CURTY*(AGP__SZPTY)! Current PGPLOT device type
      INTEGER I                   ! Loop count
      INTEGER NAMLEN              ! Used length of CURTY
*.

*  Check status on entry
      IF( STATUS .NE. SAI__OK ) RETURN

*  Inquire the current PGPLOT workstation, and convert to upper case.
      CALL PGQINF( 'TYPE', CURTY, NAMLEN )
      CALL CHR_UCASE( CURTY )

*  We now find the PGPLOT device type corresponding to the required AGI
*  device type. Loop through the known device types, looking for the AGI
*  name.
      DO I = 1, AGP__NDEV
         IF( AGP_ANM( I ) .EQ. AGINAM ) THEN

*  We've found the entry for the required AGI device type. Compare the
*  corresponding PGPLOT device type with the current PGPLOT device type.
*  Report an error and abort if they differ.
            IF( AGP_PTY( I ) .NE. CURTY ) THEN
               STATUS = AGI__DIFDV
               CALL MSG_SETC( 'PTY', AGP_PTY( I ) )
               CALL MSG_SETC( 'CUR', CURTY )
               CALL ERR_REP( 'AGP1_CHKDV_ERR1', 'Current PGPLOT '//
     :                       'device has changed from ^PTY to '//
     :                       '^CUR (programming error).', STATUS )
               GO TO 999
            END IF

         END IF
      END DO

 999  CONTINUE

      END
