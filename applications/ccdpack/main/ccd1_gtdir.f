      SUBROUTINE CCD1_GTDIR( USEEXT, ID, DIRECT, FRMEXT, STATUS )
*+
*  Name:
*     CCD1_GTDIR

*  Purpose:
*     To get the CCD readout direction

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GTDIR( USEEXT, ID, DIRECT, FRMEXT, STATUS )

*  Description:
*     This routine returns the readout direction. The "intrinsic" value
*     for the readout may be accesses by two routes. If USEEXT is false
*     then the ADAM parameter DIRECTION is used to get a value. This
*     value is tested to see if it is X or Y, if it is not
*     then the user is reprompted for another value. If the value is X
*     then the DIRECT argument is assigned a value of 1, if the
*     character is Y then the DIRECT argument is assigned a value of 2.
*
*     If USEEXT is true then the NDF itself is accessed and the CCDPACK
*     extension item DIRECTION is obtained. If this doesn't exist
*     then the user is prompted, otherwise the value is returned.
*     On exit the value or the argument FRMEXT indicates whether the
*     value was obtained from the NDF extension or not.

*  Arguments:
*     USEEXT = LOGICAL (Given)
*       Whether to look in the extension of the NDF for the readout
*       direction or not.
*     ID = INTEGER (Given)
*       NDF identifier. Only used if USEEXT is true.
*     DIRECT = INTEGER (Returned)
*        The readout direction 1 or 2.
*     FRMEXT = LOGICAL (Returned)
*        Whether or not the value was actually obtained from the NDF
*        extension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1991 (PDRAPER):
*        Original version.
*     4-JAN-1994 (PDRAPER):
*        Added option to get DIRECTION from NDF extension.
*     18-JAN-1994 (PDRAPER):
*        Added FRMEXT argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL USEEXT
      INTEGER ID

*  Arguments Returned:
      INTEGER DIRECT
      LOGICAL FRMEXT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 1 ) RETURN   ! The user return
      INTEGER NTRY               ! Number of tries to get value
      LOGICAL OK                 ! Extension item ok
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return for FRMEXT is USEEXT value.
      FRMEXT = USEEXT

*  If USEEXT is true then try to access a value from the NDF extension.
      OK = .FALSE.
      IF ( USEEXT ) THEN
         CALL CCG1_FCH0C( ID, 'DIRECTION', RETURN, OK, STATUS )

*  If ok then validate the direction.
         IF ( OK ) THEN

*  Check the value.
            CALL CHR_UCASE( RETURN )
            IF ( RETURN .EQ. 'X' ) THEN
               DIRECT = 1
            ELSE IF ( RETURN .EQ. 'Y' ) THEN
               DIRECT = 2
            ELSE
*  Bad value.
               CALL CCD1_MSG( ' ', ' Warning - a bad DIRECTION'//
     :         ' value has been detected', STATUS )
               OK = .FALSE.
            END IF
         END IF
      END IF

*  If do not have a value by now prompt the user.
      IF ( .NOT. OK ) THEN

*  Value not obtained from extension.
         FRMEXT = .FALSE.
         NTRY = 0
 1       CONTINUE
         CALL PAR_GET0C( 'DIRECTION', RETURN, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         NTRY = NTRY + 1

*  Check the value.
         CALL CHR_UCASE( RETURN )
         IF ( RETURN .EQ. 'X' ) THEN
            DIRECT = 1
         ELSE IF ( RETURN .EQ. 'Y' ) THEN
            DIRECT = 2
         ELSE

*  Invalid return try again, up to limit.
            IF ( NTRY .LE. 4 ) THEN
               CALL MSG_OUT( 'BAD-DIRECT',
     :' Bad direction specification, must be X or Y try again', STATUS )
               CALL PAR_CANCL( 'DIRECTION', STATUS )
               GO TO 1
            END IF
         END IF
      END IF
 99   END
* $Id$
