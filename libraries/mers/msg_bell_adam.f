      SUBROUTINE MSG_BELL( STATUS )
*+
*  Name:
*     MSG_BELL

*  Purpose:
*     Deliver an ASCII BEL character.

*  Language:
*    Starlink Fortran 77

*  Invocation:
*     CALL MSG_BELL( STATUS )

*  Description:
*     A bell character and a new line is delivered to the user. If the 
*     user interface in use supports the ASCII BEL character, this routine 
*     will ring a bell and print a new line on the terminal.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Notes:
*     -  This subroutine is the ADAM version of MSG1_PRINT.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (PCTR):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'MSG_ERR'                 ! MSG_ error codes
 
*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER ASCBEL                    ! ASCII BEL code
      PARAMETER ( ASCBEL = 7 )

*  Local Variables:
      CHARACTER BELCHR * 1              ! The bell character

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Use SUBPAR_WRMSG to deliver the bell character and MSG_SYNC to ensure
*  the output buffer is delivered.
      BELCHR = CHAR( ASCBEL )
      CALL SUBPAR_WRMSG( BELCHR, STATUS )
      CALL MSG_SYNC( STATUS )

*  Check the returned status and report an error message if necessary.
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = MSG__OPTER
         CALL EMS_MARK
         CALL EMS_REP( 'MSG_BELL_OPTER', 
     :   'Error encountered during BELL output.', STATUS )
         CALL EMS_RLSE
      END IF

      END
