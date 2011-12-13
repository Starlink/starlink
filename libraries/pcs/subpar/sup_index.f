      SUBROUTINE SUBPAR_INDEX( NAMECODE, STATUS )
*+
*  Name:
*     SUBPAR_INDEX

*  Purpose:
*     Index into the parameter list for the current action.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_INDEX( NAMECODE, STATUS )

*  Description:
*     The routine accepts a namecode identifying one of the parameters
*     of the current action and returns a new namecode identifying the
*     next parameter (the order in which parameters are identified is
*     not defined).

*  Arguments:
*     NAMECODE = INTEGER (Given and Returned)
*        On entry: the name code of a parameter for the current action.
*        On exit: the namecode of the next parameter for the current
*        action.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If NAMECODE is zero on entry (or does not identify a parameter
*     of the current action), then the returned namecode will identify
*     the first parameter for the current action.
*     -  A namecode of zero is returned when there are no further
*     parameters for the current action.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     7-SEP-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'       ! SUBPAR global variables

*  Arguments Given and Returned:
      INTEGER NAMECODE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check if the namecode supplied refers to a parameter of the current
*  action. If not, then return the namecode of the first parameter for
*  the current action.
      IF ( ( NAMECODE .LT. PROGADD( 1, PROGNUM ) ) .OR.
     :     ( NAMECODE .GT. PROGADD( 2, PROGNUM ) ) ) THEN
         NAMECODE = PROGADD( 1, PROGNUM )

*  If the namecode supplied refers to the final parameter, then return
*  a value of zero.
      ELSE IF ( NAMECODE .EQ. PROGADD( 2, PROGNUM ) ) THEN
         NAMECODE = 0

*  Otherwise, increment the namecode to refer to the next parameter.
      ELSE
         NAMECODE = NAMECODE + 1
      END IF

      END
