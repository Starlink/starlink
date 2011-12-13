      SUBROUTINE IRA1_ASDEF( LOC, DEF, STATUS )
*+
*  Name:
*     IRA1_ASDEF

*  Purpose:
*     See if an astrometry structure is in the DEFINED state.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_ASDEF( LOC, DEF, STATUS )

*  Description:
*     If the STATE component of the astrometry structure (AS)
*     exists and has the value other DEFINED, then DEF is returned
*     .true. Otherwise, DEF is returned .false.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to the astrometry structure.
*     DEF = LOGICAL (Returned)
*        Returned true if the AS is in the DEFINED state.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990, 1991, 1993 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     17-DEC-1990 (DSB):
*        Original version.
*     24-APR-1991 (DSB):
*        Name changed from IRA_$ASDEF to IRA1_ASDEF
*     11-FEB-1993 (DSB):
*        Error reported supressed if STATE doesn't exist.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Arguments Given:
      CHARACTER LOC*(*)

*  Arguments Returned:
      LOGICAL DEF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER STATE*(IRA__SZSTA)  ! Value of the STATE component of
                                    ! the AS.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the object is a structure.
      CALL DAT_STRUC( LOC, DEF, STATUS )

*  If so, see if the component STATE exists.
      IF( DEF ) CALL DAT_THERE( LOC, 'STATE', DEF, STATUS )

*  If it does, get its value.
      IF( DEF ) THEN
         CALL CMP_GET0C( LOC, 'STATE', STATE, STATUS )

*  Check its value.
         IF( STATE .EQ. 'DEFINED' ) THEN
            DEF = .TRUE.

         ELSE
            DEF = .FALSE.

         END IF

      END IF

      END
