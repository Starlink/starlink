      SUBROUTINE PAR_CANCL ( PARAM, STATUS )
*+
*  Name:
*     PAR_CANCL

*  Purpose:
*     Cancels a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PAR_CANCL( PARAM, STATUS )

*  Description:
*     The named parameter is cancelled.  A subsequent attempt to get a
*     value for the parameter will result in a new value being obtained
*     by the underlying parameter system.

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The name of the parameter to be cancelled.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The routine attempts to execute regardless of the value of
*     STATUS.  If the import value is not SAI__OK, then it is left
*     unchanged, even if the routine fails to complete.  If the STATUS
*     is SAI__OK on entry and the routine fails to complete, STATUS
*     will be set to an appropriate error number, and there will one or
*     more additional error reports.

*  Algorithm:
*     Call the underlying parameter-system primitives.

*  Copyright:
*     Copyright (C) 1988, 1990, 1992 Science & Engineering Research Council.
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
*     BDK: B D Kelly (REVAD::BDK)
*     AJC: A J Chipperfield (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1990 (BDK):
*        Original version.
*     1-JUN-1988 (AJC):
*        Revised prologue.
*     9-NOV-1990 (AJC):
*        Revised prologue again.
*     1992 March 27 (MJC):
*        Used SST prologues.
*     1992 November 13 (MJC):
*        Commented the code, and renamed the NAMECODE identifier.
*        Re-tidied the prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                  ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'              ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM        ! Parameter name

*  Status:
      INTEGER STATUS                 ! Global status

*  Local Variables:
      INTEGER NAMCOD                 ! Pointer to parameter
      INTEGER ISTAT                  ! Temporary status

*.

*  Record the input status.
      ISTAT = STATUS

*  Use a good status so that the SUBPAR calls have a chance of working.
      STATUS = SAI__OK

*  Find the parameter-system pointer to the internal parameter space
*  associated with the parameter.
      CALL SUBPAR_FINDPAR( PARAM, NAMCOD, STATUS )

*  Cancel the parameter
      CALL SUBPAR_CANCL( NAMCOD, STATUS )

*  Reset the global status to its former bad value.
      IF ( ISTAT .NE. SAI__OK ) STATUS = ISTAT

      END
