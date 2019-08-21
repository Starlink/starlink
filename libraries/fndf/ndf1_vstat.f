      SUBROUTINE NDF1_VSTAT( STATE, VSTATE, STATUS )
*+
*  Name:
*     NDF1_VSTAT

*  Purpose:
*     Validate an NDF state string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VSTAT( STATE, VSTATE, STATUS )

*  Description:
*     The routine validates an NDF state string, returning an upper
*     case version if it is valid. Otherwise, an error is reported.

*  Arguments:
*     STATE = CHARACTER * ( * ) (Given)
*        The NDF state string to be validated. Valid values are
*        'OLD', 'NEW' or 'UNKNOWN' (case insensitive).
*     VSTATE = CHARACTER * ( * ) (Returned)
*        The validated NDF state string in upper case (not returned if
*        the STATE value is invalid).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     DSB: D.S. Berry (STARLINK)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     21-JUN-1993 (DSB):
*        Original version, based on NDF1_VMOD.
*     23-JUN-1993 (RFWS):
*        Minor changes.
*     23-SEP-1993 (RFWS):
*        Allow abbreviation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STATE

*  Arguments Returned:
      CHARACTER * ( * ) VSTATE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the NDF state string supplied against each permitted value in
*  turn, returning the appropriate validated value.

*  NDF exists:
      IF ( NDF1_SIMLR( STATE, 'OLD', NDF__MINAB ) ) THEN
         CALL NDF1_CCPY( 'OLD', VSTATE, STATUS )

*  NDF doesn't exist:
      ELSE IF ( NDF1_SIMLR( STATE, 'NEW', NDF__MINAB ) ) THEN
         CALL NDF1_CCPY( 'NEW', VSTATE, STATUS )

*  NDF's existence unknown:
      ELSE IF ( NDF1_SIMLR( STATE, 'UNKNOWN', NDF__MINAB ) ) THEN
         CALL NDF1_CCPY( 'UNKNOWN', VSTATE, STATUS )

*  If the NDF state was not recognised, then report an error.
      ELSE
         STATUS = NDF__STAIN
         CALL MSG_SETC( 'BADSTATE', STATE )
         CALL ERR_REP( 'NDF1_VSTAT_BAD',
     :   'Invalid NDF state ''^BADSTATE'' specified (possible ' //
     :   'programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VSTAT', STATUS )

      END
