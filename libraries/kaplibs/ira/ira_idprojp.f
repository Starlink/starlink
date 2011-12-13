      SUBROUTINE IRA_IDPROJP( IDA, IPAR, PROJP, STATUS )
*+
*  Name:
*     IRA_IDPROJP

*  Purpose:
*     Get a projection parameter value associated with an IRA identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_IDPROJP( IDA, IPAR, PROJP, STATUS )

*  Description:
*     This routine returns the value of a projection parameter
*     associated with an IRA identifier.

*  Arguments:
*     IDA = INTEGER (Given)
*        The IRA identifier for the astrometry information.
*     IPAR = INTEGER (Given)
*        The index of the required projection parameter, in the range 1
*        to IRA__MAXP.
*     PROJP = DOUBLE PRECISION (Returned)
*        Returned holding the value of the projection parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     4-OCT-2004 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common values.
*        ACM_PROJP( IRA__MAXP, IRA__MAX ) = DOUBLE PRECISION (Read)
*           Projection parameter values from the associated AS.

*  Arguments Given:
      INTEGER          IDA
      INTEGER          IPAR

*  Arguments Returned:
      DOUBLE PRECISION PROJP

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Store the required projection parameter.
         PROJP = ACM_PROJP( IPAR, IDA )

*  If an error occurred, give a context message.
      ELSE
         CALL ERR_REP( 'IRA_IDPROJP_ERR1', 'IRA_IDPROJP: Unable to '//
     :                 'get a projection parameter value for an IRA '//
     :                 'identifier', STATUS )
      END IF

      END
