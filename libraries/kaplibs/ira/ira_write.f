      SUBROUTINE IRA_WRITE( IDA, LOC, STATUS )
*+
*  Name:
*     IRA_WRITE

*  Purpose:
*     Writes astrometry information into an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_WRITE( IDA, LOC, STATUS )

*  Description:
*     The astrometry information identified by IDA is stored within the
*     object located by argument LOC. The constant IRA__HDSTY gives the
*     HDS type required for the object. The object must have this type
*     and be empty.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     LOC = CHARACTER * ( * ) (Given)
*        A locator to the object to which the astrometry information is
*        to be written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-AUG-1992 (DSB):
*        Original version.
*     11-FEB-1993 (DSB):
*        Radically changed to accept an IRA identifier as input rather
*        than projection name, parameters, etc.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'IRA_ERR'          ! IRA_ errors constants

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_EPOCH( IRA__MAX ) = DOUBLE PRECISION (Read)
*           The Julian epoch of observation.
*        ACM_PROJN( IRA__MAX ) = CHARACTER (Read)
*           The full name of the projection.
*        ACM_PROJP( IRA__MAXP, IRA__MAX ) = DOUBLE PRECISION (Read)
*           Projection parameters.
*        ACM_SCS( IRA__MAX ) = CHARACTER (Read)
*           The full name of the sky coordinate system, with an optional
*           equinox specifier.

*  Arguments Given:
      INTEGER   IDA
      CHARACTER LOC*(*)

*  Status:
      INTEGER   STATUS           ! Global status

*  Local Variables:
      CHARACTER
     :         PROJ*(IRA__SZPRJ) ! Full name of the projection.

      INTEGER
     :         NP                ! No. of projection parameters.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )

*  Create the astrometry structure components within the supplied HDS
*  object.
      CALL IRA1_ASCRE( LOC, STATUS )

*  Get the number of projection parameters.
      CALL IRA1_CHPRJ( ACM_PROJN( IDA ), PROJ, NP, STATUS )

*  Store the name of the sky coordinate system produced by the
*  projection, the epoch of the observations, the projection name, and
*  the projection parameters, in the astrometry structure.
      CALL CMP_PUT0C( LOC, 'SCS', ACM_SCS( IDA ), STATUS )
      CALL CMP_PUT0D( LOC, 'EPOCH', ACM_EPOCH( IDA ), STATUS )
      CALL CMP_PUT0C( LOC, 'PROJ_NAME', ACM_PROJN( IDA ), STATUS )
      CALL CMP_PUT1D( LOC, 'PROJ_PARS', NP, ACM_PROJP( 1, IDA ),
     :                STATUS )

*  Set the AS into the DEFINED state.
      CALL IRA1_ASSET( LOC, STATUS )

*  If an error occurred, give the context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_WRITE_ERR1',
     :    'IRA_WRITE: Unable to write astrometry information to an '//
     :    'HDS object', STATUS )
      END IF

      END
