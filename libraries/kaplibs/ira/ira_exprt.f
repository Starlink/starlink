      SUBROUTINE IRA_EXPRT( IDA, INDF, STATUS )
*+
*  Name:
*     IRA_EXPRT

*  Purpose:
*     Stores astrometry information in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_EXPRT( IDA, INDF, STATUS )

*  Description:
*     An HDS structure is created containing the astrometry information
*     identified by IDA. This "astrometry structure" is stored as a
*     component of an extension within the NDF specified by INDF (any
*     previous astrometry structure is over-written). The names of the
*     NDF extension and the astrometry structure are set by a call to
*     IRA_LOCAT. If no such call is made the names of the extension and
*     astrometry structure retain the values set up in IRA_INIT ("IRAS"
*     and "ASTROMETRY").  The astrometry structure has an HDS data type
*     of IRAS_ASTROMETRY. The NDF extension must already exist before
*     calling this routine.
*
*     Any existing astrometry structure is first deleted from the NDF
*     (in which ever extension it was found) before creating the new
*     one.
*
*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     INDF = INTEGER (Given)
*        The identifier for the NDF in which the astrometry information
*        is to be stored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1993 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants

*  Arguments Given:
      INTEGER   IDA
      INTEGER   INDF

*  Status:
      INTEGER   STATUS           ! Global status

*  Local Variables:
      CHARACTER
     :         LOC*(DAT__SZLOC)  ! HDS locator to astrometry structure.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )

*  Obtain an HDS locator to an empty astrometry structure within the
*  NDF (if an astrometry structure already exists, it is deleted first).
      CALL IRA1_ASNDF( INDF, LOC, STATUS )

*  Write the astrometry information to the astrometry structure.
      CALL IRA_WRITE( IDA, LOC, STATUS )

*  Annul the locator.
      CALL DAT_ANNUL( LOC, STATUS )

*  If an error occurred, give the context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'IRA_EXPRT_ERR1',
     :    'IRA_EXPRT: Unable to export astrometry information to ^NDF',
     :                STATUS )
      END IF

      END
