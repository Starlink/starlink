      SUBROUTINE NDF_UNMAP( INDF, COMP, STATUS )
*+
*  Name:
*     NDF_UNMAP

*  Purpose:
*     Unmap an NDF or a mapped NDF array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_UNMAP( INDF, COMP, STATUS )

*  Description:
*     The routine unmaps an NDF, or an individual NDF array which has
*     previously been mapped for READ, UPDATE or WRITE access.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF component to be unmapped: 'AXIS', 'DATA',
*        'QUALITY', 'VARIANCE' or '*'. The last value acts as a wild
*        card, causing all mapped arrays to be unmapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  A component name of 'AXIS' will act as a partial wild card,
*     unmapping any axis arrays which are mapped, but leaving other
*     components unchanged. The routine NDF_AUNMP may be used to unmap
*     individual axis arrays.
*     -  A comma-separated list of component names may also be given, in
*     which case each component will be unmapped in turn.
*     -  An error will be reported if a component has not previously
*     been mapped for access, except in the case where a value of '*'
*     is given for COMP, or where 'AXIS' is used to unmap axis arrays.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Import the NDF identifier and unmap the specified component(s).
*     -  Restore the error context, reporting additional context
*     information if appropriate.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-OCT-1989 (RFWS):
*        Original, derived from the equivalent ARY_ routine.
*     11-JAN-1990 (RFWS):
*        Changed the wild card character description in the routine
*        prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER TSTAT              ! Temporary status variable

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Import the NDF identifier and unmap its component(s).
      STATUS = SAI__OK
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF1_UMP( IACB, COMP, STATUS )
      END IF

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Report context information and call error tracing routine if
*  appropriate.
         ELSE
            CALL ERR_REP( 'NDF_UNMAP_ERR',
     :      'NDF_UNMAP: Error unmapping an NDF or an array ' //
     :      'component of an NDF.', STATUS )
            CALL NDF1_TRACE( 'NDF_UNMAP', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
