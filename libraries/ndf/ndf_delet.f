      SUBROUTINE NDF_DELET( INDF, STATUS )
*+
*  Name:
*     NDF_DELET

*  Purpose:
*     Delete an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_DELET( INDF, STATUS )

*  Description:
*     The routine deletes the specified NDF. If this is a base NDF,
*     then the associated data object is erased and all NDF identifiers
*     which refer to it (or to sections derived from it) become
*     invalid. If any NDF components are mapped for access, then they
*     are first unmapped. If an NDF section is specified, then this
*     routine is equivalent to calling NDF_ANNUL, and no other
*     identifiers are affected.

*  Arguments:
*     INDF = INTEGER (Given and Returned)
*        Identifier for the NDF to be deleted. A value of NDF__NOID is
*        returned (as defined in the include file NDF_PAR).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Import the NDF identifier.
*     -  Check that DELETE access to the NDF is available.
*     -  If so, then perform a deletion operation on the ACB entry.
*     -  Reset the identifier value.
*     -  Restore the error context.

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
*     22-NOV-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     16-MAR-1990 (RFWS):
*        Ensure that the INDF value is reset even if the routine fails.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Arguments Given and Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER TSTAT              ! Temporary status variable

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Import the NDF identifier.
      STATUS = SAI__OK
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check that DELETE access to the NDF is available.
      CALL NDF1_CHACC( IACB, 'DELETE', STATUS )

*  If access is available, then perform a deletion operation on the ACB
*  entry. Reset the identifier value.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF1_DEL( IACB, STATUS )
      END IF
      INDF = NDF__NOID

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand. Release the error stack.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Report context information and call error tracing routine if
*  appropriate.
         ELSE
            CALL ERR_REP( 'NDF_DELET_ERR',
     :      'NDF_DELET: Error deleting an NDF.', STATUS )
            CALL NDF1_TRACE( 'NDF_DELET', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
