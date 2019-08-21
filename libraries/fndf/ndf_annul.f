      SUBROUTINE NDF_ANNUL( INDF, STATUS )
*+
*  Name:
*     NDF_ANNUL

*  Purpose:
*     Annul an NDF identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ANNUL( INDF, STATUS )

*  Description:
*     The routine annuls the NDF identifier supplied so that it is no
*     longer recognised as a valid identifier by the NDF_ routines.
*     Any resources associated with it are released and made available
*     for re-use. If any NDF components are mapped for access, then
*     they are automatically unmapped by this routine.

*  Arguments:
*     INDF = INTEGER (Given and Returned)
*        The NDF identifier to be annulled. A value of NDF__NOID is
*        returned (as defined in the include file NDF_PAR).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances. In particular, it
*     will fail if the identifier supplied is not initially valid, but
*     this will only be reported if STATUS is set to SAI__OK on entry.
*     -  An error will result if an attempt is made to annul the last
*     remaining identifier associated with an NDF whose DATA component
*     has not been defined (unless it is a temporary NDF, in which case
*     it will be deleted at this point).

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

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
*        Original, derived from the equivalent ARY_ system routine.
*     6-OCT-1989 (RFWS):
*        Added STATUS check after calling NDF1_IMPID.
*     3-JUN-1993 (RFWS):
*        Added error message logging for history recording.
*     2-OCT-1998 (RFWS):
*        Use ERR_BEGIN and ERR_END.
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

*.

*  Log any pending error message information for subsequent recording
*  in NDF history records.
      CALL NDF1_HLERR( STATUS )

*  Begin a new error reporting context.
      CALL ERR_BEGIN( STATUS )

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Annul the associated ACB entry and reset the NDF identifier value.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF1_ANL( IACB, STATUS )
      END IF
      INDF = NDF__NOID

* If an error occurred, report context information and call the error
* tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ANNUL_ERR',
     :                 'NDF_ANNUL: Error annulling an NDF identifier.',
     :                 STATUS )
         CALL NDF1_TRACE( 'NDF_ANNUL', STATUS )
      END IF

*  End the error reporting context.
      CALL ERR_END( STATUS )

      END
