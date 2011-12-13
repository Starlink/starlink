      SUBROUTINE NDF_AUNMP( INDF, COMP, IAXIS, STATUS )
*+
*  Name:
*     NDF_AUNMP

*  Purpose:
*     Unmap an NDF axis array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_AUNMP( INDF, COMP, IAXIS, STATUS )

*  Description:
*     The routine unmaps an NDF axis array which has previously been
*     mapped for READ, UPDATE or WRITE access.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the axis array component to be unmapped: 'CENTRE',
*        'VARIANCE', 'WIDTH' or '*'. The last value acts as a wild
*        card, causing all mapped axis components to be unmapped.
*     IAXIS = INTEGER (Given)
*        Number of the NDF axis whose array is to be unmapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  A comma-separated list of axis component names may also be
*     given, in which case each component will be unmapped in turn.
*     -  A value of zero may be supplied for the IAXIS argument, in
*     which case the routine will unmap the specified component(s) for
*     all the NDF's axes.
*     -  An error will be reported if a component has not previously
*     been mapped for access, except in cases where a wild card
*     unmapping operation is specified (either with a component name of
*     '*' or an axis number of zero).

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     9-OCT-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

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
      INTEGER IAXIS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER TSTAT              ! Temporary status variable

*.


*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Import the NDF identifier.
      STATUS = SAI__OK
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Unmap the NDF axis component(s).
      CALL NDF1_AUMP( IAXIS, IACB, COMP, STATUS )

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Report context information and call error tracing routine if
*  appropriate.
         ELSE
            CALL ERR_REP( 'NDF_AUNMP_ERR',
     :      'NDF_AUNMP: Error unmapping an NDF axis array.',
     :      STATUS )
            CALL NDF1_TRACE( 'NDF_AUNMP', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
