      SUBROUTINE CCD1_GTCMP( NNDF, NDF, IPAIR, NPIX, NCMP, NOV, STATUS )
*+
*  Name:
*     CCD1_GTCMP

*  Purpose:
*     Identify pair-wise overlaps between NDFs for MAKEMOS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GTCMP( NNDF, NDF, IPAIR, NPIX, NCMP, NOV, STATUS )

*  Description:
*     The routine examines each possible pair of NDFs taken from a list
*     supplied by the MAKEMOS application and forms a list of those
*     pairs which have pixels in common (as identified by their pixel
*     indices). It also returns a count of the number of pixels of
*     overlap for each pair and the number of overlaps which each NDF
*     has with others.  An error is reported and STATUS set if any NDF
*     does not overlap with others.

*  Arguments:
*     NNDF = INTEGER (Given)
*        Number of NDFs.
*     NDF( NNDF ) = INTEGER (Given)
*        Array of identifiers for the NDFs to be considered.
*     IPAIR( 2, * ) = INTEGER (Returned)
*        A list of pairs of indices (into the NDF array) indicating
*        which pairs of NDFs overlap. The second dimension size should
*        be at least equal to the number of pair-wise overlaps between
*        the NDFs supplied. The number of overlaps returned is given
*        by the NCMP argument.
*     NPIX( * ) = INTEGER (Returned)
*        The number of pixels of overlap between each pair.  The size
*        of this array should be at least equal to the number of
*        pair-wise overlaps between the NDFs supplied.  The number of
*        values returned is given by the NCMP argument.
*     NCMP = INTEGER (Returned)
*        Number of overlaps in total.
*     NOV( NNDF ) = INTEGER (Returned)
*        Number of overlaps which each NDF has with others,
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     5-AUG-1992 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NNDF
      INTEGER NDF( NNDF )

*  Arguments Returned:
      INTEGER IPAIR( 2, * )
      INTEGER NPIX( * )
      INTEGER NCMP
      INTEGER NOV( NNDF )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER PIC                ! Number of pixels in common

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the count of overlaps for each NDF.
      DO 1 I = 1, NNDF
         NOV( I ) = 0
 1    CONTINUE

*  Loop to consider all combinations of NDFs in pairs.
      NCMP = 0
      DO 3 J = 1, NNDF
         DO 2 I = J + 1, NNDF

*  See if the NDFs in each pair have pixels in common.
            CALL CCD1_OVLAP( NDF( I ), NDF( J ), PIC, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            IF ( PIC .GT. 0 ) THEN

*  If so, then increment the count of overlaps and store the NDF
*  indices and the number of pixels in common.
               NCMP = NCMP + 1
               IPAIR( 1, NCMP ) = I
               IPAIR( 2, NCMP ) = J
               NPIX( NCMP ) = PIC

*  Increment the count of overlaps for each NDF.
               NOV( I ) = NOV( I ) + 1
               NOV( J ) = NOV( J ) + 1
            END IF
 2       CONTINUE
 3    CONTINUE

*  Check that each NDF overlaps with at least one other and report an
*  error if it does not.
      DO 4 I = 1, NNDF
         IF ( NOV( I ) .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDF( I ) )
            CALL ERR_REP( 'CCD1_GTCMP_LONE',
     :                    'The NDF ^NDF has no pixels in common ' //
     :                    'with the other NDFs supplied; unable ' //
     :                    'to determine the required ' //
     :                    'scale-factor/zero-point corrections.',
     :                    STATUS )
            GO TO 99
         END IF
 4    CONTINUE

*  Arrive here if an error occurs.
  99  END
* $Id$
