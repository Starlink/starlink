      SUBROUTINE IMG1_FTSCP( A, NIN, B, NOUT, STATUS )
*+
* Name:
*    IMG1_FTSCP

*  Purpose:
*     Copies a "FITS block".

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_FTSCP( A, NIN, B, NOUT, STATUS )

*  Description:
*     This routine copies a FITS block from one array into another.
*     FITS blocks are 1-D character arrays with a string size of 80.  If
*     any elements of the input array are blank then they are not copied
*     to the output array (this indicates a deleted record in IMG). The
*     number of real elements in the output array is returned as NOUT.
*     Extra elements of the output array are set blank.

*  Arguments:
*     A( NIN ) = CHARACTER * ( * ) (Given)
*        The FITS block to copy.
*     NIN = INTEGER (Given)
*        The number of entries (elements) in A. B should usually be at
*        least this large unless the number of blank records are
*        already known.
*     B( * ) = CHARACTER * ( * ) (Returned)
*        The copy of A excluding any blank records.
*     NOUT = INTEGER (Returned)
*       The number of elements used in B on exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     20-JUL-1994 (PDRAPER):
*        Original version.
*     27-JUL-1994 (PDRAPER):
*        Added checks for blank records.
*     20-OCT-2000 (PDRAPER):
*        Now fills extra records with blanks.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NIN
      CHARACTER * ( * ) A( NIN )

*  Arguments Returned:
      CHARACTER * ( * ) B( * )
      INTEGER NOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      NOUT = 0
      DO 1 I = 1, NIN
         IF ( A( I ) .NE. ' ' ) THEN
            NOUT = NOUT + 1
            B( NOUT ) = A( I )
         END IF
 1    CONTINUE

*  Fill any remaining output elements with blanks (this is required by
*  the FITS standard).
      IF( NOUT .LT. NIN ) THEN
         DO 2 I = NOUT + 1, NIN
            B( I ) = ' '
 2       CONTINUE
      END IF

      END
* $Id$
