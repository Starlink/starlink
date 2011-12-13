      SUBROUTINE CCD1_NRMED( N, A, AMED, W, STATUS )
*+
*  Name:
*     CCD1_NRMED

*  Purpose:
*     Normalise corrections to a median value of zero.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NRMED( N, A, AMED, W, STATUS )

*  Description:
*     The routine normalises a set of corrections so that their median
*     is zero by replacing each value A(I) by (A(I)-AMED) where AMED is
*     the median value. The value of AMED is also returned.

*  Arguments:
*     N = INTEGER (Given)
*        Number of corrections.
*     A( N ) = DOUBLE PRECISION (Given and Returned)
*        The corrections to be normalised, updated on output.
*     AMED = DOUBLE PRECISION (Returned)
*        The median value used for normalisation.
*     W( N ) = DOUBLE PRECISION (Returned)
*        Workspace.
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
*     PDRAPER: P.W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  History:
*     23-APR-1992 (RFWS):
*        Original version.
*     19-SEP-1996 (PDRAPER):
*        Converted to use PDA sorting routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER N

*  Arguments Given and Returned:
      DOUBLE PRECISION A( N )
      DOUBLE PRECISION AMED

*  Arguments Returned:
      DOUBLE PRECISION W( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER IMED               ! Array index of median value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the values to the work array.
      DO 1 I = 1, N
         W( I ) = A( I )
 1    CONTINUE

*  Sort them into ascending order.
      CALL PDA_QSAD( N, W )

*  Find the median value.
      IMED = ( N / 2 ) + 1
      IF ( MOD( N, 2 ) .EQ. 0 ) THEN
         AMED = 0.5D0 * ( W( IMED ) + W( IMED - 1 ) )
      ELSE
         AMED = W( IMED )
      END IF

*  Normalise the values.
      DO 2 I = 1, N
         A( I ) = A( I ) - AMED
 2    CONTINUE

      END
* $Id$
