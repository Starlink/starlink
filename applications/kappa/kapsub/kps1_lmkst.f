      SUBROUTINE KPS1_LMKST( NAX, NPIN, INDIM, INPOS, ID0, INID,
     :                       START, NPOUT, OUTPOS, OUTID, STATUS )
*+
*  Name:
*     KPS1_LMKST

*  Purpose:
*     Copy given positions to the output positions list created by LISTMAKE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LMKST( NAX, NPIN, INDIM, INPOS, ID0, INID, START, NPOUT,
*                      OUTPOS, OUTID, STATUS )

*  Description:
*     This routine copies positions and identifiers from the input
*     arrays to the output arrays, starting at a given index within the
*     output arrays.

*  Arguments:
*     NAX = INTEGER (Given)
*        The number of axes for the supplied positions.
*     NPIN = INTEGER (Given)
*        The number of supplied positions.
*     INDIM = INTEGER (Given)
*        The size of the first dimension of the input positions array.
*        Must be no smaller than NPIN.
*     INPOS( INDIM, NAX ) = DOUBLE PRECISION (Given)
*        The supplied positions.
*     ID0 = INTEGER (Given)
*        The integer identifier value to associate with the first
*        supplied position. Identifiers for subsequent positions increase
*        by 1 for each position. If this is supplied less than or equal
*        to zero, then its value is ignored and the identifiers supplied
*        in array INID are used instead.
*     INID( NPIN ) = INTEGER (Given)
*        The individual integer identifiers to associate with each
*        supplied position. Only accessed if ID0 is less than or equal to
*        zero.
*     START = INTEGER (Given)
*        The index within the output arrays at which to store the first
*        supplied position.
*     NPOUT = INTEGER (Given)
*        The size of the first dimension of array OUTPOS.
*     OUTPOS( NPOUT, NAX ) = DOUBLE PRECISION (Returned)
*        The returned positions.
*     OUTID( NPOUT ) = INTEGER (Returned)
*        The returned position identifiers.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NAX
      INTEGER NPIN
      INTEGER INDIM
      DOUBLE PRECISION INPOS( INDIM, NAX )
      INTEGER ID0
      INTEGER INID( NPIN )
      INTEGER START
      INTEGER NPOUT

*  Arguments Returned:
      DOUBLE PRECISION OUTPOS( NPOUT, NAX )
      INTEGER OUTID( NPOUT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Input position count
      INTEGER J                  ! Output position count
      INTEGER K                  ! Axis count
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the index of the previous position stored in the output
*  arrays.
      J = START - 1

*  Check each input position.
      DO I = 1, MIN( NPIN, NPOUT - START + 1 )

*  Find the index of the position in the output arrays.
         J = J + 1

*  Copy all axis values for this position from the input array to the
*  output array.
         DO K = 1, NAX
            OUTPOS( J, K ) = INPOS( I, K )
         END DO

*  Copy the position identifier from the input array to the output array.
         IF( ID0 .GT. 0 ) THEN
            OUTID( J ) = ID0 + I - 1
         ELSE
            OUTID( J ) = INID( I )
         END IF

      END DO

      END
