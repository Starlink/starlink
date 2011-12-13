      SUBROUTINE CCD1_EXLIS( INRAN, INID, INX, INY, NIN, LOW, HIGH,
     :                       OUTRAN, OUTID, OUTX, OUTY, NOUT, STATUS )
*+
*  Name:
*     CCD1_EXLIS

*  Purpose:
*     Extract list points from a superlist.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_EXLIS( INRAN, INID, INX, INY, NIN, LOW, HIGH, OUTRAN,
*                      OUTID, OUTX, OUTY, NOUT, STATUS )

*  Description:
*     This routine pulls a subset of points out of a longer list and
*     writes it into a second list.  Each point for input and output
*     consists of two integer values (rank and ID) and two double
*     precision values (X and Y coordinates).  Points are selected
*     for copying only if their rank value is within the range
*     LOW..HIGH inclusive.

*  Arguments:
*     INRAN( * ) = INTEGER (Given)
*        Rank values of the input list.
*     INID( * ) = INTEGER (Given)
*        ID values of the input list.
*     INX( * ) = DOUBLE PRECISION (Given)
*        X coordinate values of the input list.
*     INY( * ) = DOUBLE PRECISION (Given)
*        Y coordinate values of the input list.
*     NIN = INTEGER (Given)
*        The number of members of the input list (size of INRAN, INID,
*        INX and INY).
*     LOW = INTEGER (Given)
*        Input list members will only make it into the output list if
*        their INRAN value is greater than or equal to LOW.
*     HIGH = INTEGER (Given)
*        Input list members will only make it into the output list if
*        their INRAN value is less than or equal to HIGH.
*     OUTRAN( * ) = INTEGER (Returned)
*        Rank values of output list.
*     OUTID( * ) = INTEGER (Returned)
*        ID values of output list.
*     OUTX( * ) = DOUBLE PRECISION (Returned)
*        X coordinate values of output list.
*     OUTY( * ) = DOUBLE PRECISION (Returned)
*        Y coordinate values of output list.
*     NOUT = INTEGER (Returned)
*        Number of members written into output list (size of arrays
*        OUTRAN, OUTID, OUTX and OUTY).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-FEB-2001 (MBT):
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
      INTEGER INRAN( * )
      INTEGER INID( * )
      DOUBLE PRECISION INX( * )
      DOUBLE PRECISION INY( * )
      INTEGER NIN
      INTEGER LOW
      INTEGER HIGH

*  Arguments Returned:
      INTEGER OUTRAN( * )
      INTEGER OUTID( * )
      DOUBLE PRECISION OUTX( * )
      DOUBLE PRECISION OUTY( * )
      INTEGER NOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NOUT = 0
      DO I = 1, NIN
         IF ( INRAN( I ) .GE. LOW .AND. INRAN( I ) .LE. HIGH ) THEN
            NOUT = NOUT + 1
            OUTRAN( NOUT ) = INRAN( I )
            OUTID( NOUT ) = INID( I )
            OUTX( NOUT ) = INX( I )
            OUTY( NOUT ) = INY( I )
         END IF
      END DO

      END
* $Id$
