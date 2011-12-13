      SUBROUTINE CCD1_CHUSB( IDIN, XIN, YIN, NIN, XLO, XHI, YLO, YHI,
     :                       IDOUT, XOUT, YOUT, NOUT, STATUS )
*+
*  Name:
*     CCD1_CHUSB

*  Purpose:
*     Selects positions within given bounds from a list.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_CHUSB( IDIN, XIN, YIN, NIN, XLO, XHI, YLO, YHI,
*                      IDOUT, XOUT, YOUT, NOUT, STATUS )

*  Description:
*     This routine selects a subset of an input position list
*     (ID, X coordinate, Y coordinate) according to whether they fall
*     between given upper and lower X and Y bounds, and writes
*     them to an output list.  Each point from the input list will
*     be copied to the output list only if XLO <= XIN( * ) <= XHI
*     and YLO < YIN( * ) < YHI.

*  Arguments:
*     IDIN( NIN ) = INTEGER (Given)
*        The ID values of the input list.
*     XIN( NIN ) = DOUBLE PRECISION (Given)
*        The X coordinate values of the input list.
*     YIN( NIN ) = DOUBLE PRECISION (Given)
*        The Y coordinate values of the input list.
*     NIN = INTEGER (Given)
*        The number of values in the input list.
*     XLO = DOUBLE PRECISION (Given)
*        Lower acceptable bound for X coordinate.
*     XHI = DOUBLE PRECISION (Given)
*        Upper acceptable bound for X coordinate.
*     YLO = DOUBLE PRECISION (Given)
*        Lower acceptable bound for Y coordinate.
*     YHI = DOUBLE PRECISION (Given)
*        Upper acceptable bound for Y coordinate.
*     IDOUT( * ) = INTEGER (Returned)
*        The ID values of the output list.
*     XOUT( * ) = DOUBLE PRECISION (Returned)
*        The X coordinate values of the output list.
*     YOUT( * ) = DOUBLE PRECISION (Returned)
*        The Y coordinate values of the output list.
*     NOUT = INTEGER (Returned)
*        The number of points in the output list, hence the number of
*        elements returned in the IDOUT, XOUT and YOUT arrays.
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
*     7-MAR-2001 (MBT):
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
      INTEGER NIN
      INTEGER IDIN( NIN )
      DOUBLE PRECISION XIN( NIN )
      DOUBLE PRECISION YIN( NIN )
      DOUBLE PRECISION XLO
      DOUBLE PRECISION XHI
      DOUBLE PRECISION YLO
      DOUBLE PRECISION YHI

*  Arguments Given and Returned:
      INTEGER IDOUT( * )
      DOUBLE PRECISION XOUT( * )
      DOUBLE PRECISION YOUT( * )
      INTEGER NOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise number of returned values.
      NOUT = 0

*  Copy items from input to output list.
      DO I = 1, NIN
         IF ( XIN( I ) .GE. XLO .AND. XIN( I ) .LE. XHI .AND.
     :        YIN( I ) .GE. YLO .AND. YIN( I ) .LE. YHI ) THEN
            NOUT = NOUT + 1
            IDOUT( NOUT ) = IDIN( I )
            XOUT( NOUT ) = XIN( I )
            YOUT( NOUT ) = YIN( I )
         END IF
      END DO

      END
* $Id$
