      SUBROUTINE CCD1_CHUSP( GOOD, INPX, INPY, NINP, OUTPX, OUTPY, IND,
     :                       NOUTP, STATUS )
*+
*  Name:
*     CCD1_CHUSP

*  Purpose:
*     Copy positions conditionally.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_CHUSP( GOOD, INPX, INPY, NINP, OUTPX, OUTPY, IND,
*                      NOUTP, STATUS )

*  Description:
*     This routine copies positions (an X and Y coordinate) from
*     an input list to an output list only if a corresponding element
*     of an array of logical flags is true.  It also returns an array
*     which maps the index numbers of the output points to those of
*     the input ones, so that it is possible to keep track of which
*     have been selected and which rejected.

*  Arguments:
*     GOOD( NINP ) = LOGICAL (Given)
*        An array of flags to indicate whether to select the
*        corresponding elements of INPX and INPY.
*     INPX( NINP ) = DOUBLE PRECISION (Given)
*        X coordinates of input list of points.
*     INPY( NINP ) = DOUBLE PRECISION (Given)
*        Y coordinates of input list of points.
*     NINP = INTEGER (Given)
*        Number of points in input list.
*     OUTPX( NINP ) = DOUBLE PRECISION (Returned)
*        X coordinates of output list, i.e. points from input list which
*        were found to be within the polygon.
*     OUTPY( NINP ) = DOUBLE PRECISION (Returned)
*        Y coordinates of output list, i.e. points from input list which
*        were found to be within the polygon.
*     IND( NINP ) = INTEGER (Returned)
*        Index from input list of each point in output list.
*     NOUTP = INTEGER (Returned)
*        Number of points in output list.
*     STATUS = INTEGER (Given and returned)
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
*     MBT: Mark Taylor (STARLINK - IoA)
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
      INTEGER NINP
      LOGICAL GOOD( NINP )
      DOUBLE PRECISION INPX( NINP )
      DOUBLE PRECISION INPY( NINP )

*  Arguments Returned:
      DOUBLE PRECISION OUTPX( NINP )
      DOUBLE PRECISION OUTPY( NINP )
      INTEGER IND( NINP )
      INTEGER NOUTP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise number of returned (included) points.
      NOUTP = 0

*  Go through list of input points.
      DO 1 I = 1, NINP

*  If inside, copy point to output list.
         IF ( GOOD( I ) ) THEN
            NOUTP = NOUTP + 1
            OUTPX( NOUTP ) = INPX( I )
            OUTPY( NOUTP ) = INPY( I )
            IND( NOUTP ) = I
         END IF
 1    CONTINUE

      END
* $Id$
