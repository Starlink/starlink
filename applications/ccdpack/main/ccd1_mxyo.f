      SUBROUTINE CCD1_MXYO( X1, Y1, INDI1, N1, X2, Y2, INDI2, N2,
     :                      XOFF, YOFF, ERROR, XO1, YO1, XO2, YO2,
     :                      NMATCH, INDO1, INDO2, STATUS )
*+
*  Name:
*     CCD1_MXYO

*  Purpose:
*     Matches X and Y positions with known offsets for equality (within
*     error).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MXYO( X1, Y1, INDI1, N1, X2, Y2, INDI2, N2,
*                     XOFF, YOFF, ERROR, XO1, YO2, XO2, YO2,
*                     NMATCH, INDO1, INDO2, STATUS )

*  Description:
*     This routine matches all the positions X1,Y1 and X2,Y2 for
*     equality, after the positions X2,Y2 have been offset by the amount
*     XOFF,YOFF. Equality is determined by a position being within
*     +/- ERROR of the other position. When two positions are matched
*     their orginal values are written to the output lists XO1,YO1
*     and XO2,YO2.  If the original positions in the lists are given
*     in arrays INDI1, INDI2, then the final ones are returned in
*     INDO1, INDO2.

*  Arguments:
*     X1( N1 ) = DOUBLE PRECISION (Given)
*        The first set of X positions.
*     Y1( N1 ) = DOUBLE PRECISION (Given)
*        The first set of Y positions.
*     INDI1( N1 ) = INTEGER (Given)
*        Indices for each of the points in the first set.
*     N1 = INTEGER (Given)
*        The number of values in X1 and Y1.
*     X2( N2 ) = DOUBLE PRECISION (Given)
*        The second set of X positions.
*     Y2( N2 ) = DOUBLE PRECISION (Given)
*        The second set of Y positions.
*     INDI2( N2 ) = INTEGER (Given)
*        Indices for each of the points in the second set.
*     N2 = INTEGER (Given)
*        The number of values in X2 and Y2.
*     XOFF = DOUBLE PRECISION (Given)
*        The offset in X which is applied to positions X2 to
*        transform to positions X1. This factor is additive.
*     YOFF = DOUBLE PRECISION (Given)
*        The offset in Y which is applied to positions Y2 to
*        transform to positions Y1. This factor is additive.
*     ERROR = DOUBLE PRECISION (Given)
*        The allowed error in the position match. For a match to occur
*     XO1( N1 ) = DOUBLE PRECISION (Returned)
*        The matched X1 positions.
*     YO1( N1 ) = DOUBLE PRECISION (Returned)
*        The matched Y1 positions.
*     XO2( N2 ) = DOUBLE PRECISION (Returned)
*        The matched X2 positions.
*     YO2( N2 ) = DOUBLE PRECISION (Returned)
*        The matched X2 positions.
*     NMATCH = INTEGER (Returned)
*        The number of matches.
*     INDO1( N1 ) = INTEGER (Returned)
*        The indices of the X1,Y1 positions in the original lists.
*     INDO2( N2 ) = INTEGER (Returned)
*        The indices of the X2,Y2 positions in the original lists.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  the first match which occurs is choosen, no note is made of
*     better matches which occur later.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     11-JAN-1993 (PDRAPER):
*        Original version.
*     6-APR-1993 (PDRAPER):
*        Added index arrays.
*     08-FEB-1999 (MBT):
*        Added index arrays on input as well as output.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER N1
      DOUBLE PRECISION X1( N1 )
      DOUBLE PRECISION Y1( N1 )
      INTEGER INDI1( N1 )
      INTEGER N2
      DOUBLE PRECISION X2( N2 )
      DOUBLE PRECISION Y2( N2 )
      INTEGER INDI2( N2 )
      DOUBLE PRECISION XOFF
      DOUBLE PRECISION YOFF
      DOUBLE PRECISION ERROR

*  Arguments Returned:
      DOUBLE PRECISION XO1( N1 )
      DOUBLE PRECISION YO1( N1 )
      DOUBLE PRECISION XO2( N2 )
      DOUBLE PRECISION YO2( N2 )
      INTEGER NMATCH
      INTEGER INDO1( N1 )
      INTEGER INDO2( N2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      DOUBLE PRECISION XDIFF     ! Difference in X values
      DOUBLE PRECISION YDIFF     ! Difference in Y values

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set no. of output positions.
      NMATCH = 0

*  Loop over possible intercomparisons of the two sets of positions.
      IF ( N2 .GT. N1 ) THEN
         DO 1 I = 1, N1
            DO 2 J = 1, N2

*  Form the difference in X and Y positions. Offsetting the second
*  positions by XOFF and YOFF
               XDIFF = ABS( X1( I ) - ( X2( J ) + XOFF ) )
               YDIFF = ABS( Y1( I ) - ( Y2( J ) + YOFF ) )

*  Are they matched?
               IF ( XDIFF .LE. ERROR .AND. YDIFF .LE. ERROR ) THEN

*  Yes they are. Write out the records.
                  NMATCH = NMATCH + 1
                  XO1( NMATCH ) = X1( I )
                  YO1( NMATCH ) = Y1( I )
                  XO2( NMATCH ) = X2( J )
                  YO2( NMATCH ) = Y2( J )

*  Record the original positions.
                  INDO1( NMATCH ) = INDI1( I )
                  INDO2( NMATCH ) = INDI2( J )

*  Skip as no more matches are allowed.
                  GO TO 1
               END IF
 2          CONTINUE
 1       CONTINUE
      ELSE

*  Same looping in other sense
         DO 3 I = 1, N2
            DO 4 J = 1, N1

*  Form the difference in X and Y positions. Offsetting the second
*  positions by XOFF and YOFF
               XDIFF = ABS( X1( J ) - ( X2( I ) + XOFF ) )
               YDIFF = ABS( Y1( J ) - ( Y2( I ) + YOFF ) )

*  Are they matched?
               IF ( XDIFF .LE. ERROR .AND. YDIFF .LE. ERROR ) THEN

*  Yes they are. Write out the records.
                  NMATCH = NMATCH + 1
                  XO1( NMATCH ) = X1( J )
                  YO1( NMATCH ) = Y1( J )
                  XO2( NMATCH ) = X2( I )
                  YO2( NMATCH ) = Y2( I )

*  Record the original positions.
                  INDO1( NMATCH ) = INDI1( J )
                  INDO2( NMATCH ) = INDI2( I )

*  Skip as no more matches are allowed.
                  GO TO 3
               END IF
 4          CONTINUE
 3       CONTINUE
      END IF

      END
* $Id$
