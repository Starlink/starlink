      SUBROUTINE CCD1_SNGL( MAXDIS, XIN1, YIN1, INDI1, NREC1, XIN2,
     :                      YIN2, INDI2, NREC2, XOUT1, YOUT1, XOUT2,
     :                      YOUT2, NOUT, XOFF, YOFF, INDO1, INDO2,
     :                      STATUS )
*+
*  Name:
*     CCD1_SNGL

*  Purpose:
*     Matches position lists one of which contains only a single entry.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_SNGL( MAXDIS, XIN1, YIN2, INDI1, NREC1, XIN2, YIN2,
*                     INDI2, NREC2, XOUT1, YOUT1, XOUT2, YOUT2, NOUT,
*                     XOFF, YOFF, INDO1, INDO2, STATUS )

*  Description:
*     This routine takes two sets of X and Y positions and determines
*     the X and Y translations between the two.  This routine is for
*     the special case in which one or both of the input position lists
*     contains only a single position.  In the case that one of the lists
*     contains more than one position, all matches are considered, and
*     the one requiring the smallest displacement is chosen.

*  Arguments:
*     MAXDIS = DOUBLE PRECISION (Given)
*        The maximum acceptable displacement in pixels between two
*        frames.  If an object match requires a displacement greater
*        than this it will be rejected.  If it is set to zero, there
*        are no restrictions.
*     XIN1( NREC1 ) = DOUBLE PRECISION (Given)
*        First set of X positions.
*     YIN1( NREC1 ) = DOUBLE PRECISION (Given)
*        First set of Y positions.
*     INDI1( NREC1 ) = INTEGER (Given)
*        The index numbers for the first set of positions.
*     NREC1 = INTEGER (Given)
*        The number of values given in the XIN1 and YIN1 arrays.
*     XIN2( NREC2 ) = DOUBLE PRECISION (Given)
*        Second set of X positions.
*     YIN2( NREC2 ) = DOUBLE PRECISION (Given)
*        Second set of Y positions.
*     INDI2( NREC2 ) = INTEGER (Given)
*        The index numbers for the second set of positions.
*     NREC2 = INTEGER (Given)
*        The number of values given in the XIN2 and YIN2 arrays.
*     XOUT1( NREC1 ) = DOUBLE PRECISION (Returned)
*        X values selected from first set of input X positions.
*     YOUT1( NREC1 ) = DOUBLE PRECISION (Returned)
*        Y values selected from first set of input Y positions.
*     XOUT2( NREC2 ) = DOUBLE PRECISION (Returned)
*        Y values selected from second set of input X positions.
*     YOUT2( NREC2 ) = DOUBLE PRECISION (Returned)
*        Y values selected from second set of input Y positions.
*     NOUT = INTEGER (Returned)
*        The number of matched positions.
*     XOFF = DOUBLE PRECISION (Returned)
*        The offset in X which was selected.
*     YOFF = DOUBLE PRECISION (Returned)
*        The offset in Y which was selected.
*     INDO1( NREC1 ) = INTEGER (Returned)
*        The index numbers for the first set of matched positions.
*     INDO2( NREC2 ) = INTEGER (Returned)
*        The index numbers for the second set of matched positions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     29-MAR-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      DOUBLE PRECISION MAXDIS
      INTEGER NREC1
      DOUBLE PRECISION XIN1( NREC1 )
      DOUBLE PRECISION YIN1( NREC1 )
      INTEGER INDI1( NREC1 )
      INTEGER NREC2
      DOUBLE PRECISION XIN2( NREC2 )
      DOUBLE PRECISION YIN2( NREC2 )
      INTEGER INDI2( NREC2 )

*  Arguments Returned:
      INTEGER NOUT
      DOUBLE PRECISION XOUT1( NREC1 )
      DOUBLE PRECISION YOUT1( NREC1 )
      DOUBLE PRECISION XOUT2( NREC2 )
      DOUBLE PRECISION YOUT2( NREC2 )
      DOUBLE PRECISION XOFF
      DOUBLE PRECISION YOFF
      INTEGER INDO1( NREC1 )
      INTEGER INDO2( NREC2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL MATCH              ! Successful match has been achieved
      DOUBLE PRECISION D2        ! Displacement squared for match
      DOUBLE PRECISION D2MAX     ! Maximum allowed displacement squared value
      DOUBLE PRECISION D2CUR     ! Minimum so far displacement squared value
      DOUBLE PRECISION XO        ! X offset
      DOUBLE PRECISION YO        ! Y offset
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Match has not been found yet.
      MATCH = .FALSE.

*  Set value for squared displacement below which any match is the best
*  acceptable one yet.  If MAXDIS is not null (zero), then this value
*  starts at the maximum possible value.  If MAXDIS is defined then it
*  it set accordingly.  In either case, any match found with a smaller
*  displacement becomes the current best match.
      IF ( MAXDIS .EQ. 0D0 ) THEN
         D2CUR = VAL__MAXD
      ELSE
         D2CUR = MAXDIS * MAXDIS
      END IF

*  List 1 is the one with a single value.
      IF ( NREC1 .EQ. 1 ) THEN

*  Calculate the offset between the point in list 1 and each point in
*  list 2.
         DO 1 I = 1, NREC2
            XO = XIN1( 1 ) - XIN2( I )
            YO = YIN1( 1 ) - YIN2( I )
            D2 = XO * XO + YO * YO

*  If the offset is smaller than previously considered ones, this is the
*  best match so far: set list 2 output values.
            IF ( D2 .LT. D2CUR ) THEN
               XOUT2( 1 ) = XIN2( I )
               YOUT2( 1 ) = YIN2( I )
               INDO2( 1 ) = INDI2( I )
               XOFF = XO
               YOFF = YO
               D2CUR = D2
               MATCH = .TRUE.
            END IF
 1       CONTINUE

*  If a match was achieved, set list 1 output values.
         IF ( MATCH ) THEN
            NOUT = 1
            XOUT1( 1 ) = XIN1( 1 )
            YOUT1( 1 ) = YIN1( 1 )
            INDO1( 1 ) = INDI1( 1 )
         ELSE
            NOUT = 0
         END IF

*  List 2 is the one with a single value.
      ELSE IF ( NREC2 .EQ. 1 ) THEN

*  Calculate the offset between each point in list 1 and the point in
*  list 2.
         DO 2 I = 1, NREC1
            XO = XIN1( I ) - XIN2( 1 )
            YO = YIN1( I ) - YIN2( 1 )
            D2 = XO * XO + YO * YO

*  If the offset is smaller than previously considered ones, this is the
*  best match so far: set list 1 output values.
            IF ( D2 .LT. D2CUR ) THEN
               XOUT1( 1 ) = XIN1( I )
               YOUT1( 1 ) = YIN1( I )
               INDO1( 1 ) = INDI1( I )
               XOFF = XO
               YOFF = YO
               D2CUR = D2
               MATCH = .TRUE.
            END IF
 2       CONTINUE

*  If a match was achieved, set list 2 output values.
         IF ( MATCH ) THEN
            NOUT = 1
            XOUT2( 1 ) = XIN2( 1 )
            YOUT2( 1 ) = YIN2( 1 )
            INDO2( 1 ) = INDI2( 1 )
         ELSE
            NOUT = 0
         END IF

*  Neither list has a single value - this ought not to happen.
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_SNGL_BADPAR',
     :   'Bad call of CCD1_SNGL: neither list has one element', STATUS )
      END IF

*  Set error status according to whether matching was successful.
      IF ( MATCH ) THEN
         NOUT = 1
      ELSE
         NOUT = 0
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_SNGL_MAXDISP',
     :'  1-object Match failed: minimum displacement exceeds MAXDISP',
     :                 STATUS )
      END IF

      END
* $Id$
