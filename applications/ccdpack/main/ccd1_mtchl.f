      SUBROUTINE CCD1_MTCHL( ID1, X1, Y1, NVAL1, ID2, X2, Y2, NVAL2,
     :                       NMATCH, STATUS )
*+
*  Name:
*     CCD1_MTCHL

*  Purpose:
*     Matches the values in ID1 and ID2 returning the corresponding
*     X1,Y1,X2,Y2 whose ID match.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MTCHL( ID1, X1, Y1, NVAL1, ID2, X2, Y2, NVAL2,
*                      NMATCH, STATUS )

*  Description:
*     This routine looks at the values in ID1 and ID2 and compares then
*     for equality. Only those values who are equal are returned.
*     Values in X1, Y1, X2 and Y2 whose corresponding identifiers are
*     matched are also returned.

*  Arguments:
*     ID1( NVAL1 ) = INTEGER (Given and Returned)
*        Identifiers for positions X1 and Y1.
*     X1( NVAL1 ) = DOUBLE PRECISION (Given and Returned)
*        X positions related to identifiers ID1.
*     Y1( NVAL1 ) = DOUBLE PRECISION (Given and Returned)
*        Y positions related to identifiers ID1.
*     NVAL1 = INTEGER (Given)
*        Number of entries in input lists ID1, X1 and Y1.
*     ID2( NVAL2 ) = INTEGER (Given and Returned)
*        Identifiers for positions X2 and Y2.
*     X2( NVAL2 ) = DOUBLE PRECISION (Given and Returned)
*        X positions related to identifiers ID2.
*     Y2( NVAL2 ) = DOUBLE PRECISION (Given and Returned)
*        Y positions related to identifiers ID2.
*     NVAL2 = INTEGER (Given)
*        Number of entries in input lists ID2, X2 and Y2.
*     NMATCH = INTEGER (Returned)
*        The number of matched positions. No error results from this
*        condition.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     10-JUL-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NVAL1
      INTEGER NVAL2

*  Arguments Given and Returned:
      INTEGER ID1( NVAL1 )
      DOUBLE PRECISION X1( NVAL1 )
      DOUBLE PRECISION Y1( NVAL1 )
      INTEGER ID2( NVAL2 )
      DOUBLE PRECISION X2( NVAL2 )
      DOUBLE PRECISION Y2( NVAL2 )

*  Arguments Returned:
      INTEGER NMATCH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NOW                ! Position at which storage is now
                                 ! occurring
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER ITMP               ! Temporary storage variable
      DOUBLE PRECISION DTMP      ! Temporary storage variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise number of matches.
      NMATCH = 0
      NOW = 1

*  Which list is larger?
      IF ( NVAL1 .GE. NVAL2 ) THEN

*  First list larger than second.
         DO 1 I = 1, NVAL2
            DO 2 J = NOW, NVAL1
               IF ( ID2( I ) .EQ. ID1( J ) ) THEN

*  Position matched, move this to position NOW and swap it.
                  ITMP = ID1( NOW )
                  ID1( NOW ) = ID1( J )
                  ID1( J ) = ITMP

*  Swap the comparison identifier to NOW also.
                  ITMP = ID2( NOW )
                  ID2( NOW ) = ID2( I )
                  ID2( I ) = ITMP

*  Swap the X and Y's
                  DTMP = X1( NOW )
                  X1( NOW ) = X1( J )
                  X1( J ) = DTMP
                  DTMP = Y1( NOW )
                  Y1( NOW ) = Y1( J )
                  Y1( J ) = DTMP

                  DTMP = X2( NOW )
                  X2( NOW ) = X2( I )
                  X2( I ) = DTMP
                  DTMP = Y2( NOW )
                  Y2( NOW ) = Y2( I )
                  Y2( I ) = DTMP

*  Increment number of matches.
                  NMATCH = NMATCH + 1
                  NOW = NOW + 1

*  Finished looking skip extra loops.
                  GO TO 1
               END IF
 2          CONTINUE
 1       CONTINUE
      ELSE

*  Second list larger than first.
         DO 3 I = 1, NVAL1
            DO 4 J = NOW, NVAL2
               IF ( ID1( I ) .EQ. ID2( J ) ) THEN

*  Position matched, move this to position NOW and swap it.
                  ITMP = ID2( NOW )
                  ID2( NOW ) = ID2( J )
                  ID2( J ) = ITMP

*  Swap the comparison identifier to NOW also.
                  ITMP = ID1( NOW )
                  ID1( NOW ) = ID1( I )
                  ID1( I ) = ITMP

*  Swap the X and Y's
                  DTMP = X2( NOW )
                  X2( NOW ) = X2( J )
                  X2( J ) = DTMP
                  DTMP = Y2( NOW )
                  Y2( NOW ) = Y2( J )
                  Y2( J ) = DTMP

                  DTMP = X1( NOW )
                  X1( NOW ) = X1( I )
                  X1( I ) = DTMP
                  DTMP = Y1( NOW )
                  Y1( NOW ) = Y1( I )
                  Y1( I ) = DTMP

*  Increment number of matches.
                  NMATCH = NMATCH + 1
                  NOW = NOW + 1

*  Finished looking skip extra loops.
                  GO TO 3
               END IF
 4          CONTINUE
 3       CONTINUE
      END IF

      END
* $Id$
