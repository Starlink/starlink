      SUBROUTINE SPD_FDACR( FILL, DIM1, DIM2, SHIFT0, SHIFT1,
     :   ARRAY, STATUS )
*+
*  Name:
*     SPD_FDAC{CDR}

*  Purpose:
*     Shift along first axis of an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FDACR( FILL, DIM1, DIM2, SHIFT0, SHIFT1, ARRAY,
*        STATUS )

*  Description:
*     This routine shifts information within an array. The array must be
*     formally two-dimensional and the shift is along the first axis.
*     The second dimension can be 1 of course.

*  Arguments:
*     FILL = REAL (Given)
*        The value to be used to pad the gap caused by the shift.
*     DIM1 = INTEGER (Given)
*        The first dimension of ARRAY.
*     DIM2 = INTEGER (Given)
*        The second dimension of ARRAY.
*     SHIFT0 = INTEGER (Given)
*        The columns 1...SHIFT0 will be unaffected by the shift
*        performed here.
*     SHIFT1 = INTEGER (Given)
*        The amount of rightward shift to be applied. If this is
*        negative, the shift will in fact be leftward.
*     ARRAY( DIM1, DIM2 ) = REAL (Given and Returned)
*        The array to which the shift is to be applied.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     The shift is governed by two parameters, the shift origin SHIFT0
*     and the shift amount SHIFT1. The action differs on whether the
*     shift amount is positive (shift right) or negative (shift left).
*
*     Rightward shift (SHIFT1.GT.0):
*     before: |xxxxxx||xxxxxxxx****|***|
*             |      | \            \
*             |      |  \            \
*     after:  |xxxxxx|***|xxxxxxxx****||
*             |      |   |         |  |
*             1......|...|.........|..DIM1
*                    |   |         DIM1-SHIFT1
*                    |   SHIFT0+1+SHIFT1
*                    SHIFT0
*
*     Leftward shift (SHIFT1.LT.0):
*     before: |xxxxxx|***|xxxxxxxx****||
*             |      |  /            /
*             |      | /            /
*     after   |xxxxxx||xxxxxxxx****|***|
*             |      |   |         |  |
*             1......|...|.........|..DIM1
*                    |   |         DIM1+SHIFT1
*                    |   SHIFT0+1-SHIFT1
*                    SHIFT0

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     28 Feb 1992 (hme):
*        Original version (SPABE).
*     03 Mar 1994 (hme):
*        Simplified to deal with only one array.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL FILL
      INTEGER DIM1
      INTEGER DIM2
      INTEGER SHIFT0
      INTEGER SHIFT1

*  Arguments Given and Returned:
      REAL ARRAY( DIM1, DIM2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J               ! Loop indices

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If rightward shift.
      IF ( SHIFT1 .GT. 0 ) THEN

*     Shift.
         DO 2 J = 1, DIM2
            DO 1 I = DIM1, SHIFT0+1+SHIFT1, -1
               ARRAY(I,J) = ARRAY(I-SHIFT1,J)
 1          CONTINUE
 2       CONTINUE

*     Pad the gap.
         DO 4 J = 1, DIM2
            DO 3 I = SHIFT0+SHIFT1, SHIFT0+1, -1
               ARRAY(I,J) = FILL
 3          CONTINUE
 4       CONTINUE

*  If leftward shift.
      ELSE IF ( SHIFT1 .LT. 0 ) THEN
         DO 6 J = 1, DIM2
            DO 5 I = SHIFT0+1, DIM1+SHIFT1, +1
               ARRAY(I,J) = ARRAY(I-SHIFT1,J)
 5          CONTINUE
 6       CONTINUE

*     Pad the gap.
         DO 8 J = 1, DIM2
            DO 7 I = DIM1+1+SHIFT1, DIM1, +1
               ARRAY(I,J) = FILL
 7          CONTINUE
 8       CONTINUE
      END IF

*  Return.
      END
