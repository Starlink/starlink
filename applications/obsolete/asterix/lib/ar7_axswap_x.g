      SUBROUTINE AR7_AXSWAP_<T>( DIMS, IN, SAX, ODIMS, OUT, STATUS )
*+
*  Name:
*     AR7_AXSWAP_<T>

*  Purpose:
*     Swap <COMM> array about specified axes

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL AR7_AXSWAP_<T>( DIMS, IN, SAX, ODIMS, OUT, STATUS )

*  Description:
*     Changes the order of the dimensions of a 7-D array.

*  Arguments:
*     DIMS[7] = INTEGER (given)
*        The dimensions of the 7-D input array
*     IN[] = <TYPE> (given)
*        The input array
*     SAX[7] = INTEGER (given)
*        The required new axis order
*     ODIMS[7] = INTEGER (given)
*        The output dimensions of the 7-D output array
*     OUT[] = <TYPE> (returned)
*        The output array
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     IN and OUT must not overlap in memory

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Timing:
*     {routine_timing}

*  References:
*     AR7 Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/ar7.html

*  Keywords:
*     package:ar7, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     13 Dec 1989 (DJA):
*        Original version.
*      6 Jun 1994 (DJA):
*        Code made generic
*     14 Dec 1995 (DJA):
*        Header tidied up
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			DIMS(7), SAX(7), ODIMS(7)
      <TYPE>                    IN(*)

*  Arguments Returned:
      <TYPE>                    OUT(*)

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call internal routine to expand out dimensions arrays
      CALL AR7_AXSWAP_<T>_INT( DIMS, DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                         DIMS(5), DIMS(6), DIMS(7), IN, SAX,
     :                         ODIMS(1), ODIMS(2), ODIMS(3), ODIMS(4),
     :                         ODIMS(5), ODIMS(6), ODIMS(7), OUT )

      END



      SUBROUTINE AR7_AXSWAP_<T>_INT( DIMS, L1, L2, L3, L4, L5, L6, L7,
     :                      IN, SAX, O1, O2, O3, O4, O5, O6, O7, OUT )
*+
*  Name:
*     AR7_AXSWAP_<T>_INT

*  Purpose:
*     Swap <COMM> array about specified axes

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL AR7_AXSWAP_<T>_INT( DIMS, L1, L2, L3, L4, L5, L6, L7, IN,
*                              SAX, O1, O2, O3, O4, O5, O6, O7, OUT )

*  Description:
*     Changes the order of the dimensions of a 7-D array.

*  Arguments:
*     DIMS[7] = INTEGER (given)
*        The dimensions of the 7-D input array
*     L1..L7 = INTEGER (given)
*        The individual elements of DIMS
*     IN[L1,..,L7 ] = <TYPE> (given)
*        The input array
*     SAX[7] = INTEGER (given)
*        The required new axis order
*     O1..O7 = INTEGER (given)
*        The individual elements of ODIMS
*     OUT[O1,..,O7] = <TYPE> (returned)
*        The output array

*  Keywords:
*     package:ar7, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     13 Dec 1989 (DJA):
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
      INTEGER			DIMS(7), SAX(7)
      INTEGER              	L1,L2,L3,L4,L5,L6,L7
      <TYPE>                    IN(L1,L2,L3,L4,L5,L6,L7)
      INTEGER              	O1,O2,O3,O4,O5,O6,O7

*  Arguments Returned:
      <TYPE>                    OUT(O1,O2,O3,O4,O5,O6,O7)

*  Local Variables:
      INTEGER              	II(7)
      INTEGER              	SI(7)
      INTEGER              	A,B,C,D,E,F,G
*.

*  Store position of old axis in new object
      DO A = 1, 7
        DO B = 1, 7
         IF ( SAX(B) .EQ. A ) SI(A) = B
        END DO
      END DO

*  Perform data transfer
      II(SI(7)) = 1
      DO G = 1, L7
       II(SI(6)) = 1
       DO F = 1, L6
        II(SI(5)) = 1
        DO E = 1, L5
         II(SI(4)) = 1
         DO D = 1, L4
          II(SI(3)) = 1
          DO C = 1, L3
           II(SI(2)) = 1
           DO B = 1, L2
            II(SI(1)) = 1
            DO A = 1, L1
             OUT(II(1),II(2),II(3),II(4),II(5),II(6),II(7))
     :                                    = IN(A,B,C,D,E,F,G)
             II(SI(1)) = II(SI(1)) + 1
            END DO
            II(SI(2)) = II(SI(2)) + 1
           END DO
           II(SI(3)) = II(SI(3)) + 1
          END DO
          II(SI(4)) = II(SI(4)) + 1
         END DO
         II(SI(5)) = II(SI(5)) + 1
        END DO
        II(SI(6)) = II(SI(6)) + 1
       END DO
       II(SI(7)) = II(SI(7)) + 1
      END DO

      END
