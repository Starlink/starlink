      SUBROUTINE SORT_IDX<T>( N, IN, ASCEND, INDEX, STATUS )
*+
*  Name:
*     SORT_IDX<T>

*  Purpose:
*     Create an index for a 1-dimensional <COMM> array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SORT_IDX<T>( N, IN, ASCEND, INDEX, STATUS )

*  Description:
*     Creates an integer index or rank list for the input array IN. The
*     index lists the indices of the elements of IN in ascending or
*     descending order depending on the value of ASCEND.

*  Arguments:
*     N = INTEGER (given)
*        The number of data values to rank
*     IN = <TYPE>[] (given)
*        The values to rank
*     ASCEND = LOGICAL (given)
*        Rank into ascending order?
*     INDEX = INTEGER[] (returned)
*        The ranking array
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     SORT Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/sort.html

*  Keywords:
*     package:sort, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 May 1993 (DJA):
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
      INTEGER               	N                  	! Number of names
      <TYPE>                    IN(*)              	! Input list data
      LOGICAL                	ASCEND             	! Ascending order?

*  Arguments Returned:
      INTEGER                	INDEX(*)           	! Output index

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER                	I, J, L, IR        	! Loop over list data
      INTEGER			SWAP			! Index swap point
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Too few points?
      IF ( N .LT. 2 ) RETURN

*  Initialise
      L = N/2 + 1
      IR = N

*  Loop forever
      DO WHILE ( .TRUE. )

        IF ( L .GT. 1 ) THEN
          L = L - 1
          SWAP = INDEX(L)
        ELSE
          SWAP = INDEX(IR)
          INDEX(IR) = INDEX(1)
          IR = IR - 1
          IF ( IR .EQ. 1 ) THEN
            INDEX(1) = SWAP
            GOTO 50
          END IF
        END IF

        I = L
        J = L + L

        IF ( ASCEND ) THEN
          DO WHILE ( J .LE. IR )
            IF ( (J.LT.IR) .AND.
     :         (IN(INDEX(J)).LT.IN(INDEX(J+1))) ) J = J + 1
            IF ( IN(SWAP) .LT. IN(INDEX(J)) ) THEN
              INDEX(I) = INDEX(J)
              I = J
              J = J * 2
            ELSE
              J = IR + 1
            END IF
          END DO
        ELSE
          DO WHILE ( J .LE. IR )
            IF ( (J.LT.IR) .AND.
     :         (IN(INDEX(J)).GT.IN(INDEX(J+1))) ) J = J + 1
            IF ( IN(SWAP) .GT. IN(INDEX(J)) ) THEN
              INDEX(I) = INDEX(J)
              I = J
              J = J * 2
            ELSE
              J = IR + 1
            END IF
          END DO
        END IF

        INDEX(I) = SWAP

      END DO
 50   CONTINUE

      END
