      SUBROUTINE ARR_CHKREG( X, N, REG, BASE, SCALE, STATUS )
*+
*  Name:
*     ARR_CHKREG

*  Purpose:
*     Checks whether array is regularly spaced

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_CHKREG( X, N, REG, BASE, SCALE, STATUS )

*  Description:
*     The N values in X are checked to see whether they can be represented
*     by a BASE and SCALE pair with no loss of precision.

*  Arguments:
*     X[N] = REAL (given)
*        The array of values to be tested
*     N = INTEGER (given)
*        The size of the X array
*     REG = LOGICAL (returned)
*        Can values be represented as base and scale?
*     BASE = REAL (returned)
*        The origin of the scaling, = X(1)
*     SCALE = REAL (returned)
*        The difference between adjacent elements
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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RJV: Robert Vallance (University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     18 Mar 1988 (RJV):
*        Original version.
*     28 Nov 1989 (DJA):
*        Now compares delta_x/spacing, rather than comparing delta_x absolutely
*      7 Feb 1992 (RJV):
*        Compares the least significant of 6 sig.figs. and allows
*        a tolerance of 1
*      4 Dec 1995 (DJA):
*        Compare each element with 1st element - less susceptible to
*        rounding errors
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			N
      REAL			X(*)

*  Arguments Returned:
      LOGICAL			REG
      REAL			BASE, SCALE

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      REAL			TOL
        PARAMETER 		( TOL = 1.0 )

*  Local Variables:
      REAL 			DX
      REAL 			FACT

      INTEGER 			I,J
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

        IF (N.GT.1) THEN
          SCALE=(X(N)-X(1))/REAL(N-1)
          IF ( SCALE .EQ. 0.0 ) THEN
             REG=.FALSE.
          ELSE
             BASE=X(1)
             FACT=10.0**(4-INT(LOG10(ABS(SCALE))))
             I=2
             REG=.TRUE.
          END IF
          DO WHILE (REG.AND.I.LT.N)
            J=I+1
	    DX = (X(J)-X(1))/REAL(J-1)
            IF ( DX .NE. 0.0 ) THEN
               REG = ( ABS(DX-SCALE)*FACT .LE. TOL )
            ELSE
               REG = .FALSE.
            END IF
            I=J
          ENDDO
          IF (.NOT.REG) THEN
            BASE=0.0
            SCALE=0.0
          ENDIF
        ELSE
          REG=.TRUE.
          BASE=X(1)
          SCALE=0.0
        ENDIF

      END
