      SUBROUTINE MATH_INTRP<T>( N, XIN, YIN, XOUT, DEG, YOUT, STATUS )
*+
*  Name:
*     MATH_INTRP<T>

*  Purpose:
*     Interpolate a <COMM> array YIN by X value

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MATH_INTRP<T>( N, XIN, YIN, XOUT, DEG, YOUT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     N = INTEGER (given)
*        Number of X,Y pairs
*     XIN[] = <TYPE> (given)
*        Ordinate values
*     YIN[] = <TYPE> (given)
*        Abscissa values
*     XOUT = <TYPE> (given)
*        X value to interpolation
*     DEG = INTEGER (given)
*        The preferred degree of interpolation
*     YOUT = <TYPE> (returned)
*        The interpolated value
*     STATUS = INTEGER (given and returned)
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
*     MATH Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/math.html

*  Keywords:
*     package:math, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Apr 1995 (DJA):
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
      INTEGER			N			! See above
      <TYPE>  			XIN(*)
      <TYPE>  			YIN(*)
      <TYPE>			XOUT
      INTEGER			DEG

*  Arguments Returned:
      <TYPE>			YOUT			! Interpolated value

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      INTEGER			NP			! # points to use
        PARAMETER		( NP = 5 )

*  Local Variables:
      REAL			TX(NP)			! X interp values
      REAL			TY(NP)			! X interp values
      REAL			XREQ, Y

      INTEGER			I,J 			! Loop variables
      INTEGER			IX			! Nearest pixel
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that supplied X value lies inside user range
      IF ( XOUT .LE. XIN(1) ) THEN
        YOUT = YIN(1)

      ELSE IF ( XOUT .GE. XIN(N) ) THEN
        YOUT = YIN(N)

      ELSE

*    Locate mid point by binary search
        CALL ARR_BSRCH<T>( N, XIN, XOUT, IX, STATUS )

*    Load interpolation arrays
        J = 0
        DO I = MAX(1,IX-2), MIN(N,IX+2)
          J = J + 1
          TX(J) = XIN(I)
          TY(J) = YIN(I)
        END DO

*    Perform interpolation
        XREQ = XOUT
        CALL MATH_INTERP( J, TX, TY, 1, XOUT, DEG, Y, STATUS )
        YOUT = Y

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'MATH_INTRP<T>', STATUS )

      END
