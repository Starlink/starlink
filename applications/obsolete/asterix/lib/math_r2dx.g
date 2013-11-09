      SUBROUTINE MATH_R2D<T>( X, Y, ROTA, RX, RY )
*+
*  Name:
*     MATH_R2D<T>

*  Purpose:
*     Rotate 2D <COMM> vector by an angle ROTA

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MATH_R2DX( X, Y, ROTA, RX, RY )

*  Description:
*     {routine_description}

*  Arguments:
*     X = <TYPE> (given)
*        X coordinate
*     Y = <TYPE> (given)
*        Y coordinate
*     ROTA = <TYPE> (given)
*        Rotation angle in radians
*     RX = <TYPE> (returned)
*        Rotated X coordinate
*     RY = <TYPE> (returned)
*        Rotated Y coordinate

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
*     MATH Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/math.html

*  Keywords:
*     package:math, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Jun 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      <TYPE>			X, Y, ROTA

*  Arguments Returned:
      <TYPE>			RX, RY

*  Local Variables:
      <TYPE>			CROTA, SROTA			! Cos,Sin ROTA
*.

*  Evaluate cos and sin of rotation
      CROTA = COS(ROTA)
      SROTA = SIN(ROTA)

*  Find rotated values
      RX = X * CROTA - Y * SROTA
      RY = X * SROTA + Y * CROTA

      END
