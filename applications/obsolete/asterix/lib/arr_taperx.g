      SUBROUTINE ARR_TAPER<T>( NPTS, FRAC, A, STATUS )
*+
*  Name:
*     ARR_TAPER<T>

*  Purpose:
*     Apply cosine bell taper to a <COMM> array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_TAPER<T>( NPTS, FRAC, A, STATUS )

*  Description:
*     A cosine bell taper is applied to N*FRAC/2 points at each end of
*     the <COMM> array A.

*  Arguments:
*     NPTS = INTEGER (given)
*        The array to be tapered
*     FRAC = REAL (given)
*        The fraction of the length of the array over whic hthe taper is
*        applied
*     A[] = <TYPE> (given and returned)
*        The array to be tapered
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

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Jun 1990 (DJA):
*        Original version.
*     21 Apr 1995 (DJA):
*        Tidied up the prologue
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MATH_PAR'

*  Arguments Given:
      INTEGER			NPTS			! See above
      REAL			FRAC			!

*  Arguments Given and Returned:
      <TYPE>			A(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      DOUBLE PRECISION       	WEIGHT                  ! Tapering value

      INTEGER                	LAST
      INTEGER                	I                       ! Loop counter
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check fraction is sensible
      IF ( FRAC .GT. 1.0 ) FRAC = 1.0

*  The number of points to taper at each end of the array
      LAST = NINT( ( REAL(NPTS) * FRAC ) / 2.0 )

*  Apply the taper
      DO I = 1, LAST

*    Find taper factor in D.P.
        WEIGHT = 0.5D0 * (1D0 - COS(MATH__DPI*(DBLE(I)-0.5)/DBLE(LAST)))

        A(I) = A(I) * WEIGHT
        A(NPTS+1-I) = A(NPTS+1-I) * WEIGHT

      END DO

      END
