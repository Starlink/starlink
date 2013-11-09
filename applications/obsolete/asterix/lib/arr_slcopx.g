      SUBROUTINE ARR_SLCOP<T>( NDIM, DIMS, IN, LBND, UBND, OUT, STATUS )
*+
*  Name:
*     ARR_SLCOP<T>

*  Purpose:
*     Extract a slice of <COMM> elements from an array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_SLCOP<T>( NDIM, DIMS, IN, LBND, UBND, OUT, STATUS )

*  Description:
*     Extracts a slice from an array of <COMM>s of dimensionality less
*     than or equal 7.

*  Arguments:
*     NDIM = INTEGER (given)
*        Dimensionality of input array
*     DIMS[NDIM] = INTEGER (given)
*        Dimensions of input array
*     IN[] = <TYPE> (given)
*        The input data
*     LBND[NDIM] = INTEGER (given)
*        Lower indices of slice
*     UBND[NDIM] = INTEGER (given)
*        Upper indices of slice
*     OUT[] = <TYPE> (returned)
*        The output slice
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
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Sep 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			NDIM, DIMS(*), LBND(*), UBND(*)
      <TYPE>			IN(*)

*  Arguments Returned:
      <TYPE>			OUT(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over dimensions
      INTEGER			IDIMS(ADI__MXDIM)	! I/p dimensions
      INTEGER			ILBND(ADI__MXDIM)	! Slice lower bound
      INTEGER			ODIMS(ADI__MXDIM)	! O/p dimensions
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct output dimensions and make local copy of input dimensions
      DO I = 1, NDIM
        IDIMS(I) = DIMS(I)
        ILBND(I) = LBND(I)
        ODIMS(I) = UBND(I) - LBND(I) + 1
      END DO

*  Pad both input and output dimensions
      CALL AR7_PAD( NDIM, IDIMS, STATUS )
      CALL AR7_PAD( NDIM, ILBND, STATUS )
      CALL AR7_PAD( NDIM, ODIMS, STATUS )

*  Copy the data
      CALL ARR_SLCOP<T>_INT( IDIMS(1), IDIMS(2), IDIMS(3), IDIMS(4),
     :                       IDIMS(5), IDIMS(6), IDIMS(7), IN,
     :                       ILBND(1), ILBND(2), ILBND(3), ILBND(4),
     :                       ILBND(5), ILBND(6), ILBND(7),
     :                       ODIMS(1), ODIMS(2), ODIMS(3), ODIMS(4),
     :                       ODIMS(5), ODIMS(6), ODIMS(7),
     :                       OUT, STATUS )

      END



      SUBROUTINE ARR_SLCOP<T>_INT( I1, I2, I3, I4, I5, I6, I7, IN,
     :                             S1, S2, S3, S4, S5, S6, S7,
     :                             O1, O2, O3, O4, O5, O6, O7, OUT,
     :                             STATUS )
*+
*  Name:
*     ARR_SLCOP<T>

*  Purpose:
*     Extract a slice of <COMM> elements from an array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_SLCOP<T>_INT( NDIM, DIMS, IN, LBND, UBND, OUT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NDIM = INTEGER (given)
*        Dimensionality of input array
*     I1..I7 = INTEGER (given)
*        Dimensions of input array
*     IN[I1,I2,I3,I4,I5,I6,I7] = <TYPE> (given)
*        The input data
*     S1..S7 = INTEGER (given)
*        Start pixel in input for 1st output pixel
*     O1..O7 = INTEGER (given)
*        Dimensions of output array
*     OUT[O1,O2,O3,O4,O5,O6,O7] = <TYPE> (returned)
*        The output slice
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
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Sep 1995 (DJA):
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
      INTEGER			I1, I2, I3, I4, I5, I6, I7
      INTEGER			S1, S2, S3, S4, S5, S6, S7
      INTEGER			O1, O2, O3, O4, O5, O6, O7
      <TYPE>			IN(I1, I2, I3, I4, I5, I6, I7)

*  Arguments Returned:
      <TYPE>			OUT(O1, O2, O3, O4, O5, O6, O7)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I,J,K,L,M,N,O		! Loop over output
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over output
      DO O = 1, O7
        DO N = 1, O6
          DO M = 1, O5
            DO L = 1, O4
              DO K = 1, O3
                DO J = 1, O2
                  DO I = 1, O1
                    OUT(I,J,K,L,M,N,O) = IN(S1+I-1, S2+J-1, S3+K-1,
     :                                      S4+L-1, S5+M-1, S6+N-1,
     :                                      S7+O-1 )
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END
