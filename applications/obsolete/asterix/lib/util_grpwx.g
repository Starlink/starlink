      SUBROUTINE UTIL_GRPW<T>( N, DATA, WOK, WGHT, QOK, QUAL, GIDX,
     :                        NGRP, GDATA, GWGHT, GQUAL, STATUS )
*+
*  Name:
*     UTIL_GRPW<T>

*  Purpose:
*     Group a data array (and optional weights/quality) using group index

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL UTIL_GRPW<T>( N, DATA, WOK, WGHT, QOK, QUAL, GIDX, NGRP, GDATA,
*                        GWGHT, GQUAL, STATUS ) )

*  Description:
*     Data is grouped according to a grouping index array. If quality is
*     specified then if any input pixel is good, then the output grouped
*     pixel is good.

*  Arguments:
*     N = INTEGER (given)
*        Number of points in arrays to be grouped
*     DATA[N] = <TYPE> (given)
*        The input primary data
*     WOK = LOGICAL (given)
*        Weights present?
*     WGHT[N] = <TYPE> (given)
*        The input weights
*     QOK = LOGICAL (given)
*        Quality present?
*     QUAL[N] = LOGICAL (given)
*        The input quality
*     GIDX[N] = INTEGER (given)
*        The grouping index. The I'th value holds the number of the output
*        group bin
*     NGRP = INTEGER (given)
*        The number of output group bins
*     GDATA[N] = <TYPE> (returned)
*        The grouped primary data
*     GWGHT[N] = <TYPE> (returned)
*        The grouped weights
*     GQUAL[N] = LOGICAL (returned)
*        The grouped quality
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
*     UTIL Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/util.html

*  Keywords:
*     package:util, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Mar 1996 (DJA):
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
      INTEGER			N, NGRP, GIDX(*)
      <TYPE>			DATA(*), WGHT(*)
      LOGICAL			QOK, WOK, QUAL(*)

*  Arguments Returned:
      <TYPE>			GDATA(*), GWGHT(*)
      LOGICAL			GQUAL(*)

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			I,J			! Loop over input/output
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise output data array
      DO J = 1, NGRP
        GDATA(J) = 0
      END DO

*  Initialise weights
      IF ( WOK ) THEN
        DO J = 1, NGRP
          GWGHT(J) = 0
        END DO
      END IF

*  Switch on presence of quality
      IF ( QOK ) THEN

*    Initialise
        DO J = 1, NGRP
          GQUAL(J) = .FALSE.
        END DO

*    Loop over input data
*    Weights and quality
        IF ( WOK ) THEN
          DO I = 1, N
            IF ( QUAL(I) .AND. (WGHT(I) .NE. 0.0) ) THEN
              J = GIDX(I)
              GDATA(J) = GDATA(J) + DATA(I)
              GWGHT(J) = GWGHT(J) + 1.0/WGHT(I)
              GQUAL(J) = .TRUE.
            END IF
          END DO

*    No weights, quality
        ELSE
          DO I = 1, N
            IF ( QUAL(I) ) THEN
              J = GIDX(I)
              GDATA(J) = GDATA(J) + DATA(I)
              GQUAL(J) = .TRUE.
            END IF
          END DO
        END IF

      ELSE

*    Loop over input data
        IF ( WOK ) THEN
          DO I = 1, N
            IF ( WGHT(I) .NE. 0.0 ) THEN
              J = GIDX(I)
              GDATA(J) = GDATA(J) + DATA(I)
              GWGHT(J) = GWGHT(J) + 1.0/WGHT(I)
            END IF
          END DO
        ELSE
          DO I = 1, N
            J = GIDX(I)
            GDATA(J) = GDATA(J) + DATA(I)
          END DO
        END IF

      END IF

*  Finalise weights
      IF ( WOK ) THEN
        DO J = 1, NGRP
          IF ( GWGHT(J) .NE. 0.0 ) GWGHT(J) = 1.0 / GWGHT(J)
        END DO
      END IF

      END
