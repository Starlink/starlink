      SUBROUTINE UTIL_GRPV<T>( N, DATA, VOK, VAR, QOK, QUAL, GIDX,
     :                         NGRP, GDATA, GVAR, GQUAL, STATUS )
*+
*  Name:
*     UTIL_GRPVL<T>

*  Purpose:
*     Group a data array (and optional variance/quality) using group index

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL UTIL_GRPV<T>( N, DATA, VOK, VAR, QOK, QUAL, GIDX, NGRP, GDATA,
*                       GVAR, GQUAL, STATUS ) )

*  Description:
*     Data is grouped according to a grouping index array. If quality is
*     specified then if any input pixel is good, then the output grouped
*     pixel is good.

*  Arguments:
*     N = INTEGER (given)
*        Number of points in arrays to be grouped
*     DATA[N] = <TYPE> (given)
*        The input primary data
*     VOK = LOGICAL (given)
*        Variances present?
*     VAR[N] = <TYPE> (given)
*        The input variances
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
*     GVAR[N] = <TYPE> (returned)
*        The grouped variances
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
*     FIT Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fit.html

*  Keywords:
*     package:fit, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

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
      <TYPE>			DATA(*), VAR(*)
      LOGICAL			QOK, VOK, QUAL(*)

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

*  Initialise variances
      IF ( VOK ) THEN
        DO J = 1, NGRP
          GVAR(J) = 0
        END DO
      END IF

*  Switch on presence of quality
      IF ( QOK ) THEN

*    Initialise
        DO J = 1, NGRP
          GQUAL(J) = .FALSE.
        END DO

*    Loop over input data
*    Variance and quality
        IF ( WOK ) THEN
          DO I = 1, N
            IF ( QUAL(I) ) THEN
              J = GIDX(I)
              GDATA(J) = GDATA(J) + DATA(I)
              GVAR(J) = GVAR(J) + VAR(I)
              GQUAL(J) = .TRUE.
            END IF
          END DO

*    No variance, quality
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
        IF ( VOK ) THEN
          DO I = 1, N
            J = GIDX(I)
            GDATA(J) = GDATA(J) + DATA(I)
            GVAR(J) = GVAR(J) + VAR(I)
          END DO
        ELSE
          DO I = 1, N
            J = GIDX(I)
            GDATA(J) = GDATA(J) + DATA(I)
          END DO
        END IF

      END IF

      END
