      SUBROUTINE TCI_MJD2TAI( MJD, TAI )
*+
*  Name:
*     TCI_MJD2TAI

*  Purpose:
*     Convert Modified Julian Day (MJD) to International Atomic Time (TAI)

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TCI_MJD2TAI( MJD, TAI )

*  Description:
*     Converts a decimal MJD to international atomic time (TAI). Takes leap
*     seconds into account.

*  Arguments:
*     MJD = DOUBLE PRECISION (given)
*        The Modified Julian Day
*     TAI = DOUBLE PRECISION (returned)
*        The International Atomic Time in days

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
*     TCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/tci.html

*  Keywords:
*     package:tci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     24 Jul 1991 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      DOUBLE PRECISION		MJD

*  Arguments Returned:
      DOUBLE PRECISION		TAI

*  External References:
      EXTERNAL			SLA_DAT
        DOUBLE PRECISION        SLA_DAT                 ! Leap seconds routine

*  Local Constants:
      DOUBLE PRECISION       	LEAPS_AT_1970
        PARAMETER            	( LEAPS_AT_1970 = 10.0D0 )
      DOUBLE PRECISION       	MJD_AT_1970
        PARAMETER            	( MJD_AT_1970 = 41317.0D0 )
      DOUBLE PRECISION       	SECONDS_IN_DAY
        PARAMETER            	( SECONDS_IN_DAY = 86400.0D0 )
*.

*  Find atomic time
      TAI = MJD +
     :     ((SLA_DAT(MJD)-LEAPS_AT_1970)/SECONDS_IN_DAY) -
     :     MJD_AT_1970

      END
