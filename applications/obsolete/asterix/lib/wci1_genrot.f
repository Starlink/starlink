      SUBROUTINE WCI1_GENROT( SLONG, SLAT, PLONG, MAT, STATUS )
*+
*  Name:
*     WCI1_GENROT

*  Purpose:
*     Generates rotation matrix given poles

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WCI1_GENROT( SLONG, SLAT, PLONG, MAT, STATUS )

*  Description:
*     Generate rotation matrix from native sphericals to standard
*     sphericals where (SLONG,SLAT) position of the 'special point'
*     in the standard system, and PLONG is the longitude of the
*     standard system's north pole in the native system.

*  Arguments:
*     SLONG = DOUBLE (given)
*        Longitude of special point in standard system (degrees)
*     SLAT = DOUBLE (given)
*        Latitude of special point in standard system (degrees)
*     PLONG = DOUBLE (given)
*        Longitude of standard north pole in native system (degrees)
*     MAT[3,3] = DOUBLE (returned)
*        The rotation matrix
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
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     5 Jan 1995 (DJA):
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
      DOUBLE PRECISION		SLONG			! Special longitude
      DOUBLE PRECISION		SLAT   			! Special latitude
      DOUBLE PRECISION		PLONG			! Longitude of NSP

*  Arguments Returned:
      DOUBLE PRECISION		MAT(3,3)		! Rotation matrix

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local variables:
      DOUBLE PRECISION		SIN_AL, COS_AL		! sin/cos SLONG
      DOUBLE PRECISION		SIN_DE, COS_DE		! sin/cos SLAT
      DOUBLE PRECISION		SIN_LP, COS_LP		! sin/cos PLONG
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Pre-compute trig functions
      SIN_AL = SIND(SLONG)
      COS_AL = COSD(SLONG)
      SIN_DE = SIND(SLAT)
      COS_DE = COSD(SLAT)
      SIN_LP = SIND(PLONG)
      COS_LP = COSD(PLONG)

*  Generate the matrix
      MAT(1,1) = -SIN_AL*SIN_LP - COS_AL*COS_LP*SIN_DE
      MAT(1,2) = COS_AL*SIN_LP - SIN_AL*COS_LP*SIN_DE
      MAT(1,3) = COS_LP*COS_DE
      MAT(2,1) = SIN_AL*COS_LP - COS_AL*SIN_LP*SIN_DE
      MAT(2,2) = - COS_AL*COS_LP - SIN_AL*SIN_LP*SIN_DE
      MAT(2,3) = SIN_LP*COS_DE
      MAT(3,1) = COS_AL*COS_DE
      MAT(3,2) = SIN_AL*COS_DE
      MAT(3,3) = SIN_DE

      END
