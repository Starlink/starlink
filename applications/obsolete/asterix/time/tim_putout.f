      SUBROUTINE TIM_PUTOUT( PARAM, OCLASS, NFREQ, POWER, BASE,
     :                       SCALE, OFID, STATUS )
*+
*  Name:
*     TIM_PUTOUT

*  Purpose:
*     Start production of an output file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TIM_PUTOUT( PARAM, OCLASS, NFREQ, POWER, BASE, SCALE, OFID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PARAM = CHARACTER*(*) (given)
*        Name of the environment parameter used to open file
*     OCLASS = CHARACTER*(*) (given)
*        Output class of interface object
*     NFREQ = INTEGER (given)
*        Number of points in output dataset
*     POWER[] = REAL (given)
*        Power values for output data
*     BASE = REAL (given)
*        Frequency of first bin
*     SCALE = REAL (given)
*        Change in  frequency per bin
*     OFID = INTEGER (returned)
*        ADI identifier of opened file
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

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  References:
*     TIM Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/tim.html

*  Keywords:
*     package:tim, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Dec 1995 (DJA):
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
      CHARACTER*(*)             PARAM, OCLASS
      INTEGER                   NFREQ
      REAL                      POWER(*), BASE, SCALE

*  Arguments Returned:
      INTEGER                   OFID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      REAL                      SPARR(2)                ! Spaced array data
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create the file
      CALL USI_CREAT( PARAM, ADI__NULLID, OFID, STATUS )

*  Create interface object
      CALL BDI_LINK( OCLASS, 1, NFREQ, 'REAL', OFID, STATUS )

*  Create the frequency axis
      SPARR(1) = BASE
      SPARR(2) = SCALE
      CALL BDI_AXPUT1R( OFID, 1, 'SpacedData', 2, SPARR, STATUS )

*  Write the power data
      CALL BDI_PUT1R( OFID, 'Data', NFREQ, POWER, STATUS )

      END
