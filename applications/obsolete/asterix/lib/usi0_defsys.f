      SUBROUTINE USI0_DEFSYS( NAME, PARID, STATUS )
*+
*  Name:
*     USI0_DEFSYS

*  Purpose:
*     Define a new parameter system

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL USI0_DEFSYS( NAME, PARID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NAME = CHARACTER*(*) (given)
*        Name of new parameter system
*     PARID = INTEGER (returned)
*        Parameter system name code
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
*     USI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/usi.html

*  Keywords:
*     package:usi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     6 Mar 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'USI_CMN'                                 ! USI common block
*       USI_INIT = LOGICAL (given)
*         USI class definitions loaded?

*  Arguments Given:
      CHARACTER*(*)		NAME

*  Arguments Returned:
      INTEGER			PARID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			USI0_BLK		! Ensures inclusion
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      INTEGER			I			! Loop over routine slots
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Increment counter
      USI_NPS = USI_NPS + 1
      PARID = USI_NPS

*  Store the name
      PS_NAME(PARID) = NAME
      PS_NLEN(PARID) = CHR_LEN(NAME)

*  Zero the routine storage
      DO I = 1, USI__MXPRTN
        PS_RTN(I,PARID) = 0
      END DO

      END
