      SUBROUTINE DYN_INIT()
*+
*  Name:
*     DYN_INIT

*  Purpose:
*     Explicitly initialise the DYN package

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DYN_INIT( )

*  Description:
*     Explicitly initialise the DYN package

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
*     DYN Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/dyn.html

*  Keywords:
*     package:dyn, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Mar 1995 (DJA):
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
      INCLUDE 'DYN_CMN'                                 ! DYN common block
*       DYN_INIT = LOGICAL (given)
*         DYN class definitions loaded?

*  External References:
      EXTERNAL			DYN0_BLK		! Ensures inclusion
*.

*  Check initialised
      IF ( .NOT. DYN_ISINIT ) CALL DYN0_INIT()

      END
