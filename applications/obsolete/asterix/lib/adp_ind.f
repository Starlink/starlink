      SUBROUTINE ADP_IND( DELI )
*+
*  Name:
*     ADP_IND

*  Purpose:
*     Adjust indentation of diagnostic output

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADP_IND( DELI )

*  Description:
*     {routine_description}

*  Arguments:
*     DELI = INTEGER (given)
*        The amount to add to the indentation level

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
*     ADP Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adp.html

*  Keywords:
*     package:adp, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     21 Dec 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'ADP_CMN'                                 ! ADP common block
*       ADP_INDENT = LOGICAL (given and returned)
*         ADP indentation level

*  Arguments Given:
      INTEGER			DELI

*  External References:
      EXTERNAL			ADP0_BLK		! Ensures inclusion
*.

*  Adjust indentation
      ADP_INDENT = ADP_INDENT + DELI

      END
