      SUBROUTINE ADP_OUT( TXT )
*+
*  Name:
*     ADP_OUT

*  Purpose:
*     Write diagnostic string to console

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADP_OUT( TXT )

*  Description:
*     {routine_description}

*  Arguments:
*     TXT = CHARACTER*(*) (given)
*        The diagnostic string. May contain ADAM style message tokens

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
*       ADP_INDENT = LOGICAL (given)
*         ADP indentation level

*  Arguments Given:
      CHARACTER*(*)		TXT

*  External References:
      EXTERNAL			ADP0_BLK		! Ensures inclusion

*  Local Variables:
      CHARACTER*132		LTXT			! Expanded text

      INTEGER			TLEN			! Length of LTXT used
*.

*  Indentation blanks
      IF ( ADP_INDENT .GT. 0 ) THEN
        CALL CHR_FILL( ' ', LTXT(:ADP_INDENT) )
      END IF

*  Expand the text
      CALL MSG_MAKE( TXT, LTXT(1+ADP_INDENT:), TLEN )

*  Write out string prepending blanks
      CALL MSG_PRNT( LTXT(:TLEN) )

*  Flush output stream
      CALL FLUSH(6)

      END
