      SUBROUTINE TCI_CTIME( TSTR, STATUS )
*+
*  Name:
*     TCI_CTIME

*  Purpose:
*     Get system time in TCI format

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TCI_CTIME( TSTR, STATUS )

*  Description:
*     Gets system time in TCI format, namely "dd-mmm-yy hh:mm:ss". This
*     string is 18 characters long and this is the minimum sensible
*     declared length for TSTR.

*  Arguments:
*     TSTR = CHARACTER*(*) (returned)
*        The formatted time string
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
*     TCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/tci.html

*  Keywords:
*     package:tci, usage:public, time

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Mar 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Returned:
      CHARACTER*(*)		TSTR			! Formatted time

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			PSX_TIME
      EXTERNAL			PSX_CTIME

*  Local Variables:
      CHARACTER*24           	PT              	! PSX time string

      INTEGER			FSTAT			! i/o status
      INTEGER    		NTICKS          	! System time
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get time
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_CTIME( NTICKS, PT, STATUS )

*    Format sensibly
 10   FORMAT( A2, '-', A3, '-', A2, ' ', A8 )
      WRITE( TSTR, 10, IOSTAT=FSTAT ) PT(9:10), PT(5:7),
     :                              PT(23:24), PT(12:19)
      IF ( FSTAT .NE. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Error formatting system time', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'TCI_CTIME', STATUS )

      END
