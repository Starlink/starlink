      SUBROUTINE UDI0_FOPEN( FILEN, CLASS, MODE, ID, STATUS )
*+
*  Name:
*     UDI0_FOPEN

*  Purpose:
*     Wrap up ADI_FOPEN to support command line argument expansion

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL UDI0_FOPEN( FILEN, CLASS, MODE, ID, STATUS )

*  Description:
*     Calls ADI_FOPEN to open the named file. If the input argument is
*     of the form $<n> where <n> is a number then the command line
*     argument of that number is used as the filename.

*  Arguments:
*     FILEN = CHARACTER*(*) (given)
*        The file to open (see above)
*     CLASS = CHARACTER*(*) (given)
*        The class to associate the file
*     MODE = CHARACTER*(*) (given)
*        The file access mode, should be READ or UPDATE
*     ID = INTEGER (returned)
*        The ADI identifier of the opened file object chain
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
*     UDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/udi.html

*  Keywords:
*     package:udi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     7 Sep 1995 (DJA):
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
      CHARACTER*(*)		FILEN, CLASS, MODE

*  Arguments Returned:
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External references:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*200		LFILE			! Local file name

      INTEGER			IARG			! Argument number
      INTEGER			LLEN			! Length of LFILE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start of a command line number?
      IF ( FILEN(1:1) .EQ. '$' ) THEN

*    Translate number
        CALL CHR_CTOI( FILEN(2:), IARG, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Get command argument
          CALL GETARG( IARG, LFILE )
          IF ( LFILE .GT. ' ' ) THEN
            LLEN = CHR_LEN(LFILE)
            CALL ADI_FOPEN( LFILE(:LLEN), CLASS, MODE, ID, STATUS )

          ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', IARG )
            CALL ERR_REP( ' ', 'Blank command line argument'/
     :                    /' number ^N', STATUS )
          END IF

        ELSE
          CALL MSG_SETC( 'N', FILEN(2:) )
          CALL ERR_REP( ' ', 'Badly formed command line argument '/
     :                  /' number /^N/', STATUS )
        END IF

*  Otherwise simply open file
      ELSE
        CALL ADI_FOPEN( FILEN, CLASS, MODE, ID, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'UDI0_FOPEN', STATUS )

      END
