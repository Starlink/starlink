      SUBROUTINE EDI_QFND( ID, QUANT, NAME, NUM, STATUS )
*+
*  Name:
*     EDI_QFND

*  Purpose:
*     Identify a list by a quantity code

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_QFND( ID, QUANT, NAME, NUM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
*     QUANT = CHARACTER*(*) (given)
*        Names of quantities for which defaults are to be set
*     NAME = CHARACTER*(*) (returned)
*        Name of list found
*     NUM = INTEGER (returned)
*        The list number for that quantity. Zero if not known
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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     21 Aug 1995 (DJA):
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
      INTEGER			ID
      CHARACTER*(*)		QUANT

*  Arguments Returned:
      INTEGER			NUM
      CHARACTER*(*)		NAME

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_SIMLR
        LOGICAL			CHR_SIMLR

*  Local Constants:
      CHARACTER*17		TIME_L
        PARAMETER		( TIME_L = 'TIME/RAW_TIMETAG/' )
      CHARACTER*31		XPOS_L
        PARAMETER		( XPOS_L =
     :             'X_CORR/X_RAW/X/LOCX/RAWX/RA/' )
      CHARACTER*31		YPOS_L
        PARAMETER		( YPOS_L =
     :             'Y_CORR/Y_RAW/Y/LOCY/RAWY/DEC/' )
      CHARACTER*50		ENGY_L
        PARAMETER		( ENGY_L =
     :             'PHA/PI/CORR_PHA/CORR_PH_CH/PULSE_HEIGHT_CH/' )

*  Local Variables:
      CHARACTER*40		TLIST			! Try list

      INTEGER			IC, EC			! Character pointers
      INTEGER			LID			! List identifier

      LOGICAL			FOUND			! Found a list?
      LOGICAL			OK			! List is ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      NUM = 0
      NAME = ' '

*  Time quantity?
      IF ( CHR_SIMLR( QUANT, 'T' ) ) THEN
        TLIST = TIME_L

*  X position
      ELSE IF ( CHR_SIMLR( QUANT, 'X' ) ) THEN
        TLIST = XPOS_L

*  Y position
      ELSE IF ( CHR_SIMLR( QUANT, 'Y' ) ) THEN
        TLIST = YPOS_L

*  Energy?
      ELSE IF ( CHR_SIMLR( QUANT, 'E' ) ) THEN
        TLIST = ENGY_L

*  Phase?
      ELSE IF ( CHR_SIMLR( QUANT, 'P' ) ) THEN
        TLIST = 'PHASE'

*  Otherwise unknown
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'Q', QUANT )
        CALL ERR_REP( 'EDI_QFND', 'Unrecognised quantity code /^Q/',
     :                STATUS )
        GOTO 99

      END IF

*  Look through try list, looking for a match
      IC = 1
      FOUND = .FALSE.
      DO WHILE ( (IC.GT.0) .AND. .NOT. FOUND )
        EC = INDEX( TLIST(IC:), '/' )
        IF ( EC .EQ. 0 ) THEN
          IC = 0
        ELSE

*      Named list exists?
          CALL EDI_CHK( ID, TLIST(IC:IC+EC-2), OK, STATUS )
          IF ( OK ) THEN
            FOUND = .TRUE.
            NAME = TLIST(IC:IC+EC-2)
            CALL EDI_IDXNAM( ID, TLIST(IC:IC+EC-2), LID, STATUS )
            CALL ADI_CGET0I( LID, '.Number', NUM, STATUS )
            CALL ADI_ERASE( LID, STATUS )

          ELSE
            IC = IC + EC

          END IF

        END IF

      END DO

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_QFND', STATUS )

      END
