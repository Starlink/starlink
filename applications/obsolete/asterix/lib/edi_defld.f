      SUBROUTINE EDI_DEFLD( ID, PAR, QUANTS, HOW, STATUS )
*+
*  Name:
*     EDI_DEFLD

*  Purpose:
*     Provides defaults to a parameter by locating lists of a certain type

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_DEFLD( ID, PAR, QUANTS, HOW, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
*     PAR = CHARACTER*(*) (given)
*        Name of environment variable whose default is to be set
*     QUANTS = CHARACTER*(*) (given)
*        Names of quantities for which defaults are to be set
*     HOW = CHARACTER*(*) (given)
*        How default is to be set. Can take values NAME or NUMBER. Case is
*        not significant
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
      CHARACTER*(*)		PAR,QUANTS,HOW

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
      EXTERNAL			CHR_SIMLR
        LOGICAL			CHR_SIMLR

*  Local Constants:
      CHARACTER*17		TIME_L
        PARAMETER		( TIME_L = 'TIME/RAW_TIMETAG/' )
      CHARACTER*21		XPOS_L
        PARAMETER		( XPOS_L = 'X_CORR/X_RAW/X/RA/' )
      CHARACTER*21		YPOS_L
        PARAMETER		( YPOS_L = 'Y_CORR/Y_RAW/Y/DEC/' )
      CHARACTER*16		ENGY_L
        PARAMETER		( ENGY_L = 'PHA/PI/CORR_PHA/' )
      INTEGER			MXDEF
        PARAMETER		( MXDEF = 5 )

*  Local Variables:
      CHARACTER*80		LNAMS			! Default names
      CHARACTER*40		TLIST			! Try list

      INTEGER			IC, EC			! Character pointers
      INTEGER			IQ			! Loop over QUANTS
      INTEGER			LID			! List identifier
      INTEGER			LNLEN			! LNAMS used length
      INTEGER			LNUMS(MXDEF)		! List numbers
      INTEGER			NDEF			! Number of defaults

      LOGICAL			BYNAME			! Default by name?
      LOGICAL			FOUND			! Found a list?
      LOGICAL			OK			! List is ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do it by name?
      BYNAME = CHR_SIMLR( HOW, 'NAME' )

*  For each element of QUANTS
      DO IQ = 1, CHR_LEN(QUANTS)

*    Time quantity?
        IF ( CHR_SIMLR( QUANTS(IQ:IQ), 'T' ) ) THEN
          TLIST = TIME_L

*    X position
        ELSE IF ( CHR_SIMLR( QUANTS(IQ:IQ), 'X' ) ) THEN
          TLIST = XPOS_L

*    Y position
        ELSE IF ( CHR_SIMLR( QUANTS(IQ:IQ), 'Y' ) ) THEN
          TLIST = YPOS_L

*    Energy?
        ELSE IF ( CHR_SIMLR( QUANTS(IQ:IQ), 'E' ) ) THEN
          TLIST = ENGY_L

*    Phase?
        ELSE IF ( CHR_SIMLR( QUANTS(IQ:IQ), 'P' ) ) THEN
          TLIST = 'PHASE'

*    Otherwise unknown
        ELSE
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'Q', QUANTS(IQ:IQ) )
          CALL ERR_REP( 'EDI_DEFLD', 'Unrecognised quantity code /^Q/',
     :                  STATUS )
          GOTO 99

        END IF

*    Look through try list, looking for a match
        IC = 1
        FOUND = .FALSE.
        DO WHILE ( (IC.GT.0) .AND. .NOT. FOUND )
          EC = INDEX( TLIST(IC:), '/' )
          IF ( EC .EQ. 0 ) THEN
            IC = 0
          ELSE

*        Named list exists?
            CALL EDI_CHK( ID, TLIST(IC:IC+EC-2), OK, STATUS )
            IF ( OK ) THEN
              FOUND = .TRUE.
              NDEF = NDEF + 1

*          Default by name?
              IF ( BYNAME ) THEN
                IF ( NDEF .EQ. 1 ) THEN
                  LNAMS = TLIST(IC:IC+EC-2)
                  LNLEN = EC - 1
                ELSE
                  LNAMS = LNAMS(:LNLEN) // TLIST(IC:IC+EC-2)
                  LNLEN = LNLEN + EC - 1
                END IF

*          By number
              ELSE
                CALL EDI_IDXNAM( ID, TLIST(IC:IC+EC-2), LID, STATUS )
                CALL ADI_CGET0I( LID, '.Number', LNUMS(NDEF), STATUS )
                CALL ADI_ERASE( LID, STATUS )
              END IF

            ELSE
              IC = IC + EC

            END IF

          END IF

        END DO

*    If no default found then abort
        IF ( .NOT. FOUND ) GOTO 99

      END DO

*  Set the default
      IF ( BYNAME ) THEN
        CALL USI_DEF0C( PAR, LNAMS(:LNLEN), STATUS )
      ELSE
        CALL USI_DEF1I( PAR, NDEF, LNUMS, STATUS )
      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_DEFLD', STATUS )

      END
