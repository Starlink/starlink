      SUBROUTINE HSI1_GETREC( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI1_GETREC

*  Purpose:
*     Retrieve a particular history record from an HDS dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI1_GETREC( NARG, ARGS, OARG, STATUS )

*  Description:
*     Retrieve a particular history record from an HDS dataset. Checks
*     whether the record number is legal, and returns creator, date and
*     possibly text.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:private, history, read

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     16 Mar 1995 (DJA):
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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*30              CDATE                   ! Creation date
      CHARACTER*(DAT__SZLOC)    CRLOC                   ! Current RECORDS cell
      CHARACTER*(DAT__SZLOC)    HLOC                    ! HISTORY object
      CHARACTER*30		HOST			! History host
      CHARACTER*(DAT__SZLOC)    LOC                     ! Dataset locator
      CHARACTER*(DAT__SZLOC)    RLOC                    ! RECORDS object
      CHARACTER*132		TEXT			! History text
      CHARACTER*(DAT__SZLOC)    TXLOC                   ! TEXT object
      CHARACTER*(DAT__SZLOC)    TXCLOC                  ! TEXT object cell

      INTEGER			CID			! ADI array cell
      INTEGER			CREC			! Current # records
      INTEGER                   ILINE                   ! Loop over text lines
      INTEGER			IREC			! Record required
      INTEGER                   NLINE                   ! Number of TEXT lines

      LOGICAL                   THERE                   ! Component exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract dataset locator
      CALL ADI1_GETLOC( ARGS(1), LOC, STATUS )

*  Locate the top-level HISTORY object
      CALL DAT_FIND( LOC, 'HISTORY', HLOC, STATUS )

*  Located ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Current record number
        CALL CMP_GET0I( HLOC, 'CURRENT_RECORD', CREC, STATUS )

*    Number required
        CALL ADI_GET0I( ARGS(2), IREC, STATUS )
        IF ( (IREC.LE.0) .OR. (IREC.GT.CREC) ) THEN
          CALL MSG_SETI( 'REC', IREC )
          CALL ERR_REP( ' ', 'Illegal history record number /^REC/',
     :                  STATUS )
        END IF

*    Locate the CREC'th record
        CALL DAT_FIND( HLOC, 'RECORDS', RLOC, STATUS )
        CALL DAT_CELL( RLOC, 1, CREC, CRLOC, STATUS )

*    Construct return data
        CALL ADI_NEW0( 'HistoryRecord', OARG, STATUS )

*    Write date
        CALL CMP_GET0C( CRLOC, 'DATE', CDATE, STATUS )
        CALL ADI_CPUT0C( OARG, 'Date', CDATE, STATUS )

*    Write command
        CALL CMP_GET0C( CRLOC, 'COMMAND', TEXT, STATUS )
        CALL ADI_CPUT0C( OARG, 'Creator', TEXT(:CHR_LEN(TEXT)), STATUS )

*    Write host
        CALL DAT_THERE( CRLOC, 'HOST', THERE, STATUS )
        IF ( THERE ) THEN
          CALL CMP_GET0C( CRLOC, 'HOST', HOST, STATUS )
        ELSE
          HOST = 'unknown'
        END IF
        CALL ADI_CPUT0C( OARG, 'Host', HOST, STATUS )

*    Write text
        CALL DAT_THERE( CRLOC, 'TEXT', THERE, STATUS )
        IF ( THERE ) THEN
          CALL DAT_FIND( CRLOC, 'TEXT', TXLOC, STATUS )
          CALL DAT_SIZE( TXLOC, NLINE, STATUS )
          CALL ADI_CNEW1C( OARG, 'Text', NLINE, STATUS )
          DO ILINE = 1, NLINE
            CALL DAT_CELL( TXLOC, 1, ILINE, TXCLOC, STATUS )
            CALL ADI_CCELL( OARG, 'Text', 1, ILINE, CID, STATUS )
            CALL DAT_GET0C( TXCLOC, TEXT, STATUS )
            CALL ADI_PUT0C( CID, TEXT(:MAX(1,CHR_LEN(TEXT))), STATUS )
            CALL ADI_ERASE( CID, STATUS )
            CALL DAT_ANNUL( TXCLOC, STATUS )
          END DO
          CALL DAT_ANNUL( TXLOC, STATUS )
        END IF

*    Release locators
        CALL DAT_ANNUL( CRLOC, STATUS )
        CALL DAT_ANNUL( RLOC, STATUS )
        CALL DAT_ANNUL( HLOC, STATUS )

      ELSE
        CALL ERR_ANNUL( STATUS )
        OARG = ADI__NULLID

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI1_GETREC', STATUS )

      END
