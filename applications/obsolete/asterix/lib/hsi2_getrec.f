      SUBROUTINE HSI2_GETREC( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI2_GETREC

*  Purpose:
*     Retrieve a particular history record from an HDS dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI2_GETREC( NARG, ARGS, OARG, STATUS )

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
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:private, history, read

*  Copyright:
*     Copyright (C) University of Birmingham, 1997

*  Authors:
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Feb 1997 (RB):
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
      EXTERNAL			ADI2_MKIDX
        CHARACTER*8		ADI2_MKIDX

*  Local Variables:
      CHARACTER*72		CMNT, TXT
      CHARACTER*8		NAME, DATE, TIME

      INTEGER			CID			! ADI array cell
      INTEGER                   ILINE                   ! Loop over text lines
      INTEGER			IREC			! Record required
      INTEGER                   NLINE                   ! Number of TEXT lines
      INTEGER			HDUID, CACHEID
      INTEGER			LUN, HDUTYPE

      LOGICAL                   THERE                   ! Component exists?
      LOGICAL			DIDCRE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract dataset locator
      CALL ADI2_GETLUN( ARGS(1), LUN, STATUS )
      CALL ADI_GET0I( ARGS(2), IREC, STATUS )
      CALL FTMAHD( LUN, IREC, HDUTYPE, STATUS )
      IF ( IREC .EQ. 1 ) THEN
        NAME = ' '
      ELSE
        CALL FTGKYS( LUN, 'EXTNAME', NAME, CMNT, STATUS )
      END IF
      CALL ADI2_FNDHDU( ARGS(1), NAME, .FALSE., HDUID, STATUS )

*  Located ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Construct return data
        CALL ADI_NEW0( 'HistoryRecord', OARG, STATUS )

*    Write date and time
        CALL ADI2_CFIND( ARGS(1), NAME, '.DATE', ' ', .FALSE., .FALSE.,
     :                   'CHAR', 0, 0, DIDCRE, CACHEID, STATUS )
        IF ( CACHEID .EQ. ADI__NULLID ) THEN
          DATE = 'no_date'
        ELSE
          CALL ADI_CGET0C( CACHEID, 'Value', DATE, STATUS )
          CALL ADI_ERASE( CACHEID, STATUS )
        END IF

        CALL ADI2_CFIND( ARGS(1), NAME, '.TIME', ' ', .FALSE., .FALSE.,
     :                   'CHAR', 0, 0, DIDCRE, CACHEID, STATUS )
        IF ( CACHEID .EQ. ADI__NULLID ) THEN
          TIME = 'no_time'
        ELSE
          CALL ADI_CGET0C( CACHEID, 'Value', TIME, STATUS )
          CALL ADI_ERASE( CACHEID, STATUS )
        END IF
        CALL ADI_CPUT0C( OARG, 'Date', DATE//' '//TIME, STATUS )

*    Write command
        IF ( NAME .EQ. ' ' ) NAME = 'PRIMARY'
        CALL ADI_CPUT0C( OARG, 'Creator', NAME, STATUS )

*    Write host
        CALL ADI_CPUT0C( OARG, 'Host', 'unknown', STATUS )

*    Write text
        CALL ADI_THERE( HDUID, 'HistoryCount', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CGET0I( HDUID, 'HistoryCount', NLINE, STATUS )

          IF ( NLINE .GT. 0 ) THEN
            CALL ADI_CNEW1C( OARG, 'Text', NLINE, STATUS )
            DO ILINE = 1, NLINE
              NAME = ADI2_MKIDX( 'HIST', ILINE )
              CALL ADI2_HGKYC( HDUID, NAME, TXT, CMNT, STATUS )
              CALL ADI_CCELL( OARG, 'Text', 1, ILINE, CID, STATUS )
              CALL ADI_PUT0C( CID, TXT(:MAX(1,CHR_LEN(TXT)))//' ', STATUS )
              CALL ADI_ERASE( CID, STATUS )
            END DO
          END IF

        END IF

      ELSE
        CALL ERR_ANNUL( STATUS )
        OARG = ADI__NULLID

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI2_GETREC', STATUS )

      END
