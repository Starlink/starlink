      SUBROUTINE MAGIC( STATUS )
*+
*  Name:
*     MAGIC

*  Purpose:
*     Set magic values according to quality array

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL MAGIC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  Usage:
*     magic {parameter_usage}

*  Environment Parameters:
*     {parameter_name}[pdims] = {parameter_type} ({parameter_access_mode})
*        {parameter_description}

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

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     magic, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RJV: Bob Vallance (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     24 Nov 1994 V1.8-0 (DJA):
*        New user interface
*     17 Nov 1995 V2.0-0 (DJA):
*        Full ADI port
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'
      INCLUDE 'PRM_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'MAGIC Version 2.1-0' )

*  Local Variables:
      CHARACTER*8               MSTR			! Mask string

      REAL                      MAGVAL			! Magic value

      INTEGER 			DPTR,QPTR		! Mapped iinput data
      INTEGER                   IFID                    ! Input dataset id
      INTEGER 			LEN			! # data elements
      INTEGER                   OFID                    ! Output dataset id

      BYTE 			MASK			! Quality mask value

      LOGICAL                   OVER                    ! Overwrite mode?
      LOGICAL 			DOK,QOK			! Data/quality ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Overwrite mode?
      CALL USI_GET0L( 'OVER', OVER, STATUS )

*  Overwrite case - input becomes output
      IF ( OVER ) THEN
        CALL USI_ASSOC( 'INP', 'BinDS', 'UPDATE', OFID, STATUS )

      ELSE

*    Otherwise clone from input
        CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )
        CALL USI_CLONE( 'INP', 'OUT', 'BinDS', OFID, STATUS )

      ENDIF

*  Check data and quality
      CALL BDI_CHK( OFID, 'Data', DOK, STATUS )
      CALL BDI_CHK( OFID, 'Quality', QOK, STATUS )
      CALL BDI_GETNEL( OFID, LEN, STATUS )
      IF (STATUS.EQ.SAI__OK.AND.DOK.AND.QOK) THEN

*    Get the magic value
        CALL USI_GET0R( 'MAGIC', MAGVAL, STATUS )
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL( STATUS )
          MAGVAL = VAL__BADR
        END IF

*    Allow user to change the mask
        CALL BDI_GET0UB( OFID, 'QualityMask', MASK, STATUS )
        CALL STR_BTOC( MASK, MSTR, STATUS )
        CALL USI_DEF0C( 'MASK', MSTR, STATUS )
        CALL USI_GET0C( 'MASK', MSTR, STATUS )
        CALL STR_CTOB( MSTR, MASK, STATUS )

*    Map data to be updated, and the quality
        CALL BDI_MAPR( OFID, 'Data', 'UPDATE', DPTR, STATUS )
        CALL BDI_MAPUB( OFID, 'Quality', 'READ', QPTR, STATUS )

*    Set the magic values
        CALL MAGIC_WRITE( OFID, MASK, MAGVAL, LEN, %VAL(QPTR),
     :                    %VAL(DPTR), STATUS )

      END IF

*  Tidy up
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  MAGIC_WRITE
      SUBROUTINE MAGIC_WRITE(FID,MASK,MAGIC,LEN,Q,D,STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*    Import :
      INTEGER FID
      BYTE MASK
      REAL MAGIC
      INTEGER LEN
      BYTE Q(LEN)
      REAL D(LEN)
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB
*    Local variables :
      LOGICAL FLAG
      INTEGER I
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No bad values yet
      FLAG=.FALSE.

      DO I = 1, LEN
        IF ( BIT_ANDUB(Q(I),MASK) .NE. QUAL__GOOD ) THEN
          D(I) = MAGIC
          FLAG = .TRUE.
        END IF
      END DO

*  Write magic flag if required
      IF ( FLAG ) THEN
        CALL BDI_PUT0L( FID, 'MagicFlag', FLAG, STATUS )
      END IF

*  Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'MAGIC_WRITE', STATUS )
      END IF

      END
