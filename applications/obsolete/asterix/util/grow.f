      SUBROUTINE GROW( STATUS )
*+
*  Name:
*     GROW

*  Purpose:
*     Grow a 1-D dataset into one dimension of an n-D dataset

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL GROW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  Usage:
*     grow {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Dataset to be altered, or grown into
*     GROW = CHAR (read)
*        Dataset to be grown with

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
*     grow, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 May 1996 V2.0-0 (DJA):
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

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'GROW Version V2.0-0' )

*  Local Variables:
      INTEGER			AXIS			! Axis to grow along
      INTEGER			GFID			! Dataset to grow
      INTEGER			GDPTR, GQPTR, GVPTR	! Grow data
      INTEGER			GNDIM, GDIM		! Grow input dimensions
      INTEGER			IFID			! Dataset to be altered
      INTEGER			IDPTR, IQPTR, IVPTR	! Input data
      INTEGER			NDIM, DIMS(ADI__MXDIM)	! Input dimensions

      LOGICAL			DO_DATA,DO_QUAL,DO_VAR	! Grow these bits?
      LOGICAL			DOK, QOK, VOK		! These bits exist?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get dataset to be altered
      CALL USI_ASSOC( 'INP', 'BinDS', 'UPDATE', IFID, STATUS )

*  Get dataset to grow with
      CALL USI_ASSOC( 'GROW', 'BinDS', 'READ', GFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get dimensions of inputs
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      CALL BDI_GETSHP( GFID, 1, GDIM, GNDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  If not one dimensional chose axis to grow into
      IF ( NDIM .EQ. 1 ) THEN
        AXIS = 1
      ELSE
        CALL USI_GET0I( 'AXIS', AXIS, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*     Check in range
        IF ( (AXIS.LT.1) .OR. (AXIS.GT.NDIM) ) THEN
          CALL MSG_SETI( 'A', AXIS )
          CALL MSG_SETI( 'ND', NDIM )
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Axis number /^A/ illegal, should be '/
     :                  /'between 1 and ^ND', STATUS )
          GOTO 99
        END IF

      END IF

*  Check dimensions are compatible
      IF ( GDIM .NE. DIMS(AXIS) ) THEN
        CALL MSG_SETI( 'A', AXIS )
        CALL BDI_DESCID( 'G', GFID, STATUS )
        CALL BDI_DESCID( 'INP', IFID, STATUS )
        CALL ERR_REP( ' ', 'Dimension of ^G does not match size '/
     :                /'of axis ^A of ^INP', STATUS )
        GOTO 99
      END IF

*  Pad dimensions to 7-D
      CALL AR7_PAD( NDIM, DIMS, STATUS )

*  Get keywords
      CALL USI_GET0L( 'DATA', DO_DATA, STATUS )
      CALL USI_GET0L( 'QUAL', DO_QUAL, STATUS )
      CALL USI_GET0L( 'VAR', DO_VAR, STATUS )

*  Map input data
      IF ( DO_DATA ) THEN
        CALL BDI_MAPR( IFID, 'Data', 'UPDATE', IDPTR, STATUS )
        CALL BDI_MAPR( GFID, 'Data', 'READ', GDPTR, STATUS )
        DOK = .TRUE.
      ELSE
        DOK = .FALSE.
      END IF
      IF ( DO_QUAL ) THEN
        CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
        IF ( QOK ) THEN
          CALL BDI_MAPUB( IFID, 'Quality', 'UPDATE', IQPTR, STATUS )
          CALL BDI_MAPUB( GFID, 'Quality', 'READ', GQPTR, STATUS )
        END IF
      ELSE
        QOK = .FALSE.
      END IF
      IF ( DO_VAR ) THEN
        CALL BDI_CHK( IFID, 'Variance', VOK, STATUS )
        IF ( VOK ) THEN
          CALL BDI_MAPR( IFID, 'Variance', 'UPDATE', IVPTR, STATUS )
          CALL BDI_MAPR( GFID, 'Variance', 'READ', GVPTR, STATUS )
        END IF
      ELSE
        VOK = .FALSE.
      END IF

*  Switch on axis value
      IF ( AXIS .EQ. 1 ) THEN
        CALL GROW_AX1( DIMS(1), DIMS(2), DIMS(3), DIMS(4), DIMS(5),
     :                 DIMS(6), DIMS(7), DOK, %VAL(IDPTR), %VAL(GDPTR),
     :                 QOK, %VAL(IQPTR), %VAL(GQPTR), VOK, %VAL(IVPTR),
     :                 %VAL(GVPTR), STATUS )

      ELSE IF ( AXIS .EQ. 2 ) THEN
      ELSE IF ( AXIS .EQ. 3 ) THEN
      ELSE IF ( AXIS .EQ. 4 ) THEN
      ELSE IF ( AXIS .EQ. 5 ) THEN
      ELSE IF ( AXIS .EQ. 6 ) THEN
      ELSE IF ( AXIS .EQ. 7 ) THEN
      END IF

*  Free the inputs
      CALL USI_CANCL( 'INP', STATUS )
      CALL USI_CANCL( 'GROW', STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE GROW_AX1( L1, L2, L3, L4, L5, L6, L7, DOK, DATA,
     :                     GDATA, QOK, QUAL, GQUAL, VOK, VAR, GVAR,
     :                     STATUS )
*+
*  Name:
*     GROW_AX1

*  Purpose:
*     Grow a dataset into the 1st axis of another

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GROW_AX1( L1, L2, L3, L4, L5, L6, L7, DOK, DATA, GDATA,
*                    QOK, QUAL, GQUAL, VOK, VAR, GVAR, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     L<N> = INTEGER (given)
*        Size of dimension <N>
*     DOK = LOGICAL (given)
*        Copy data?
*     DATA[L1,L2,L3,L4,L5,L6,L7] = REAL (returned)
*        Data to be altered
*     GDATA[*] = REAL (given)
*        Data to be grown
*     QOK = LOGICAL (given)
*        Copy quality?
*     QUAL[L1,L2,L3,L4,L5,L6,L7] = BYTE (returned)
*        Quality to be altered
*     GQUAL[*] = BYTE (given)
*        Quality to be grown
*     VOK = LOGICAL (given)
*        Copy variance?
*     VAR[L1,L2,L3,L4,L5,L6,L7] = REAL (returned)
*        Variance to be altered
*     GVAR[*] = REAL (given)
*        Variance to be grown
*     STATUS = INTEGER (given)
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

*  Keywords:
*     grow, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 May 1996 (DJA):
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
      INTEGER			L1, L2, L3, L4, L5, L6, L7
      LOGICAL			DOK, QOK, VOK
      REAL			GDATA(*), GVAR(*)
      BYTE			GQUAL(*)

*  Arguments Returned:
      REAL			DATA(L1,L2,L3,L4,L5,L6,L7),
     :                          VAR(L1,L2,L3,L4,L5,L6,L7)
      BYTE			QUAL(L1,L2,L3,L4,L5,L6,L7)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I, J, K, L, M, N, O 	! Loop variables
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over data
      DO O = 1, L7
        DO N = 1, L6
          DO M = 1, L5
            DO L = 1, L4
              DO K = 1, L3
                DO J = 1, L2

*              Copy data?
                  IF ( DOK ) THEN
                    DO I = 1, L1
                      DATA(I,J,K,L,M,N,O) = GDATA(I)
                    END DO
                  END IF

*              Copy quality?
                  IF ( QOK ) THEN
                    DO I = 1, L1
                      QUAL(I,J,K,L,M,N,O) = GQUAL(I)
                    END DO
                  END IF

*              Copy variance?
                  IF ( QOK ) THEN
                    DO I = 1, L1
                      VAR(I,J,K,L,M,N,O) = GVAR(I)
                    END DO
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END
