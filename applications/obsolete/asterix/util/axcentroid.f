      SUBROUTINE AXCENTROID( STATUS )
*+
*  Name:
*     AXCENTROID

*  Purpose:
*     Finds the data weighted mean axis value for any dataset axis

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL AXCENTROID( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The program makes a copy of its input, reducing the dimensionality
*     by one and replacing the data value with the data weighted mean
*     mean lost axis value. In the simple 1-D case the mean axis value
*     is printed out.

*  Usage:
*     axcentroid {parameter_usage}

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
*     axcentroid, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Mar 1992 V1.6-0 (DJA):
*        Original version
*     14 Oct 1992 V1.7-0 (DJA):
*        Major bug fix
*     24 Nov 1992 V1.7-1 (DJA):
*        Now uses quality in 1-d case
*     26 Sep 1993 V1.7-2 (DJA):
*        Bug fixed in quality handling
*     28 Feb 1994 V1.7-3 (DJA):
*        Use BIT_ routines to do bit manipulations
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*      6 Dec 1995 V2.0-0 (DJA):
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
      INCLUDE 'QUAL_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'AXCENTROID Version V2.0-0' )

*  Local Variables:
      CHARACTER*80		TEXT			! History text
      CHARACTER*40     		UNITS                   ! Axis units

      REAL             		AXCEN                   ! Channel weighted mean

      INTEGER                   AXIS                    ! Axis for weighting
      INTEGER                   DIMS(ADI__MXDIM)        ! Input dimensions
      INTEGER                   IAPTR                   ! Weighting axis data
      INTEGER                   IAX, JAX                ! Loops over axes
      INTEGER                   IDPTR                   ! Input data values
      INTEGER			IFID			! Input dataset id
      INTEGER                   IQPTR                   ! Input quality values
      INTEGER                   NAX                     ! Number of axes
      INTEGER                   NDIM                    ! Input dimensionality
      INTEGER                   ODIMS(ADI__MXDIM)       ! Output dimensions
      INTEGER                   ODPTR                   ! Output data
      INTEGER			OFID			! Output dataset id
      INTEGER                   ONDIM                   ! Output dimensionality
      INTEGER                   OQPTR                   ! Output quality
      INTEGER			TLEN			! Amount of TEXT used

      BYTE                      OQUAL                   ! Output quality point

      LOGICAL                   ISDS                    ! Input structured?
      LOGICAL                   OK                      ! Validity test
      LOGICAL                   QOK                     ! Input quality there?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )
      CALL ADI_DERVD( IFID, 'BinDS', ISDS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check data
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( .NOT. OK ) THEN
        CALL MSG_PRNT( '! Invalid data' )
        STATUS = SAI__ERROR
        GOTO 99
      ELSE IF ( NDIM .EQ. 0 ) THEN
        CALL MSG_PRNT( '! Input data is scalar' )
        STATUS = SAI__ERROR
        GOTO 99
      END IF
      CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )

*  Select axis
      IF ( NDIM .EQ. 1 ) THEN
        AXIS = 1
      ELSE

*    List axes
        CALL AXIS_TLIST( IFID, NDIM, STATUS )

*    Select axis
        CALL USI_GET0I( 'AXIS', AXIS, STATUS )
        IF ( (AXIS.LT.1) .OR. (AXIS.GT.NDIM) ) THEN
          CALL MSG_PRNT( '! Invalid axis number' )
          STATUS = SAI__ERROR
        END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Map input data
      CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Use axis values or channels
      CALL BDI_AXCHK( IFID, AXIS, 'Data', OK, STATUS )
      IF ( OK ) THEN
        CALL BDI_AXMAPR( IFID, AXIS, 'READ', IAPTR, STATUS )
      ELSE
        CALL DYN_MAPR( 1, DIMS(AXIS), IAPTR, STATUS )
        CALL ARR_REG1R( 1.0, 1.0, DIMS(AXIS), %VAL(IAPTR), STATUS )
      END IF

*  Map input quality
      IF ( QOK ) THEN
        CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', IQPTR,
     :                 STATUS )
      END IF

*  Create output file?
      IF ( NDIM .GT. 1 ) THEN

*    Construct dimensions
        JAX = 1
        DO IAX = 1, NDIM
          IF ( IAX .NE. AXIS ) THEN
            ODIMS(JAX) = DIMS(IAX)
            JAX = JAX + 1
          END IF
        END DO
        ONDIM = NDIM - 1

*    Associate object
        CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
        CALL BDI_LINK( 'BinDS', ONDIM, ODIMS, 'REAL', OFID,
     :                 STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Create output data
        CALL BDI_MAPR( OFID, 'Data', 'WRITE', ODPTR, STATUS )

*    Create quality if present in input
        IF ( QOK ) THEN
          CALL BDI_PUT( OFID, 'QualityMask', 'UBYTE', 0, 0,
     :                  QUAL__MASK, STATUS )
          CALL BDI_MAP( OFID, 'Quality', 'UBYTE', 'WRITE', OQPTR,
     :                  STATUS )
        END IF

*    Copy stuff from input
        IF ( ISDS ) THEN

*      Copy axes
          IF ( NAX .GT. 0 ) THEN
            JAX = 1
            DO IAX = 1, NAX
              IF ( IAX .NE. AXIS ) THEN
                CALL BDI_AXCOPY( IFID, IAX, ' ', OFID, JAX,
     :                           STATUS )
                JAX = JAX + 1
              END IF
            END DO
          END IF

*      Ancillary bits
          CALL HSI_COPY( IFID, OFID, STATUS )
          CALL UDI_COPANC( IFID, 'grf', OFID, STATUS )
          CALL BDI_COPY( IFID, 'Title,Label,Units', OFID, STATUS )

        END IF

      END IF

*  Pad dimensions to 7D
      CALL AR7_PAD( NDIM, DIMS, STATUS )
      CALL AR7_PAD( ONDIM, ODIMS, STATUS )

*  Act on data
      IF ( NDIM .GT. 1 ) THEN
        CALL AXCENTROID_INT( DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                     DIMS(5), DIMS(6), DIMS(7), AXIS,
     :                     %VAL(IAPTR), %VAL(IDPTR), QOK,
     :                     %VAL(IQPTR),
     :                     ODIMS(1), ODIMS(2), ODIMS(3), ODIMS(4),
     :                     ODIMS(5), ODIMS(6), ODIMS(7),
     :                     %VAL(ODPTR), %VAL(OQPTR), STATUS )
      ELSE
        CALL AXCENTROID_INT( DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                     DIMS(5), DIMS(6), DIMS(7), AXIS,
     :                     %VAL(IAPTR), %VAL(IDPTR), QOK,
     :                     %VAL(IQPTR),
     :                     1, 1, 1, 1, 1, 1, 1,
     :                     AXCEN, OQUAL, STATUS )

        IF ( NAX .GT. 0 ) THEN
          CALL BDI_AXGET0C( IFID, AXIS, 'Units', UNITS, STATUS )
          CALL MSG_SETC( 'UNIT', UNITS )
        ELSE
          CALL MSG_SETC( 'UNIT', 'pixels' )
        END IF
        CALL MSG_SETI( 'AX', AXIS )
        CALL MSG_SETR( 'CEN', AXCEN )
        CALL MSG_PRNT( 'Centroid wrt axis ^AX is ^CEN ^UNIT.' )
      END IF

*  Write history
      IF ( NDIM .GT. 1 ) THEN
        CALL HSI_ADD( OFID, VERSION, STATUS )
        CALL MSG_SETI( 'AX', AXIS )
        CALL MSG_MAKE( 'Centroided axis ^AX', TEXT, TLEN )
        CALL HSI_PTXT( OFID, 1, TEXT(:TLEN), STATUS )
      END IF

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  AXCENTROID_INT - Peform ratioing on 7D data sets
      SUBROUTINE AXCENTROID_INT( L1,L2,L3,L4,L5,L6,L7,AXIS,
     :                         I_A, I_D, QFLAG, I_Q,
     :                         M1,M2,M3,M4,M5,M6,M7, O_D, O_Q,
     :                         STATUS )
*
*    Description :
*
*    Method:
*
*    Author :
*
*     David Allan (BHVAD::DJA)
*
*    History :
*
*     8 Sep 88 : Original ( DJA )
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER                STATUS              ! Run-time error code
*
*    Import :
*
      INTEGER                AXIS                ! Axis to ratio on
      REAL                   I_A(*)              ! Axis values
      LOGICAL                QFLAG               ! Use quality?
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input qual
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      INTEGER                M1,M2,M3,M4,M5,M6,M7!
*
*    Export :
*
      BYTE                   O_Q(M1,M2,M3,M4,M5,M6,M7) ! Output quality
      REAL                   O_D(M1,M2,M3,M4,M5,M6,M7) ! Output data
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Accumulate the data from I_D into W_D and O_D
      IF ( AXIS .EQ. 7 ) THEN
        CALL AXCENTROID_7( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      ELSE IF ( AXIS .EQ. 6 ) THEN
        CALL AXCENTROID_6( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      ELSE IF ( AXIS .EQ. 5 ) THEN
        CALL AXCENTROID_5( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      ELSE IF ( AXIS .EQ. 4 ) THEN
        CALL AXCENTROID_4( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      ELSE IF ( AXIS .EQ. 3 ) THEN
        CALL AXCENTROID_3( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      ELSE IF ( AXIS .EQ. 2 ) THEN
        CALL AXCENTROID_2( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      ELSE IF ( AXIS .EQ. 1 ) THEN
        CALL AXCENTROID_1( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      END IF

      END




*+ AXCENTROID_7 - Perform sum over axis 7
      SUBROUTINE AXCENTROID_7( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q,
     :                 QFLAG, M1,M2,M3,M4,M5,M6, O_D, O_Q )

*    Description :
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
*
*    Export :
*
      INTEGER                M1,M2,M3,M4,M5,M6   !
      REAL                   O_D(M1,M2,M3,M4,M5,M6) ! Output data
      BYTE                   O_Q(M1,M2,M3,M4,M5,M6) ! Output quality
*
*    Local variables :
*
      REAL                   DSUM, ADSUM         ! Sums over data
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
      LOGICAL                GOOD,ANYGOOD        ! Quality tests
*-

*    For axis 7 check on index O - output to (I,J,K,L,M,N)
      DO N = 1, L6
        DO M = 1, L5
          DO L = 1, L4
            DO K = 1, L3
              DO J = 1, L2
                DO I = 1, L1

*                Initialise
                  DSUM = 0.0
                  ADSUM = 0.0
                  GOOD = .TRUE.
                  ANYGOOD = .FALSE.

*                Sum the numerator and denominator
                  DO O = 1, L7
                    IF ( QFLAG ) THEN
                      GOOD = I_Q(I,J,K,L,M,N,O)
                    ELSE
                      GOOD = .TRUE.
                    END IF
                    IF ( GOOD ) THEN
                      DSUM = DSUM + I_D(I,J,K,L,M,N,O)
                      ADSUM = ADSUM + I_D(I,J,K,L,M,N,O)*I_A(O)
                      ANYGOOD = .TRUE.
                    END IF
                  END DO

*                Set output data
                  GOOD = ( (QFLAG.AND.ANYGOOD) .OR. .NOT. QFLAG )
                  IF ( GOOD ) THEN
                    O_D(I,J,K,L,M,N) = ADSUM / DSUM
                    IF ( QFLAG ) O_Q(I,J,K,L,M,N) = QUAL__GOOD
                  ELSE
                    O_D(I,J,K,L,M,N) = 0.0
                    O_Q(I,J,K,L,M,N) = QUAL__BAD
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END



*+ AXCENTROID_6 - Perform sum over axis 6
      SUBROUTINE AXCENTROID_6( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q,
     :                 QFLAG, M1,M2,M3,M4,M5,M7, O_D, O_Q )

*    Description :
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
*
*    Export :
*
      INTEGER                M1,M2,M3,M4,M5,M7   !
      REAL                   O_D(M1,M2,M3,M4,M5,M7) ! Output data
      BYTE                   O_Q(M1,M2,M3,M4,M5,M7) ! Output quality
*
*    Local variables :
*
      REAL                   DSUM, ADSUM         ! Sums over data
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
      LOGICAL                GOOD,ANYGOOD        ! Quality tests
*-

*    For axis 6 check on index N - output to (I,J,K,L,M,O)
      DO O = 1, L7
        DO M = 1, L5
          DO L = 1, L4
            DO K = 1, L3
              DO J = 1, L2
                DO I = 1, L1

*                Initialise
                  DSUM = 0.0
                  ADSUM = 0.0
                  GOOD = .TRUE.
                  ANYGOOD = .FALSE.

*                Sum the numerator and denominator
                  DO N = 1, L6
                    IF ( QFLAG ) THEN
                      GOOD = I_Q(I,J,K,L,M,N,O)
                    ELSE
                      GOOD = .TRUE.
                    END IF
                    IF ( GOOD ) THEN
                      DSUM = DSUM + I_D(I,J,K,L,M,N,O)
                      ADSUM = ADSUM + I_D(I,J,K,L,M,N,O)*I_A(N)
                      ANYGOOD = .TRUE.
                    END IF
                  END DO

*                Set output data
                  GOOD = ( (QFLAG.AND.ANYGOOD) .OR. .NOT. QFLAG )
                  IF ( GOOD ) THEN
                    O_D(I,J,K,L,M,O) = ADSUM / DSUM
                    IF ( QFLAG ) O_Q(I,J,K,L,M,O) = QUAL__GOOD
                  ELSE
                    O_D(I,J,K,L,M,O) = 0.0
                    O_Q(I,J,K,L,M,O) = QUAL__BAD
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END



*+ AXCENTROID_5 - Perform sum over axis 5
      SUBROUTINE AXCENTROID_5( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q,
     :                 QFLAG, M1,M2,M3,M4,M6,M7, O_D, O_Q )

*    Description :
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
*
*    Export :
*
      INTEGER                M1,M2,M3,M4,M6,M7   !
      REAL                   O_D(M1,M2,M3,M4,M6,M7) ! Output data
      BYTE                   O_Q(M1,M2,M3,M4,M6,M7) ! Output quality
*
*    Local variables :
*
      REAL                   DSUM, ADSUM         ! Sums over data
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
      LOGICAL                GOOD,ANYGOOD        ! Quality tests
*-

*    For axis 5 check on index M - output to (I,J,K,L,N,O)
      DO O = 1, L7
        DO N = 1, L6
          DO L = 1, L4
            DO K = 1, L3
              DO J = 1, L2
                DO I = 1, L1

*                Initialise
                  DSUM = 0.0
                  ADSUM = 0.0
                  GOOD = .TRUE.
                  ANYGOOD = .FALSE.

*                Sum the numerator and denominator
                  DO M = 1, L5
                    IF ( QFLAG ) THEN
                      GOOD = I_Q(I,J,K,L,M,N,O)
                    ELSE
                      GOOD = .TRUE.
                    END IF
                    IF ( GOOD ) THEN
                      DSUM = DSUM + I_D(I,J,K,L,M,N,O)
                      ADSUM = ADSUM + I_D(I,J,K,L,M,N,O)*I_A(M)
                      ANYGOOD = .TRUE.
                    END IF
                  END DO

*                Set output data
                  GOOD = ( (QFLAG.AND.ANYGOOD) .OR. .NOT. QFLAG )
                  IF ( GOOD ) THEN
                    O_D(I,J,K,L,N,O) = ADSUM / DSUM
                    IF ( QFLAG ) O_Q(I,J,K,L,N,O) = QUAL__GOOD
                  ELSE
                    O_D(I,J,K,L,N,O) = 0.0
                    O_Q(I,J,K,L,N,O) = QUAL__BAD
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END



*+ AXCENTROID_4 - Perform sum over axis 4
      SUBROUTINE AXCENTROID_4( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q,
     :                 QFLAG, M1,M2,M3,M5,M6,M7, O_D, O_Q )
*
*    Description :
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
*
*    Export :
*
      INTEGER                M1,M2,M3,M5,M6,M7   !
      REAL                   O_D(M1,M2,M3,M5,M6,M7) ! Output data
      BYTE                   O_Q(M1,M2,M3,M5,M6,M7) ! Output quality
*
*    Local variables :
*
      REAL                   DSUM, ADSUM         ! Sums over data
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
      LOGICAL                GOOD,ANYGOOD        ! Quality tests
*-

*    For axis 4 check on index L - output to (I,J,K,M,N,O)
      DO O = 1, L7
        DO N = 1, L6
          DO M = 1, L5
            DO K = 1, L3
              DO J = 1, L2
                DO I = 1, L1

*                Initialise
                  DSUM = 0.0
                  ADSUM = 0.0
                  GOOD = .TRUE.
                  ANYGOOD = .FALSE.

*                Sum the numerator and denominator
                  DO L = 1, L4
                    IF ( QFLAG ) THEN
                      GOOD = I_Q(I,J,K,L,M,N,O)
                    ELSE
                      GOOD = .TRUE.
                    END IF
                    IF ( GOOD ) THEN
                      DSUM = DSUM + I_D(I,J,K,L,M,N,O)
                      ADSUM = ADSUM + I_D(I,J,K,L,M,N,O)*I_A(L)
                      ANYGOOD = .TRUE.
                    END IF
                  END DO

*                Set output data
                  GOOD = ( (QFLAG.AND.ANYGOOD) .OR. .NOT. QFLAG )
                  IF ( GOOD ) THEN
                    O_D(I,J,K,M,N,O) = ADSUM / DSUM
                    IF ( QFLAG ) O_Q(I,J,K,M,N,O) = QUAL__GOOD
                  ELSE
                    O_D(I,J,K,M,N,O) = 0.0
                    O_Q(I,J,K,M,N,O) = QUAL__BAD
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END



*+ AXCENTROID_3 - Perform sum over axis 3
      SUBROUTINE AXCENTROID_3( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q,
     :                 QFLAG, M1,M2,M4,M5,M6,M7, O_D, O_Q )

*    Description :
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
*
*    Export :
*
      INTEGER                M1,M2,M4,M5,M6,M7   !
      REAL                   O_D(M1,M2,M4,M5,M6,M7) ! Output data
      BYTE                   O_Q(M1,M2,M4,M5,M6,M7) ! Output quality
*
*    Local variables :
*
      REAL                   DSUM, ADSUM         ! Sums over data
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
      LOGICAL                GOOD,ANYGOOD        ! Quality tests
*-

*    For axis 3 check on index K - output to (I,J,L,M,N,O)
      DO O = 1, L7
        DO N = 1, L6
          DO M = 1, L5
            DO L = 1, L4
              DO J = 1, L2
                DO I = 1, L1

*                Initialise
                  DSUM = 0.0
                  ADSUM = 0.0
                  ANYGOOD = .FALSE.

*                Sum the numerator and denominator
                  DO K = 1, L3
                    IF ( QFLAG ) THEN
                      GOOD = I_Q(I,J,K,L,M,N,O)
                    ELSE
                      GOOD = .TRUE.
                    END IF
                    IF ( GOOD ) THEN
                      DSUM = DSUM + I_D(I,J,K,L,M,N,O)
                      ADSUM = ADSUM + I_D(I,J,K,L,M,N,O)*I_A(K)
                      ANYGOOD = .TRUE.
                    END IF
                  END DO

*                Set output data
                  GOOD = ( (QFLAG.AND.ANYGOOD) .OR. .NOT. QFLAG )
                  IF ( GOOD ) THEN
                    O_D(I,J,L,M,N,O) = ADSUM / DSUM
                    IF ( QFLAG ) O_Q(I,J,L,M,N,O) = QUAL__GOOD
                  ELSE
                    O_D(I,J,L,M,N,O) = 0.0
                    O_Q(I,J,L,M,N,O) = QUAL__BAD
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END



*+ AXCENTROID_2 - Perform sum over axis 2
      SUBROUTINE AXCENTROID_2( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q,
     :                 QFLAG, M1,M3,M4,M5,M6,M7, O_D, O_Q )

*    Description :
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
*
*    Export :
*
      INTEGER                M1,M3,M4,M5,M6,M7   !
      REAL                   O_D(M1,M3,M4,M5,M6,M7) ! Output data
      BYTE                   O_Q(M1,M3,M4,M5,M6,M7) ! Output quality
*
*    Local variables :
*
      REAL                   DSUM, ADSUM         ! Sums over data
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
      LOGICAL                GOOD,ANYGOOD        ! Quality tests
*-

*    For axis 2 check on index J - output to (I,K,L,M,N,O)
      DO O = 1, L7
        DO N = 1, L6
          DO M = 1, L5
            DO L = 1, L4
              DO K = 1, L3
                DO I = 1, L1

*                Initialise
                  DSUM = 0.0
                  ADSUM = 0.0
                  GOOD = .TRUE.
                  ANYGOOD = .FALSE.

*                Sum the numerator and denominator
                  DO J = 1, L2
                    IF ( QFLAG ) THEN
                      GOOD = I_Q(I,J,K,L,M,N,O)
                    ELSE
                      GOOD = .TRUE.
                    END IF
                    IF ( GOOD ) THEN
                      DSUM = DSUM + I_D(I,J,K,L,M,N,O)
                      ADSUM = ADSUM + I_D(I,J,K,L,M,N,O)*I_A(J)
                      ANYGOOD = .TRUE.
                    END IF
                  END DO

*                Set output data
                  GOOD = ( (QFLAG.AND.ANYGOOD) .OR. .NOT. QFLAG )
                  IF ( GOOD ) THEN
                    O_D(I,K,L,M,N,O) = ADSUM / DSUM
                    IF ( QFLAG ) O_Q(I,K,L,M,N,O) = QUAL__GOOD
                  ELSE
                    O_D(I,K,L,M,N,O) = 0.0
                    O_Q(I,K,L,M,N,O) = QUAL__BAD
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END



*+ AXCENTROID_1 - Perform sum over axis 1
      SUBROUTINE AXCENTROID_1( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q,
     :                 QFLAG, M2, M3,M4,M5,M6,M7, O_D, O_Q )

*    Description :
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
*
*    Export :
*
      INTEGER                M2,M3,M4,M5,M6,M7   !
      REAL                   O_D(M2,M3,M4,M5,M6,M7) ! Output data
      BYTE                   O_Q(M2,M3,M4,M5,M6,M7) ! Output quality
*
*    Local variables :
*
      REAL                   DSUM, ADSUM         ! Sums over data
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
      LOGICAL                GOOD,ANYGOOD        ! Quality tests
*-

*    For axis 1 check on index I - output to (J,K,L,M,N,O)
      DO O = 1, L7
        DO N = 1, L6
          DO M = 1, L5
            DO L = 1, L4
              DO K = 1, L3
                DO J = 1, L2

*                Initialise
                  DSUM = 0.0
                  ADSUM = 0.0
                  GOOD = .TRUE.
                  ANYGOOD = .FALSE.

*                Sum the numerator and denominator
                  DO I = 1, L1
                    IF ( QFLAG ) THEN
                      GOOD = I_Q(I,J,K,L,M,N,O)
                    ELSE
                      GOOD = .TRUE.
                    END IF
                    IF ( GOOD ) THEN
                      DSUM = DSUM + I_D(I,J,K,L,M,N,O)
                      ADSUM = ADSUM + I_D(I,J,K,L,M,N,O)*I_A(I)
                      ANYGOOD = .TRUE.
                    END IF
                  END DO

*                Set output data
                  GOOD = ( (QFLAG.AND.ANYGOOD) .OR. .NOT. QFLAG )
                  IF ( GOOD ) THEN
                    O_D(J,K,L,M,N,O) = ADSUM / DSUM
                    IF ( QFLAG ) O_Q(J,K,L,M,N,O) = QUAL__GOOD
                  ELSE
                    O_D(J,K,L,M,N,O) = 0.0
                    O_Q(J,K,L,M,N,O) = QUAL__BAD
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END
