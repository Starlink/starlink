*+  AXCENTROID - Finds <brief title for application>
      SUBROUTINE AXCENTROID( STATUS )
*    Description :
*     <description of what the application does - for user info>
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the application works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     23 Mar 92 : V1.6-0  Original (DJA)
*     14 Oct 92 : V1.7-0  Major bug fix (DJA)
*     24 Nov 92 : V1.7-1  Now uses quality in 1-d case (DJA)
*     26 Sep 93 : V1.7-2  Bug fixed in quality handling (DJA)
*     28 Feb 94 : V1.7-3  Use BIT_ routines to do bit manipulations (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER        ILOC*(DAT__SZLOC)              ! Input dataset
      CHARACTER        ITYPE*(DAT__SZTYP)             ! Input dataset type
      CHARACTER        OLOC*(DAT__SZLOC)              ! Output dataset
      CHARACTER*40     UNITS                          ! Axis units

      REAL             AXCEN                          ! Channel weighted mean

      INTEGER          AXIS                           ! Axis for weighting
      INTEGER          DNDIM                          ! Dummy dimensionality
      INTEGER          DDIMS(DAT__MXDIM)              ! Dummy dimensions
      INTEGER          DIMS(DAT__MXDIM)               ! Input dimensions
      INTEGER          IAPTR                          ! Weighting axis data
      INTEGER          IAX, JAX                       ! Loops over axes
      INTEGER          IDPTR                          ! Input data values
      INTEGER          IQPTR                          ! Input quality values
      INTEGER          IVPTR                          ! Input variance values
      INTEGER          NAX                            ! Number of axes
      INTEGER          NDIM                           ! Input dimensionality
      INTEGER          ODIMS(DAT__MXDIM)              ! Output dimensions
      INTEGER          ODPTR                          ! Output data
      INTEGER          ONDIM                          ! Output dimensionality
      INTEGER          OQPTR                          ! Output quality
      INTEGER          OVPTR                          ! Output variance

      BYTE             MASK                           ! Input quality mask
      BYTE             OQUAL                          ! Output quality point

      LOGICAL          OK                             ! Validity test
      LOGICAL          PRIM                           ! Input primitive
      LOGICAL          QOK                            ! Input quality there?
      LOGICAL          VOK                            ! Input variance there?
*
*    Version :
*

      CHARACTER*30     VERSION
        PARAMETER      (VERSION = 'AXCENTROID Version 1.7-3' )
*-

*    Version id
      CALL MSG_PRNT( VERSION )

*    Initialise
      CALL AST_INIT()

*    Get input
      CALL USI_ASSOCI( 'INP', 'READ', ILOC, PRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check data
      CALL BDA_CHKDATA( ILOC, OK, NDIM, DIMS, STATUS )
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
      CALL BDA_CHKQUAL( ILOC, QOK, DNDIM, DDIMS, STATUS )
      CALL BDA_CHKVAR( ILOC, VOK, DNDIM, DDIMS, STATUS )

*    Select axis
      IF ( NDIM .EQ. 1 ) THEN
        AXIS = 1
      ELSE

*      List axes
        CALL AXIS_LIST( ILOC, NDIM, STATUS )

*      Select axis
        CALL PAR_GET0I( 'AXIS', AXIS, STATUS )
        IF ( (AXIS.LT.1) .OR. (AXIS.GT.NDIM) ) THEN
          CALL MSG_PRNT( '! Invalid axis number' )
          STATUS = SAI__ERROR
        END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map input data
      CALL BDA_MAPDATA( ILOC, 'READ', IDPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Use axis values or channels
      CALL BDA_CHKAXES( ILOC, NAX, STATUS )
      IF ( NAX .GT. 0 ) THEN
        CALL BDA_MAPAXVAL( ILOC, 'READ', AXIS, IAPTR, STATUS )
      ELSE
        CALL DYN_MAPR( 1, DIMS(AXIS), IAPTR, STATUS )
        CALL ARR_REG1R( 1.0, 1.0, DIMS(AXIS), %VAL(IAPTR), STATUS )
      END IF

*    Map input quality
      IF ( QOK ) THEN
        CALL BDA_GETMASK( ILOC, MASK, STATUS )
        CALL BDA_MAPQUAL( ILOC, 'READ', IQPTR, STATUS )
      END IF

*    Create output file?
      IF ( NDIM .GT. 1 ) THEN

*      Associate object
        CALL DAT_TYPE( ILOC, ITYPE, STATUS )
        CALL USI_ASSOCO( 'OUT', ITYPE, OLOC, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Construct dimensions
        JAX = 1
        DO IAX = 1, NDIM
          IF ( IAX .NE. AXIS ) THEN
            ODIMS(JAX) = DIMS(IAX)
            JAX = JAX + 1
          END IF
        END DO
        ONDIM = NDIM - 1

*      Create output data
        CALL BDA_CREDATA( OLOC, ONDIM, ODIMS, STATUS )
        CALL BDA_MAPDATA( OLOC, 'WRITE', ODPTR, STATUS )

*      Create quality if present in input
        IF ( QOK ) THEN
          CALL BDA_CREQUAL( OLOC, ONDIM, ODIMS, STATUS )
          CALL BDA_PUTMASK( OLOC, MASK, STATUS )
          CALL BDA_MAPQUAL( ILOC, 'READ', IQPTR, STATUS )
          CALL BDA_MAPQUAL( OLOC, 'WRITE', OQPTR, STATUS )
        END IF

*      Create variance if present in input
C        IF ( VOK ) THEN
C          CALL BDA_CREVAR( OLOC, ONDIM, ODIMS, STATUS )
C          CALL BDA_MAPVAR( ILOC, 'READ', IVPTR, STATUS )
C          CALL BDA_MAPVAR( OLOC, 'WRITE', OVPTR, STATUS )
C        END IF
        VOK = .FALSE.

*      Copy stuff from input
        IF ( .NOT. PRIM ) THEN

*        Copy axes
          IF ( NAX .GT. 0 ) THEN
            JAX = 1
            DO IAX = 1, NAX
              IF ( IAX .NE. AXIS ) THEN
                CALL BDA_COPAXIS( ILOC, OLOC, IAX, JAX, STATUS )
                JAX = JAX + 1
              END IF
            END DO
          END IF

*        Ancillary bits
          CALL HIST_COPY( ILOC, OLOC, STATUS )
          CALL BDA_COPMORE( ILOC, OLOC, STATUS )
          CALL BDA_COPTEXT( ILOC, OLOC, STATUS )

        END IF

*    Reset flags if scalar output
      ELSE
        VOK = .FALSE.

      END IF

*    Pad dimensions to 7D
      DO IAX = NDIM + 1, DAT__MXDIM
        DIMS(IAX) = 1
      END DO
      DO IAX = ONDIM + 1, DAT__MXDIM
        ODIMS(IAX) = 1
      END DO

*    Act on data
      IF ( NDIM .GT. 1 ) THEN
        CALL AXCENTROID_INT( DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                     DIMS(5), DIMS(6), DIMS(7), AXIS,
     :                     %VAL(IAPTR), %VAL(IDPTR), QOK, MASK,
     :                     %VAL(IQPTR), VOK, %VAL(IVPTR),
     :                     ODIMS(1), ODIMS(2), ODIMS(3), ODIMS(4),
     :                     ODIMS(5), ODIMS(6), ODIMS(7),
     :                     %VAL(ODPTR), %VAL(OQPTR), %VAL(OVPTR),
     :                     STATUS )
      ELSE
        CALL AXCENTROID_INT( DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                     DIMS(5), DIMS(6), DIMS(7), AXIS,
     :                     %VAL(IAPTR), %VAL(IDPTR), QOK, MASK,
     :                     %VAL(IQPTR), VOK, 0, 1, 1, 1, 1, 1, 1, 1,
     :                     AXCEN, OQUAL, 0, STATUS )

        IF ( NAX .GT. 0 ) THEN
          CALL BDA_GETAXUNITS( ILOC, AXIS, UNITS, STATUS )
          CALL MSG_SETC( 'UNIT', UNITS )
        ELSE
          CALL MSG_SETC( 'UNIT', 'pixels' )
        END IF
        CALL MSG_SETI( 'AX', AXIS )
        CALL MSG_SETR( 'CEN', AXCEN )
        CALL MSG_PRNT( 'Centroid wrt axis ^AX is ^CEN ^UNIT.' )
      END IF

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  AXCENTROID_INT - Peform ratioing on 7D data sets
      SUBROUTINE AXCENTROID_INT( L1,L2,L3,L4,L5,L6,L7,AXIS,
     :                         I_A, I_D, QFLAG, MASK,I_Q, VFLAG, I_V,
     :                         M1,M2,M3,M4,M5,M6,M7, O_D, O_Q, O_V,
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
      INCLUDE 'DAT_PAR'
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
      BYTE                   MASK                ! Quality mask

      LOGICAL                VFLAG               ! Use variance?
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input qual
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      REAL                   I_V(L1,L2,L3,L4,L5,L6,L7) ! Input vari
      INTEGER                M1,M2,M3,M4,M5,M6,M7!
*
*    Export :
*
      BYTE                   O_Q(M1,M2,M3,M4,M5,M6,M7) ! Output quality
      REAL                   O_D(M1,M2,M3,M4,M5,M6,M7) ! Output data
      REAL                   O_V(M1,M2,M3,M4,M5,M6,M7) ! Output variance
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Accumulate the data from I_D into W_D and O_D
      IF ( AXIS .EQ. 7 ) THEN
        CALL AXCENTROID_7( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, VFLAG, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      ELSE IF ( AXIS .EQ. 6 ) THEN
        CALL AXCENTROID_6( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, VFLAG, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      ELSE IF ( AXIS .EQ. 5 ) THEN
        CALL AXCENTROID_5( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, VFLAG, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      ELSE IF ( AXIS .EQ. 4 ) THEN
        CALL AXCENTROID_4( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, VFLAG, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      ELSE IF ( AXIS .EQ. 3 ) THEN
        CALL AXCENTROID_3( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, VFLAG, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      ELSE IF ( AXIS .EQ. 2 ) THEN
        CALL AXCENTROID_2( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, VFLAG, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      ELSE IF ( AXIS .EQ. 1 ) THEN
        CALL AXCENTROID_1( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q, QFLAG,
     :                     MASK, VFLAG, M1,M2,M3,M4,M5,M6, O_D, O_Q )

      END IF

      END




*+ AXCENTROID_7 - Perform sum over axis 7
      SUBROUTINE AXCENTROID_7( L1,L2,L3,L4,L5,L6,L7, I_A, I_D, I_Q,
     :                 QFLAG, MASK,VFLAG, M1,M2,M3,M4,M5,M6, O_D, O_Q )

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
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      LOGICAL                VFLAG               ! Use variance

      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      BYTE                   I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
      BYTE                   MASK                ! Quality mask
*
*    Export :
*
      INTEGER                M1,M2,M3,M4,M5,M6   !
      REAL                   O_D(M1,M2,M3,M4,M5,M6) ! Output data
      BYTE                   O_Q(M1,M2,M3,M4,M5,M6) ! Output quality
*
*    Functions :
*
      BYTE		     BIT_ANDUB
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
                      GOOD = BIT_ANDUB(I_Q(I,J,K,L,M,N,O),MASK)
     :                                .EQ.QUAL__GOOD
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
     :                 QFLAG, MASK,VFLAG, M1,M2,M3,M4,M5,M7, O_D, O_Q )

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
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      LOGICAL                VFLAG               ! Use variance

      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      BYTE                   I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
      BYTE                   MASK                ! Quality mask
*
*    Export :
*
      INTEGER                M1,M2,M3,M4,M5,M7   !
      REAL                   O_D(M1,M2,M3,M4,M5,M7) ! Output data
      BYTE                   O_Q(M1,M2,M3,M4,M5,M7) ! Output quality
*
*    Functions :
*
      BYTE		     BIT_ANDUB
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
                      GOOD = BIT_ANDUB(I_Q(I,J,K,L,M,N,O),MASK)
     :                                .EQ.QUAL__GOOD
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
     :                 QFLAG, MASK,VFLAG, M1,M2,M3,M4,M6,M7, O_D, O_Q )

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
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      LOGICAL                VFLAG               ! Use variance

      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      BYTE                   I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
      BYTE                   MASK                ! Quality mask
*
*    Export :
*
      INTEGER                M1,M2,M3,M4,M6,M7   !
      REAL                   O_D(M1,M2,M3,M4,M6,M7) ! Output data
      BYTE                   O_Q(M1,M2,M3,M4,M6,M7) ! Output quality
*
*    Functions :
*
      BYTE		     BIT_ANDUB
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
                      GOOD = BIT_ANDUB(I_Q(I,J,K,L,M,N,O),MASK)
     :                                .EQ.QUAL__GOOD
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
     :                 QFLAG, MASK,VFLAG, M1,M2,M3,M5,M6,M7, O_D, O_Q )

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
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      LOGICAL                VFLAG               ! Use variance

      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      BYTE                   I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
      BYTE                   MASK                ! Quality mask
*
*    Export :
*
      INTEGER                M1,M2,M3,M5,M6,M7   !
      REAL                   O_D(M1,M2,M3,M5,M6,M7) ! Output data
      BYTE                   O_Q(M1,M2,M3,M5,M6,M7) ! Output quality
*
*    Functions :
*
      BYTE		     BIT_ANDUB
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
                      GOOD = BIT_ANDUB(I_Q(I,J,K,L,M,N,O),MASK)
     :                                .EQ.QUAL__GOOD
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
     :                 QFLAG, MASK,VFLAG, M1,M2,M4,M5,M6,M7, O_D, O_Q )

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
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      LOGICAL                VFLAG               ! Use variance

      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      BYTE                   I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
      BYTE                   MASK                ! Quality mask
*
*    Export :
*
      INTEGER                M1,M2,M4,M5,M6,M7   !
      REAL                   O_D(M1,M2,M4,M5,M6,M7) ! Output data
      BYTE                   O_Q(M1,M2,M4,M5,M6,M7) ! Output quality
*
*    Functions :
*
      BYTE		     BIT_ANDUB
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
                      GOOD = BIT_ANDUB(I_Q(I,J,K,L,M,N,O),MASK)
     :                                .EQ.QUAL__GOOD
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
     :                 QFLAG, MASK,VFLAG, M1,M3,M4,M5,M6,M7, O_D, O_Q )

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
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      LOGICAL                VFLAG               ! Use variance

      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      BYTE                   I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
      BYTE                   MASK                ! Quality mask
*
*    Export :
*
      INTEGER                M1,M3,M4,M5,M6,M7   !
      REAL                   O_D(M1,M3,M4,M5,M6,M7) ! Output data
      BYTE                   O_Q(M1,M3,M4,M5,M6,M7) ! Output quality
*
*    Functions :
*
      BYTE		     BIT_ANDUB
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
                      GOOD = BIT_ANDUB(I_Q(I,J,K,L,M,N,O),MASK)
     :                                .EQ.QUAL__GOOD
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
     :                 QFLAG, MASK,VFLAG, M2, M3,M4,M5,M6,M7, O_D, O_Q )

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
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Input :
*
      LOGICAL                QFLAG               ! Use quality
      LOGICAL                VFLAG               ! Use variance

      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      REAL                   I_A(*)              ! Axis values
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      BYTE                   I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input quality
      BYTE                   MASK                ! Quality mask
*
*    Export :
*
      INTEGER                M2,M3,M4,M5,M6,M7   !
      REAL                   O_D(M2,M3,M4,M5,M6,M7) ! Output data
      BYTE                   O_Q(M2,M3,M4,M5,M6,M7) ! Output quality
*
*    Functions :
*
      BYTE		     BIT_ANDUB
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
                      GOOD = BIT_ANDUB(I_Q(I,J,K,L,M,N,O),MASK)
     :                                .EQ.QUAL__GOOD
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
