*+  OPERATE Operates (e.g. with LOG10) on a data array class object
      SUBROUTINE OPERATE( STATUS )
*
*    Description :
*
*     Acts with an operator on a data array class object, and recalculates
*     any errors using            sig[f(x)]=(df/dx)*sig[x] .
*     (If in doubt about form of df/dx see CRC Maths Tables, classmark QA47)
*     If data is negative and LOG or LOG10 is used, then data is set to 1.E-38
*     and error such that when added to data it will have value plus 1 sigma.
*     The LABEL (if present) is amended to reflect the operation
*     performed, and all other components are unchanged.
*     If input is primitive, prompts for error. If none, output is primitive.
*     In all other cases output is a structure. If parameter OVER is TRUE
*     (default is NO, parameter is ONLY entered through command line) then
*     input object is overwritten.
*     Code which must be changed when further operations are added is marked
*     with "!!".
*     Currently supported operations are: '-', LOG, LOG10, SQRT, EXP, 10**  !!
*
*    Parameters :
*
*     INP=UNIV(R)
*         Input data object
*     OUT=UNIV(U)
*         Output data object
*     ERR=UNIV(R)
*          Error for primitive input
*     OPER=CHAR(R)
*         Operation to be performed
*     OVER=LOGICAL(R)
*          Overwrite existing file?
*
*    Method :
*    Deficiencies :
*
*     Inadequate treatment of LOG errors for non-positive data
*     Difficulties with overwriting primitives by structures and vice-versa
*     -this is avoided by simply not allowing overwrites to be attempted when
*      they are known to fail.
*
*    Bugs :
*
*    Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*
*    History :
*
*     13 Mar 86 : Original
*      2 Mar 86 : Avoid enormous data error entries when taking logs (BHVAD::GKS)
*      7 Mar 86 : More explosion proofing (BHVAD::GKS)
*     25 Feb 87 : Add '-', LOG, EXP and 10**
*                 Fix fault in LOG10 errors (BHVAD::ADM)
*     10 Mar 87 : Adapted to handle primitive input/output (BHVAD::ADM)
*     16 Mar 87 : Modified to handle problems overwriting existing objects
*                    (BHVAD::ADM)
*     27 Jun 88 : V1.0-1  ASTERIX88 (ADM)
*     23 May 91 : V1.5-0  Code tidied (DJA)
*     26 Jul 93 : V1.7-0  Use PRM constants for extreme data values. Fixed
*                         fault in LOG10 errors (probably introduced 25/2/87!)
*                         (DJA).
*     25 Feb 94 : V1.7-1  Use BIT_ routines to do bit manipulations (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     28 Mar 95 : V1.8-1 Use new data interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    External references :
*
      INTEGER CHR_LEN
*
*    Local variables :
*
      CHARACTER*200          	FILE(4)               ! Input file spec
      CHARACTER*200          	FILEO(4)              ! Output file spec
      CHARACTER*6            	OPER                  ! Operator
      CHARACTER*80           	LABEL                 ! Data label
      CHARACTER*80           	STRING                ! Temp. String

      DOUBLE PRECISION       	EVALUE                ! Manual error estimate

      INTEGER                	DIMS(ADI__MXDIM)      ! Dimensions of data array
      INTEGER                	DPTR                  ! Output data array
      INTEGER			IFID		      ! Input dataset id
      INTEGER                LEVELS                   ! Levels of input
      INTEGER                LEVELSO                  ! Levels of Output
      INTEGER                NDIM                     ! # dimensions
      INTEGER                NELM                     ! # input data values
      INTEGER			OFID		      ! Output dataset id
      INTEGER                	QPTR                  ! Output quality
      INTEGER                	TDIMS(ADI__MXDIM)     ! Dimensions
      INTEGER                	TNDIM                 ! # dimensions
      INTEGER                	VPTR                  ! Output variance

      BYTE                   MASK                     ! Quality mask

      LOGICAL                ERR                      ! Data errors present?
      LOGICAL                INPUT                    ! Controls operator input
      LOGICAL                OK                       ! Component present and defined?
      LOGICAL                OVER                     ! Input object to be overwritten?
      LOGICAL                PRIM                     ! Input is primitive?
      LOGICAL                QOK                      ! QUALITY present?
*
*    Version :
*
      CHARACTER*26           VERSION
        PARAMETER         ( VERSION = 'OPERATE Version 1.8-1' )
*-

*    Version
      CALL MSG_PRNT( VERSION )

*    Initialise
      CALL AST_INIT

*    Check for overwriting
      CALL USI_GET0L( 'OVER', OVER, STATUS )

*    Get input object
      IF ( OVER ) THEN
        CALL USI_TASSOCI( 'INP', '*', 'UPDATE', OFID, STATUS )
      ELSE
        CALL USI_TASSOC2( 'INP', 'OUT', 'READ', IFID, OFID, STATUS )
        CALL ADI_FCOPY( IFID, OFID, STATUS )
        CALL USI_NAMEO( LEVELSO, FILEO, STATUS )
      END IF
      CALL USI_NAMEI( LEVELS, FILE, STATUS )
      CALL BDI_PRIM( OFID, PRIM, STATUS )

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map the data
      CALL BDI_CHKDATA( OFID, OK, NDIM, DIMS, STATUS )
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )
      IF ( OK ) THEN
        CALL BDI_MAPTDATA( OFID, '_DOUBLE', 'UPDATE', DPTR, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid data', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    If input is primitive, get error estimate
      IF ( PRIM ) THEN

        CALL USI_GET0D( 'ERR', EVALUE, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          ERR = .TRUE.
          CALL DYN_MAPD( 1, NELM, VPTR, STATUS )
          CALL ARR_INIT1R( EVALUE**2, NELM, %VAL(VPTR), STATUS )
        ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
          CALL ERR_ANNUL( STATUS )
          ERR = .FALSE.
        END IF
        QOK = .FALSE.

      ELSE

*      Get error if present
        CALL BDI_CHKVAR ( OFID, ERR, TNDIM, TDIMS, STATUS )
        IF ( ERR ) THEN
          CALL BDI_MAPTVAR( OFID, '_DOUBLE', 'UPDATE', VPTR, STATUS )
        END IF

*      And quality
        CALL BDI_CHKQUAL( OFID, QOK, TNDIM, TDIMS, STATUS )
        IF ( QOK ) THEN
          CALL BDI_MAPQUAL( OFID, 'UPDATE', QPTR, STATUS )
          CALL BDI_GETMASK( OFID, MASK, STATUS )
        ELSE
          CALL BDI_CREQUAL( OFID, NDIM, DIMS, STATUS )
          CALL BDI_MAPQUAL( OFID, 'WRITE', QPTR, STATUS )
          CALL ARR_INIT1B( QUAL__GOOD, NELM, %VAL(QPTR), STATUS )
          CALL BDI_PUTMASK( OFID, QUAL__MASK, STATUS )
          MASK = QUAL__MASK
        END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      CALL MSG_SETI( 'NDAT', NELM )
      CALL MSG_PRNT( '^NDAT data points to be processed' )

*    Get operation and check validity
      INPUT = .TRUE.

      DO WHILE ( INPUT )
        CALL USI_GET0C( 'OPER', OPER, STATUS )
        CALL CHR_UCASE( OPER )

        IF ( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
          GOTO 99

        ELSE IF ( OPER(1:1) .EQ. '-' )     THEN
          INPUT = .FALSE.
        ELSE IF ( OPER(1:3) .EQ. 'ABS' )   THEN
          INPUT = .FALSE.
        ELSE IF ( OPER(1:5) .EQ. 'LOG10' ) THEN
          INPUT = .FALSE.
        ELSE IF ( OPER(1:3) .EQ. 'LOG' )   THEN
          INPUT = .FALSE.
        ELSE IF ( OPER(1:4) .EQ. 'SQRT' )  THEN
          INPUT = .FALSE.
        ELSE IF ( OPER(1:3) .EQ. 'EXP' )   THEN
          INPUT = .FALSE.
        ELSE IF ( OPER(1:4) .EQ. '10**' )  THEN
          INPUT = .FALSE.
        ELSE
          CALL MSG_SETC( 'OP', OPER )
          CALL MSG_PRNT( 'The operation /^OP/ is not available' )
          CALL USI_CANCL( 'OPER', STATUS )

        END IF
      END DO

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Perform the operations
      IF ( OPER(1:1) .EQ. '-' ) THEN
        CALL OPERATE_MINUS( NELM, %VAL(DPTR), STATUS )

      ELSE IF ( OPER(1:3) .EQ. 'ABS' )   THEN
        CALL OPERATE_ABS( NELM, %VAL(DPTR), STATUS )

      ELSE IF ( OPER(1:5) .EQ. 'LOG10' ) THEN
        CALL OPERATE_LOG10( ERR, QOK, NELM, %VAL(DPTR), %VAL(VPTR),
     :                                   MASK, %VAL(QPTR), STATUS )

      ELSE IF ( OPER(1:3) .EQ. 'LOG' )   THEN
        CALL OPERATE_LOG( ERR, QOK, NELM, %VAL(DPTR), %VAL(VPTR),
     :                                 MASK, %VAL(QPTR), STATUS )

      ELSE IF ( OPER(1:4) .EQ. 'SQRT' )  THEN
        CALL OPERATE_SQRT( ERR, QOK, NELM, %VAL(DPTR), %VAL(VPTR),
     :                                  MASK, %VAL(QPTR), STATUS )

      ELSE IF ( OPER(1:3) .EQ. 'EXP' )   THEN
        CALL OPERATE_EXP( ERR, QOK, NELM, %VAL(DPTR), %VAL(VPTR),
     :                               MASK, %VAL(QPTR), STATUS )

      ELSE IF ( OPER(1:4) .EQ. '10**' )  THEN
        CALL OPERATE_ANTILOG10( ERR, QOK, NELM, %VAL(DPTR), %VAL(VPTR),
     :                                       MASK, %VAL(QPTR), STATUS )

      END IF

*    Amend data label if present
      IF ( .NOT. PRIM ) THEN
        CALL BDI_GETLABEL( OFID, LABEL, STATUS )
        STRING = LABEL(1:CHR_LEN(LABEL))
        LABEL  = OPER(1:CHR_LEN(OPER))//' '//STRING
        STRING = ':'//LABEL(1:CHR_LEN(LABEL))//':'
        CALL BDI_PUTLABEL( OFID, STRING, STATUS )
      END IF

*    History
      IF ( .NOT. PRIM ) THEN
        CALL HSI_ADD( OFID, VERSION, STATUS )
        CALL HSI_PTXT( OFID, LEVELS, FILE, STATUS )
        IF ( .NOT. OVER ) THEN
          CALL HSI_PTXT(OFID,LEVELSO,FILEO,STATUS)
        END IF
        CALL HSI_PTXT( OFID, 1, 'Operator = '//OPER(:CHR_LEN(OPER)),
     :                 STATUS )
      END IF

*    Release output
      IF ( .NOT. OVER ) CALL BDI_RELEASE( IFID, STATUS )
      CALL BDI_RELEASE( OFID, STATUS )
      IF ( PRIM .AND. ERR ) THEN
        CALL DYN_UNMAP( VPTR, STATUS )
      END IF

*    Tidy up and exit
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END



*+  OPERATE_MINUS - Change sign of data values
      SUBROUTINE OPERATE_MINUS( NDAT, DATA, STATUS )
*    Description :
*     DATA(I) = - DATA(I)
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews (PLA_AST88@uk.ac.bham.sr.star)
*    History :
*     18/18/88:  Original (PLA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER                NDAT                     ! Number of data points
*    Import-Export :
      DOUBLE PRECISION       DATA(NDAT)               ! Data values
*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER                I
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, NDAT
         DATA(I) = - DATA(I)
      END DO

      END



*+  OPERATE_ABS - Absolute value
      SUBROUTINE OPERATE_ABS( NDAT, DATA, STATUS )
*    Description :
*     DATA(I) = |DATA(I)|
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews (PLA_AST88@uk.ac.bham.sr.star)
*    History :
*     18/18/88:  Original (PLA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER                NDAT                     ! Number of data points
*    Import-Export :
      DOUBLE PRECISION       DATA(NDAT)               ! Data values
*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER                I
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, NDAT
         DATA(I) = ABS(DATA(I))
      END DO
      END



*+  OPERATE_LOG10 - Log to the base 10
      SUBROUTINE OPERATE_LOG10( VAROK, QOK, NDAT, DATA, VAR, MASK, QUAL,
     :                                                          STATUS )
*    Description :
*     DATA(I) = LOG10(DATA(I))
*     VAR(I)  = LOG10(e)**2 * (1.0/DATA(I)**2) * VAR(I)
*     QUAL(I) = QUAL(I) for DATA(I) > 0.0
*     QUAL(I) = QUAL(I) + <value representing arithmetic error=2> for DATA(I) =< 0.0
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews ( PLA_AST88@uk.ac.bham.sr/star)
*    History :
*     18/10/88:  original (PLA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'
*    Import :
      LOGICAL                VAROK                    ! Variance OK?
      LOGICAL                QOK                      ! Quality OK?

      INTEGER                NDAT                     ! Number of data points
*    Import-Export :
      DOUBLE PRECISION       DATA(NDAT)               ! Data values
      DOUBLE PRECISION       VAR(*)                   ! Variance values

      BYTE                   MASK                     ! Quality mask
      BYTE                   QUAL(NDAT)               ! Quality values
*    Status :
      INTEGER                STATUS
*    Functions :
      BYTE		     BIT_ORUB
*    Local constants :
      DOUBLE PRECISION       C                        ! C = ( LOG10(e)**2 )
         PARAMETER         ( C = 0.1886116969479110 )
*    Local variables :
      INTEGER                I
      INTEGER                OLDBADQUAL
      INTEGER                NEWBADQUAL
      INTEGER                BAD
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      OLDBADQUAL = 0
      NEWBADQUAL = 0
      BAD        = 0

      DO I = 1, NDAT
        IF ( QOK ) THEN
          IF ( QUAL(I) .EQ. 0 ) THEN
            IF ( DATA(I) .GT. 0.0 ) THEN
              IF ( VAROK ) THEN
                VAR(I) = VAR(I) * C / DATA(I)**2
              END IF
              DATA(I) = LOG10( DATA(I) )

            ELSE
              NEWBADQUAL = NEWBADQUAL + 1
              QUAL(I)    = BIT_ORUB(QUAL(I),QUAL__ARITH)

            END IF
          ELSE
            OLDBADQUAL =OLDBADQUAL + 1

          END IF
        ELSE
          IF ( DATA(I) .GT. 0.0 ) THEN
            DATA(I) = LOG10( DATA(I) )

          ELSE
            BAD     = BAD + 1
            DATA(I) = VAL__MIND

          END IF
        END IF
      END DO

      IF ( OLDBADQUAL .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', OLDBADQUAL )
        CALL MSG_PRNT( '^BAD data points excluded by QUALITY' )
      END IF

      IF ( NEWBADQUAL .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', NEWBADQUAL )
        CALL MSG_PRNT( '^BAD NEW bad quality points (data <= 0)' )
      END IF

      IF ( BAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', BAD )
        CALL MSG_PRNT( '^BAD data points set to -1.7E38 (data <= 0)' )
      END IF

      END



*+  OPERATE_LOG - Natural Log
      SUBROUTINE OPERATE_LOG( VAROK, QOK, NDAT, DATA, VAR,
     :                                 MASK, QUAL, STATUS)
*    Description :
*     DATA(I) = LOG(DATA(I))
*     VAR(I)  = ( 1.0/DATA(I) )**2 * VAR(I)
*     QUAL(I) = QUAL(I) for DATA(I) > 0.0
*     QUAL(I) = QUAL(I) + <value representing arithmetic error=2> for DATA(I) =< 0.0
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews ( PLA_AST88@uk.ac.bham.sr/star)
*    History :
*     18/10/88:  original (PLA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'
*    Import :
      LOGICAL                QOK                      ! Quality OK?
      LOGICAL                VAROK                    ! Variance OK?

      INTEGER                NDAT                     ! Number of data points
*    Import-Export :
      DOUBLE PRECISION       DATA(NDAT)               ! Data values
      DOUBLE PRECISION       VAR(*)                   ! Variance values

      BYTE                   MASK                     ! Quality mask
      BYTE                   QUAL(NDAT)               ! Quality values
*    Status :
      INTEGER                STATUS
*    Functions :
      BYTE		     BIT_ORUB
*    Local variables :
      INTEGER                I
      INTEGER                OLDBADQUAL
      INTEGER                NEWBADQUAL
      INTEGER                BAD
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      OLDBADQUAL = 0
      NEWBADQUAL = 0
      BAD        = 0

      IF ( QOK ) THEN
        DO I = 1, NDAT
          IF ( QUAL(I) .EQ. 0 ) THEN
            IF ( DATA(I) .GT. 0.0 ) THEN
              IF ( VAROK ) THEN
                VAR(I) = VAR(I) / ( DATA(I)**2 )
              END IF
              DATA(I) = LOG( DATA(I) )

            ELSE
              NEWBADQUAL = NEWBADQUAL + 1
              QUAL(I)    = BIT_ORUB(QUAL(I),QUAL__ARITH)

            END IF
          ELSE
            OLDBADQUAL = OLDBADQUAL + 1

          END IF
        END DO
      ELSE
        DO I = 1, NDAT
          IF ( DATA(I) .GT. 0.0 ) THEN
            DATA(I) = LOG( DATA(I) )

          ELSE
            BAD     = BAD + 1
            DATA(I) = VAL__MIND

          END IF
        END DO
      END IF

      IF ( OLDBADQUAL .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', OLDBADQUAL )
        CALL MSG_PRNT( '^BAD data points excluded by QUALITY' )
      END IF

      IF ( NEWBADQUAL .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', NEWBADQUAL )
        CALL MSG_PRNT( '^BAD NEW bad quality points (data <= 0)' )
      END IF

      IF ( BAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', BAD )
        CALL MSG_PRNT( '^BAD data points set to -1.7E38 (data <= 0)' )
      END IF

      END



*+  OPERATE_SQRT - Square root
      SUBROUTINE OPERATE_SQRT(VAROK, QOK, NDAT, DATA, VAR, MASK,
     :                                             QUAL, STATUS)
*    Description :
*     DATA(I) = SQRT(DATA(I)) if DATA(I) > 0.0
*     DATA(I) = 0.0 otherwise
*     VAR(I)  = VAR(I) / ( 4.0 * DATA(I) ) if DATA(I) > 0.0
*     VAR(I)  = 2.25 * VAR(I) otherwise. ( = ( 3*error/2 )**2 )
*     QUAL(I) = QUAL(I) for DATA(I) > 0.0
*     QUAL(I) = QUAL(I) + <value representing arithmetic error=2> for DATA(I) =< 0.0
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews ( PLA_AST88@uk.ac.bham.sr/star)
*    History :
*     18/10/88:  original (PLA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      LOGICAL                QOK                      ! Quality OK?
      LOGICAL                VAROK                    ! Variance OK?

      INTEGER                NDAT                     ! Number of data points
*    Import-Export :
      DOUBLE PRECISION       DATA(NDAT)               ! Data values
      DOUBLE PRECISION       VAR(*)                   ! Variance values

      BYTE                   MASK                     ! Quality mask
      BYTE                   QUAL(NDAT)               ! Quality values
*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER                I
      INTEGER                OLDBADQUAL
      INTEGER                BAD

      DOUBLE PRECISION       TEMP
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      OLDBADQUAL = 0
      BAD        = 0

      IF ( QOK ) THEN
        DO I = 1, NDAT
          IF ( QUAL(I) .EQ. 0 ) THEN
            IF ( DATA(I) .GT. 0.0 ) THEN
              IF ( VAROK ) THEN
                VAR(I) = VAR(I) / ( 4.0D0 * DATA(I) )

              END IF
              DATA(I) = SQRT( DATA(I) )

            ELSE
              IF ( VAROK ) THEN
                TEMP = DATA(I) + ( VAR(I) / ( 4.0D0 * ABS(DATA(I)) ) )

                IF ( TEMP .GT. 0 ) THEN
                  VAR(I) = TEMP

                ELSE
                  VAR(I) = 2.25D0 * VAR(I)

                END IF
              END IF
              DATA(I) = 0.0D0
              BAD     = BAD + 1

            END IF
          ELSE
            OLDBADQUAL = OLDBADQUAL + 1

          END IF
        END DO
      ELSE
        DO I = 1, NDAT
          IF ( DATA(I) .GT. 0.0 ) THEN
            DATA(I) = SQRT(DATA(I))

          ELSE
            DATA(I) = 0.0D0
            BAD     = BAD + 1

          END IF
        END DO
      END IF

      IF ( OLDBADQUAL .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', OLDBADQUAL )
        CALL MSG_PRNT( '^BAD data points excluded by QUALITY' )
      END IF

      IF ( BAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', BAD )
        CALL MSG_PRNT( '^BAD data points set to 0 (data <= 0)' )
      END IF

      END



*+  OPERATE_EXP - e**(DATA)
      SUBROUTINE OPERATE_EXP( VAROK, QOK, NDAT, DATA, VAR, MASK,
     :                                             QUAL, STATUS)
*    Description :
*     DATA(I) = EXP(DATA(I)) for DATA(I) < 88.0288
*     VAR(I)  = VAR(I) * EXP( 2.0 * DATA(I) ) for DATA(I) < 40
*     QUAL(I) = QUAL(I) for DATA(I) > 0.0
*     QUAL(I) = QUAL(I) + <value representing arithmetic error=2> for DATA(I) =< 0.0
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews ( PLA_AST88@uk.ac.bham.sr/star)
*    History :
*     18/10/88:  original (PLA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'
*    Import :
      LOGICAL                QOK                      ! Quality OK?
      LOGICAL                VAROK                    ! Variance OK?

      INTEGER                NDAT                     ! Number of data points
*    Import-Export :
      DOUBLE PRECISION       DATA(NDAT)               ! Data values
      DOUBLE PRECISION       VAR(*)                   ! Variance values

      BYTE                   MASK                     ! Quality mask
      BYTE                   QUAL(NDAT)               ! Quality values
*    Status :
      INTEGER                STATUS
*    Functions :
      BYTE		     BIT_ORUB
*    Local constants :
      DOUBLE PRECISION       MAX1
         PARAMETER         ( MAX1 = 88.0288D0 )
      DOUBLE PRECISION       MAX2
         PARAMETER         ( MAX2 = 1.3D19 )
*    Local variables :
      INTEGER                I
      INTEGER                OLDBADQUAL
      INTEGER                NEWBADQUAL
      INTEGER                BAD
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      OLDBADQUAL = 0
      NEWBADQUAL = 0
      BAD        = 0

      IF ( QOK ) THEN
        DO I = 1, NDAT
          IF ( QUAL(I) .EQ. 0 ) THEN
            IF ( DATA(I) .LE. MAX1 ) THEN
              DATA(I) = EXP( DATA(I) )

              IF ( VAROK ) THEN
                IF ( DATA(I) .LT. MAX2 / SQRT(VAR(I)) ) THEN
                  VAR(I) = VAR(I) * DATA(I) * DATA(I)

                ELSE
                  NEWBADQUAL = NEWBADQUAL + 1
                  QUAL(I)    = BIT_ORUB(QUAL(I),QUAL__ARITH)

                END IF
              END IF
            ELSE
              NEWBADQUAL = NEWBADQUAL + 1
              QUAL(I)    = BIT_ORUB(QUAL(I),QUAL__ARITH)

            END IF
          ELSE
            OLDBADQUAL = OLDBADQUAL + 1

          END IF
        END DO
      ELSE
        DO I = 1, NDAT
          IF ( DATA(I) .LE. MAX1 ) THEN
            DATA(I) = EXP( DATA(I) )

          ELSE
            DATA(I) = VAL__MAXD
            BAD = BAD + 1

          END IF
        END DO
      END IF

      IF ( OLDBADQUAL .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', OLDBADQUAL )
        CALL MSG_PRNT( '^BAD data points excluded by QUALITY' )
      END IF

      IF ( NEWBADQUAL .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', NEWBADQUAL )
        CALL MSG_PRNT( '^BAD NEW bad quality points (too big)' )
      END IF

      IF ( BAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', BAD )
        CALL MSG_PRNT( '^BAD data points set to 1.7E38 '//
     :                                  '(data too big)' )
      END IF

      END



*+  OPERATE_ANTILOG10 - ANTILOG base 10
      SUBROUTINE OPERATE_ANTILOG10( VAROK, QOK, NDAT, DATA, VAR, MASK,
     :                                                  QUAL, STATUS )
*    Description :
*     DATA(I) = 10**(DATA(I))
*     VAR(I)  = VAR(I) * LOG10(e)**2 * 10**( 2.0 * DATA(I) )
*     QUAL(I) = QUAL(I) for DATA(I) > 0.0
*     QUAL(I) = QUAL(I) + <value representing arithmetic error=2> for DATA(I) =< 0.0
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews ( PLA_AST88@uk.ac.bham.sr/star)
*    History :
*     18/10/88:  original (PLA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'
*    Import :
      LOGICAL                QOK                      ! Quality OK?
      LOGICAL                VAROK                    ! Variance OK?

      INTEGER                NDAT                     ! Number of data points
*    Import-Export :
      DOUBLE PRECISION       DATA(NDAT)               ! Data values
      DOUBLE PRECISION       VAR(*)                   ! Variance values

      BYTE                   MASK                     ! Quality mask
      BYTE                   QUAL(NDAT)               ! Quality values
*    Status :
      INTEGER                STATUS
*    Functions :
      BYTE		     BIT_ORUB
*    Local constants :
      DOUBLE PRECISION       MAX1
         PARAMETER         ( MAX1 = 38.23D0 )
      DOUBLE PRECISION       C1
         PARAMETER         ( C1 = 18.75172766360737 )
      DOUBLE PRECISION       C2
         PARAMETER         ( C2 = 4.605170185988091 )
      DOUBLE PRECISION       C3
         PARAMETER         ( C3 = 5.301898110478398 )
*    Local variables :
      INTEGER                I
      INTEGER                OLDBADQUAL
      INTEGER                NEWBADQUAL
      INTEGER                BAD
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      OLDBADQUAL = 0
      NEWBADQUAL = 0
      BAD        = 0

      IF ( QOK ) THEN
        DO I = 1, NDAT
          IF ( QUAL(I) .EQ. 0 ) THEN
            IF ( DATA(I) .LE. MAX1 ) THEN
              IF ( VAROK ) THEN
                IF ( DATA(I) .LT. C1 - (LOG(VAR(I)) / C2) ) THEN
                  VAR(I) = C3 * VAR(I) * EXP( DATA(I) * C2 )

                ELSE
                  NEWBADQUAL = NEWBADQUAL + 1
                  QUAL(I)    = BIT_ORUB(QUAL(I),QUAL__ARITH)

                END IF
              END IF
              DATA(I) = 10**( DATA(I) )

            ELSE
              NEWBADQUAL = NEWBADQUAL + 1
              QUAL(I)    = BIT_ORUB(QUAL(I),QUAL__ARITH)

            END IF
          ELSE
            OLDBADQUAL = OLDBADQUAL + 1

          END IF
        END DO
      ELSE
        DO I = 1, NDAT
          IF ( DATA(I) .LE. MAX1 ) THEN
            DATA(I) = 10**( DATA(I) )
          ELSE
            DATA(I) = VAL__MAXD
            BAD     = BAD + 1
          END IF
        END DO
      END IF

      IF ( OLDBADQUAL .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', OLDBADQUAL )
        CALL MSG_PRNT( '^BAD data points excluded by QUALITY' )
      END IF

      IF ( NEWBADQUAL .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', NEWBADQUAL )
        CALL MSG_PRNT( '^BAD NEW bad quality points (too big)' )
      END IF

      IF ( BAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', BAD )
        CALL MSG_PRNT( '^BAD data points set to 1.7E38 '//
     :                                  '(data too big)' )
      END IF

      END
