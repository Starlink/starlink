      SUBROUTINE OPERATE( STATUS )
*+
*  Name:
*     OPERATE

*  Purpose:
*     Applies a unary operator to the data in the input file

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL OPERATE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
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

*  Usage:
*     operate {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input data object
*     OUT = CHAR (read)
*        Output data object
*     ERR = CHAR (read)
*        Error for primitive input
*     OPER = CHAR (read)
*        Operation to be performed
*     OVER = LOGICAL (read)
*        Overwrite existing file?

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
*     Inadequate treatment of LOG errors for non-positive data

*  References:
*     {task_references}...

*  Keywords:
*     operate, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     13 Mar 1986 (TJP):
*        Original version.
*      2 Mar 1986 (GKS):
*        Avoid enormous data error entries when taking logs
*      7 Mar 1986 (GKS):
*        More explosion proofing
*     25 Feb 1987 (ADM):
*        Add '-', LOG, EXP and 10**. Fix fault in LOG10 errors
*     10 Mar 1987 (ADM):
*        Adapted to handle primitive input/output
*     16 Mar 1987 (ADM):
*        Modified to handle problems overwriting existing objects
*     27 Jun 1988 V1.0-1 (ADM):
*        ASTERIX88 conversion
*     23 May 1991 V1.5-0 (DJA):
*        Code tidied
*     26 Jul 1993 V1.7-0 (DJA):
*        Use PRM constants for extreme data values. Fixed
*        fault in LOG10 errors (probably introduced 25/2/87!)
*     25 Feb 1994 V1.7-1 (DJA):
*        Use BIT_ routines to do bit manipulations
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     28 Mar 1995 V1.8-1 (DJA):
*        Use new data interface
*      3 Oct 1995 V2.0-0 (DJA):
*        Full ADI port. Only create quality if new bad points.
*     10 Jan 1996 V2.0-1 (DJA):
*        Changed to use USI_NAMES
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'OPERATE Version V2.0-0' )

*  Local Variables:
      CHARACTER*6            	OPER                  	! Operator
      CHARACTER*80           	LABEL                 	! Data label
      CHARACTER*80           	STRING                	! Temp. String

      DOUBLE PRECISION       	EVALUE                	! Manual error estimate

      INTEGER                	DPTR                  	! Output data array
      INTEGER			IFID		      	! Input dataset id
      INTEGER			IFILES			! Input file list
      INTEGER			OFILES			! Output file list
      INTEGER			NBAD			! New bad points
      INTEGER                   NELM                  	! # input data values
      INTEGER			OFID		      	! Output dataset id
      INTEGER                	OQPTR, QPTR                  ! Output quality
      INTEGER                   SLEN                  	! Length of STRING
      INTEGER                	VPTR                  	! Output variance

      BYTE                      MASK                  	! Quality mask

      LOGICAL                   ERR                   	! Data errors present?
      LOGICAL                   INPUT                 	! Controls operator input
      LOGICAL                   OK                    	! Component present and defined?
      LOGICAL                   OVER                  	! Input object to be overwritten?
      LOGICAL                   PRIM                  	! Input is primitive?
      LOGICAL                   QOK                   	! QUALITY present?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Check for overwriting
      CALL USI_GET0L( 'OVER', OVER, STATUS )

*  Get input object
      IF ( OVER ) THEN
        CALL USI_ASSOC( 'INP', 'BinDS|Array', 'UPDATE', OFID, STATUS )
      ELSE
        CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )
        CALL USI_CLONE( 'INP', 'OUT', 'BinDS', OFID, STATUS )
        CALL USI_NAMES( 'O', OFILES, STATUS )
      END IF
      CALL USI_NAMES( 'I', IFILES, STATUS )

*  Is input a structured data object?
      CALL ADI_DERVD( OFID, 'Array', PRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Map the data
      CALL BDI_CHK( OFID, 'Data', OK, STATUS )
      CALL BDI_GETNEL( OFID, NELM, STATUS )
      IF ( OK ) THEN
        CALL BDI_MAPD( OFID, 'Data', 'UPDATE', DPTR, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid data', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  If input is primitive, get error estimate
      IF ( PRIM ) THEN

        CALL USI_GET0D( 'ERR', EVALUE, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          ERR = .TRUE.
          CALL DYN_MAPD( 1, NELM, VPTR, STATUS )
          CALL ARR_INIT1D( EVALUE**2, NELM, %VAL(VPTR), STATUS )
        ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
          CALL ERR_ANNUL( STATUS )
          ERR = .FALSE.
        END IF
        QOK = .FALSE.

      ELSE

*    Get error if present
        CALL BDI_CHK( OFID, 'Variance', ERR, STATUS )
        IF ( ERR ) THEN
          CALL BDI_MAPD( OFID, 'Variance', 'UPDATE', VPTR, STATUS )
        END IF

*    And quality
        CALL BDI_CHK( OFID, 'Quality', QOK, STATUS )
        IF ( QOK ) THEN
          CALL BDI_MAPUB( OFID, 'Quality', 'UPDATE', QPTR, STATUS )
          CALL BDI_GET0UB( OFID, 'QualityMask', MASK, STATUS )
        ELSE
          CALL DYN_MAPB( 1, NELM, QPTR, STATUS )
          CALL ARR_INIT1B( QUAL__GOOD, NELM, %VAL(QPTR), STATUS )
          MASK = QUAL__MASK
        END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      CALL MSG_SETI( 'NDAT', NELM )
      CALL MSG_PRNT( '^NDAT data points to be processed' )

*  Get operation and check validity
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
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Perform the operations
      NBAD = 0
      IF ( OPER(1:1) .EQ. '-' ) THEN
        CALL OPERATE_MINUS( NELM, %VAL(DPTR), STATUS )

      ELSE IF ( OPER(1:3) .EQ. 'ABS' )   THEN
        CALL OPERATE_ABS( NELM, %VAL(DPTR), STATUS )

      ELSE IF ( OPER(1:5) .EQ. 'LOG10' ) THEN
        CALL OPERATE_LOG10( ERR, QOK, NELM, %VAL(DPTR), %VAL(VPTR),
     :                             MASK, %VAL(QPTR), NBAD, STATUS )

      ELSE IF ( OPER(1:3) .EQ. 'LOG' )   THEN
        CALL OPERATE_LOG( ERR, QOK, NELM, %VAL(DPTR), %VAL(VPTR),
     :                           MASK, %VAL(QPTR), NBAD, STATUS )

      ELSE IF ( OPER(1:4) .EQ. 'SQRT' )  THEN
        CALL OPERATE_SQRT( ERR, QOK, NELM, %VAL(DPTR), %VAL(VPTR),
     :                            MASK, %VAL(QPTR), NBAD, STATUS )

      ELSE IF ( OPER(1:3) .EQ. 'EXP' )   THEN
        CALL OPERATE_EXP( ERR, QOK, NELM, %VAL(DPTR), %VAL(VPTR),
     :                           MASK, %VAL(QPTR), NBAD, STATUS )

      ELSE IF ( OPER(1:4) .EQ. '10**' )  THEN
        CALL OPERATE_ANTILOG10( ERR, QOK, NELM, %VAL(DPTR), %VAL(VPTR),
     :                                MASK, %VAL(QPTR), NBAD, STATUS )

      END IF

*  Write quality if new bad points, and it didn't exist in input
      IF ( (NBAD.GT.0) .AND. .NOT. QOK ) THEN
        CALL BDI_MAPUB( OFID, 'Quality', 'WRITE', OQPTR, STATUS )
        CALL ARR_COP1B( NELM, %VAL(QPTR), %VAL(OQPTR), STATUS )
        CALL BDI_PUT0UB( OFID, 'QualityMask', MASK, STATUS )
      END IF

*  Amend data label if present
      IF ( .NOT. PRIM ) THEN
        CALL BDI_GET0C( OFID, 'Label', LABEL, STATUS )
        IF ( LABEL .GT. ' ' ) THEN
          CALL MSG_SETC( 'LAB', LABEL )
          CALL MSG_SETC( 'OP', OPER )
          CALL MSG_MAKE( '^OP( ^LAB )', STRING, SLEN )
          CALL BDI_PUT0C( OFID, 'Label', STRING(:SLEN), STATUS )
        END IF
      END IF

*  History
      IF ( .NOT. PRIM ) THEN
        CALL HSI_ADD( OFID, VERSION, STATUS )
        CALL HSI_PTXTI( OFID, IFILES, .TRUE., STATUS )
        IF ( .NOT. OVER ) THEN
          CALL HSI_PTXTI( OFID, OFILES, .TRUE., STATUS )
        END IF
        CALL HSI_PTXT( OFID, 1, 'Operator = '//OPER, STATUS )
      END IF

*  Release dynamic output errors if used
      IF ( PRIM .AND. ERR ) THEN
        CALL DYN_UNMAP( VPTR, STATUS )
      END IF

*  Release dynamic quality
      IF ( .NOT. QOK ) THEN
        CALL DYN_UNMAP( QPTR, STATUS )
      END IF

*  Tidy up and exit
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
     :                                                  NEWBAD, STATUS )
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
      INTEGER                NEWBAD

*    Status :
      INTEGER                STATUS
*    Functions :
      BYTE		     BIT_ORUB
*    Local constants :
      DOUBLE PRECISION       C                        ! C = ( LOG10(e)**2 )
         PARAMETER         ( C = 0.1886116969479110 )
*    Local variables :
      INTEGER                I
      INTEGER                OLDBAD
      INTEGER                BAD
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      OLDBAD = 0
      NEWBAD = 0
      BAD = 0

      DO I = 1, NDAT
        IF ( QOK ) THEN
          IF ( QUAL(I) .EQ. 0 ) THEN
            IF ( DATA(I) .GT. 0.0 ) THEN
              IF ( VAROK ) THEN
                VAR(I) = VAR(I) * C / DATA(I)**2
              END IF
              DATA(I) = LOG10( DATA(I) )

            ELSE
              NEWBAD = NEWBAD + 1
              QUAL(I)    = BIT_ORUB(QUAL(I),QUAL__ARITH)

            END IF
          ELSE
            OLDBAD = OLDBAD + 1

          END IF
        ELSE
          IF ( DATA(I) .GT. 0.0 ) THEN
            DATA(I) = LOG10( DATA(I) )

          ELSE
            BAD     = BAD + 1
            NEWBAD = NEWBAD + 1
            DATA(I) = VAL__MIND

          END IF
        END IF
      END DO

      IF ( OLDBAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', OLDBAD )
        CALL MSG_PRNT( '^BAD data points excluded by QUALITY' )
      END IF

      IF ( NEWBAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', NEWBAD )
        CALL MSG_PRNT( '^BAD NEW bad quality points (data <= 0)' )
      END IF

      IF ( BAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', BAD )
        CALL MSG_PRNT( '^BAD data points set to -1.7E38 (data <= 0)' )
      END IF

      END



*+  OPERATE_LOG - Natural Log
      SUBROUTINE OPERATE_LOG( VAROK, QOK, NDAT, DATA, VAR,
     :                                 MASK, QUAL, NEWBAD, STATUS)
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
      INTEGER                NEWBAD
*    Status :
      INTEGER                STATUS
*    Functions :
      BYTE		     BIT_ORUB
*    Local variables :
      INTEGER                I
      INTEGER                OLDBAD
      INTEGER                BAD
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      OLDBAD = 0
      NEWBAD = 0
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
              NEWBAD = NEWBAD + 1
              QUAL(I)    = BIT_ORUB(QUAL(I),QUAL__ARITH)

            END IF
          ELSE
            OLDBAD = OLDBAD + 1

          END IF
        END DO
      ELSE
        DO I = 1, NDAT
          IF ( DATA(I) .GT. 0.0 ) THEN
            DATA(I) = LOG( DATA(I) )

          ELSE
            NEWBAD = NEWBAD + 1
            BAD     = BAD + 1
            DATA(I) = VAL__MIND

          END IF
        END DO
      END IF

      IF ( OLDBAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', OLDBAD )
        CALL MSG_PRNT( '^BAD data points excluded by QUALITY' )
      END IF

      IF ( NEWBAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', NEWBAD )
        CALL MSG_PRNT( '^BAD NEW bad quality points (data <= 0)' )
      END IF

      IF ( BAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', BAD )
        CALL MSG_PRNT( '^BAD data points set to -1.7E38 (data <= 0)' )
      END IF

      END



*+  OPERATE_SQRT - Square root
      SUBROUTINE OPERATE_SQRT( VAROK, QOK, NDAT, DATA, VAR, MASK,
     :                         QUAL, BAD, STATUS )
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
      INTEGER                BAD
*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER                I
      INTEGER                OLDBAD

      DOUBLE PRECISION       TEMP
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      OLDBAD = 0
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
            OLDBAD = OLDBAD + 1

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

      IF ( OLDBAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', OLDBAD )
        CALL MSG_PRNT( '^BAD data points excluded by QUALITY' )
      END IF

      IF ( BAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', BAD )
        CALL MSG_PRNT( '^BAD data points set to 0 (data <= 0)' )
      END IF

      END



*+  OPERATE_EXP - e**(DATA)
      SUBROUTINE OPERATE_EXP( VAROK, QOK, NDAT, DATA, VAR, MASK,
     :                                    QUAL, NEWBAD, STATUS )
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
      INTEGER                OLDBAD
      INTEGER                NEWBAD
      INTEGER                BAD
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      OLDBAD = 0
      NEWBAD = 0
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
                  NEWBAD = NEWBAD + 1
                  QUAL(I)    = BIT_ORUB(QUAL(I),QUAL__ARITH)

                END IF
              END IF
            ELSE
              NEWBAD = NEWBAD + 1
              QUAL(I)    = BIT_ORUB(QUAL(I),QUAL__ARITH)

            END IF
          ELSE
            OLDBAD = OLDBAD + 1

          END IF
        END DO
      ELSE
        DO I = 1, NDAT
          IF ( DATA(I) .LE. MAX1 ) THEN
            DATA(I) = EXP( DATA(I) )

          ELSE
            DATA(I) = VAL__MAXD
            BAD = BAD + 1
            NEWBAD = NEWBAD + 1

          END IF
        END DO
      END IF

      IF ( OLDBAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', OLDBAD )
        CALL MSG_PRNT( '^BAD data points excluded by QUALITY' )
      END IF

      IF ( NEWBAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', NEWBAD )
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
     :                                          QUAL, NEWBAD, STATUS )
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
      INTEGER                NEWBAD
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
      INTEGER                OLDBAD
      INTEGER                BAD
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      OLDBAD = 0
      NEWBAD = 0
      BAD        = 0

      IF ( QOK ) THEN
        DO I = 1, NDAT
          IF ( QUAL(I) .EQ. 0 ) THEN
            IF ( DATA(I) .LE. MAX1 ) THEN
              IF ( VAROK ) THEN
                IF ( DATA(I) .LT. C1 - (LOG(VAR(I)) / C2) ) THEN
                  VAR(I) = C3 * VAR(I) * EXP( DATA(I) * C2 )

                ELSE
                  NEWBAD = NEWBAD + 1
                  QUAL(I)    = BIT_ORUB(QUAL(I),QUAL__ARITH)

                END IF
              END IF
              DATA(I) = 10**( DATA(I) )

            ELSE
              NEWBAD = NEWBAD + 1
              QUAL(I) = BIT_ORUB(QUAL(I),QUAL__ARITH)

            END IF
          ELSE
            OLDBAD = OLDBAD + 1

          END IF
        END DO
      ELSE
        DO I = 1, NDAT
          IF ( DATA(I) .LE. MAX1 ) THEN
            DATA(I) = 10**( DATA(I) )
          ELSE
            DATA(I) = VAL__MAXD
            BAD     = BAD + 1
            NEWBAD = NEWBAD + 1
          END IF
        END DO
      END IF

      IF ( OLDBAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', OLDBAD )
        CALL MSG_PRNT( '^BAD data points excluded by QUALITY' )
      END IF

      IF ( NEWBAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', NEWBAD )
        CALL MSG_PRNT( '^BAD NEW bad quality points (too big)' )
      END IF

      IF ( BAD .GT. 0 ) THEN
        CALL MSG_SETI( 'BAD', BAD )
        CALL MSG_PRNT( '^BAD data points set to 1.7E38 '//
     :                                  '(data too big)' )
      END IF

      END
