      SUBROUTINE ARITHMETIC( STATUS )
*+
*  Name:
*     ARITHMETIC

*  Purpose:
*     Performs C = A o B

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL ARITHMETIC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Performs a specified arithmetic operation on two data objects
*     one or both of which may be primitive and stores the result in
*     a third data object, which may be either of the two input objects
*     (if not primitive).
*     If not primitive, then the input data objects are assumed to be in the
*     DATA_ARRAY subclass.
*     The output object is always in the DATA_ARRAY subclass and has the
*     same type as any non-primitive input object or SUMS if both are
*     primitive.
*     If both input objects are arrays, they must have identical axes. (The
*     axis data from the first object is propagated.)

*  Usage:
*     arithmetic {parameter_usage}

*  Environment Parameters:
*     INP1 = CHAR (read)
*        First input data object
*     INP2 = CHAR (read)
*        Second input data object
*     OUT = CHAR (read)
*        Output data object
*     OVER = LOGICAL (read)
*        Overwrite first input?
*     OPER = CHAR (read)
*        Type of operation (+,-,*,/)
*     ERRORS = LOGICAL (read)
*        Supply error objects for primitive inputs?
*     ERROR1 = CHAR (read)
*        Error estimate for first primitive data object
*     ERROR2 = CHAR (read)
*        Error estimate for second primitive data object

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
*     arithmetic, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     JCMP: Jim Peden( University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Jan 1986 V0.4-1 (JCMP):
*        Modified from various programs
*     30 Apr 1986 V0.4-2 (JCMP):
*        Better error propagation
*     16 May 1986 V0.4-3 (JCMP):
*        Better condition handling
*     14 Aug 1986 V0.5-1 (JCMP):
*        Data quality handling
*     14 Nov 1986 V0.5-2 (JKD):
*        Modified UTIL_ASSOC3 to allow ops on 2 prim objects
*      8 Jan 1987 V0.6-1 (TJP):
*        Bug fixed in quality handling for scalar divisor
*     13 Jan 1987 V0.6-2 (TJP):
*        Mod. to allow primitive output
*      4 Jun 1987 : V0.6-1 Renamed ARITHMETIC V0.6-1
*      3 May 1988 V1.0-1 (ADM):
*        Asterix88 version
*      9 Dec 1989 V1.0-2 (ADM):
*        Now uses BDA calls - code tidied
*     12 Jan 1990 V1.0-3 (DJA):
*        A few bug fixes done
*     16 Feb 1990 V1.2-1 (DJA):
*        Quality bug if none in input fixed. Now uses QUAL_PAR include file
*     27 Feb 1990 V1.2-2 (RJV):
*        Variance handling fixed
*      1 Mar 1990 V1.2-3 (RJV):
*        Overwrite mode now works
*     28 May 1990 V1.2-4 (DJA):
*        Overwrite mode really does work now (honest)
*     10 Aug 1990 V1.2-5 (DJA):
*        Overwrite for primitive input doesn't try to create quality
*     10 May 1991 V1.4-0 (DJA):
*        Points with bad quality no longer operated on
*     23 May 1991 V1.4-1 (DJA):
*        Bug fix from above
*     28 May 1991 V1.4-2 (DJA):
*        Manual error more robust
*     25 Mar 1992 V1.6-0 (DJA):
*        Use ERR_ANNUL
*      9 Jul 1992 V1.6-1 (DJA):
*        Corrected erroneous ACCESS2 argument to USI_ASSOC3.
*      2 Dec 1993 V1.7-0 (DJA):
*        Removed use of structures - was causing many problems on Solaris
*      1 Mar 1994 V1.7-1 (DJA):
*        Updated handling of bit quality - now use BIT_ and QUAL__
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     12 Jan 1995 V1.8-1 (DJA):
*        Use new data interfaces
*      8 Aug 1995 V1.8-2 (DJA):
*        Create quality in 1st input in overwrite mode if it exists in 2nd
*     11 Dec 1995 V2.0-0 (DJA):
*        ADI port
*      6 Mar 1996 V2.0-1 (DJA):
*        Removed scalar insert to history as this now done by USI
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
      INCLUDE 'QUAL_PAR'        ! Asterix quality definitions

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL                  BIT_ANDUB
        BYTE                    BIT_ANDUB

*  Local Constants:
      INTEGER			MAXTXT			! Max amount of history
        PARAMETER		( MAXTXT = 8 )

      CHARACTER*30              ICLS			! Input class list
        PARAMETER		( ICLS = 'BinDS|Array|Scalar' )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'ARITHMETIC Version 2.1-0' )

*  Local Variables:
      CHARACTER              	OPER             	! Operation
      CHARACTER*6           	MODE             	! 'READ' or 'UPDATE'
      CHARACTER*40		OUT_TYPE         	! Output array type

      CHARACTER*80 		TEXT(MAXTXT)            ! History text

      INTEGER			EID1, EID2		! External error ids
      INTEGER			FID1, FID2		! Input file ids
      INTEGER 			I			! index
      INTEGER			IDUM(ADI__MXDIM)	! Dummy return values
      INTEGER 			IN1_DPTR, IN2_DPTR, OUT_DPTR    ! Dataset data pointers
      INTEGER 			IN1_DIMS(ADI__MXDIM)    ! 1st input dimensions
      INTEGER 			IN2_DIMS(ADI__MXDIM)    ! 2nd input dimensions
      INTEGER 			IN1_NELM, IN2_NELM, OUT_NELM    ! # elements in datasets
      INTEGER 			IN1_NDIM, IN2_NDIM, OUT_NDIM    ! Dimensionality of datasets
      INTEGER 			IN1_QPTR, IN2_QPTR, OUT_QPTR    ! Dataset quality pointers
      INTEGER 			IN1_VPTR, IN2_VPTR, OUT_VPTR    ! Dataset variance pointers
      INTEGER 			IN1_VNELM, IN2_VNELM, OUT_VNELM ! # elements in dataset variances
      INTEGER 			NBAD                    ! # new bad points
      INTEGER			NHIN			! # i/p history lines
      INTEGER 			NIG                     ! # ignored points
      INTEGER			NLINE			! # o/p history lines
      INTEGER			OFID			! Output file id
      INTEGER 			OUT_DIMS(ADI__MXDIM)    ! Output dimensions
      INTEGER			TID

      BYTE                    	IN1_MASK        	! 1st i/p quality mask
      BYTE                    	IN2_MASK        	! 2nd i/p quality mask
      BYTE                    	OUT_MASK        	! Output quality mask

      LOGICAL                 	DQD 	      		! Division operation
      LOGICAL                 	ERRORS          	! Prompt for input errors?
      LOGICAL                 	IN1_PRIM        	! 1st input primitive?
      LOGICAL                 	IN2_PRIM        	! 2nd input primitive?
      LOGICAL                 	IN1_QOK, IN2_QOK, OUT_QOK
      LOGICAL                 	IN1_SCALAR      	! 1st input scalar?
      LOGICAL                 	IN2_SCALAR      	! 2nd input scalar?
      LOGICAL                 	IN1_UERR        	! Supplied error for input #1
      LOGICAL                 	IN2_UERR        	! Supplied error for input #1
      LOGICAL                 	IN1_VOK, IN2_VOK	! Variance present in inputs?
      LOGICAL                 	OK              	! General validity test
      LOGICAL                 	OUT_PRIM        	! Output primitive?
      LOGICAL                 	OVER            	! Overwrite input file?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Initialise logicals
      IN1_UERR = .FALSE.
      IN2_UERR = .FALSE.
      IN1_VOK = .FALSE.
      IN2_VOK = .FALSE.

*  Overwrite existing input?
      CALL USI_GET0L( 'OVER', OVER, STATUS )

*  Associate input data objects
      IF ( OVER ) THEN
        CALL MSG_PRNT( 'First input file will be overwritten' )
        CALL USI_ASSOC( 'INP1', ICLS, 'UPDATE', FID1, STATUS )
      ELSE
        CALL USI_ASSOC( 'INP1', ICLS, 'UPDATE', FID1, STATUS )
      END IF
      CALL USI_ASSOC( 'INP2', ICLS, 'READ', FID2, STATUS )

*  Inputs are primitive?
      CALL ADI_DERVD( FID1, 'BinDS', IN1_PRIM, STATUS )
      IN1_PRIM = (.NOT. IN1_PRIM)
      CALL ADI_DERVD( FID2, 'BinDS', IN2_PRIM, STATUS )
      IN2_PRIM = (.NOT. IN2_PRIM)

*  Get output object
      IF ( OVER ) THEN
        OFID = FID1
      ELSE
        CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      END IF
      OUT_PRIM = IN1_PRIM

*  See if errors wanted for primitive input
      IF ( IN1_PRIM .OR. IN2_PRIM ) THEN
        CALL USI_GET0L( 'ERRORS', ERRORS, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check size of 1st input dataset
      CALL BDI_CHK( FID1, 'Data', OK, STATUS )
      CALL BDI_GETSHP( FID1, ADI__MXDIM, IN1_DIMS, IN1_NDIM, STATUS )
      IF ( OK ) THEN
        IN1_SCALAR = ( IN1_NDIM .EQ. 0 )
        CALL ARR_SUMDIM( IN1_NDIM, IN1_DIMS, IN1_NELM )
      ELSE
        CALL ERR_REP( ' ', 'Bad input data - not numeric', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check size of 2nd input dataset
      CALL BDI_CHK( FID2, 'Data', OK, STATUS )
      CALL BDI_GETSHP( FID2, ADI__MXDIM, IN2_DIMS, IN2_NDIM, STATUS )
      IF ( OK ) THEN
        IN2_SCALAR = ( IN2_NDIM .EQ. 0 )
        CALL ARR_SUMDIM( IN2_NDIM, IN2_DIMS, IN2_NELM )
      ELSE
        CALL ERR_REP( ' ', 'Bad input data - not numeric', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Datasets must be same size or unity
      IF ( IN2_NELM .NE. IN1_NELM ) THEN
        IF ( .NOT. ( IN1_NELM .EQ.1.OR. IN2_NELM .EQ.1 ) ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Bad data sizes: not equal'/
     :             /' and neither one is unity', STATUS )
          GOTO 99
        END IF
      END IF

*  Obtain errors and validate for first object
      IF ( IN1_PRIM ) THEN
        IF ( ERRORS ) THEN
          CALL USI_ASSOC( 'ERROR1', ICLS, 'READ', EID1, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            IN1_UERR = .TRUE.
            IN1_VOK = .TRUE.
          ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            IN1_VOK = .FALSE.
          END IF
        END IF
      ELSE
        CALL BDI_CHK( FID1, 'Variance', IN1_VOK, STATUS )
        IN1_VNELM = IN1_NELM
      END IF
      IF ( ( STATUS .EQ. SAI__OK ) .AND. IN1_VOK ) THEN
        IF ( IN1_UERR ) THEN
          CALL BDI_GETNEL( EID1, IN1_VNELM, STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP(' ','1st object''s error data is invalid',
     :                                                   STATUS )
        ELSE IF ( IN1_NELM .NE. IN1_VNELM ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Error data size does not match'/
     :                  /' data size for 1st object', STATUS )

        END IF
      ELSE
        IN1_VNELM = 0
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Obtain errors and validate for first second
      IF ( IN2_PRIM ) THEN
        IF ( ERRORS ) THEN
          CALL USI_ASSOC( 'ERROR2', ICLS, 'READ', EID2, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            IN2_VOK = .TRUE.
            IN2_UERR = IN2_VOK
          ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            IN2_VOK = .FALSE.
          END IF
        END IF
      ELSE
        CALL BDI_CHK( FID2, 'Variance', IN2_VOK, STATUS )
        IN2_VNELM = IN2_NELM
      END IF
      IF ( ( STATUS .EQ. SAI__OK ) .AND. IN2_VOK ) THEN
        IF ( IN2_UERR ) THEN
          CALL BDI_GETNEL( EID2, IN2_VNELM, STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP(' ','2nd object''s error data is invalid',
     :                                                   STATUS )
        ELSE IF ( IN2_NELM .NE. IN2_VNELM ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Error data size does not match'/
     :                  /' data size for 2nd object', STATUS )
        END IF
      ELSE
        IN2_VNELM = 0
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Obtain operation type
      CALL USI_GET0C( 'OPER', OPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Select mapping mode
      IF ( OVER ) THEN
        MODE = 'UPDATE'
      ELSE
        MODE = 'READ'
      END IF

*  Map first object
      CALL BDI_MAPR( FID1, 'Data', MODE, IN1_DPTR, STATUS )
      IF ( IN1_PRIM .AND. IN1_VOK ) THEN

*    Map workspace
        CALL DYN_MAPR( 1, IN1_VNELM, IN1_VPTR, STATUS )

*    Load data from errors object
        CALL BDI_GETR( EID1, 'Data', IN1_NDIM, IN1_DIMS,
     :                   %VAL(IN1_VPTR), IDUM, STATUS )

*    Square errors to create variances
        CALL ARR_SQR1R( %VAL(IN1_VPTR), IN1_VNELM, STATUS )

*  Otherwise map variance if present
      ELSE IF ( IN1_VOK ) THEN
        CALL BDI_MAPR( FID1, 'Variance', MODE, IN1_VPTR, STATUS )

      END IF

*  And second object - always map for read
      CALL BDI_MAPR( FID2, 'Data', 'READ', IN2_DPTR, STATUS )
      IF ( IN2_PRIM .AND. IN2_VOK ) THEN

*    Map workspace
        CALL DYN_MAPR( 1, IN2_VNELM, IN2_VPTR, STATUS )

*    Load data from errors object
        CALL BDI_GETR( EID2, 'Data', IN2_NDIM, IN2_DIMS,
     :                   %VAL(IN2_VPTR), IDUM, STATUS )

*    Square errors to create variances
        CALL ARR_SQR1R( %VAL(IN2_VPTR), IN2_VNELM, STATUS )

*  Otherwise map variance if present
      ELSE IF ( IN2_VOK ) THEN
        CALL BDI_MAPR( FID2, 'Variance', 'READ', IN2_VPTR, STATUS )

      END IF

*  Variance can be calculated but not available in 1st input
      IF ( OVER .AND. IN2_VOK .AND. .NOT. IN1_VOK ) THEN
        CALL BDI_MAPR( FID1, 'Variance', 'WRITE', IN1_VPTR, STATUS )
      END IF

*  Create components in output dataset
      IF ( .NOT. OVER ) THEN

*    Decide on output dimensions
        IF ( .NOT. IN1_SCALAR ) THEN
          OUT_NDIM = IN1_NDIM
          DO I = 1, ADI__MXDIM
            OUT_DIMS(I) = IN1_DIMS(I)
          END DO
        ELSE
          OUT_NDIM = IN2_NDIM
          DO I = 1, ADI__MXDIM
            OUT_DIMS(I) = IN2_DIMS(I)
          END DO
        END IF

*    Output is scalar if both inputs are scalar
        IF ( IN1_SCALAR .AND. IN2_SCALAR ) THEN
          OUT_TYPE = 'Scalar'

*    Output is primitive if both inputs are primitive
        ELSE IF ( OUT_PRIM ) THEN
          OUT_TYPE = 'Array'

*    Otherwise choose class of best input
        ELSE IF ( .NOT. IN1_PRIM ) THEN
          CALL ADI_TYPE( FID1, OUT_TYPE, STATUS )

        ELSE IF ( .NOT. IN2_PRIM ) THEN
          CALL ADI_TYPE( FID2, OUT_TYPE, STATUS )

        END IF

*    Create output interface object
        IF ( .NOT. OVER ) THEN
          CALL BDI_LINK( OUT_TYPE, OUT_NDIM, OUT_DIMS, 'REAL',
     :                   OFID, STATUS )
        END IF

      END IF

*  Find no. output points.
      IF ( OVER ) THEN
        OUT_NELM = MAX(IN1_NELM,IN2_NELM)
        OUT_VNELM = MAX(IN1_VNELM,IN2_VNELM)
      ELSE
        CALL ARR_SUMDIM( OUT_NDIM, OUT_DIMS, OUT_NELM )
        IF ( IN1_VNELM.EQ.0 .AND. IN2_VNELM.EQ.0 ) THEN
          OUT_VNELM = 0
        ELSE
          OUT_VNELM = OUT_NELM
        END IF
      END IF

*  Map output dataset data
      IF ( OVER ) THEN
        OUT_DPTR = IN1_DPTR
        OUT_VPTR = IN1_VPTR
      ELSE
        CALL BDI_MAPR( OFID, 'Data', 'WRITE', OUT_DPTR, STATUS )
        IF ( IN1_VOK .OR. IN2_VOK  ) THEN
          CALL BDI_MAPR( OFID, 'Variance', 'WRITE', OUT_VPTR, STATUS )
        ELSE
          OUT_VNELM = 0
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  See what input quality flags we have
      CALL BDI_CHK( FID1, 'Quality', IN1_QOK, STATUS )
      CALL BDI_CHK( FID2, 'Quality', IN2_QOK, STATUS )

*  Decide whether to set up or modify QUALITY in output dataset
      DQD = (OPER.EQ.'/') .AND. (IN2_NELM.GT.1) .AND. .NOT. OUT_PRIM
      OUT_QOK = .FALSE.
      OUT_QPTR = 0
      OUT_MASK = QUAL__MASK
      IF ( IN1_QOK .OR. IN2_QOK .OR. DQD ) THEN

*     Overwriting input?
         IF ( OVER .AND. .NOT. OUT_PRIM ) THEN

*       No existing quality?
           IF ( .NOT. IN1_QOK ) THEN
*   Create output QUALITY
             CALL BDI_PUT0UB( FID1, 'QualityMask', QUAL__MASK, STATUS )
             CALL BDI_MAPUB( FID1, 'Quality', 'WRITE', IN1_QPTR,
     :                       STATUS )
             OUT_QOK = .TRUE.

*       Quality exists
           ELSE
             CALL BDI_MAPUB( FID1, 'Quality', 'UPDATE', IN1_QPTR,
     :                       STATUS )
             OUT_QPTR = IN1_QPTR
             IF ( IN2_QOK ) THEN
               CALL BDI_MAPUB( FID2,'Quality','READ',IN2_QPTR,STATUS)
               OUT_QOK = .TRUE.
             END IF
           END IF

*     Not overwriting
         ELSE

*       Create output QUALITY
           CALL BDI_MAPUB( OFID, 'Quality', 'WRITE', OUT_QPTR, STATUS )

*       Map 1st input quality
           IF ( IN1_QOK ) THEN
             CALL BDI_MAPUB( FID1, 'Quality', 'READ', IN1_QPTR, STATUS )
             CALL BDI_GET0UB( FID1, 'QualityMask', IN1_MASK, STATUS )
           ELSE
             IN1_MASK = QUAL__MASK
           END IF

*       Map 2nd input quality
           IF ( IN2_QOK ) THEN
             CALL BDI_MAPUB( FID2, 'Quality', 'READ', IN2_QPTR, STATUS )
             CALL BDI_GET0UB( FID2, 'QualityMask', IN2_MASK, STATUS )
           ELSE
             IN2_MASK = QUAL__MASK
           END IF

*       Write output quality mask
           OUT_MASK = BIT_ANDUB(IN1_MASK,IN2_MASK)
           CALL BDI_PUT0UB( OFID, 'QualityMask', OUT_MASK, STATUS )

         END IF

      END IF
      OUT_QOK = ( OUT_QPTR .NE. 0 )

*  Perform operation
      IF ( OUT_QOK ) THEN

*    Generate output quality
        CALL ARITHMETIC_QUAL( OUT_NELM, IN1_QOK, %VAL(IN1_QPTR),
     :         IN2_QOK, %VAL(IN2_QPTR), %VAL(OUT_QPTR), STATUS )

*    Perform operation with quality
        CALL ARITHMETIC_SUMSQ( OPER, IN1_NELM, %VAL(IN1_DPTR),
     :                         IN1_VNELM, %VAL(IN1_VPTR),
     :                         IN2_NELM, %VAL(IN2_DPTR),
     :                         IN2_VNELM, %VAL(IN2_VPTR),
     :                         OUT_NELM,
     :              %VAL(OUT_QPTR), OUT_MASK,
     :         %VAL(OUT_DPTR),OUT_VNELM,%VAL(OUT_VPTR),
     :                                       NIG,NBAD,STATUS)

*    Report ignored points
        IF ( NIG .GT. 0 ) THEN
          CALL MSG_SETI( 'NIG', NIG )
          CALL MSG_PRNT( 'WARNING : ^NIG points ignored due to'/
     :                                         /' bad quality' )
          IF ( IN1_NELM .EQ. 1 ) THEN
              CALL MSG_PRNT( '          Data copied from second '/
     :                          /'input object for these points' )
          ELSE IF ( .NOT. OVER ) THEN
            CALL MSG_PRNT( '          Data copied from first '/
     :                       /'input object for these points' )
          END IF
        END IF

*    Report new bad points
        IF ( NBAD .GT. 0 ) THEN
          CALL MSG_SETI( 'NBAD', NBAD )
          CALL MSG_PRNT( 'WARNING : ^NBAD new bad points due to '/
     :                                       /'division by zero' )
        END IF

      ELSE
        CALL ARITHMETIC_SUMS( OPER,IN1_NELM,%VAL(IN1_DPTR),IN1_VNELM,
     :             %VAL(IN1_VPTR), IN2_NELM, %VAL(IN2_DPTR),
     :               IN2_VNELM, %VAL(IN2_VPTR), OUT_NELM,
     :           %VAL(OUT_DPTR),OUT_VNELM,%VAL(OUT_VPTR),STATUS)
      END IF

*  Transfer ancillary info
      IF ( .NOT. OVER ) THEN
        IF ( .NOT. ( IN1_PRIM .AND. IN2_PRIM ) ) THEN
          IF ( IN1_PRIM ) THEN
            TID = FID2
          ELSE
            TID = FID1
          END IF

*      Copy top level text
          CALL BDI_COPY( TID, 'Title,Label,Units', OFID, ' ', STATUS )

*      Copy axis data
          DO I = 1, OUT_NDIM
            CALL BDI_AXCOPY( TID, I, ' ', OFID, I, STATUS )
          END DO

*      Copy ancillaries
          CALL UDI_COPANC( TID, ' ', OFID, STATUS )

*      Copy HISTORY
          CALL HSI_COPY( TID, OFID, STATUS )

        END IF
      END IF

*  History update unless output is primitive
      IF ( .NOT. OUT_PRIM ) THEN
        CALL HSI_ADD( OFID, VERSION, STATUS )
        CALL HSI_PTXT( OFID, 1, 'Operation: '//OPER, STATUS )
        IF ( OVER ) THEN
          CALL HSI_PTXT( OFID, 1, 'Dataset was overwritten', STATUS )
        END IF
        NHIN = 1
        TEXT(1) = 'Input 1 : {INP1}'
        TEXT(2) = '  value = '
        NHIN = NHIN + 1
        TEXT(NHIN) = 'Input 2 : {INP2}'

*    Convert text
        NLINE = MAXTXT
        CALL USI_TEXT( NHIN, TEXT, NLINE, STATUS )
        CALL HSI_PTXT( OFID, NLINE, TEXT, STATUS )

      END IF

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE ARITHMETIC_QUAL(N,I1QOK,I1Q,I2QOK,I2Q,OQ,STATUS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'        ! Asterix quality definitions
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
        INTEGER N		! # points
        LOGICAL I1QOK,I2QOK
        BYTE I1Q(*),I2Q(*)
*    Export :
        BYTE OQ(*)		! data quality array
*    Functions :
        BYTE BIT_ORUB
*    Local variables :
	INTEGER I
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

        IF (I1QOK.AND.I2QOK) THEN

          DO I=1,N
            OQ(I)=BIT_ORUB(I1Q(I),I2Q(I))
          END DO

        ELSEIF (I1QOK) THEN

          DO I=1,N
            OQ(I)=I1Q(I)
          END DO

        ELSEIF (I2QOK) THEN

          DO I=1,N
            OQ(I)=I2Q(I)
          END DO

        ELSE

          DO I=1,N
            OQ(I)=QUAL__GOOD
          END DO

        END IF

      END IF

      END



*+  ARITHMETIC_SUMS - Perform 4 arithmetic operations specified by OPER
      SUBROUTINE ARITHMETIC_SUMS(OPER,
     :                   NA,A,NAV,AV,NB,B,NBV,BV,NC,C,NCV,CV,STATUS)
*    Description :
*     Performs one of 4 arithmetic operations (+,-,*,/) specified in OPER
*     on the REAL arrays:
*           For I=1 to NC, C(I)=A(I) <OPER> B(I)
*     Note that NA and NB need not be equal but if they are not, then one
*     of them must be unity (case of operating on an array with a scalar).
*     Variances are handled correctly, if present.
*     Error functions used are:
*        + or - :    CV=(AV + BV)
*             * :    CV=(AV*B**2) + (BV*A**2)
*             / :    CV=(AV/B**2) + (BV*(A/B**2)**2)
*     Note: RETURNS without action if OPER is incorrectly specified.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     28 Jan 86:  original (BHVAD::JCMP)
*     27 Feb 90:  variance handling corrected (RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
	CHARACTER OPER		! required operation
	INTEGER NA		! # of A elements
	REAL A(NA)		! input 1
	INTEGER NAV		! # of A variance elements
	REAL AV(*)		! may be of zero length
	INTEGER NB		! # of B elements
	REAL B(NB)		! input 2
	INTEGER NBV		! # of B variance elements
	REAL BV(*)		! may be of zero length
	INTEGER NC		! # C elements
	REAL C(NC)		! output
	INTEGER NCV		! # of C variance elements
*    Import-Export :
*    Export :
	REAL CV(*)		! may be of zero length
*    Status :
      INTEGER STATUS
*
*    Local variables :
*
	INTEGER I
*-

      IF (STATUS.NE.SAI__OK) RETURN

* Deal with variances first before data values are overwritten

	IF(NCV.NE.0) THEN

	   IF(OPER.EQ.'+'.OR.OPER.EQ.'-') THEN

*  value array  +-*/  value array  (value means has variance)
             IF     (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 CV(I)=AV(I)+BV(I)
               END DO

*  constant array +-*/ value array
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 CV(I)=BV(I)
               END DO

*  value array +-*/ constant array
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 CV(I)=AV(I)
               END DO

*  scalar value +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               CV(1)=AV(1)+BV(1)

*  scalar constant +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               CV(1)=BV(1)

*  scalar value +-*/ scalar constant
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               CV(1)=AV(1)

*  scalar value +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 CV(I)=AV(1)+BV(I)
               END DO

*  value array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               DO I=1,NA
                 CV(I)=AV(I)+BV(1)
               END DO

*  scalar constant +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 CV(I)=BV(I)
               END DO

*  value array +-*/ scalar constant
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 CV(I)=AV(I)
               END DO

*  scalar value +-*/ constant array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NB
                 CV(I)=AV(1)
               END DO

*  constant array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               DO I=1,NA
                 CV(I)=BV(1)
               END DO

             END IF


	   ELSEIF(OPER.EQ.'*') THEN

*  value array  +-*/  value array  (value means has variance)
             IF     (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 CV(I)=AV(I)*B(I)**2 + BV(I)*A(I)**2
               END DO

*  constant array +-*/ value array
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 CV(I)=BV(I)*A(I)**2
               END DO

*  value array +-*/ constant array
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 CV(I)=AV(I)*B(I)**2
               END DO

*  scalar value +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               CV(1)=AV(1)*B(1)**2 + BV(1)*A(1)**2

*  scalar constant +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               CV(1)=BV(1)*A(1)**2

*  scalar value +-*/ scalar constant
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               CV(1)=AV(1)*B(1)**2

*  scalar value +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 CV(I)=AV(1)*B(I)**2 + BV(I)*A(1)**2
               END DO

*  value array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               DO I=1,NA
                 CV(I)=AV(I)*B(1)**2 + BV(1)*A(I)**2
               END DO

*  scalar constant +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 CV(I)=BV(I)*A(1)**2
               END DO

*  value array +-*/ scalar constant
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 CV(I)=AV(I)*B(1)**2
               END DO

*  scalar value +-*/ constant array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NB
                 CV(I)=AV(1)*B(I)**2
               END DO

*  constant array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               DO I=1,NA
                 CV(I)=BV(1)*A(I)**2
               END DO

             END IF


	   ELSEIF(OPER.EQ.'/') THEN

*  value array  +-*/  value array  (value means has variance)
             IF     (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 IF (B(I).NE.0.0) THEN
                   CV(I)=AV(I)/B(I)**2 + BV(I)*A(I)**2/B(I)**4
                 ELSE
                   CV(I)=0.0
                 END IF
               END DO

*  constant array +-*/ value array
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 IF (B(I).NE.0.0) THEN
                   CV(I)=BV(I)*A(I)**2/B(I)**4
                 ELSE
                   CV(I)=0.0
                 END IF
               END DO

*  value array +-*/ constant array
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 IF (B(I).NE.0.0) THEN
                   CV(I)=AV(I)/B(I)**2
                 ELSE
                   CV(I)=0.0
                 END IF
               END DO

*  scalar value +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF (B(1).NE.0.0) THEN
                 CV(1)=AV(1)/B(1)**2 + BV(1)*A(1)**2/B(1)**4
               ELSE
                 CV(1)=0.0
               END IF

*  scalar constant +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF (B(1).NE.0.0) THEN
                 CV(1)=BV(1)*A(1)**2/B(1)**4
               ELSE
                 CV(1)=0.0
               END IF

*  scalar value +-*/ scalar constant
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               IF (B(1).NE.0.0) THEN
                 CV(1)=AV(1)/B(1)**2
               ELSE
                 CV(1)=0.0
               END IF

*  scalar value +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 IF (B(I).NE.0.0) THEN
                   CV(I)=AV(1)/B(I)**2 + BV(I)*A(1)**2/B(I)**4
                 ELSE
                   CV(I)=0.0
                 END IF
               END DO

*  value array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF (B(1).NE.0.0) THEN
                 DO I=1,NA
                   CV(I)=AV(I)/B(1)**2 + BV(1)*A(I)**2/B(1)**4
                 END DO
               ELSE
                 DO I=1,NA
                   CV(I)=0.0
                 END DO
               END IF

*  scalar constant +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 IF (B(I).NE.0.0) THEN
                   CV(I)=BV(I)*A(1)**2/B(I)**4
                 ELSE
                   CV(I)=0.0
                 END IF
               END DO

*  value array +-*/ scalar constant
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               IF (B(1).NE.0.0) THEN
                 DO I=1,NA
                   CV(I)=AV(I)/B(1)**2
                 END DO
               ELSE
                 DO I=1,NA
                   CV(I)=0.0
                 END DO
               END IF

*  scalar value +-*/ constant array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NB
                 IF (B(I).NE.0.0) THEN
                   CV(I)=AV(1)/B(I)**2
                 ELSE
                   CV(I)=0.0
                 END IF
               END DO

*  constant array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF (B(1).NE.0.0) THEN
                 DO I=1,NA
                   CV(I)=BV(1)*A(I)**2/B(1)**4
                 END DO
               ELSE
                 DO I=1,NA
                   CV(I)=0.0
                 END DO
               END IF

             END IF


	   END IF

	END IF

*  now do data
	IF(OPER.EQ.'+') THEN
	   IF(NA.EQ.1.AND.NB.NE.1) THEN
	      DO I=1,NC
	         C(I)=A(1)+B(I)
	      END DO
	   ELSEIF(NA.NE.1.AND.NB.EQ.1) THEN
	      DO I=1,NC
	         C(I)=A(I)+B(1)
	      END DO
	   ELSE
	      DO I=1,NC
	         C(I)=A(I)+B(I)
	      END DO
	   END IF

	ELSEIF(OPER.EQ.'-') THEN
	   IF(NA.EQ.1.AND.NB.NE.1) THEN
	      DO I=1,NC
	         C(I)=A(1)-B(I)
	      END DO
	   ELSEIF(NA.NE.1.AND.NB.EQ.1) THEN
	      DO I=1,NC
	         C(I)=A(I)-B(1)
	      END DO
	   ELSE
	      DO I=1,NC
	         C(I)=A(I)-B(I)
	      END DO
	   END IF

	ELSEIF(OPER.EQ.'*') THEN
	   IF(NA.EQ.1.AND.NB.NE.1) THEN
	      DO I=1,NC
	         C(I)=A(1)*B(I)
	      END DO
	   ELSEIF(NA.NE.1.AND.NB.EQ.1) THEN
	      DO I=1,NC
	         C(I)=A(I)*B(1)
	      END DO
	   ELSE
	      DO I=1,NC
	         C(I)=A(I)*B(I)
	      END DO
	   END IF

	ELSEIF(OPER.EQ.'/') THEN
	   IF(NA.EQ.1.AND.NB.NE.1) THEN
	      DO I=1,NC
	         IF(B(I).NE.0.) THEN
	            C(I)=A(1)/B(I)
                 ELSE
                    C(I)=0.0
	         END IF
	      END DO
	   ELSEIF(NA.NE.1.AND.NB.EQ.1) THEN
	      IF(B(1).NE.0.) THEN
	         DO I=1,NC
	            C(I)=A(I)/B(1)
	         END DO
              ELSE
                 DO I=1,NC
                   C(I)=0.0
                 END DO
	      END IF
	   ELSE
	      DO I=1,NC
	         IF(B(I).NE.0.) THEN
	            C(I)=A(I)/B(I)
                 ELSE
                    C(I)=0.0
	         END IF
	      END DO
	   END IF

	END IF

	END



*+  ARITHMETIC_SUMSQ - Perform 4 arithmetic operations specified by OPER
      SUBROUTINE ARITHMETIC_SUMSQ(OPER,
     :      NA,A,NAV,AV,NB,B,NBV,BV,NC,CQ,CQM,C,NCV,CV,NIG,NBAD,STATUS)
*    Description :
*     Performs one of 4 arithmetic operations (+,-,*,/) specified in OPER
*     on the REAL arrays:
*           For I=1 to NC, C(I)=A(I) <OPER> B(I) if CQ(I) ok
*     Note that NA and NB need not be equal but if they are not, then one
*     of them must be unity (case of operating on an array with a scalar).
*     Variances are handled correctly, if present.
*     Error functions used are:
*        + or - :    CV=(AV + BV)
*             * :    CV=(AV*B**2) + (BV*A**2)
*             / :    CV=(AV/B**2) + (BV*(A/B**2)**2)
*     Note: RETURNS without action if OPER is incorrectly specified.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     28 Jan 86:  original (BHVAD::JCMP)
*     27 Feb 90:  variance handling corrected (RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'        ! Asterix quality definitions
*
*    Import :
*
	CHARACTER OPER		! required operation
	INTEGER NA		! # of A elements
	REAL A(NA)		! input 1
	INTEGER NAV		! # of A variance elements
	REAL AV(*)		! may be of zero length
	INTEGER NB		! # of B elements
	REAL B(NB)		! input 2
	INTEGER NBV		! # of B variance elements
	REAL BV(*)		! may be of zero length
	INTEGER NC		! # C elements
        BYTE CQM                ! Output quality mask
	REAL C(NC)		! output
	INTEGER NCV		! # of C variance elements
*    Import-Export :
        BYTE CQ(*)              ! Output quality
*    Export :
	REAL CV(*)		! may be of zero length
        INTEGER NIG             ! Number of ignored points
        INTEGER NBAD            ! Number of new bad points
*    Status :
      INTEGER STATUS
*    Global variables :
*
*    Functions :
*
      BYTE			BIT_ANDUB
      BYTE			BIT_ORUB
*    Local variables :
	INTEGER I
*    Internal References :
*    Local data :
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      NIG = 0
      NBAD = 0

* Deal with variances first before data values are overwritten

	IF(NCV.NE.0) THEN

	   IF(OPER.EQ.'+'.OR.OPER.EQ.'-') THEN

*  value array  +-*/  value array  (value means has variance)
             IF ( NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   CV(I)=AV(I)+BV(I)
                 ELSE
                   CV(I)=AV(I)
                 END IF
               END DO

*  constant array +-*/ value array
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 CV(I)=BV(I)
               END DO

*  value array +-*/ constant array
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 CV(I)=AV(I)
               END DO

*  scalar value +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF ( BIT_ANDUB(CQ(1),CQM) .EQ. QUAL__GOOD ) THEN
                 CV(1)=AV(1)+BV(1)
               ELSE
                 CV(1)=AV(1)
               END IF

*  scalar constant +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF ( BIT_ANDUB(CQ(1),CQM) .EQ. QUAL__GOOD ) THEN
                 CV(1)=BV(1)
               ELSE
                 CV(1)=BV(1)
               END IF

*  scalar value +-*/ scalar constant
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               CV(1)=AV(1)

*  scalar value +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   CV(I)=AV(1)+BV(I)
                 ELSE
                   CV(I)=BV(I)
                 END IF
               END DO

*  value array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               DO I=1,NA
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   CV(I)=AV(I)+BV(1)
                 ELSE
                   CV(I)=AV(I)
                 END IF
               END DO

*  scalar constant +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 CV(I)=BV(I)
               END DO

*  value array +-*/ scalar constant
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 CV(I)=AV(I)
               END DO

*  scalar value +-*/ constant array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NB
                 CV(I)=AV(1)
               END DO

*  constant array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               DO I=1,NA
                 CV(I)=BV(1)
               END DO

             END IF

	   ELSEIF(OPER.EQ.'*') THEN

*  value array  +-*/  value array  (value means has variance)
             IF (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   CV(I)=AV(I)*B(I)**2 + BV(I)*A(I)**2
                 ELSE
                   CV(I)=AV(I)
                 END IF
               END DO

*  constant array +-*/ value array
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   CV(I) = BV(I)*A(I)**2
                 ELSE
                   CV(I) = BV(I)
                 END IF
               END DO

*  value array +-*/ constant array
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   CV(I)=AV(I)*B(I)**2
                 ELSE
                   CV(I) = AV(I)
                 END IF
               END DO

*  scalar value +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF ( BIT_ANDUB(CQ(1),CQM) .EQ. QUAL__GOOD ) THEN
                 CV(1)=AV(1)*B(1)**2 + BV(1)*A(1)**2
               ELSE
                 CV(1) = AV(1)
               END IF

*  scalar constant +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF ( BIT_ANDUB(CQ(1),CQM) .EQ. QUAL__GOOD ) THEN
                 CV(1)=BV(1)*A(1)**2
               ELSE
                 CV(1)=BV(1)
               END IF

*  scalar value +-*/ scalar constant
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               IF ( BIT_ANDUB(CQ(1),CQM) .EQ. QUAL__GOOD ) THEN
                 CV(1)=AV(1)*B(1)**2
               ELSE
                 CV(1)=AV(1)
               END IF

*  scalar value +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   CV(I)=AV(1)*B(I)**2 + BV(I)*A(1)**2
                 ELSE
                   CV(I)=AV(1)
                 END IF
               END DO

*  value array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               DO I=1,NA
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   CV(I)=AV(I)*B(1)**2 + BV(1)*A(I)**2
                 ELSE
                   CV(I)=AV(I)
                 END IF
               END DO

*  scalar constant +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   CV(I)=BV(I)*A(1)**2
                 ELSE
                   CV(I)=BV(I)
                 END IF
               END DO

*  value array +-*/ scalar constant
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   CV(I)=AV(I)*B(1)**2
                 ELSE
                   CV(I)=AV(I)
                 END IF
               END DO

*  scalar value +-*/ constant array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NB
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   CV(I)=AV(1)*B(I)**2
                 ELSE
                   CV(I)=AV(1)
                 END IF
               END DO

*  constant array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               DO I=1,NA
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   CV(I)=BV(1)*A(I)**2
                 ELSE
                   CV(I)=BV(1)
                 END IF
               END DO

             END IF


	   ELSEIF(OPER.EQ.'/') THEN

*  value array  +-*/  value array  (value means has variance)
             IF     (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   IF (B(I).NE.0.0) THEN
                     CV(I)=AV(I)/B(I)**2 + BV(I)*A(I)**2/B(I)**4
                   ELSE
                     CV(I)=0.0
                   END IF
                 ELSE
                   CV(I)=AV(I)
                 END IF
               END DO

*  constant array +-*/ value array
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   IF (B(I).NE.0.0) THEN
                     CV(I)=BV(I)*A(I)**2/B(I)**4
                   ELSE
                     CV(I)=0.0
                   END IF
                 ELSE
                   CV(I)=BV(I)
                 END IF
               END DO

*  value array +-*/ constant array
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   IF (B(I).NE.0.0) THEN
                     CV(I)=AV(I)/B(I)**2
                   ELSE
                     CV(I)=0.0
                   END IF
                 ELSE
                   CV(I)=AV(I)
                 END IF
               END DO

*  scalar value +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF ( BIT_ANDUB(CQ(1),CQM) .EQ. QUAL__GOOD ) THEN
                 IF (B(1).NE.0.0) THEN
                   CV(1)=AV(1)/B(1)**2 + BV(1)*A(1)**2/B(1)**4
                 ELSE
                   CV(1)=0.0
                 END IF
               ELSE
                 CV(1)=AV(1)
               END IF

*  scalar constant +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF ( BIT_ANDUB(CQ(1),CQM) .EQ. QUAL__GOOD ) THEN
                 IF (B(1).NE.0.0) THEN
                   CV(1)=BV(1)*A(1)**2/B(1)**4
                 ELSE
                   CV(1)=0.0
                 END IF
               ELSE
                 CV(1)=BV(1)
               END IF

*  scalar value +-*/ scalar constant
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               IF ( BIT_ANDUB(CQ(1),CQM) .EQ. QUAL__GOOD ) THEN
                 IF (B(1).NE.0.0) THEN
                   CV(1)=AV(1)/B(1)**2
                 ELSE
                   CV(1)=0.0
                 END IF
               ELSE
                 CV(1)=AV(1)
               END IF

*  scalar value +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   IF (B(I).NE.0.0) THEN
                     CV(I)=AV(1)/B(I)**2 + BV(I)*A(1)**2/B(I)**4
                   ELSE
                     CV(I)=0.0
                   END IF
                 ELSE
                   CV(I)=BV(I)
                 END IF
               END DO

*  value array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF (B(1).NE.0.0) THEN
                 DO I=1,NA
                   IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                     CV(I)=AV(I)/B(1)**2 + BV(1)*A(I)**2/B(1)**4
                   ELSE
                     CV(I)=AV(I)
                   END IF
                 END DO
               ELSE
                 DO I=1,NA
                   IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                     CV(I)=0.0
                   ELSE
                     CV(I)=AV(I)
                   END IF
                 END DO
               END IF

*  scalar constant +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   IF (B(I).NE.0.0) THEN
                     CV(I)=BV(I)*A(1)**2/B(I)**4
                   ELSE
                     CV(I)=0.0
                   END IF
                 ELSE
                   CV(I)=BV(I)
                 END IF
               END DO

*  value array +-*/ scalar constant
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               IF (B(1).NE.0.0) THEN
                 DO I=1,NA
                   IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                     CV(I)=AV(I)/B(1)**2
                   ELSE
                     CV(I)=AV(I)
                   END IF
                 END DO
               ELSE
                 DO I=1,NA
                   IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                     CV(I)=0.0
                   ELSE
                     CV(I)=AV(I)
                   END IF
                 END DO
               END IF

*  scalar value +-*/ constant array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NB
                 IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                   IF (B(I).NE.0.0) THEN
                     CV(I)=AV(1)/B(I)**2
                   ELSE
                     CV(I)=0.0
                   END IF
                 ELSE
                   CV(I)=AV(1)
                 END IF
               END DO

*  constant array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF (B(1).NE.0.0) THEN
                 DO I=1,NA
                   IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                     CV(I)=BV(1)*A(I)**2/B(1)**4
                   ELSE
                     CV(I)=BV(1)
                   END IF
                 END DO
               ELSE
                 DO I=1,NA
                   IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                     CV(I)=0.0
                   ELSE
                     CV(I)=BV(1)
                   END IF

                 END DO
               END IF

             END IF

	   END IF

	END IF

*  now do data
	IF(OPER.EQ.'+') THEN
	   IF(NA.EQ.1.AND.NB.NE.1) THEN
	      DO I=1,NC
                IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                  C(I)=A(1)+B(I)
                ELSE
                  NIG = NIG + 1
                  C(I)=B(I)
                END IF
	      END DO
	   ELSEIF(NA.NE.1.AND.NB.EQ.1) THEN
	      DO I=1,NC
                IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                  C(I)=A(I)+B(1)
                ELSE
                  NIG = NIG + 1
                  C(I)=A(I)
                END IF
	      END DO
	   ELSE
	      DO I=1,NC
                IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                  C(I)=A(I)+B(I)
                ELSE
                  NIG = NIG + 1
                  C(I)=A(I)
                END IF
	      END DO
	   END IF

	ELSEIF(OPER.EQ.'-') THEN
	   IF(NA.EQ.1.AND.NB.NE.1) THEN
	      DO I=1,NC
                IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                  C(I)=A(1)-B(I)
                ELSE
                  NIG = NIG + 1
                  C(I)=B(I)
                END IF
	      END DO
	   ELSEIF(NA.NE.1.AND.NB.EQ.1) THEN
	      DO I=1,NC
                IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                  C(I)=A(I)-B(1)
                ELSE
                  NIG = NIG + 1
                  C(I)=A(I)
                END IF
	      END DO
	   ELSE
	      DO I=1,NC
                IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                  C(I)=A(I)-B(I)
                ELSE
                  NIG = NIG + 1
                  C(I)=A(I)
                END IF
	      END DO
	   END IF

	ELSEIF(OPER.EQ.'*') THEN
	   IF(NA.EQ.1.AND.NB.NE.1) THEN
	      DO I=1,NC
                IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                  C(I)=A(1)*B(I)
                ELSE
                  NIG = NIG + 1
                  C(I)= B(I)
                END IF
	      END DO
	   ELSEIF(NA.NE.1.AND.NB.EQ.1) THEN
	      DO I=1,NC
                IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                  C(I)=A(I)*B(1)
                ELSE
                  NIG = NIG + 1
                  C(I)= A(I)
                END IF
	      END DO
	   ELSE
	      DO I=1,NC
                IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                  C(I)=A(I)*B(I)
                ELSE
                  NIG = NIG + 1
                  C(I)= A(I)
                END IF
	      END DO
	   END IF

	ELSEIF(OPER.EQ.'/') THEN
	   IF(NA.EQ.1.AND.NB.NE.1) THEN
	      DO I=1,NC
                IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
	          IF(B(I).NE.0.) THEN
	            C(I)=A(1)/B(I)
                  ELSE
                    C(I)=0.0
                    CQ(I) = BIT_ORUB(CQ(I),QUAL__ARITH)
                    NBAD = NBAD + 1
	          END IF
                ELSE
                  NIG = NIG + 1
                  C(I) = B(I)
                END IF
	      END DO
	   ELSEIF(NA.NE.1.AND.NB.EQ.1) THEN
	      IF(B(1).NE.0.) THEN
	         DO I=1,NC
                   IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                     C(I)=A(I)/B(1)
                   ELSE
                     NIG = NIG + 1
                     C(I)=A(I)
                   END IF
	         END DO
              ELSE
                 DO I=1,NC
                   C(I)=0.0
                   CQ(I) = BIT_ORUB(CQ(I),QUAL__ARITH)
                 END DO
                 NBAD = NC
	      END IF
	   ELSE
	      DO I=1,NC
                IF ( BIT_ANDUB(CQ(I),CQM) .EQ. QUAL__GOOD ) THEN
                  IF ( B(I).NE.0.0 ) THEN
	            C(I)=A(I)/B(I)
                  ELSE
                    C(I)=0.0
                    CQ(I) = QUAL__ARITH
                    NBAD = NBAD + 1
                  END IF
                ELSE
                  NIG = NIG + 1
                  C(I)=A(I)
                  CQ(I) = BIT_ORUB(CQ(I),QUAL__ARITH)
                END IF
	      END DO
	   END IF

	END IF

	END
