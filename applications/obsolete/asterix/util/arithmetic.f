*+  ARITHMETIC  - Does C = A o B
      SUBROUTINE ARITHMETIC ( STATUS )
*
*    Description :
*
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
*
*    Parameters :
*
*     INP1=UNIV(R)
*           First input data object
*     INP2=UNIV(R)
*           Second input data object
*     OUT=UNIV(U)
*           Output data object
*     OPER=CHAR(R)
*           Type of operation (+,-,*,/)
*     ERR1=UNIV(R)
*           error estimate for first primitive data object
*     ERR2=UNIV(R)
*           error estimate for second primitive data object
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Jim Peden (BHVAD::JCMP)
*
*    History :
*
*     27 Jan 86 : V0.4-1 Modified from various programs (BHVAD::JCMP)
*     30 Apr 86 : V0.4-2 Better error propagation (BHVAD::JCMP)
*     16 May 86 : V0.4-3 Better condition handling (BHVAD::JCMP)
*     14 Aug 86 : V0.5-1 Data quality handling (BHVAD::JCMP)
*     14 Nov 86 : V0.5-2 Modified UTIL_ASSOC3 to allow ops on 2 prim objs JKD
*      8 Jan 87 : V0.6-1 Bug fixed in quality handling for scalar divisor (TJP)
*     13 Jan 87 : V0.6-2 Mod. to allow primitive output (TJP)
*      4 Jun 87 : V0.6-1 Renamed ARITHMETIC V0.6-1
*      3 May 88 : V1.0-1 Asterix88 version (BHVAD::ADM)
*      9 Dec 89 : V1.0-2 Now uses BDA calls - code tidied (BHVAD::DJA)
*     12 Jan 90 : V1.0-3 A few bug fixes done (BHVAD::DJA)
*     16 Feb 90 : V1.2-1 Quality bug if none in input fixed. Now uses
*                        quality include file QUAL_PAR (BHVAD::DJA)
*     27 Feb 90 : V1.2-2 Variance handling fixed (RJV)
*      1 Mar 90 : V1.2-3 Overwrite mode now works (RJV)
*     28 May 90 : V1.2-4 Overwrite mode really does work now (honest) (DJA)
*     10 Aug 90 : V1.2-5 Overwrite for primitive input doesn't try to
*                        create QUALITY (RJV)
*     10 May 91 : V1.4-0 Points with bad quality no longer operated on (DJA)
*     23 May 91 : V1.4-1 Bug fix from above (DJA)
*     28 May 91 : V1.4-2 Manual error more robust (DJA)
*     25 Mar 92 : V1.6-0 Use ERR_ANNUL (DJA)
*      9 Jul 92 : V1.6-1 Corrected erroneous ACCESS2 argument to USI_ASSOC3.
*      2 Dec 93 : V1.7-0 Removed use of structures - was causing many problems
*                        on Solaris (DJA).
*      1 Mar 94 : V1.7-1 Updated handling of bit quality - now use BIT_
*                        routines and QUAL__ constants (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'        ! Asterix quality definitions
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      BYTE			BIT_ANDUB
*
*    Local variables :
*
      CHARACTER              OPER             ! Operation
      CHARACTER*(DAT__SZLOC) LOC	      ! Temp locator
      CHARACTER*10           MODE             ! 'READ' or 'UPDATE'
      CHARACTER*(DAT__SZLOC) IN1_LOC, IN2_LOC ! Input datasets
C     CHARACTER*200          IN1_NAME, IN2_NAME
      CHARACTER*(DAT__SZLOC) IN1_VLOC, IN2_VLOC !
      CHARACTER*(DAT__SZLOC) OUT_LOC          ! Output dataset
      CHARACTER*(DAT__SZTYP) OUT_TYPE         ! Output array type

      CHARACTER*80 TEXTI(8)                   ! Input files
      CHARACTER*80 TEXTO(8)                   ! Output file
      CHARACTER*80 TEXT(7)                    ! History text

      INTEGER I				      ! index
      INTEGER LEVELS                          ! # lines in input name
      INTEGER LEVELSO                         ! # lines in output
      INTEGER NAX                             ! # axes present
      INTEGER NBAD                            ! # new bad points
      INTEGER NIG                             ! Number of ignored points
      INTEGER IN1_DPTR, IN2_DPTR, OUT_DPTR    ! Dataset data pointers
      INTEGER IN1_DIMS(DAT__MXDIM)            ! 1st input dimensions
      INTEGER IN2_DIMS(DAT__MXDIM)            ! 2nd input dimensions
      INTEGER OUT_DIMS(DAT__MXDIM)            ! Output dimensions
      INTEGER IN1_NELM, IN2_NELM, OUT_NELM    ! # elements in datasets
      INTEGER IN1_NDIM, IN2_NDIM, OUT_NDIM    ! Dimensionality of datasets
      INTEGER IN1_VDIMS(DAT__MXDIM)           ! Dimensions of 1st input error
      INTEGER IN2_VDIMS(DAT__MXDIM)           ! Dimensions of 2nd input error
      INTEGER IN1_QPTR, IN2_QPTR, OUT_QPTR    ! Dataset quality pointers
      INTEGER IN1_VPTR, IN2_VPTR, OUT_VPTR    ! Dataset variance pointers
      INTEGER IN1_VNDIM, IN2_VNDIM            ! Dataset variance dimensionality
      INTEGER IN1_VNELM, IN2_VNELM, OUT_VNELM ! # elements in dataset variances
      INTEGER                 TDIMS(DAT__MXDIM)
      INTEGER                 TNDIM

      BYTE                    IN1_MASK        ! 1st input quality mask
      BYTE                    IN2_MASK        ! 2nd input quality mask
      BYTE                    OUT_MASK        ! Output quality mask

      LOGICAL                 DQD 	      ! Division operation
      LOGICAL                 ERRORS          ! Prompt for input errors?
      LOGICAL                 IN1_PRIM        ! 1st input primitive?
      LOGICAL                 IN2_PRIM        ! 2nd input primitive?
      LOGICAL                 IN1_QOK, IN2_QOK, OUT_QOK
      LOGICAL                 IN1_SCALAR      ! 1st input scalar?
      LOGICAL                 IN2_SCALAR      ! 2nd input scalar?
      LOGICAL                 IN1_UERR        ! Supplied error for input #1
      LOGICAL                 IN2_UERR        ! Supplied error for input #1
      LOGICAL                 IN1_VOK, IN2_VOK! Variance present in inputs?
      LOGICAL                 OK              ! General validity test
      LOGICAL                 OUT_PRIM        ! Output primitive?
      LOGICAL                 OVER            ! Overwrite input file?
      LOGICAL                 SAME12          ! Input 1 same as input 2
*
*    Local data :
*
      CHARACTER*30            VERSION
         PARAMETER            (VERSION = 'ARITHMETIC Version 1.8-0')
*-

*    Version id
      CALL MSG_PRNT( VERSION )

*    Initialise
      CALL AST_INIT()

*    Initialise logicals
      IN1_UERR = .FALSE.
      IN2_UERR = .FALSE.

*    Overwrite existing input?
      CALL USI_GET0L( 'OVER', OVER, STATUS )

*    Associate data objects
      IF ( OVER ) THEN
        CALL MSG_PRNT( 'First input file will be overwritten' )
        CALL USI_ASSOCI( 'INP1', 'UPDATE',IN1_LOC, IN1_PRIM, STATUS )
        CALL USI_ASSOCI( 'INP2', 'READ',  IN2_LOC, IN2_PRIM, STATUS )
        OUT_LOC = IN1_LOC
        OUT_PRIM = IN1_PRIM
        CALL USI_NAMEI( LEVELS, TEXTI, STATUS )
      ELSE
        CALL USI_ASSOC3('INP1','INP2','OUT','READ','READ', IN1_LOC,
     :         IN2_LOC, OUT_LOC, SAME12, IN1_PRIM, IN2_PRIM,STATUS )
        CALL USI_NAMEI(LEVELS,TEXTI,STATUS)
        CALL USI_NAMEO(LEVELSO,TEXTO,STATUS)
      END IF

*    See if errors wanted for primitive input
      IF ( IN1_PRIM .OR. IN2_PRIM ) THEN
        CALL USI_GET0L( 'ERRORS', ERRORS, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check size of input datasets
      CALL BDA_CHKDATA( IN1_LOC, OK, IN1_NDIM, IN1_DIMS, STATUS )
      IF ( OK ) THEN
        IN1_SCALAR = ( IN1_NDIM .EQ. 0 )
        IF ( IN1_SCALAR ) THEN
          IN1_NELM = 1
        ELSE
          CALL ARR_SUMDIM( IN1_NDIM, IN1_DIMS, IN1_NELM )
        END IF
      ELSE
        CALL ERR_REP( ' ', 'Bad input data - not numeric', STATUS )
      END IF
      CALL BDA_CHKDATA( IN2_LOC, OK, IN2_NDIM, IN2_DIMS, STATUS )
      IF ( OK ) THEN
        IN2_SCALAR = ( IN2_NDIM .EQ. 0 )
        IF ( IN2_SCALAR ) THEN
          IN2_NELM = 1
        ELSE
          CALL ARR_SUMDIM( IN2_NDIM, IN2_DIMS, IN2_NELM )
        END IF
      ELSE
        CALL ERR_REP( ' ', 'Bad input data - not numeric', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Datasets must be same size or unity
      IF ( IN2_NELM .NE. IN1_NELM ) THEN
        IF ( .NOT. ( IN1_NELM .EQ.1.OR. IN2_NELM .EQ.1 ) ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Bad data sizes: not equal'/
     :             /' and neither one is unity', STATUS )
          GOTO 99
        END IF
      END IF

*    Obtain errors and validate for first object
      IF ( IN1_PRIM ) THEN
        IF ( ERRORS ) THEN
          CALL USI_DASSOC( 'ERR1', 'READ', IN1_VLOC, STATUS)
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_VALID( IN1_VLOC, IN1_VOK, STATUS )
            IN1_UERR = IN1_VOK
          ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            IN1_VOK = .FALSE.
          END IF
        END IF
      ELSE
        CALL BDA_CHKVAR( IN1_LOC, IN1_VOK, IN1_VNDIM,
     :                               IN1_VDIMS, STATUS )
      END IF
      IF ( ( STATUS .EQ. SAI__OK ) .AND. IN1_VOK ) THEN
        IF ( IN1_UERR ) THEN
          CALL DAT_SHAPE( IN1_VLOC, DAT__MXDIM, IN1_VDIMS,
     :                                    IN1_VNDIM, STATUS )
        END IF
        CALL ARR_SUMDIM( IN1_VNDIM, IN1_VDIMS, IN1_VNELM )
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

*    Obtain errors and validate for first second
      IF ( IN2_PRIM ) THEN
        IF ( ERRORS ) THEN
          CALL USI_DASSOC( 'ERR2', 'READ', IN2_VLOC, STATUS)
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_VALID( IN2_VLOC, IN2_VOK, STATUS )
            IN2_UERR = IN2_VOK
          ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            IN2_VOK = .FALSE.
          END IF
        END IF
      ELSE
        CALL BDA_CHKVAR( IN2_LOC, IN2_VOK, IN2_VNDIM,
     :                               IN2_VDIMS, STATUS )
      END IF
      IF ( ( STATUS .EQ. SAI__OK ) .AND. IN2_VOK ) THEN
        IF ( IN2_UERR ) THEN
          CALL DAT_SHAPE( IN2_VLOC, DAT__MXDIM, IN2_VDIMS,
     :                                    IN2_VNDIM, STATUS )
        END IF
        CALL ARR_SUMDIM( IN2_VNDIM, IN2_VDIMS, IN2_VNELM )
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

*    Obtain operation type
      CALL USI_GET0C( 'OPER', OPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Select mapping mode
      IF ( OVER ) THEN
        MODE='UPDATE'
      ELSE
        MODE='READ'
      END IF

*    Map first object
      CALL BDA_MAPDATA( IN1_LOC, MODE, IN1_DPTR, STATUS )
      IF ( IN1_PRIM .AND. IN1_VOK ) THEN
         CALL DAT_MAPV( IN1_VLOC, '_REAL', MODE, IN1_VPTR,
     :                                    IN1_VNELM, STATUS )
         IF ( IN1_UERR ) THEN
            CALL ARR_SQR1R( %VAL(IN1_VPTR), IN1_VNELM, STATUS )
         END IF
      ELSE IF ( IN1_VOK ) THEN
         CALL BDA_MAPVAR( IN1_LOC, MODE, IN1_VPTR, STATUS )
      END IF

*    And second object - always map for read
      CALL BDA_MAPDATA( IN2_LOC, 'READ', IN2_DPTR, STATUS )
      IF ( IN2_PRIM .AND. IN2_VOK ) THEN
        CALL DAT_MAPV( IN2_VLOC, '_REAL', 'READ', IN2_VPTR,
     :                                     IN2_VNELM, STATUS )
        IF ( IN2_UERR ) THEN
          CALL ARR_SQR1R( %VAL(IN2_VPTR), IN2_VNELM, STATUS )
        END IF
      ELSE IF ( IN2_VOK ) THEN
        CALL BDA_MAPVAR( IN2_LOC, 'READ', IN2_VPTR, STATUS )
      END IF

*    Adjust names of inputs if scalar
c      IF ( IN1_SCALAR ) THEN
c        CALL CHR_RTOC( %VAL(IN1_DPTR), IN1_NAME, TLEN )
c      END IF
c      IF ( IN2_SCALAR ) THEN
c        CALL CHR_RTOC( %VAL(IN2_DPTR), IN2.NAME, TLEN )
c      END IF

*    Create components in output dataset
      IF ( .NOT. IN1_PRIM ) THEN
        CALL CMP_TYPE( IN1_LOC, 'DATA_ARRAY', OUT_TYPE, STATUS )
      ELSE IF ( .NOT. IN2_PRIM ) THEN
        CALL CMP_TYPE( IN2_LOC, 'DATA_ARRAY', OUT_TYPE, STATUS )
      ELSE IF ( IN1_PRIM ) THEN
        CALL DAT_TYPE( IN1_LOC, OUT_TYPE, STATUS )
      ELSE IF ( IN2_PRIM ) THEN
        CALL DAT_TYPE( IN2_LOC, OUT_TYPE, STATUS )
      END IF
      IF ( .NOT. OVER ) THEN
        IF ( .NOT. IN1_SCALAR ) THEN
          OUT_NDIM = IN1_NDIM
          DO I = 1, DAT__MXDIM
            OUT_DIMS(I) = IN1_DIMS(I)
          END DO
        ELSE
          OUT_NDIM = IN2_NDIM
          DO I = 1, DAT__MXDIM
            OUT_DIMS(I) = IN2_DIMS(I)
          END DO
        END IF
        CALL BDA_CREDATA( OUT_LOC, OUT_NDIM, OUT_DIMS, STATUS )
        IF ( IN1_VOK .OR. IN2_VOK ) THEN
          CALL BDA_CREVAR( OUT_LOC, OUT_NDIM, OUT_DIMS, STATUS )
        END IF
      END IF

*    Find no. output points.
      IF (OVER) THEN
        OUT_NELM=MAX(IN1_NELM,IN2_NELM)
        OUT_VNELM=MAX(IN1_VNELM,IN2_VNELM)
      ELSE
        IF ( OUT_NDIM .GT. 0 ) THEN
          CALL ARR_SUMDIM( OUT_NDIM, OUT_DIMS, OUT_NELM )
        ELSE
          OUT_NELM = 1
        END IF
        IF ( IN1_VNELM.EQ.0 .AND. IN2_VNELM.EQ.0 ) THEN
          OUT_VNELM = 0
        ELSE
          OUT_VNELM = OUT_NELM
        END IF
      END IF

*    Map output dataset data
      IF ( OVER ) THEN
        OUT_DPTR = IN1_DPTR
        OUT_VPTR = IN1_VPTR
      ELSE
        CALL BDA_MAPDATA( OUT_LOC, 'WRITE', OUT_DPTR, STATUS )
        IF ( IN1_VOK .OR. IN2_VOK  ) THEN
          CALL BDA_MAPVAR( OUT_LOC, 'WRITE', OUT_VPTR, STATUS )
        ELSE
          OUT_VNELM = 0
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    See what input quality flags we have
      CALL BDA_CHKQUAL( IN1_LOC, IN1_QOK, TNDIM, TDIMS, STATUS )
      CALL BDA_CHKQUAL( IN2_LOC, IN2_QOK, TNDIM, TDIMS, STATUS )

*    Decide whether to set up or modify QUALITY in output dataset
      DQD = (OPER.EQ.'/') .AND. (IN2_NELM.GT.1) .AND. .NOT. OUT_PRIM
      OUT_QOK = .FALSE.
      OUT_QPTR = 0
      OUT_MASK = QUAL__MASK
      IF ( IN1_QOK .OR. IN2_QOK .OR. DQD ) THEN
         IF ( OVER .AND. .NOT. OUT_PRIM ) THEN
           IF ( .NOT. IN1_QOK ) THEN
*   Create output QUALITY
             CALL BDA_CREQUAL(IN1_LOC,IN1_NDIM,IN1_DIMS,STATUS)
             CALL BDA_PUTMASK(IN1_LOC,QUAL__MASK,STATUS)
             CALL BDA_MAPQUAL(IN1_LOC,'W',OUT_QPTR,STATUS)
             OUT_QOK = .TRUE.
           ELSE
             CALL BDA_MAPQUAL(IN1_LOC,'U',IN1_QPTR,STATUS)
             OUT_QPTR = IN1_QPTR
             IF ( IN2_QOK ) THEN
               CALL BDA_MAPQUAL(IN2_LOC,'R',IN2_QPTR,STATUS)
               OUT_QOK = .TRUE.
             END IF
           END IF

         ELSE

*          Create output QUALITY
            CALL BDA_CREQUAL(OUT_LOC,OUT_NDIM,OUT_DIMS,STATUS)
            CALL BDA_MAPQUAL( OUT_LOC,'W',OUT_QPTR,STATUS )

            IF ( IN1_QOK ) THEN
              CALL BDA_MAPQUAL(IN1_LOC,'R',IN1_QPTR,STATUS)
              CALL BDA_GETMASK( IN1_LOC, IN1_MASK, STATUS )
            ELSE
              IN1_MASK = QUAL__MASK
            END IF

            IF ( IN2_QOK ) THEN
              CALL BDA_MAPQUAL(IN2_LOC,'R',IN2_QPTR,STATUS)
              CALL BDA_GETMASK( IN2_LOC, IN2_MASK, STATUS )
            ELSE
              IN2_MASK = QUAL__MASK
            END IF

            OUT_MASK = BIT_ANDUB(IN1_MASK,IN2_MASK)
            CALL BDA_PUTMASK( OUT_LOC, OUT_MASK, STATUS )

         END IF

      END IF
      OUT_QOK = ( OUT_QPTR .NE. 0 )

*    Perform operation
      IF ( OUT_QOK ) THEN

        CALL ARITHMETIC_QUAL(OUT_NELM,IN1_QOK,%VAL(IN1_QPTR),
     :                                IN2_QOK,%VAL(IN2_QPTR),
     :                                   %VAL(OUT_QPTR), STATUS )

        CALL ARITHMETIC_SUMSQ( OPER, IN1_NELM, %VAL(IN1_DPTR),
     :                           IN1_VNELM, %VAL(IN1_VPTR),
     :                               IN2_NELM, %VAL(IN2_DPTR),
     :                           IN2_VNELM, %VAL(IN2_VPTR),
     :                               OUT_NELM,
     :              %VAL(OUT_QPTR), OUT_MASK,
     :         %VAL(OUT_DPTR),OUT_VNELM,%VAL(OUT_VPTR),
     :                                       NIG,NBAD,STATUS)

*      Report ignored points
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

*      Report new bad points
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

*    Transfer ancillary info
      IF ( .NOT. OVER ) THEN
        IF ( .NOT. ( IN1_PRIM .AND. IN2_PRIM ) ) THEN
          IF ( IN1_PRIM ) THEN
            LOC = IN2_LOC
          ELSE
            LOC = IN1_LOC
          END IF

*        Copy top level text
          CALL BDA_COPTEXT( LOC, OUT_LOC, STATUS )

*        Copy axis data
          CALL BDA_CHKAXES( LOC, NAX, STATUS )
          IF ( NAX .GT. 0 ) THEN
            CALL BDA_COPAXES( LOC, OUT_LOC, STATUS )
          END IF

*        Copy MORE box
          CALL BDA_COPMORE( LOC, OUT_LOC, STATUS )

*        Copy HISTORY
          CALL HIST_COPY( LOC, OUT_LOC, STATUS )

        END IF
      END IF

*    History update
      IF ( .NOT. OUT_PRIM ) THEN
        CALL HIST_ADD( OUT_LOC, VERSION, STATUS )
        TEXT(1)='Operation: '//OPER
        CALL HIST_PTXT( OUT_LOC, 1, TEXT, STATUS )
        IF ( .NOT. OVER ) THEN
          CALL HIST_PTXT( OUT_LOC, LEVELSO, TEXTO, STATUS )
        ELSE
          TEXT(1) = 'Dataset was overwritten'
          CALL HIST_PTXT( OUT_LOC, 1, TEXT, STATUS )
        END IF
        CALL HIST_PTXT( OUT_LOC, LEVELS, TEXTI, STATUS )
      END IF

*    Tidy up
 99   CONTINUE

      CALL BDA_UNMAP( OUT_LOC,STATUS )
      CALL BDA_RELEASE( IN2_LOC,STATUS )
      IF ( .NOT. OVER ) THEN
        CALL BDA_RELEASE( IN1_LOC, STATUS )
      END IF

      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE ARITHMETIC_QUAL(N,I1QOK,I1Q,I2QOK,I2Q,OQ,STATUS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
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
*    Local variables :
	INTEGER I
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

        IF (I1QOK.AND.I2QOK) THEN

          DO I=1,N
            OQ(I)=(I1Q(I).OR.I2Q(I))
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
      INCLUDE 'DAT_PAR'
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
      INCLUDE 'DAT_PAR'
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
