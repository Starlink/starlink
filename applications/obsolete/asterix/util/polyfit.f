*+  POLYFIT - Fits 1D polynomials to nD data. Can subtract fit from data too.
      SUBROUTINE POLYFIT(STATUS)
*
*    Description :
*
*     Calculates a 1 dimensional polynomial fit to the data.
*     If the data is greater than 1D, then a series of 1D fits are performed.
*     The output dataset can either consist of:
*         input data - fit; or
*         fit, depending upon the value of the parameter POLY.
*     The input dataset may be over written
*
*    Parameters :
*
*     POLY        = '_LOGICAL' (read)  POLYFIT or DTREND mode?
*     IN          = 'UNIV'     (read)  Input object
*     POLY_DEGREE = '_INTEGER' (read)  Degree of polynomial
*     OUT         = 'UNIV'     (write) Output object
*
*    Method :
*
*     Adapted originally by APW from SAO POLYFIT program. Degree of
*     polynomial is the highest power of x.
*
*    Deficiencies :
*
*     Does not work if primitive output object specified.
*
*    Bugs :
*    Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*     Phillip Andrews (BHVAD::PLA)
*
*    History :
*
*     29 Jun 84 : Original (BHVAD::TJP)
*     12 Dec 84 : Mapped for UPDATE instead of WRITE (TJP)
*     30 Mar 87 : Code tidied up, and brought to STARLINK standard. (PLA)
*      2 Sep 87 : V1.0-2 Operates on primative input object. (PLA)
*     16 Sep 88 : V1.0-3 OVERWRITE option, & now uses MATH_POLY subroutine (PLA)
*      6 Oct 88 : V1.0-4 Displays correct values of the coefficients (PLA)
*     14 Nov 88 : V1.0-5 Handles nD datasets by performing repeated 1D fits.
*                        MATH_POLY code improved and included within this file
*                        because normalization of AXIS values done out side of
*                        altered MATH_POLY (here POLYFIT_DOIT). (PLA)
*      8 Oct 92 : V1.7-0 Changes for alterations in compiler 3 years ago. This
*                        program is either bug free or never used (DJA)
*     30 Jun 93 : V1.7-1 Fixed by in above update (DJA)
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
*
*    Status :
*
      INTEGER STATUS
*
*    Local Constants :
*
      CHARACTER*19           Fmt             ! Format for output of coefficients
         PARAMETER          (Fmt = '(1X,G15.6,A,I2,A)')
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) ILOC            ! Input data object locator
      CHARACTER*(DAT__SZLOC) OLOC            ! Output data object locator
      CHARACTER*80           TEXT (5)        ! History file message

      INTEGER                AXPTR           ! Pointers to mapped axis values
      INTEGER                BLEN            ! No. of fits to be performed
      INTEGER                DPTR            ! Pointer to input data array
      INTEGER                I, N            ! Dummy variables for loops.
      INTEGER                LDIMS(DAT__MXDIM) ! Size of each dimension
      INTEGER                NBAD            ! No.of bad quality data
      INTEGER                NBPTR           ! Pointer to number of bad points array
      INTEGER                NDAT            ! Total number of data points
      INTEGER                NDEG            ! Degree of polynomial fitted
      INTEGER                NDIM            ! Dimensionality of data
      INTEGER                NVAL            ! Number of values mapped
      INTEGER                QPTR            ! Pointer to data quality
      INTEGER                TLDIMS(DAT__MXDIM) ! Dummy size of each dimension
      INTEGER                TNDIM           ! Dummy dimensionality of component
      INTEGER                VPTR            ! Pointer to data variances
      INTEGER                WTPTR           ! Data weights (=1/variance**2)

      LOGICAL                AXREG           ! Is the AXIS data regularly spaced?
      LOGICAL                BAD             ! Are there any bad quality values?
      LOGICAL                DATOK           ! Is the DATA OK?
      LOGICAL                INPRIM          ! Input primitive object ?
      LOGICAL                OK              ! Are various components OK to be used
      LOGICAL                OVERWRITE       ! Over write input object?
      LOGICAL                POLY            ! Selects DTREND or POLYFIT function.
      LOGICAL                QUALOK          ! Is QUALITY present & ok ?
      LOGICAL                VAROK           ! Is VARIANCE present & ok ?
      LOGICAL                WFIT            ! Perform weighted fit?
*
*    Version id :
*
      CHARACTER*22           VERSION
        PARAMETER            ( VERSION = 'POLYFIT Version 1.7-1' )
*-

*    Check status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version id
      CALL MSG_PRNT( VERSION )

*    Initialize
      CALL AST_INIT
      CALL ARR_INIT1I( 1, DAT__MXDIM, LDIMS, STATUS )

*    Ask if detrending or polynomial fitting required:
      CALL PAR_GET0L( 'POLYFIT', POLY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( POLY ) THEN
        CALL PAR_GET0L( 'DTREND', POLY, STATUS )
        POLY = .NOT. POLY
      END IF

*    Find out if are overwritting input file
      CALL PAR_GET0L( 'OVER', OVERWRITE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Open input
      IF ( OVERWRITE ) THEN
        CALL MSG_PRNT( 'WARNING: Overwriting input object' )
        CALL USI_ASSOCI( 'INP', 'UPDATE', ILOC, INPRIM, STATUS )
        OLOC = ILOC
      ELSE
        CALL USI_ASSOC2( 'INP', 'OUT', 'READ', ILOC, OLOC, INPRIM,
     :                                                    STATUS )
      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check data
      CALL  BDA_CHKDATA (ILOC, DATOK, NDIM, LDIMS, STATUS)
      IF ( .NOT. DATOK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', '! Invalid data', STATUS )
      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Look for independent variable - if present & correct then map it
      CALL BDA_CHKAXVAL( ILOC, 1, OK, AXREG, NVAL, STATUS )

      IF ( OK ) THEN
        CALL BDA_MAPAXVAL (ILOC, 'READ', 1, AXPTR, STATUS)

      ELSE

*      Set up temporary data object containing equally spaced values
        CALL MSG_PRNT ('WARNING: Axis(1) data is invalid'/
     :                        /' - proceeding assuming regular spacing')

        CALL BDA_CREAXES (OLOC, NDIM, STATUS)

        DO I = 1, NDIM
          CALL BDA_CREAXVAL( OLOC, I, .TRUE., LDIMS(I), STATUS )
          CALL BDA_PUTAXVAL( OLOC, I, 0.0, 1.0, LDIMS(I), STATUS )
        END DO

        CALL BDA_MAPAXVAL (OLOC, 'READ', 1, AXPTR, STATUS)

      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map in data array
      IF ( .NOT. INPRIM ) THEN
        CALL HDX_COPY( ILOC, OLOC, STATUS )
        CALL BDA_MAPDATA( OLOC, 'UPDATE', DPTR, STATUS )

      ELSE
        IF ( OVERWRITE ) THEN
          CALL BDA_MAPDATA( ILOC, 'UPDATE', DPTR, STATUS )

        ELSE
          CALL HIST_COPY( ILOC, OLOC, STATUS )
          CALL BDA_COPDATA( ILOC, OLOC, STATUS )
          CALL BDA_MAPDATA( OLOC, 'UPDATE', DPTR, STATUS )

        END IF
      END IF

*    Map data variance if present & correct
      CALL BDA_CHKVAR( ILOC, VAROK, TNDIM, TLDIMS, STATUS )
      IF ( VAROK ) THEN
        CALL BDA_MAPVAR( ILOC, 'READ', VPTR, STATUS )
      END IF

*    Check data quality - exclude any bad points from fit.
      CALL BDA_CHKQUAL( ILOC, QUALOK, TNDIM, TLDIMS, STATUS )
      IF ( QUALOK ) THEN
        CALL BDA_MAPLQUAL( ILOC, 'READ', BAD, QPTR, STATUS )
        IF ( .NOT. BAD ) THEN
          CALL BDA_UNMAPLQUAL( ILOC, STATUS )
          QUALOK = .FALSE.
        END IF
      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Inform user about number of data points
      CALL ARR_SUMDIM( NDIM, LDIMS, NDAT )

      CALL MSG_SETI( 'NDAT', NDAT )
      CALL MSG_PRNT( '^NDAT points entered' )

      IF ( NDIM .GT. 1 ) THEN
        CALL MSG_SETI( 'NDAT', LDIMS(1) )
        CALL MSG_PRNT( '^NDAT points in each 1 dimensional fit' )
      END IF

*    User input.
      CALL PAR_GET0I( 'POLY_DEGREE', NDEG, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    NDEG must lie between 0 and 10
      IF ( NDEG .GT. 10 ) THEN
        NDEG = 10
        CALL MSG_PRNT( 'WARNING: Will only calculate up to '//
     :                              '10th order polynomial.' )
      ELSE IF ( NDEG .LT. 0 ) THEN
        NDEG = 0
        CALL MSG_PRNT( 'WARNING: Minimum degree of polynomial is zero.')

      END IF

      BLEN = NDAT / LDIMS(1)
      WFIT = .FALSE.


      IF ( VAROK .OR. QUALOK ) THEN
        WFIT = .TRUE.

*      Create dynamic array to hold weights
        CALL DYN_MAPR( 1, NDAT, WTPTR, STATUS )

*      Create dynamic array to hold number of bad points
        CALL DYN_MAPI( 1, BLEN, NBPTR, STATUS )

*      Check status
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Set up array of weights.
        CALL POLYFIT_WEIGHTS (VAROK, QUALOK, LDIMS(1), BLEN, %VAL(VPTR),
     :              %VAL(QPTR), %VAL(WTPTR), %VAL(NBPTR), NBAD, STATUS )

        IF (NBAD .GT. 0 ) THEN
          CALL MSG_SETI( 'BADPTS', NBAD )
          CALL MSG_PRNT( 'A total of ^BADPTS bad data points'/
     :                     /' will be excluded from the fit' )

          IF ( NBAD .EQ. NDAT ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'FATAL ERROR: All data excluded',
     :                                                  STATUS )
          END IF

        END IF

*      Loop over NDIM-1 dimensions, performing 1 d fits
        CALL POLYFIT_LOOPWT (POLY, NDEG, NDIM, LDIMS(1), BLEN,
     :        %VAL(AXPTR), %VAL(NBPTR), %VAL(WTPTR), %VAL(DPTR), STATUS)

      ELSE

*      Loop over NDIM-1 dimensions, performing 1 d fits
        CALL POLYFIT_LOOPNOWT( POLY, NDEG, NDIM, LDIMS(1), BLEN,
     :                         %VAL(AXPTR), %VAL(DPTR), STATUS )

      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    History file entry
      CALL HIST_ADD( OLOC, VERSION, STATUS )
      CALL USI_NAMEI( I, TEXT, STATUS )
      CALL MSG_SETI( 'NDEG', NDEG )
      I = I + 1

      IF ( POLY ) THEN
        CALL MSG_MAKE( 'Polynomial fit of degree ^NDEG produced.',
     :                                                TEXT(I), N )
      ELSE
        CALL MSG_MAKE( 'Polynomial of degree ^NDEG subtracted.',
     :                                              TEXT(I), N )
      END IF
      CALL HIST_PTXT( OLOC, I, TEXT, STATUS )

 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END



*+  POLYFIT_WEIGHTS - Uses DATA_ERROR & DATA_QUALITY info to calc weights.
      SUBROUTINE POLYFIT_WEIGHTS( VAROK, QUALOK, NDAT, BLEN, VAR, QUAL,
     :                                          WT, BAD, NBAD, STATUS )
*    Description :
*      If data errors are available then a set of weights=1/(error)**2 is
*      set up in array WT.
*      If data quality information is available (LOG=.TRUE.) then this
*      is scanned and any bad (non-zero quality) data are given zero
*      weight.
*    History :
*     date:  original (institution::username)
*     30 Mar 87: NODDY_PRO header added (pla@uk.ac.bham.sr/star)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER                NDAT            ! No. of data points per fit
      INTEGER                BLEN            ! No. of fits.

      LOGICAL                VAROK           ! Is quality present & ok ?
      LOGICAL                QUALOK          ! Is quality present & ok ?
*    Import-Export :
      REAL                   VAR(NDAT,BLEN)  ! Variance information (if present)

      LOGICAL                QUAL(NDAT,BLEN) ! Quality information (if present)
*
*    Export :
*
      REAL                   WT(NDAT,BLEN)   ! Array of errors.

      INTEGER                BAD(BLEN)       ! Number of bad data points per fit
      INTEGER                NBAD            ! Total number of bad data points.
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER                I, J            ! Dummy variables for loops.
*-

*    Check status
      IF  ( STATUS .NE. SAI__OK ) RETURN

      NBAD = 0
      CALL ARR_INIT1I(0, BLEN, BAD, STATUS)

      IF (QUALOK .AND. VAROK) THEN
        DO J = 1, BLEN
          DO I = 1, NDAT
            IF (QUAL(I,J)) THEN
              WT(I,J) = 1.0 / VAR(I,J)

            ELSE
              NBAD    = NBAD + 1
              BAD(J)  = BAD(J) + 1
              WT(I,J) = 0.0

            END IF
          END DO
        END DO
      ELSE IF (QUALOK) THEN
        DO J = 1, BLEN
          DO I = 1, NDAT
            IF (QUAL(I,J)) THEN
              WT(I,J) = 1.0

            ELSE
              NBAD     = NBAD + 1
              BAD(J)   = BAD(J) + 1
              WT(I,J)  = 0.0

            END IF
          END DO
        END DO
      ELSE IF (VAROK) THEN
        DO J = 1, BLEN
          DO I = 1, NDAT
            WT(I,J) = 1.0 / VAR(I,J)

          END DO
        END DO
      END IF
      END



*+  POLYFIT_LOOPWT - Loop over dataset performing 1d weighted fits
      SUBROUTINE POLYFIT_LOOPWT (POLY, NDEG, NDIM, NDAT, BLEN, AXIS,
     :                                        BAD, WEIGHT, DATA, STATUS)
*    Description :
*     Loops ofer n dimensioal dataset performing 1d fits
*    Environment parameters :
*    Method :
*     Calls POLYFIT_DOIT to do the 1d fits
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews (BHVAD::PLA)
*    History :
*     11/11/88:  Original (PLA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      INTEGER                NDEG               ! Degree of polynomial fitted
      INTEGER                NDIM               ! Dimensionality of data
      INTEGER                NDAT               ! No. of points per fit
      INTEGER                BLEN               ! No. of fits
      INTEGER                BAD(BLEN)          ! Number of bad data points per fit

      REAL                   AXIS(NDAT)         ! Axis(1) data
      REAL                   WEIGHT(NDAT, BLEN) ! Weight array

*    Import - Export :
      REAL                   DATA(NDAT, BLEN)   ! Data array on exit contains either
                                                ! original data - fit (POLY = .FALSE.)
                                                ! or fit (POLY = .TRUE.)
*    Status :
      INTEGER STATUS
*
*    Functions :
*
      REAL                   POLYFIT_CNM        ! n! / ( m! (n-m)! )
*
*    Local variables :
*
      INTEGER                I, B, C         ! Loop counters
      INTEGER                XNPTR           ! Normalized & exponensiated X values (used by
                                             ! subroutine POLYFIT_DOIT)
      INTEGER                SIZE(2)         ! Size of XN array

      LOGICAL                POLY            ! Selects DTREND or POLYFIT function.

      REAL                   COEFF(12)       ! Coeffs of polynomial fitted to normalized axis

      REAL                   AXMAX, AXMIN    ! Max & min axis values
      REAL                   C1, C2, FIT     ! Used in calculating fit coefficients to real axis
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set up array for normalized axis
      SIZE(1) = NDAT
      SIZE(2) = (2 * NDEG) + 1

      CALL DYN_MAPR (2, SIZE, XNPTR, STATUS)

      SIZE(2) = SIZE(2) - 1

*    Check status.
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Write normalized axis
      CALL POLYFIT_NORM( SIZE(1), SIZE(2), AXIS, %VAL(XNPTR),
     :                                         AXMAX, AXMIN )

*    Loop over fits
      DO I = 1, BLEN
        IF (NDAT - BAD(I) .GT. NDEG) THEN
*        Perform either polynomial fit or detrending:
          CALL POLYFIT_DOIT( POLY, SIZE(1), SIZE(2), NDEG, .TRUE.,
     :                       WEIGHT(1,I), %VAL(XNPTR), DATA(1,I),
     :                       COEFF, STATUS )

        END IF
      END DO

      IF (NDIM .EQ. 1) THEN
*      Display the fit
        CALL MSG_PRNT ('The fit determined was:')

        C1 = 2.0 / (AXMAX - AXMIN)
        C2 = (- AXMAX - AXMIN) / (AXMAX - AXMIN)

*      Loop over all output coefficients
        DO B = 0, NDEG
          FIT = 0.0

*        Loop over coeffs of fit to normalized data
          DO C = B, NDEG
            FIT = FIT + (COEFF(C+1) * (C1**C) * POLYFIT_CNM(C, B)
     :                * ((C2 / C1)**(C - B) ))

          END DO

          IF (B .EQ. 0) THEN
            CALL MSG_SETR ('FIT', FIT)
            CALL MSG_SETI ('N', B)

            IF (B .NE. NDEG) THEN
              CALL MSG_PRNT ('y = ^FIT * X**^N +')

            ELSE
              CALL MSG_PRNT ('y = ^FIT * X**^N')

            END IF
          ELSE IF (B .LT. NDEG) THEN
            CALL MSG_SETR ('FIT', FIT)
            CALL MSG_SETI ('N', B)
            CALL MSG_PRNT ('    ^FIT * X**^N +')

         ELSE
            CALL MSG_SETR ('FIT', FIT)
            CALL MSG_SETI ('N', B)
            CALL MSG_PRNT ('    ^FIT * X**^N')

          END IF
        END DO
      END IF

 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from POLYFIT_LOOPWT', STATUS )
      END IF

      END



*+  POLYFIT_LOOPNOWT - Loop over dataset performing 1d unweighted fits
      SUBROUTINE POLYFIT_LOOPNOWT (POLY, NDEG, NDIM, NDAT, BLEN, AXIS,
     :                                                     DATA, STATUS)
*    Description :
*     Loops ofer n dimensioal dataset performing 1d fits
*    Environment parameters :
*    Method :
*     Calls POLYFIT_DOIT to do the 1d fits
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews (BHVAD::PLA)
*    History :
*     11 Nov 88:  Original (PLA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      INTEGER                NDEG               ! Degree of polynomial fitted
      INTEGER                NDIM               ! Dimensionality of data
      INTEGER                NDAT               ! No. of points per fit
      INTEGER                BLEN               ! No. of fits

      REAL                   AXIS(NDAT)         ! Axis(1) data

*    Import - Export :
      REAL                   DATA(NDAT, BLEN)   ! Data array on exit contains either
                                                ! original data - fit (POLY = .FALSE.)
                                                ! or fit (POLY = .TRUE.)
*    Status :
      INTEGER STATUS
*
*    Functions :
*
      REAL                   POLYFIT_CNM       ! n! / ( m! (n-m)! )
*
*    Local variables :
*
      INTEGER                I, B, C         ! Loop counters
      INTEGER                XNPTR           ! Normalized & exponensiated X values (used by
                                             ! subroutine POLYFIT_DOIT)
      INTEGER                SIZE(2)         ! Size of XN array

      LOGICAL                POLY            ! Selects DTREND or POLYFIT function.

      REAL                   COEFF(12)       ! Coeffs of polynomial fitted to normalized axis

      REAL                   AXMAX, AXMIN    ! Max & min axis values
      REAL                   C1, C2, FIT     ! Used in calculating fit coefficients to real axis
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set up array for normalized axis
      SIZE(1) = NDAT
      SIZE(2) = (2 * NDEG) + 1

      CALL DYN_MAPR (2, SIZE, XNPTR, STATUS)

      SIZE(2) = SIZE(2) - 1

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Write normalized axis
      CALL POLYFIT_NORM( SIZE(1), SIZE(2), AXIS, %VAL(XNPTR), AXMAX,
     :                                                       AXMIN )

*    Loop over fits
      DO I = 1, BLEN
*      Perform either polynomial fit or detrending:
        CALL POLYFIT_DOIT (POLY, SIZE(1), SIZE(2), NDEG, .FALSE., C1,
     :                            %VAL(XNPTR), DATA(1,I), COEFF, STATUS)

      END DO

      IF (NDIM .EQ. 1) THEN
*      Display the fit
        CALL MSG_PRNT ('The fit determined was:')

        C1 = 2.0 / (AXMAX - AXMIN)
        C2 = (- AXMAX - AXMIN) / (AXMAX - AXMIN)

*      Loop over all output coefficients
        DO B = 0, NDEG
          FIT = 0.0

*        Loop over coeffs of fit to normalized data
          DO C = B, NDEG
            FIT = FIT + (COEFF(C+1) * (C1**C) * POLYFIT_CNM(C, B)
     :                * ((C2 / C1)**(C - B) ))

          END DO

          IF (B .EQ. 0) THEN
            CALL MSG_SETR ('FIT', FIT)
            CALL MSG_SETI ('N', B)

            IF (B .NE. NDEG) THEN
              CALL MSG_PRNT ('y = ^FIT * X**^N +')

            ELSE
              CALL MSG_PRNT ('y = ^FIT * X**^N')

            END IF
          ELSE IF (B .LT. NDEG) THEN
            CALL MSG_SETR ('FIT', FIT)
            CALL MSG_SETI ('N', B)
            CALL MSG_PRNT ('    ^FIT * X**^N +')

         ELSE
            CALL MSG_SETR ('FIT', FIT)
            CALL MSG_SETI ('N', B)
            CALL MSG_PRNT ('    ^FIT * X**^N')

          END IF
        END DO
      END IF

 99   IF (STATUS .NE. SAI__OK) THEN
        CALL ERR_REP( ' ', '...from POLYFIT_LOOP', STATUS )
      END IF

      END






*+  POLYFIT_NORM - Normalizes AXIS then exponensiates.
      SUBROUTINE POLYFIT_NORM (SIZE1, SIZE2, AXIS, XN, AXMAX, AXMIN)
*    Description :
*     Normalizes the AXIS using:
*       new_value = (2*value - MAX - MIN) / (MAX - MIN)
*     Exponensiates
*    History :
*     14/11/88:  original (PLA)
*    Type definitions :
      IMPLICIT NONE

*    Import :
      INTEGER    SIZE1, SIZE2         ! Size of XN array

      REAL       AXIS (SIZE1)         ! AXIS values.

*    Export :
      REAL       XN(SIZE1, 0:SIZE2)   ! Normalized axis values.
      REAL       AXMAX                ! Max value of X array.
      REAL       AXMIN                ! Min value of X array.

*    Local variables :
      INTEGER    I, J                 ! Loop counters
*-
      AXMIN = AXIS(1)
      AXMAX = AXIS(SIZE1)

      IF (AXMIN .GT. AXMAX) THEN
        AXMIN = AXIS(SIZE1)
        AXMAX = AXIS(1)
      END IF

      IF (SIZE2 .GT. 1) THEN
        DO I = 1, SIZE1
          XN(I, 0) = 1.0
          XN(I, 1) = (2 * AXIS(I) - AXMAX - AXMIN) / (AXMAX - AXMIN)

          DO J = 2, SIZE2
            IF (ABS(XN(I,1)) .GT. 0.0) THEN
              XN(I,J) = XN(I,1)**J

            ELSE
              XN(I,J) = 0.0

            END IF
          END DO
        END DO
      ELSE IF (SIZE2 .EQ. 1) THEN
        DO I = 1, SIZE1
          XN(I, 0) = 1.0
          XN(I, 1) = (2 * AXIS(I) - AXMAX - AXMIN) / (AXMAX - AXMIN)

        END DO
      ELSE IF (SIZE2 .EQ. 0) THEN
        DO I = 1, SIZE1
          XN(I, 0) = 1.0

        END DO
      END IF
      END



*+  POLYFIT_DOIT - Fits a polynomial to the DATA, replacing it with residuals or fit
      SUBROUTINE POLYFIT_DOIT (POLY, SIZE1, SIZE2, NDEG, WFIT, WEIGHT,
     :                                       XN, DATA, COEFF, STATUS )
*    Description :
*      Fits a polynomial to the DATA, replacing the data with either
*      DATA = fit value (POLY = TRUE), or
*      DATA = original data - fit value (POLY = FALSE)
*    Method :
*     From some ancient SAO routine no understands now!
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*     Phillip Andrews (BHVAD::PLA)
*    History :
*
*     16 Sep 86 : Version 4 (TJP)
*      1 Apr 87 : Code structured, header added. (PLA)
*      7 May 88 : Asterix88 version    (LTVAD::RDS)
*     17 Jun 88 : Improved the efficiency by merging two DO loops so
*                 that powers are only calculated once    (LTVAD::RDS)
*     16 Nov 88 : Normalization extracted to suit needs of POLYFIT. Code
*                 improved both for readability and speed. (PLA)
*      7 Oct 92 : Change SIZE to SIZE1/2 to accomodate change it
*                 compiler ages ago (DJA)
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
*
*    Status :
*
      INTEGER    STATUS
*
*    Import :
*
      LOGICAL    POLY                ! if true return fit, if not return residuals
      LOGICAL    WFIT                ! Is weighted fit required.

      INTEGER    SIZE1, SIZE2        ! Size of XN array
      INTEGER    NDEG                ! Degree of polynomial ( max.power of X )

      REAL       XN(SIZE1,0:SIZE2)   ! Normalized axis values
      REAL       WEIGHT (*)          ! Array of weights proportional to inverse
*
*    Import-Export :
*
      REAL       DATA (SIZE1)      ! Array of dependent variable becomes fit or resuduals
*
*    Export :
*
      REAL       COEFF (NDEG+2)      ! Work space, before subtraction.
                                     ! First NDEG+1 elements return coefficients
                                     ! of polynomial in XN (in ascending powers)
      REAL       SUMSQ               ! Sum of squares of weighted residuals
*
*    Local variables :
*
      INTEGER    I                   ! Dummy variable for loops.
      INTEGER    II                  !   "      "      "    "
      INTEGER    J                   !   "      "      "    "
      INTEGER    NA                  ! NDEG + 1
      INTEGER    NC                  ! NDEG + 2
      INTEGER    M                   ! (2*NDEG) + 1

      REAL       WORK1(12,12)        ! Work arrays used by
      REAL       WORK2(21)           ! this subroutine
      REAL       DMEAN               ! Mean value of DATA array.
      REAL       DCONST              ! Constant component in DATA fit.
      REAL       DENOM               ! Used in calculation of YMEAN.
      REAL       SUMBIT              ! Used to speed up the code

*-

*   Calc mean if NDEG = 0
      IF (NDEG .EQ. 0) THEN
        DMEAN = 0.0

        IF (WFIT) THEN
          DENOM = 0.0

          DO I = 1, SIZE1
            DMEAN = DMEAN + DATA(I) * WEIGHT(I)
            DENOM = DENOM + WEIGHT(I)

          END DO
          DMEAN = DMEAN / DENOM                ! Weighted mean

        ELSE
          DO I = 1, SIZE1
            DMEAN = DMEAN + DATA(I)

          END DO
          DMEAN = DMEAN / REAL(SIZE1)        ! Unweighted mean

        END IF
        COEFF(1) = DMEAN
        SUMSQ    = 0.0

        DO I = 1, SIZE1
          IF (POLY) THEN
            DATA(I) = DMEAN

          ELSE
            DATA(I) = DATA(I) - DMEAN

          END IF
        END DO
      ELSE
*      Start of sao program: initialize variables.
        NA = NDEG + 1
        NC = NDEG + 2
        M  = (2 * NA) - 1

        IF (WFIT) THEN
*        Set the (NDEG + 2)th elements of WORK1, and first NA elements of WORK2
          DO I = 1, NA
            WORK1 (I, NC) = 0.0
            WORK2 (I)     = 0.0
            II            = I - 1

            DO J = 1, SIZE1
              SUMBIT       = WEIGHT(J) * XN(J, II)
              WORK2(I)     = WORK2(I) + SUMBIT
              WORK1(I, NC) = WORK1(I, NC) + (DATA(J) * SUMBIT)

            END DO
          END DO

*        Set up rest of WORK2
          DO I = NA + 1, M
            WORK2(I) = 0.0
            II       = I - 1

            DO J = 1, SIZE1
              SUMBIT   = WEIGHT(J) * XN(J,II)
              WORK2(I) = WORK2(I) + SUMBIT

            END DO
          END DO

*        Set up rest of WORK1
          DO I = 1, NA
            DO J = 1, NA
              WORK1 (I, J) = WORK2 (I + J - 1)

            END DO
          END DO
        ELSE
*        Set the (NDEG + 2)th elements of WORK1, and first NA elements of WORK2
          DO I = 1, NA
            WORK1(I, NC) = 0.0
            WORK2(I)     = 0.0
            II           = I - 1

            DO J = 1, SIZE1
              SUMBIT       = XN(J,II)
              WORK2 (I)    = WORK2 (I) + SUMBIT
              WORK1 (I,NC) = WORK1 (I,NC) + (DATA(J) * SUMBIT)

            END DO
          END DO

*        Set up rest of WORK2
          DO I = NA + 1, M
            WORK2(I) = 0.0
            II       = I - 1

            DO J = 1, SIZE1
              SUMBIT   = XN(J,II)
              WORK2(I) = WORK2(I) + SUMBIT

            END DO
          END DO

*        Set up rest of WORK1
          DO I = 1, NA
            DO J = 1, NA
              WORK1 (I, J) = WORK2 (I + J - 1)

            END DO
          END DO
        END IF

        CALL POLYFIT_TRIANG (NA,    NC,     WORK1, STATUS)
        CALL POLYFIT_SOLVE  (WORK1, NA, NC, COEFF, STATUS)

*      Check status
        IF (STATUS .NE. SAI__OK) GOTO 99

        DO I = 1, SIZE1
          DCONST = COEFF(1)

          DO J = 2, NA
            DCONST = DCONST + (COEFF(J) * XN(I,J-1))
          END DO

          IF (.NOT. POLY) THEN
            DATA (I) = DATA (I) - DCONST                     ! Return residual

          ELSE
            DATA (I) = DCONST	                             ! Return calc y val

          END IF
        END DO
      END IF

 99   CONTINUE

      END



*+  POLYFIT_TRIANG - Triangularizes the matrix A, with M rows & M+N columns.
      SUBROUTINE POLYFIT_TRIANG (M, NC, A, STATUS)
*    Description :
*      A is the matrix with M rows and M + 1 columns to be triangularised.
*    History :
*     ????????: Original (BHVAD::TJP)
*     1 Apr 87: Structured and header added. (PLA)
*     7 May 88: Argument order changed.    (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER    STATUS
*    Import :
      INTEGER    NC           !NDEG + 2
      INTEGER    M            !No. of rows in matrix

*    Import-Export :
      REAL       A(12,12)     !Matrix to be triangularized.
*    Local variables :
      INTEGER    J            ! Dummy variable for loops.
      INTEGER    K            ! Dummy variable for loops.
      INTEGER    L            ! Dummy variable for loops.
      INTEGER    NUMCOLUMNS   ! No. of columns.
      INTEGER    MAXX         ! Max value of X?

      REAL       VALUE        ! Current element of matrix A.
*-

*   Check status
      IF (STATUS .NE. SAI__OK) RETURN

*   Initialize variables.
      NUMCOLUMNS = M + 1

      DO J = 1, M - 1
        MAXX  = J
        VALUE = ABS(A(J, J))

        DO K = (J + 1), M
          IF (VALUE - ABS(A(K, J)) .LT. 0) THEN
            VALUE = ABS(A(K, J))
            MAXX  = K

          END IF
        END DO

        DO K = J, NUMCOLUMNS
          VALUE      = A(MAXX, K)
          A(MAXX, K) = A(J, K)
          A(J, K)    = VALUE

        END DO
        VALUE = A(J, J)

        IF (VALUE .EQ. 0) GOTO 99

        DO K = J, NUMCOLUMNS
          A(J, K) = A(J, K) / VALUE

        END DO

        DO K = (J + 1), M
          VALUE = A(K, J)

          DO L = J, NUMCOLUMNS
            A(K, L) = A(K, L) - (VALUE * A(J, L))

          END DO
        END DO
      END DO
      VALUE = A(M, M)

      IF (VALUE .EQ. 0) GOTO 99

      DO K = M, NUMCOLUMNS
        A(M, K) = A(M, K) / VALUE

      END DO

 99   IF (VALUE .EQ. 0) THEN
        STATUS = SAI__ERROR

      END IF
      END




*+  POLYFIT_SOLVE - Converts (M,M+1) triangularized matrix to (M) matrix.
      SUBROUTINE POLYFIT_SOLVE (A, M, NC, X, STATUS)
*    Description :
*      A is the triangularised matrix with M rows and M+1 columns.
*      X is the solution matrix with M rows.
*    History :
*     1 Apr 87: Structured, tidied, header added. (pla@uk.ac.bham.sr.star)
*     7 Jun 88: Order of arguments changed.
*    Type Definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status
      INTEGER    STATUS
*    Import :
      INTEGER    NC           !NDEG + 2
      INTEGER    M            ! No. of rows in triangularized matrix.

      REAL       A(12,12)     ! Triangularized matrix.
*    Export :
      REAL       X(NC)        ! Solution matrix.
*    Local variables :
      INTEGER    K            ! Dummy variable for loop.
      INTEGER    I            !   "      "      "    "
      INTEGER    MP1          ! M + 1
      INTEGER    MMK          ! M - K

      REAL       SUM          ! Used in calculation.
*-
*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

      MP1 = M + 1

      DO K = 1, (M - 1)
        MMK  = M - K
        X(M) = A(M, MP1) / A(M,M)
        SUM  = 0.0

        DO I = MMK + 1, M
          SUM = SUM + A(MMK, I) * X(I)

        END DO
        X(MMK) = A(MMK, MP1) - SUM

      END DO
      END




*+  POLYFIT_CNM - Returns n! / (m! (n-m)!)
      REAL FUNCTION POLYFIT_CNM( N, M )
*    Description :
*     RETURNS      N! / (M! (N-M)1)
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phillip Andrews (PLA_AST88@uk.ac.bham.sr/star)
*    History :
*     7/10/88:  Original (PLA_AST88)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER                N, M

*    Local variables :
      REAL                   FAC(0:10)                     ! Factorial (n) for n = 0 to 10

*    Local data
      DATA  FAC /1.0, 1.0, 2.0, 6.0, 24.0, 120.0, 720.0, 5040.0,
     :                                     40320.0, 362880.0, 3628800.0/
*-

      POLYFIT_CNM = FAC(N) / ( FAC(M) * FAC(N-M) )

      END
