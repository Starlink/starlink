      SUBROUTINE ACF( STATUS )
*+
*  Name:
*     ACF

*  Purpose:
*     Calculate an autocorrelation function for time series.

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL ACF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Calculates a 1 dimensional autocorrelation function for any dataset.
*     Extra dimensions are looped over producing a series of autocorrelation
*     functions.
*
*     Errors and quality may be taken into account. The autocorrelation
*     function(s) may be BIASSED to suppress high lag components.

*  Usage:
*     acf {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input dataset
*     INP = CHAR (read)
*        Output dataset
*     AXIS = INTEGER (read)
*        Axis to correlate along
*     BIAS = LOGICAL (read)
*        Bias the ACF?
*     WEIGHT = LOGICAL (read)
*        Use variance if ok?
*     MXLAG = INTEGER (read)
*        Maximum lag

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
*     For WEIGHTED case uses method given in Weisskopf, Kahn, &
*     Sutherland (Ap.J. 199, L147 (1975)). This is corrected for
*     the extra statistical noise contribution at zero lag, AND
*     CAN RESULT IN ESTIMATED ACF VALUES > 1.
*     The UNWEIGHTED case uses a similar method, ommitimg the weights.
*
*     UNBIASSED results are normalized by  * 1 / (NPTS - LAG)
*     BIASSED results are normalized by * 1 / NPTS (suppressing large lags).

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
*     acf, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     PLA: Phil Andrews (ROSAT,University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jun 1989 V1.0-0 (PLA):
*        Original version.
*     14 Jun 1990 V1.2-0 (DJA):
*        Does check for irregular axes and bad quality
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     20 Apr 1995 V1.8-1 (DJA):
*        New data interface
*      4 Dec 1995 V2.0-0 (DJA):
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
        PARAMETER		( VERSION = 'ACF Version 2.1-0' )

*  Local Variables:
      CHARACTER*80           	TEXT(2)
      CHARACTER*80           	UNITS

      REAL			SPARR(2)		! Spaced array data

      INTEGER                	I                     	! Loop counter
      INTEGER			IAPTR			! Input axis pointer
      INTEGER                	IDPTR                 	! Input data pointer
      INTEGER			IFID			! Input dataset id
      INTEGER                	IQPTR                 	! Input quality pointer
      INTEGER                	IVPTR                 	! Input variance pointer
      INTEGER                	ODPTR                 	! Output data pointer
      INTEGER			OFID			! Output dataset id
      INTEGER                	OLDIMS(ADI__MXDIM)    	! Size of output dimensions
      INTEGER                	OQPTR                 	! Output quality pointer
      INTEGER                	OVPTR                 	! Output variance pointer
      INTEGER                	LDIMS(ADI__MXDIM)     	! Length of each dimension
      INTEGER                	NELM                  	! total number of data values
      INTEGER                	MXLAG                 	! Maximum lag required
      INTEGER                	NBAD                  	! # of bad points
      INTEGER                	NDIM                  	! Number of dimensions
      INTEGER                	T_AXIS                	! Index of time axis

      REAL                   	BASE                  ! } these define the
      REAL                   	SCALE                 ! } regular axis values

      LOGICAL                	BIAS                  	! Biassed autocovariance?
      LOGICAL                	OK                    	! Is data ok?
      LOGICAL                	QOK                   	! Is QUALITY ok?
      LOGICAL                	REG                   	! Regularly spaced T_AXIS?
      LOGICAL                	USEWT                 	! Use weighting?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Obtain input & output identifiers
      CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Check the dataset
      CALL BDI_CHK( IFID, 'DATA', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, LDIMS, NDIM, STATUS )
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR: Invalid dataset', STATUS )
        GOTO 99
      END IF

*  More than one axis?
      IF ( NDIM .GT. 1 ) THEN

*    List the axes
        CALL MSG_PRNT (' ')
        CALL MSG_PRNT (' The axes present are:-')
        CALL AXIS_TLIST( IFID, NDIM, STATUS)
        CALL MSG_PRNT (' ')

*    Try to find time axis
        CALL BDI0_FNDAXC( IFID, 'T', T_AXIS, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          T_AXIS = -1
        END IF
        IF ( T_AXIS .GT. 0 ) THEN
          CALL USI_DEF0I ('AXIS', T_AXIS, STATUS)
        END IF

*    Get time axis
        CALL USI_GET0I ('AXIS', T_AXIS, STATUS)
        IF (STATUS .NE. SAI__OK) GOTO 99

      ELSE
        T_AXIS = 1

      END IF

*  Set length of unused dimensions to 1
      CALL AR7_PAD( NDIM, LDIMS, STATUS )

*  Map input data
      CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )
      CALL ARR_SUMDIM( NDIM, LDIMS, NELM )

*  User input
      CALL USI_GET0L( 'BIAS', BIAS, STATUS )
      CALL USI_GET0L( 'WEIGHT', USEWT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF (USEWT) THEN

*    See if quality is present
        CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
        IF ( QOK ) THEN

          CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', IQPTR, STATUS )

*        Check for no bad quality events
          CALL ARR_CNT1L( NELM, %VAL(IQPTR), .FALSE., NBAD, STATUS )
          IF ( NBAD .GT. 0 ) THEN
            CALL MSG_SETI( 'NBAD', NBAD )
            CALL MSG_PRNT( 'There are ^NBAD bad quality points' )
          END IF

        END IF

*    Check and map variance
        CALL BDI_CHK( IFID, 'Variance', OK, STATUS )
        IF ( OK ) THEN
          CALL BDI_MAPR( IFID, 'Variance', 'READ', IVPTR, STATUS )

        ELSE IF ( QOK ) THEN

*        Create a dynamic array & set values to 1.0E0
          CALL DYN_MAPR( NDIM, LDIMS, IVPTR, STATUS )
          CALL ARR_INIT1R( 1.0, NELM, %VAL(IVPTR), STATUS )

        ELSE
          USEWT = .FALSE.

        END IF
      END IF
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Check correlation axis is regularly spaced.
      CALL BDI_AXCHK( IFID, T_AXIS, 'Data', OK, STATUS )
      SCALE = 1.0E0
      UNITS = 'pixels'

      IF ( OK ) THEN
        CALL BDI_AXMAPR( IFID, T_AXIS, 'Data', 'READ', IAPTR, STATUS )
        CALL ARR_CHKREG( %VAL(IAPTR), LDIMS(T_AXIS), REG, BASE, SCALE,
     :                   STATUS )
        IF ( .NOT. REG ) THEN
        CALL MSG_PRNT ('WARNING: Autocorrelation axis is not regularly '
     :                                                      //'spaced!')
          STATUS = SAI__ERROR
          GOTO 99
        END IF
        CALL BDI_AXGET0C( IFID, T_AXIS, 'Units', UNITS, STATUS )

      ELSE
        CALL MSG_PRNT ('WARNING: No axis information!')
        CALL MSG_PRNT ('         Results invalid '
     :                       //'if data points are irregularly spaced.')

      END IF

*  Ask user for maximum lag to calculate
      CALL USI_DEF0I( 'MXLAG', LDIMS(T_AXIS) - 1, STATUS )
      CALL USI_GET0I( 'MXLAG', MXLAG, STATUS )
      IF ( MXLAG .GT. (LDIMS(T_AXIS) - 1) ) THEN
        MXLAG = LDIMS(T_AXIS) - 1
        CALL MSG_SETI( 'LAG', MXLAG )
        CALL MSG_PRNT( 'WARNING: MXLAG too large. Reduced to ^LAG' )
      END IF

*  Create output dataset
      DO I = 1, ADI__MXDIM
        OLDIMS(I) = LDIMS(I)
      END DO
      OLDIMS(T_AXIS) = MXLAG + 1

*  Create interface object
      CALL BDI_LINK( 'BinDS', NDIM, OLDIMS, 'REAL', OFID, STATUS )
      CALL BDI_PUT0C( OFID, 'Label', 'Autocorrelation', STATUS )
      CALL BDI_MAPR( OFID, 'Data', 'WRITE', ODPTR, STATUS )

      DO I = 1, NDIM
        IF (I .NE. T_AXIS) THEN
          CALL BDI_CHK( IFID, I, 'Data', OK, STATUS )
          IF ( OK ) THEN
            CALL BDI_AXCOPY( IFID, I, ' ', OFID, I, STATUS )
          END IF

        ELSE
          SPARR(1) = 0.0
          SPARR(2) = SCALE
          CALL BDI_AXPUT1R( OFID, I, 'SpacedData', 2, SPARR, STATUS )
          CALL BDI_AXPUT0C( OFID, I, 'Label', 'Lag', STATUS )
          CALL BDI_AXPUT0C( OFID, I, 'Units', UNITS, STATUS )
        END IF

      END DO

      IF ( USEWT ) THEN
        CALL BDI_MAPR( OFID, 'Variance', 'WRITE', OVPTR, STATUS )
        CALL BDI_MAPUB( OFID, 'Quality', 'WRITE', OQPTR, STATUS )
        CALL BDI_PUT0UB( OFID, 'QualityMask', QUAL__MASK, STATUS )
      END IF

*  Copy ancillaries
      CALL UDI_COPANC( IFID, 'grf', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Send mapped array(s) to subroutine
      IF ( USEWT ) THEN
        CALL ACF_WITHWT (NDIM, LDIMS, LDIMS(1), LDIMS(2), LDIMS(3),
     :        LDIMS(4), LDIMS(5), LDIMS(6), LDIMS(7), OLDIMS, OLDIMS(1),
     :            OLDIMS(2), OLDIMS(3), OLDIMS(4), OLDIMS(5), OLDIMS(6),
     :       OLDIMS(7), LDIMS(T_AXIS), MXLAG+1, T_AXIS, %VAL(IDPTR),
     :              %VAL(IVPTR),
     :    QOK, BIAS, %VAL(IQPTR), %VAL(ODPTR), %VAL(OVPTR), %VAL(OQPTR),
     :                                                           STATUS)

      ELSE
        CALL ACF_QUICK (NDIM, LDIMS, LDIMS(1), LDIMS(2), LDIMS(3),
     :        LDIMS(4), LDIMS(5), LDIMS(6), LDIMS(7), OLDIMS, OLDIMS(1),
     :            OLDIMS(2), OLDIMS(3), OLDIMS(4), OLDIMS(5), OLDIMS(6),
     :              OLDIMS(7), LDIMS(T_AXIS), MXLAG+1, T_AXIS,
     :                      %VAL(IDPTR), BIAS,
     :                                              %VAL(ODPTR), STATUS)

      END IF
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Write History
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )

      IF (BIAS) THEN
        TEXT(1) = 'Biassed autocorrelation function'
      ELSE
        TEXT(1) = 'Unbiassed autocorrelation function'
      END IF

      IF (USEWT) THEN
        TEXT(2) = 'Input VARIANCE and QUALITY were used.'
      ELSE
        TEXT(2) = 'Input VARIANCE and QUALITY were NOT used.'
      END IF
      CALL HSI_PTXT( OFID, 2, TEXT, STATUS )

*  Exit
  99  CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END




*+  ACF_WITHWT - Loop over input dataset & calc ACF with weighting
      SUBROUTINE ACF_WITHWT (NDIM, LDIMS, L1, L2, L3, L4, L5, L6, L7,
     :        OLDIMS, O1, O2, O3, O4, O5, O6, O7, NPTS, NLAG, T_AXIS,
     :    INDATA, INVAR, QOK, BIAS, INQUAL, OUTDATA, OUTVAR, OUTQUAL,
     :                                                        STATUS)
*    Description :
*     Converts mapped arrays to 'real' ones prior to calculating ACF
*    Method :
*     Treat input dataset as 7 dimensional, with length of time axis set
*     to 1. Loop over nested dimensions, calculating the ACF.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews
*    History :
*     12-JUN-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      INTEGER                NDIM                  ! Number of dimensions
      INTEGER                LDIMS(7)              ! Length of each input dim.
      INTEGER                L1,L2,L3,L4,L5,L6,L7
      INTEGER                OLDIMS(7)             ! Length of each output dim.
      INTEGER                O1,O2,O3,O4,O5,O6,O7
      INTEGER                NPTS                  ! Number of points in
                                                   ! 1d series.
      INTEGER                NLAG                  ! Number of lags to calc
      INTEGER                T_AXIS                ! Axis to find ACF along.

      REAL                   INDATA (L1,L2,L3,L4,L5,L6,L7)
      REAL                   INVAR (L1,L2,L3,L4,L5,L6,L7)

      LOGICAL                QOK                   ! Input quality OK?
      LOGICAL                BIAS                  ! Use biassed autocovariance?
      LOGICAL                INQUAL(L1,L2,L3,L4,L5,L6,L7)

*    Export :
      REAL                   OUTDATA (O1,O2,O3,O4,O5,O6,O7)
      REAL                   OUTVAR (O1,O2,O3,O4,O5,O6,O7)

      BYTE                   OUTQUAL (O1,O2,O3,O4,O5,O6,O7)

*    Status :
      INTEGER                STATUS

*    Local variables :
      INTEGER                A,B,C,D,E,F,G         ! Loop counters
      INTEGER                INC                   ! Increment between data
                                                   ! values
      INTEGER                PTR1                  ! Pointer to temporary array
      INTEGER                PTR2                  ! Pointer to temporary array
      INTEGER                PTR3                  ! Pointer to temporary array
*-
*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

*    Create 2 dynamic arrays of length of time axis
      CALL DYN_MAPR (1, LDIMS(T_AXIS), PTR1, STATUS)
      CALL DYN_MAPR (1, LDIMS(T_AXIS), PTR2, STATUS)
      CALL DYN_MAPR (1, LDIMS(T_AXIS), PTR3, STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Now set length of T_AXIS dimension to 1
      LDIMS(T_AXIS) = 1

*    Calc inc
      INC = 1

      IF (T_AXIS .GT. 1) THEN
        DO A = 1, T_AXIS - 1
          INC = INC * LDIMS(A)
        END DO
      END IF

*    Loop over 7 dimensions of the input array
      DO G = 1, LDIMS(7)
        DO F = 1, LDIMS(6)
          DO E = 1, LDIMS(5)
            DO D = 1, LDIMS(4)
              DO C = 1, LDIMS(3)
                DO B = 1, LDIMS(2)
                  DO A = 1, LDIMS(1)
                    CALL ACF_COPY_WWT (NPTS, INC, INDATA(A,B,C,D,E,F,G),
     :                 INVAR(A,B,C,D,E,F,G), QOK, INQUAL(A,B,C,D,E,F,G),
     :                               %VAL(PTR1), %VAL(PTR2), %VAL(PTR3))
                    CALL ACF_DOIT_WWT (NPTS, NLAG,  %VAL(PTR1),
     :             %VAL(PTR2), %VAL(PTR3), BIAS, OUTDATA(A,B,C,D,E,F,G),
     :                    OUTVAR(A,B,C,D,E,F,G), OUTQUAL(A,B,C,D,E,F,G))

                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

*    Exit
  99  IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ACF_WITHWT', STATUS )
      END IF

      END




*+  ACF_COPY_WWT - Copy input data & variance
      SUBROUTINE ACF_COPY_WWT (NPTS, INC, INDATA, INVAR, QOK, INQUAL,
     :                                        ARRAY1, ARRAY2, ARRAY3)
*    Description :
*     Copies input data & variance into dynamic arrays, so that they
*     can be mean subtracted. Variance is converted to weight.
*    Authors :
*     Phil Andrews
*    History :
*     13-JUN-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER                NPTS                  ! Number of points in
                                                   ! 1d series.
      INTEGER                INC                   ! Increment between data
                                                   ! values

      REAL                   INDATA(*)             ! Input data
      REAL                   INVAR(*)              ! Input variance

      LOGICAL                QOK
      LOGICAL                INQUAL(*)

*    Export :
      REAL                   ARRAY1(*)
      REAL                   ARRAY2(*)
      REAL                   ARRAY3(*)

*    Local :
      INTEGER                I, J
*-
      IF (QOK) THEN
        DO I = 1, NPTS
          J = 1 + ((I - 1) * INC)

          IF (INQUAL(J)) THEN
            ARRAY1(I) = INDATA(J)
            ARRAY2(I) = 1.0E0 / INVAR(J)
            ARRAY3(I) = SQRT (ARRAY2(I))

          ELSE
            ARRAY1(I) = 0.0E0
            ARRAY2(I) = 0.0E0
            ARRAY3(I) = 0.0E0

          END IF
        END DO

      ELSE
        DO I = 1, NPTS
          J         = 1 + ((I - 1) * INC)
          ARRAY1(I) = INDATA(J)
          ARRAY2(I) = 1.0E0 / INVAR(J)
          ARRAY3(I) = SQRT (ARRAY2(I))

        END DO
      END IF
      END




*+  ACF_DOIT_WWT - Calculate the ACF with weights
      SUBROUTINE ACF_DOIT_WWT (NPTS, NLAGS, INDATA, WEIGHT, ROOTW, BIAS,
     :                                         OUTDATA, OUTVAR, OUTQUAL)
*    Description :
*     Calculates the autocorrelation function.
*     The autocovariance function is found first!
*    Authors :
*     Phil Andrews
*    History :
*     13-JUN-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER                NPTS                  ! Number of points in
                                                   ! 1d series.
      INTEGER                NLAGS                 ! Number of lags to calc


      REAL                   INDATA(*)             ! Input data
      REAL                   WEIGHT(*)             ! 1.0 / Input variance
      REAL                   ROOTW(*)              ! SQRT (WEIGHT(*))

      LOGICAL                BIAS                  ! Use biassed autocovariance?

*    Export :
      REAL                   OUTDATA(*)            ! Output data
      REAL                   OUTVAR(*)             ! Output variance

      BYTE                   OUTQUAL(*)

*    Local variables :
      INTEGER                I, J, K               ! Dummy variables
      INTEGER                LAG

      REAL                   CONVERT               ! convert from unnormalized
                                                   ! autocovasriance to ACF
      REAL                   CONVERT2              ! CONVERT**2
      REAL                   MULT                  ! Applies the bias.
      REAL                   NOISE
      REAL                   WAIT                  ! weighting
      REAL                   ZLAC                  ! zero lag autocovariance
      REAL                   TEMP1, TEMP2, TEMP3   ! Dummy variables
      REAL                   DENOM1, DENOM2        !   "       "
      REAL                   MEAN, WTSUM
*-

      MEAN  = 0.0E0
      WTSUM = 0.0E0

*    Find weighted mean of input data
      DO I = 1, NPTS
        MEAN  = MEAN + (INDATA(I) * WEIGHT(I))
        WTSUM = WTSUM + WEIGHT(I)

      END DO
      MEAN  = MEAN / WTSUM
      NOISE = REAL(NPTS) / WTSUM

*    Subtract mean from data, and calculate Zero Lag AutoCovariance
      ZLAC = 0.0E0

      DO I = 1, NPTS
        INDATA(I) = INDATA(I) - MEAN
        ZLAC      = ZLAC + ((INDATA(I)**2) * WEIGHT(I))
      END DO

*    N.B. biassed & unbiassed cases are the same for zero lag.
      ZLAC       = ZLAC / WTSUM
      CONVERT    = ZLAC - NOISE
      CONVERT2   = CONVERT**2
      OUTDATA(1) = 1.0E0       ! ZLAC / CONVERT
      OUTVAR(1)  = 0.0E0       ! 4.0E0 * ZLAC / CONVERT2

*    Calculate AUTOCOVARIANCE at non zero lags.
      IF (BIAS) THEN
        DO I = 2, NLAGS
          LAG   = I - 1

*        Calculate some values used to calculate the variance later
          DENOM1 = 0.0E0
          DENOM2 = 0.0E0

          DO J = 1, (NPTS - LAG)
            K      = J + LAG
            DENOM1 = DENOM1 + INDATA(J) * ROOTW(J) *
     :                        INDATA(K) * ROOTW(K)
            DENOM2 = DENOM2 + (INDATA(J)**2) * WEIGHT(J)
          END DO

          DO J = NPTS - LAG + 1, NPTS
            DENOM2 = DENOM2 + (INDATA(J)**2) * WEIGHT(J)

          END DO
          DENOM2 = DENOM2 - REAL(NPTS)

*        Now calculate the autocorrelation & more bits for the variance
          TEMP1 = 0.0E0
          TEMP3 = 0.0E0
          WTSUM = 0.0E0

          DO J = 1, (NPTS - LAG)
            K     = J + LAG

            WAIT  = ROOTW(J) * ROOTW(K)
            WTSUM = WTSUM + WAIT

            TEMP1 = TEMP1 + (INDATA(J) * INDATA(K) * WAIT)
            TEMP2 = INDATA(K) * ROOTW(K)

            IF (J - LAG .GE. 1) THEN
              TEMP2 = TEMP2 + (INDATA(J-LAG) * ROOTW(J-LAG))
            END IF

            IF (WEIGHT(J) .GT. 0.0E0) THEN
              TEMP3 = TEMP3 + (1.0E0 / WEIGHT(J)) *
     :                ((TEMP2 * ROOTW(J) / DENOM1) -
     :                 (2.0E0 * INDATA(J) * WEIGHT(J) / DENOM2))**2
            END IF
          END DO

          DO J = NPTS - LAG + 1, NPTS
            K = J - LAG

            IF (K .GE. 1) THEN
              TEMP2 = (INDATA(K) * ROOTW(K))
            ELSE
              TEMP2 = 0.0E0
            END IF

            IF (WEIGHT(J) .GT. 0.0E0) THEN
              TEMP3 = TEMP3 + (1.0E0 / WEIGHT(J)) *
     :                ((TEMP2 * ROOTW(J) / DENOM1) -
     :                 (2.0E0 * INDATA(J) * WEIGHT(J) / DENOM2))**2
            END IF
          END DO


          IF (WTSUM .GT. 0.0E0) THEN
*          Write AUTOCORRELATION (CONVERT converts from autocovariance)
*                                (MULT applies the bias)
            MULT       = 1.0E0 - (REAL(LAG) / REAL(NPTS))
            OUTDATA(I) = MULT * TEMP1 / (WTSUM * CONVERT)
            OUTVAR(I)  = MULT * MULT * TEMP3
            OUTQUAL(I) = 0

          ELSE
            OUTDATA(I) = 0.0E0
            OUTVAR(I)  = 1.0E0
            OUTQUAL(I) = 1

          END IF
        END DO
      ELSE ! Unbiassed determination
        DO I = 2, NLAGS
          LAG   = I - 1

*        Calculate some values used to calculate the variance later
          DENOM1 = 0.0E0
          DENOM2 = 0.0E0

          DO J = 1, (NPTS - LAG)
            K      = J + LAG
            DENOM1 = DENOM1 + INDATA(J) * ROOTW(J) *
     :                        INDATA(K) * ROOTW(K)
            DENOM2 = DENOM2 + (INDATA(J)**2) * WEIGHT(J)
          END DO

          DO J = NPTS - LAG + 1, NPTS
            DENOM2 = DENOM2 + (INDATA(J)**2) * WEIGHT(J)
          END DO
          DENOM2 = DENOM2 - REAL(NPTS)

*        Now calculate the autocovariance & more bits for the variance
          TEMP1 = 0.0E0
          TEMP3 = 0.0E0
          WTSUM = 0.0E0

          DO J = 1, (NPTS - LAG)
            K     = J + LAG

            WAIT  = ROOTW(J) * ROOTW(K)
            WTSUM = WTSUM + WAIT

            TEMP1 = TEMP1 + (INDATA(J) * INDATA(K) * WAIT)
            TEMP2 = INDATA(K) * ROOTW(K)

            IF (J - LAG .GE. 1) THEN
              TEMP2 = TEMP2 + (INDATA(J-LAG) * ROOTW(J-LAG))

            END IF

            IF (WEIGHT(J) .GT. 0.0E0) THEN
              TEMP3 = TEMP3 + (1.0E0 / WEIGHT(J)) *
     :                ((TEMP2 * ROOTW(J) / DENOM1) -
     :                 (2.0E0 * INDATA(J) * WEIGHT(J) / DENOM2))**2

            END IF
          END DO

          DO J = NPTS - LAG + 1, NPTS
            K = J - LAG

            IF (K .GE. 1) THEN
              TEMP2 = (INDATA(K) * ROOTW(K))

            ELSE
              TEMP2 = 0.0E0

            END IF

            IF (WEIGHT(J) .GT. 0.0E0) THEN
              TEMP3 = TEMP3 + (1.0E0 / WEIGHT(J)) *
     :                ((TEMP2 * ROOTW(J) / DENOM1) -
     :                 (2.0E0 * INDATA(J) * WEIGHT(J) / DENOM2))**2

            END IF
          END DO


          IF (WTSUM .GT. 0.0E0) THEN
*          Write AUTOCORRELATION (CONVERT converts from autocovariance)
            OUTDATA(I) = TEMP1 / (WTSUM * CONVERT)
            OUTVAR(I)  = TEMP3
            OUTQUAL(I) = 0

          ELSE
            OUTDATA(I) = 0.0E0
            OUTVAR(I)  = 1.0E0
            OUTQUAL(I) = 1

          END IF
        END DO
      END IF
      END



*+  ACF_QUICK - Loop over input dataset & calc ACF with OUT weighting
      SUBROUTINE ACF_QUICK (NDIM, LDIMS, L1, L2, L3, L4, L5, L6, L7,
     :   OLDIMS, O1, O2, O3, O4, O5, O6, O7, NPTS, NLAG, T_AXIS, INDATA,
     :                                            BIAS, OUTDATA, STATUS)
*    Description :
*     Converts mapped arrays to 'real' ones prior to calculating ACF
*    Method :
*     Treat input dataset as 7 dimensional, with length of time axis set
*     to 1. Loop over nested dimensions, calculating the ACF.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews
*    History :
*     12-JUN-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER                NDIM                  ! Number of dimensions
      INTEGER                LDIMS(7)              ! Length of each input dim.
      INTEGER                L1,L2,L3,L4,L5,L6,L7
      INTEGER                OLDIMS(7)             ! Length of each output dim.
      INTEGER                O1,O2,O3,O4,O5,O6,O7
      INTEGER                NPTS                  ! Number of points in
                                                   ! 1d series.
      INTEGER                NLAG                  ! Number of lags to calc
      INTEGER                T_AXIS                ! Axis to find ACF along.

      REAL                   INDATA (L1,L2,L3,L4,L5,L6,L7)

      LOGICAL                BIAS                  ! Use biassed autocovariance?
*
*    Export :
*
      REAL                   OUTDATA (O1,O2,O3,O4,O5,O6,O7)
*
*    Status :
*
      INTEGER                STATUS
*
*    Local variables :
*
      INTEGER                A,B,C,D,E,F,G         ! Loop counters
      INTEGER                INC                   ! Increment between data
                                                   ! values
      INTEGER                PTR1                  ! Pointer to temporary array
*-

*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

*    Create A dynamic array of same length as time axis
      CALL DYN_MAPR (1, LDIMS(T_AXIS), PTR1, STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Set length of T_AXIS dimension to 1
      LDIMS(T_AXIS) = 1

*    Calc inc
      INC = 1

      IF (T_AXIS .GT. 1) THEN
        DO A = 1, T_AXIS - 1
          INC = INC * LDIMS(A)

        END DO
      END IF

*    Loop over 7 dimensions of the input array
      DO G = 1, LDIMS(7)
        DO F = 1, LDIMS(6)
          DO E = 1, LDIMS(5)
            DO D = 1, LDIMS(4)
              DO C = 1, LDIMS(3)
                DO B = 1, LDIMS(2)
                  DO A = 1, LDIMS(1)
                    CALL ACF_DOIT_QUICK (NPTS, INC, NLAG, INDATA,
     :                         %VAL(PTR1), BIAS, OUTDATA(A,B,C,D,E,F,G))

                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

*    Exit
  99  IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'ACF_QUICK', STATUS )
      END IF

      END



*+  ACF_DOIT_QUICK - Calculate the ACF with OUT weights
      SUBROUTINE ACF_DOIT_QUICK (NPTS, INC, NLAGS, INDATA, DATA, BIAS,
     :                                                          OUTDATA)
*    Description :
*     Calculates the autocorrelation function.
*     The autocovariance function is found first!
*    Authors :
*     Phil Andrews
*    History :
*     13-JUN-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER                NPTS                  ! Number of points in
                                                   ! 1d series.
      INTEGER                INC                   ! Increment between data
                                                   ! values
      INTEGER                NLAGS                 ! Number of lags to calc


      REAL                   INDATA(*)             ! Input data
      REAL                   DATA(*)               ! Array for demeaned data

      LOGICAL                BIAS                  ! Use biassed autocovariance?

*    Export :
      REAL                   OUTDATA(*)            ! Output data

*    Local variables :
      INTEGER                I, J, K               ! Dummy variables
      INTEGER                LAG

      REAL                   CONVERT               ! convert from unnormalized
                                                   ! autocovasriance to ACF
      REAL                   NOISE
      REAL                   ZLAC                  ! zero lag autocovariance
      REAL                   TEMP1                 ! Dummy variable
      REAL                   MEAN
*-

      MEAN  = 0.0E0

*    Find mean of input data
      DO I = 1, NPTS
        MEAN = MEAN + INDATA(1 + ((I - 1) * INC))

      END DO
      MEAN  = MEAN / REAL(NPTS)
      NOISE = 1.0E0

*    Subtract mean from data, and calculate Zero Lag AutoCovariance
      CALL ACF_DEMEAN (NPTS, INC, INDATA, MEAN, DATA, ZLAC)

*    N.B. biassed & unbiassed cases are the same for zero lag.
      CONVERT    = ZLAC - NOISE
      OUTDATA(1) = 1.0E0       ! ZLAC / CONVERT

*    Calculate AUTOCOVARIANCE at non zero lags.
      IF (BIAS) THEN
        DO I = 2, NLAGS
          TEMP1 = 0.0E0
          LAG   = I - 1

          DO J = 1, (NPTS - LAG)
            K     = J + LAG
            TEMP1 = TEMP1 + (DATA(J) * DATA(K))

          END DO

*        Write AUTOCORRELATION (CONVERT converts from autocovariance)
          OUTDATA(I) = TEMP1 / (REAL(NPTS) * CONVERT)

        END DO
      ELSE ! Unbiassed determination
        DO I = 2, NLAGS
          TEMP1 = 0.0E0
          LAG   = I - 1

          DO J = 1, (NPTS - LAG)
            K     = J + LAG
            TEMP1 = TEMP1 + (DATA(J) * DATA(K))

          END DO

*        Write AUTOCORRELATION (CONVERT converts from autocovariance)
          OUTDATA(I) = TEMP1 / (REAL(NPTS - LAG) * CONVERT)

        END DO
      END IF
      END



*+  ACF_DEMEAN - Subtract mean & calc ZLAC
      SUBROUTINE ACF_DEMEAN (NPTS, INC, INDATA, MEAN, DATA, ZLAC)
*    Description :
*     Copies input data 7 variance into dynamic arrays, so that they
*     can be mean subtracted.
*    Authors :
*     Phil Andrews
*    History :
*     13-JUN-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER                NPTS                  ! Number of points in
                                                   ! 1d series.
      INTEGER                INC                   ! Increment between data
                                                   ! values

      REAL                   INDATA(*)             ! Input data
      REAL                   MEAN

*    Export :
      REAL                   DATA(*)
      REAL                   ZLAC                  ! zero lag autocovariance

*    Local :
      INTEGER                I
*-
      ZLAC = 0.0E0

      DO I = 1, NPTS
        DATA(I) = INDATA(1 + ((I - 1) * INC)) - MEAN
        ZLAC    = ZLAC + (DATA(I)**2)

      END DO

      ZLAC = ZLAC / REAL(NPTS)
      END
