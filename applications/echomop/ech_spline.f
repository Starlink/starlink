      SUBROUTINE SPLCALC( NCOEFF, XKNOT, CSPLINE, NDATA, XDATA, YFIT,
     :           IFAIL )
*+
*  Name:
*     ECHOMOP - SPCALC

*  Purpose:
*     Evalutes spline fits.

*  Input:
*     NCOEFF  = NUMBER OF COEFFICIENTS
*     NDATA   = NUMBER OF DATA PAIRS
*     XDATA   = DATA X VALUES

*  Output:
*     YFIT    = CORRESPONDING Y VALUE OF SPLINE FIT
*     IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.

*  Authors:
*     JULY 1984 BY KEITH HORNE

*-

*  Type Defintions:
      IMPLICIT NONE

*  Arguments:
      INTEGER NCOEFF
      INTEGER NDATA

      REAL XDATA( NDATA )
      REAL YFIT( NDATA )

      DOUBLE PRECISION XKNOT( NCOEFF / 2 )
      DOUBLE PRECISION CSPLINE( NCOEFF / 2 )

*  Local Constants:
      INTEGER K
      PARAMETER ( K = 4 ) ! Order of splines plus one.

*  Local Variables:
      DOUBLE PRECISION WORK( 3 * K ) ! Workspace for PDA call.
      DOUBLE PRECISION SPLINE( 4 )   ! Spline and derivatives.
      DOUBLE PRECISION CALC

      INTEGER I         ! Loop index.
      INTEGER J         ! Loop index.
      INTEGER IFAIL     ! Math library call status.
      INTEGER KNOT
      INTEGER LEFT
      INTEGER NCAP7
      INTEGER INBV

*  Functions Called:
      DOUBLE PRECISION ECH_DBVALU
*.

      NCAP7 = NCOEFF / 2
      IF ( NDATA .LE. 0 ) RETURN
*  NAG version.
*      IF ( NCAP7 .LE. 7 ) THEN
*  PDA version.
      IF ( NCAP7 .LE. 7 ) THEN
         CALL ECH_REPORT( 0, ' Splines unknown.' )
         GO TO 999
      END IF

*  Loop through the data.
      DO I = 1, NDATA

*     Evaluate spline at desired X-value.
         IF ( XDATA( I ) .GE. XKNOT( 1 ) .AND.
     :        XDATA( I ) .LE. XKNOT( NCAP7 ) ) THEN
            CALC = XDATA( I )

*        NAG version.
*            IFAIL = 1
*            CALL E02BBF( NCAP7, XKNOT, CSPLINE, CALC, SPLINE, IFAIL )

*        PDA version.
            IFAIL = 0
            INBV = 1
            SPLINE( 1 ) = ECH_DBVALU( XKNOT, CSPLINE, NCAP7 - 4,
     :            0, CALC, INBV, WORK, IFAIL )
            IF ( IFAIL .NE. 0 ) THEN
               GO TO 999
            END IF
            YFIT( I ) = REAL( SPLINE( 1 ) )
            GO TO 500

*     Extrapolation required.
         ELSE IF( XDATA( I ) .LT. XKNOT( 1 ) ) THEN
            KNOT = 1
            LEFT = 0

         ELSE
            KNOT = NCAP7
            LEFT = 1
         END IF

*     Evaluate spline and derivatives at the end knot.

*     NAG version.
*         IFAIL = 1
*         CALL E02BCF( NCAP7, XKNOT, CSPLINE, XKNOT( KNOT ), LEFT,
*     :        SPLINE, IFAIL )
*         IF ( IFAIL .NE. 0 ) THEN
*            GO TO 999
*         END IF

*     PDA version.
         IFAIL = 0
         INBV = 1
         DO J = 0, 1
            SPLINE( J + 1 ) = ECH_DBVALU( XKNOT, CSPLINE, NCAP7 - 4,
     :            J, XKNOT( KNOT ), INBV, WORK, IFAIL )
            IF ( IFAIL .NE. 0 ) THEN
               GO TO 999
            END IF
         END DO
         YFIT( I ) = REAL( SPLINE( 1 ) + SPLINE( 2 ) *
     :         ( DBLE( XDATA( I ) ) - XKNOT( KNOT ) ) )
  500 END DO

*  Normal return.
      IFAIL = 0
      RETURN

*  Error return.
  999 IFAIL = 1
      RETURN
      END


      SUBROUTINE SPLCALC2( NCOEFF, XKNOT, CSPLINE, NDATA, XDATA, YFIT,
     :           IFAIL )
*+
*  Name:
*     ECHOMOP - SPCALC2

*  Purpose:
*     Evalutes spline fits.

*  Inputs:
*     NCOEFF  = NUMBER OF COEFFICIENTS
*     NDATA   = NUMBER OF DATA PAIRS
*     XDATA   = DATA X VALUES

*  Outputs:
*     YFIT    = CORRESPONDING Y VALUE OF SPLINE FIT
*     IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.

*  Authors:
*     JULY 1984 BY KEITH HORNE

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER NCOEFF
      INTEGER NDATA

      DOUBLE PRECISION XDATA( NDATA )
      DOUBLE PRECISION YFIT( NDATA )
      DOUBLE PRECISION XKNOT( NCOEFF / 2 )
      DOUBLE PRECISION CSPLINE( NCOEFF / 2 )

*  Local Constants:
      INTEGER K
      PARAMETER ( K = 4 ) ! Order of splines plus one.

*  Local Variables:
      DOUBLE PRECISION WORK( 3 * K )
      DOUBLE PRECISION SPLINE( 4 )
      DOUBLE PRECISION CALC

      INTEGER I         ! Loop index.
      INTEGER IFAIL     ! Math library call status.
      INTEGER NCAP7
      INTEGER NCAP7M4
      INTEGER INBV

*  Functions Called:
      DOUBLE PRECISION ECH_DBVALU
*.

      NCAP7 = NCOEFF / 2
      NCAP7M4 = NCAP7 - 4
      IF ( NDATA .LE. 0 ) RETURN
*  NAG version.
*      IF ( NCAP7 .LE. 7 ) THEN
*  PDA version.
      IF ( NCAP7 .LE. 7 ) THEN
         CALL ECH_REPORT( 0, ' Splines unknown.' )
         GO TO 999
      END IF

*  Loop through the data: extrapolate back from first knot.
      I = 1
      IF ( XKNOT( 1 ) .LT. XDATA( NDATA ) ) THEN
         DO WHILE ( XDATA( I ) .LT. XKNOT( 1 ) )

*        NAG version.
*            IFAIL = 1
*            CALL E02BCF( NCAP7, XKNOT, CSPLINE,
*        :        XKNOT( 1 ), 0, SPLINE, IFAIL )
*            IF ( IFAIL .NE. 0 ) THEN
*               GO TO 999
*            END IF

*        PDA version.
            IFAIL = 0
            INBV = 1
            SPLINE( 1 ) = ECH_DBVALU( XKNOT, CSPLINE, NCAP7M4,
     :            0, XKNOT( 1 ), INBV, WORK, IFAIL )
            IF ( IFAIL .NE. 0 ) THEN
               GO TO 999
            END IF
            SPLINE( 2 ) = ECH_DBVALU( XKNOT, CSPLINE, NCAP7M4,
     :            1, XKNOT( 1 ), INBV, WORK, IFAIL )
            IF ( IFAIL .NE. 0 ) THEN
               GO TO 999
            END IF
            YFIT( I ) = SPLINE( 1 ) + SPLINE( 2 ) *
     :            ( XDATA( I ) - XKNOT( 1 ) )
            I = I + 1
         END DO

      ELSE
         DO WHILE ( I .LE. NDATA )

*        NAG version.
*            IFAIL = 1
*            CALL E02BCF( NCAP7, XKNOT, CSPLINE,
*        :        XKNOT( 1 ), 0, SPLINE, IFAIL )
*            IF ( IFAIL .NE. 0 ) THEN
*               GO TO 999
*            END IF

*        PDA version.
            IFAIL = 0
            INBV = 1
            SPLINE( 1 ) = ECH_DBVALU( XKNOT, CSPLINE, NCAP7M4,
     :            0, XKNOT( 1 ), INBV, WORK, IFAIL )
            IF ( IFAIL .NE. 0 ) THEN
               GO TO 999
            END IF
            SPLINE( 2 ) = ECH_DBVALU( XKNOT, CSPLINE, NCAP7M4,
     :            1, XKNOT( 1 ), INBV, WORK, IFAIL )
            IF ( IFAIL .NE. 0 ) THEN
               GO TO 999
            END IF
            YFIT( I ) = SPLINE( 1 ) + SPLINE( 2 ) *
     :            ( XDATA( I ) - XKNOT( 1 ) )
            I = I + 1
         END DO
      END IF

*  Loop through the data: between end knots.
      IF ( XDATA( NDATA ) .LE. XKNOT( NCAP7 ) ) THEN
         DO WHILE ( I .LE. NDATA )

*        NAG version.
*           CALC = XDATA( I )
*           IFAIL = 1
*           CALL E02BBF( NCAP7, XKNOT, CSPLINE, CALC, SPLINE, IFAIL )

*        PDA version.
            IFAIL = 0
            INBV = 1
            CALC = ECH_DBVALU( XKNOT, CSPLINE, NCAP7M4,
     :            0, XDATA( I ), INBV, WORK, IFAIL )
            IF ( IFAIL .NE. 0 ) THEN
               GO TO 999
            END IF
            YFIT( I ) = CALC
            I = I + 1
         END DO

      ELSE
         DO WHILE ( XDATA( I ) .LE. XKNOT( NCAP7 ) )

*        NAG version.
*           CALC = XDATA( I )
*           IFAIL = 1
*           CALL E02BBF( NCAP7, XKNOT, CSPLINE, CALC, SPLINE, IFAIL )

*        PDA version.
            IFAIL = 0
            INBV = 1
            CALC = ECH_DBVALU( XKNOT, CSPLINE, NCAP7M4,
     :            0, XDATA( I ), INBV, WORK, IFAIL )
            IF ( IFAIL .NE. 0 ) THEN
               GO TO 999
            END IF
            YFIT( I ) = CALC
            I = I + 1
         END DO
      END IF

*  Loop through the data: extrapolate from last knot.
      DO WHILE ( I .LE. NDATA )

*     NAG version.
*         IFAIL = 1
*         CALL E02BCF( NCAP7, XKNOT, CSPLINE,
*     :        XKNOT( NCAP7 ), 1, SPLINE, IFAIL )
*         IF ( IFAIL .NE. 0 ) THEN
*            GO TO 999
*         END IF

*     PDA version.
         IFAIL = 0
         INBV = 1
         SPLINE( 1 ) = ECH_DBVALU( XKNOT, CSPLINE, NCAP7M4,
     :         0, XKNOT( NCAP7 ), INBV, WORK, IFAIL )
         IF ( IFAIL .NE. 0 ) THEN
            GO TO 999
         END IF
         SPLINE( 2 ) = ECH_DBVALU( XKNOT, CSPLINE, NCAP7M4,
     :         1, XKNOT( NCAP7 ), INBV, WORK, IFAIL )
         IF ( IFAIL .NE. 0 ) THEN
            GO TO 999
         END IF
         YFIT( I ) = SPLINE( 1 ) + SPLINE( 2 ) *
     :         ( XDATA( I ) - XKNOT( NCAP7 ) )
         I = I + 1
      END DO

*  Normal return.
      IFAIL = 0
      RETURN

*  Error return.
  999 IFAIL = 1
      RETURN
      END


      SUBROUTINE SPLFIT1( NDATA, XDATA, YDATA, YSIGMA, YFIT, NCOEFF,
     :           XKNOT, CSPLINE, X, Y, W, KEY, WORKSIZE, WORK1,
     :           WORK2, IFAIL )
*+
*  Name:
*     ECHOMOP - SPLFIT1

*  Purpose:
*     Computes weighted least-squares spline fit to data pairs.

*  Input:
*     NDATA   = NUMBER OF DATA PAIRS
*     XDATA   = DATA X VALUES
*     YDATA   = DATA Y VALUES
*     YSIGMA  = UNCERTAINTY IN Y (1-SIGMA) (NEGATIVE TO REJECT)
*     KNOTREQ = NUMBER OF SPLINES REQUESTED

*  Output:
*     YFIT    = FITTED Y VALUES
*     RMS     = RMS NORMALIZED RESIDUAL OF POINTS NOT REJECTED
*     IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.

*  Authors:
*     KDH: Keith Horne
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     ??-JUL-1984 (KDH):
*       Original version.
*     ??-SEP-1984 (KDH):
*       Sorting and extrapolation added.
*     09-AUG-1996 (MJC):
*       Changed to use PDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments:
      INTEGER NDATA
      REAL XDATA( NDATA )
      REAL YDATA( NDATA )
      REAL YSIGMA( NDATA )
      REAL YFIT( NDATA )
      INTEGER NCOEFF
      DOUBLE PRECISION XKNOT( NCOEFF / 2 )
      DOUBLE PRECISION CSPLINE( NCOEFF / 2 )
      DOUBLE PRECISION X( NDATA + 2 )
      DOUBLE PRECISION Y( NDATA + 2 )
      DOUBLE PRECISION W( NDATA + 2 )
      INTEGER KEY( NDATA + 2 )  ! Key to sorted data.
      INTEGER WORKSIZE
      DOUBLE PRECISION WORK1( WORKSIZE )
      DOUBLE PRECISION WORK2( 2 * NCOEFF )

*  Local Constants:
      INTEGER MAXKNOT
      PARAMETER ( MAXKNOT = 50 )

*  Local Variables:
      DOUBLE PRECISION CALC ! Calculation intermediate.

      REAL PART         ! Fractional distance along X range to place knot.
      REAL RMS          ! RMS value, based on YSIGMAs.
      REAL YMEAN        ! Mean of YDATA.

      INTEGER I         ! Loop index.
      INTEGER J         ! PDA call fit status.
      INTEGER IFAIL     ! Status of spline fit call.
      INTEGER ISTEP     ! Diretion for loading of X/Y/W arrays.
      INTEGER ITEST     ! Loop index.
      INTEGER IGET      ! Index into the input XDATA/YDATA/YSIGMA arrays.
      INTEGER IGET1     ! Index of first point to be used.
      INTEGER IGET2     ! Index of last point to be used.
      INTEGER IPUT      ! Index into the X/Y/W arrays.
      INTEGER IPUT1     ! Index of first point to be fitted.
      INTEGER IPUT2     ! Index of last point to be fitted.
      INTEGER KNOTREQ   ! Number of knots required.
      INTEGER LOCMAX    ! Index to maximum value in XDATA.
      INTEGER LOCMIN    ! Index to minimum value in XDATA.
      INTEGER LOOKUP    ! Index of knot position in X.
      INTEGER NFIT      ! Number of data points to fit.
      INTEGER NSPLINE   ! Number of intervals for the spline.
*.

*  Count and compute mean of YDATA with positive YSIGMA,
*  and locate extreme values of XDATA.
      NFIT = 0
      CALC = 0.D0
      LOCMAX = 1
      LOCMIN = 1
      DO I = 1, NDATA
         IF ( XDATA( I ) .GE. XDATA( LOCMAX ) ) LOCMAX = I
         IF ( XDATA( I ) .LT. XDATA( LOCMIN ) ) LOCMIN = I
         IF ( YSIGMA( I ) .GT. 0.0 ) THEN
            NFIT = NFIT + 1
            CALC = CALC + DBLE( YDATA( I ) )
         END IF
      END DO
      IF ( NFIT .LE. 0 ) THEN
         GO TO 999
      END IF
      YMEAN = REAL( CALC ) / FLOAT( NFIT )

*  Use mean of YDATA if there is not enough data for a spline fit.
      IF ( NFIT .LT. 3 ) THEN
         CALL ECH_REPORT( 0,
     :        ' Warning: using mean instead spline fit.' )
         DO I = 1, NDATA
            YFIT( I ) = YMEAN
         END DO
         GO TO 1000
      END IF

*  Decide in which direction to load the data values.
      IF ( LOCMAX .GT. LOCMIN ) THEN
         ISTEP = 1      ! Forward loading
         IGET1 = 1
         IGET2 = NDATA

      ELSE
         ISTEP = -1     ! Backward loading
         IGET1 = NDATA
         IGET2 = 1
      END IF

*  Load data arrays for spline fit.
      IPUT1 = 1
      IPUT = 0
      CALC = 0.D0
      DO IGET = IGET1, IGET2, ISTEP

*     Load only data with positive sigma.
         IF ( YSIGMA( IGET ) .GT. 0.0 ) THEN
            IPUT = IPUT + 1
            X( IPUT ) = DBLE( XDATA( IGET ) )
            Y( IPUT ) = DBLE( YDATA( IGET ) )
            W( IPUT ) = DBLE( YSIGMA( IGET ) )
            CALC = CALC + W( IPUT ) * W( IPUT )
         END IF
      END DO

*  Find out how many values are to be fitted.
      IPUT2 = IPUT
      NFIT = IPUT2 - IPUT1 + 1

*  Re-scale weights to their RMS value.
      RMS = SQRT( CALC / NFIT )
      DO I = IPUT1, IPUT2

*     NAG version.
*         W( I ) = RMS / W( I )

*     PDA version.
         W( I ) = W( I ) / RMS
      END DO

*  Sort data so that X values increase (only needed for NAG version).
*      DO ITEST = IPUT1 + 1, IPUT2
*         IF ( X( ITEST ) .LT. X( ITEST - 1 ) ) THEN
*            CALL SHELLSORT( NFIT, X( IPUT1 ), KEY( IPUT1 ) )
*            DO I = IPUT1, IPUT2
*               WORK1( I ) = X( KEY( I ) )
*            END DO
*            DO I = IPUT1, IPUT2
*               X( I ) = WORK1( I )
*            END DO
*            DO I = IPUT1, IPUT2
*               WORK1( I ) = Y( KEY( I ) )
*            END DO
*            DO I = IPUT1, IPUT2
*               Y( I ) = WORK1( I )
*            END DO
*            DO I = IPUT1, IPUT2
*               WORK1( I ) = W( KEY( I ) )
*            END DO
*            DO I = IPUT1, IPUT2
*               W( I ) = WORK1( I )
*            END DO
*            GO TO 50
*         END IF
*      END DO
*   50 CONTINUE

*  Decide how many splines will be used.
*  NAG version.
*      KNOTREQ = NCOEFF / 2 - 7
*  PDA version.
      KNOTREQ = NCOEFF / 2 - 7

      NSPLINE = MAX( 1, MIN( KNOTREQ, MIN( NFIT / 2 + 1, MAXKNOT ) ) )
      IF ( NSPLINE .LT. KNOTREQ ) THEN
         CALL ECH_REPORT( 0, ' Number of splines reduced.' )
      END IF

*  Distribute spline knots.
      DO I = 1, NSPLINE + 1
         PART = ( I - 1.0 ) / NSPLINE
         LOOKUP = NINT( IPUT1 * ( 1.0 - PART ) + IPUT2 * PART )
         XKNOT( I + 3 ) = X( LOOKUP )
      END DO

*  Duplicate end knots (not needed in NAG version).
      DO I = 1, 3
         XKNOT( I ) = XKNOT( 4 )
         XKNOT( NSPLINE + I + 4 ) = XKNOT( NSPLINE + 4 )
      END DO

*  Compute spline fit (NAG version).
*      IFAIL = 1
*      CALL E02BAF( NFIT, NSPLINE + 7, X( IPUT1 ),
*     :     Y( IPUT1 ), W( IPUT1 ), XKNOT,
*     :     WORK1, WORK2, CSPLINE, CALC, IFAIL )
*      IF ( IFAIL .NE. 0 ) THEN
*         RETURN
*      END IF

*  Compute spline fit (PDA version).
      IFAIL = 0
      NFIT = NFIT - 1
      CALL PDA_DEFC( NFIT, X( IPUT1 ), Y( IPUT1 ),
     :     W( IPUT1 ), 4, NSPLINE + 7, XKNOT, 1, J, CSPLINE,
     :     WORKSIZE, WORK1, IFAIL )
      IF ( J .NE. 1 .OR. IFAIL .NE. 0 ) THEN
         RETURN
      END IF

*  Evaluate spline at required points.
      CALL SPLCALC( NCOEFF, XKNOT, CSPLINE,
     :     NDATA, XDATA, YFIT, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         RETURN
      END IF

*  Normal return.
 1000 IFAIL = 0
      RETURN

*  Error return.
  999 IFAIL = 1
      RETURN

      END


      SUBROUTINE SPLFIT2( NDATA, XDATA, YDATA, YSIGMA, YFIT, NCOEFF,
     :           XKNOT, CSPLINE, X, Y, W, KEY, WORKSIZE, WORK1,
     :           WORK2, IFAIL )
*+
*  Name:
*     ECHOMOP - SPLFIT2

*  Purpose:
*     Computes weighted least-squares spline fit to data pairs.

*  Input:
*     NDATA   = NUMBER OF DATA PAIRS
*     XDATA   = DATA X VALUES
*     YDATA   = DATA Y VALUES
*     YSIGMA  = UNCERTAINTY IN Y (1-SIGMA) (NEGATIVE TO REJECT)
*     KNOTREQ = NUMBER OF SPLINES REQUESTED

*  Output:
*     YFIT    = FITTED Y VALUES
*     RMS     = RMS NORMALIZED RESIDUAL OF POINTS NOT REJECTED
*     IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.

*  Authors:
*     KDH: Keith Horne
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     ??-JUL-1984 (KDH):
*       Original version.
*     ??-SEP-1984 (KDH):
*       Sorting and extrapolation added.
*     09-AUG-1996 (MJC):
*       Changed to use PDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments:
      INTEGER NDATA
      DOUBLE PRECISION XDATA( NDATA )
      DOUBLE PRECISION YDATA( NDATA )
      REAL YSIGMA( NDATA )
      DOUBLE PRECISION YFIT( NDATA )
      INTEGER NCOEFF
      DOUBLE PRECISION XKNOT( NCOEFF / 2 )
      DOUBLE PRECISION CSPLINE( NCOEFF / 2 )
      DOUBLE PRECISION X( NDATA + 2 )
      DOUBLE PRECISION Y( NDATA + 2 )
      DOUBLE PRECISION W( NDATA + 2 )
      INTEGER KEY( NDATA + 2 )  ! Key to sorted data.
      INTEGER WORKSIZE
      DOUBLE PRECISION WORK1( WORKSIZE )
      DOUBLE PRECISION WORK2( 2 * NCOEFF )

*  Local Constants:
      INTEGER MAXKNOT
      PARAMETER ( MAXKNOT = 50 )

*  Local Variables:
      DOUBLE PRECISION CALC       ! Calculation intermediate.
      DOUBLE PRECISION YMEAN      ! Mean of YDATA.

      REAL PART         ! Fractional distance along X range to place knot.
      REAL RMS          ! RMS value, based on YSIGMAs.

      INTEGER I         ! Loop index.
      INTEGER J         ! PDA call fit status.
      INTEGER IFAIL     ! Status of spline fit call.
      INTEGER ISTEP     ! Diretion for loading of X/Y/W arrays.
      INTEGER ITEST     ! Loop index.
      INTEGER IGET      ! Index into the input XDATA/YDATA/YSIGMA arrays.
      INTEGER IGET1     ! Index of first point to be used.
      INTEGER IGET2     ! Index of last point to be used.
      INTEGER IPUT      ! Index into the X/Y/W arrays.
      INTEGER KNOTREQ   ! Number of knots required.
      INTEGER LOCMAX    ! Index to maximum value in XDATA.
      INTEGER LOCMIN    ! Index to minimum value in XDATA.
      INTEGER LOOKUP    ! Index of knot position in X.
      INTEGER NFIT      ! Number of data points to fit.
      INTEGER NSPLINE   ! Number of intervals for the spline.
*.

*  Count and compute mean of YDATA with positive YSIGMA,
*  and locate extreme values of XDATA.
      NFIT = 0
      CALC = 0.D0
      LOCMAX = 1
      LOCMIN = 1
      DO I = 1, NDATA
         IF ( XDATA( I ) .GE. XDATA( LOCMAX ) ) LOCMAX = I
         IF ( XDATA( I ) .LT. XDATA( LOCMIN ) ) LOCMIN = I
         IF ( YSIGMA( I ) .GT. 0.0 ) THEN
            NFIT = NFIT + 1
            CALC = CALC + YDATA( I )
         END IF
      END DO
      IF ( NFIT .LE. 0 ) THEN
         GO TO 999
      END IF
      YMEAN = CALC / DBLE( NFIT )

*  Use mean of YDATA if there is not enough data for a spline fit.
      IF ( NFIT .LT. 3 ) THEN
         CALL ECH_REPORT( 0,
     :        ' Warning: using mean instead spline fit.' )
         DO I = 1, NDATA
            YFIT( I ) = YMEAN
         END DO
         GO TO 1000
      END IF

*  Decide in which direction to load the data values.
      IF ( LOCMAX .GT. LOCMIN ) THEN
         ISTEP = 1      ! Forward loading
         IGET1 = 1
         IGET2 = NDATA

      ELSE
         ISTEP = -1     ! Backward loading
         IGET1 = NDATA
         IGET2 = 1
      END IF

*  Load data arrays for spline fit.
      IPUT = 0
      CALC = 0.D0
      DO IGET = IGET1, IGET2, ISTEP

*     Load only data with positive sigma.
         IF ( YSIGMA( IGET ) .GT. 0.0 ) THEN
            IPUT = IPUT + 1
            X( IPUT ) = XDATA( IGET )
            Y( IPUT ) = YDATA( IGET )
            W( IPUT ) = YSIGMA( IGET )
            CALC = CALC + W( IPUT ) * W( IPUT )
         END IF
      END DO

*  Find out how many values are to be fitted.
      NFIT = IPUT

*  Re-scale weights to their RMS value.
      RMS = SQRT( CALC / NFIT )
      DO I = 1, IPUT

*     NAG version.
*         W( I ) = RMS / W( I )

*     PDA version.
         W( I ) = W( I ) / RMS
      END DO

*  Sort data so that X values increase (only needed for NAG version).
*      DO ITEST = 2, IPUT
*         IF ( X( ITEST ) .LT. X( ITEST - 1 ) ) THEN
*            CALL SHELLSORT( NFIT, X, KEY )
*            DO I = 1, IPUT
*               WORK1( I ) = X( KEY( I ) )
*            END DO
*            DO I = 1, IPUT
*               X( I ) = WORK1( I )
*            END DO
*            DO I = 1, IPUT
*               WORK1( I ) = Y( KEY( I ) )
*            END DO
*            DO I = 1, IPUT
*               Y( I ) = WORK1( I )
*            END DO
*            DO I = 1, IPUT
*               WORK1( I ) = W( KEY( I ) )
*            END DO
*            DO I = 1, IPUT
*               W( I ) = WORK1( I )
*            END DO
*            GO TO 50
*         END IF
*      END DO
*   50 CONTINUE

*  Decide how many splines will be used.
*  NAG version.
      KNOTREQ = NCOEFF / 2 - 7
*  PDA version.
      KNOTREQ = NCOEFF / 2 - 7
      NSPLINE = MAX( 1, MIN( KNOTREQ, MIN( NFIT / 2 + 1, MAXKNOT ) ) )
      IF ( NSPLINE .LT. KNOTREQ ) THEN
         CALL ECH_REPORT( 0, ' Number of splines reduced.' )
      END IF

*  Distribute spline knots.
      DO I = 1, NSPLINE + 1
         PART = FLOAT( I - 1 ) / FLOAT( NSPLINE )
         LOOKUP = NINT( ( 1.0 - PART ) + IPUT * PART )
         XKNOT( I + 3 ) = X( LOOKUP )
      END DO

*  Duplicate end knots (not needed in NAG version).
      DO I = 1, 3
         XKNOT( I ) = X( 1 )
         XKNOT( NSPLINE + I + 4 ) = X( IPUT )
      END DO

*  Compute spline fit (NAG version).
*      IFAIL = 1
*      CALL E02BAF( NFIT, NSPLINE + 7, X( IPUT1 ),
*     :     Y( IPUT1 ), W( IPUT1 ), XKNOT,
*     :     WORK1, WORK2, CSPLINE, CALC, IFAIL )
*      IF ( IFAIL .NE. 0 ) THEN
*         RETURN
*      END IF

*  Compute spline fit (PDA version).
      IFAIL = 0
      NFIT = NFIT - 1
      CALL PDA_DEFC( NFIT, X, Y,
     :     W, 4, NSPLINE + 7, XKNOT, 1, J, CSPLINE,
     :     WORKSIZE, WORK1, IFAIL )
      IF ( J .NE. 1 .OR. IFAIL .NE. 0 ) THEN
         RETURN
      END IF

*  Evaluate spline at required points.
      CALL SPLCALC2( NCOEFF, XKNOT, CSPLINE,
     :     NDATA, XDATA, YFIT, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         RETURN
      END IF

*  Normal return.
 1000 IFAIL = 0
      RETURN

*  Error return.
  999 IFAIL = 1
      RETURN

      END


      SUBROUTINE SPLFITA( NDATA, XDATA, YDATA, YSIGMA, YFIT,
     :           NCYCLE, THRHI, THRLO, NCOEFF, XKNOT, CSPLINE,
     :           X, Y, W, KEY, WORKSIZE, WORK1, WORK2, IFAIL )

*+
*  Name:
*     ECHOMOP - SPLFITA

*  Purpose:
*     Fits spline to data and performs reject cycles with
*     no user interaction.

*  Inputs:
*     NDATA   = number of data values
*     XDATA   = X data values
*     YDATA   = Y data values
*     YSIGMA  = Y uncertainties (1-sigma) (negative to reject)
*     NSPLINE = Number of spline nodes
*     NCYCLE  = Max number of reject cycles
*     THRHI   = high threshold for sigma clipping
*     THRLO   = low threshold for sigma clipping

*  Output:
*     YFIT    = Y FIT VALUES
*     IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.

*  History:
*     Oct 1986 KDH @ STScI.

*-

*  Type Definitions:
      IMPLICIT NONE

      INTEGER NDATA
      INTEGER NCOEFF
      INTEGER WORKSIZE

      REAL XDATA( NDATA )
      REAL YDATA( NDATA )
      REAL YSIGMA( NDATA )
      REAL YFIT( NDATA )
      REAL THRHI
      REAL THRLO

      DOUBLE PRECISION X( NDATA + 2 )
      DOUBLE PRECISION Y( NDATA + 2 )
      DOUBLE PRECISION W( NDATA + 2 )
      INTEGER KEY( NDATA + 2 )  ! Key to sorted data
      DOUBLE PRECISION WORK1( NDATA + 2 )
      DOUBLE PRECISION WORK2( 2 * NCOEFF )

      DOUBLE PRECISION XKNOT( NCOEFF / 2 )
      DOUBLE PRECISION CSPLINE( NCOEFF / 2 )

      REAL RMS

      INTEGER LASTREJ   ! Reject count from previous fit.
      INTEGER NREJ      ! Count of rejected pixels.
      INTEGER ICYCLE    ! Count of fit-reject cycles.
      INTEGER IFAIL
      INTEGER NCYCLE
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3

      CHARACTER*80 REPORT_STRING
      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2
      CHARACTER*16 REF_STR3
*.

*  Test inputs.
      IF ( NDATA .LE. 0 ) THEN
         GO TO 999
      END IF

      NREJ = 0
      ICYCLE = -1
  100 ICYCLE = ICYCLE + 1
      LASTREJ = NREJ

*  Fit the spline.
      CALL SPLFIT1( NDATA, XDATA, YDATA, YSIGMA, YFIT, NCOEFF, XKNOT,
     :     CSPLINE, X, Y, W, KEY, WORKSIZE, WORK1, WORK2, IFAIL )
      IF ( IFAIL .NE. 0 ) GO TO 999

*  Evaluate the fit.
*  Do not restore any points.
      NREJ = -1

      IF ( NCYCLE .GT. 0 ) THEN

*     Evaluate the fit and reject large outliers.
         CALL REJECT( NDATA, YDATA, YSIGMA, YFIT, THRHI, THRLO, RMS,
     :        NREJ )

*     Report results of this reject.
         CALL CHR_ITOC( ICYCLE, REF_STR1, NCHAR1 )
         CALL CHR_RTOC( RMS, REF_STR2, NCHAR2 )
         CALL CHR_ITOC( NREJ, REF_STR3, NCHAR3 )
         REPORT_STRING = ' Cycle ' //
     :         REF_STR1( :NCHAR1 ) // ': RMS=' //
     :         REF_STR2( :NCHAR2 ) // ', (sigma) rejects: ' //
     :         REF_STR3( :NCHAR3 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

*  Next fit-reject cycle.
      IF ( ICYCLE .LT. NCYCLE .AND. NREJ .NE. LASTREJ ) GO TO 100

*  Normal return.
      IFAIL = 0
      RETURN

*  Error return.
  999 IFAIL = 1
      RETURN

      END


      SUBROUTINE SPLFITD( NDATA, XDATA, YDATA, YSIGMA, YFIT,
     :           NCYCLE, THRHI, THRLO, NCOEFF, XKNOT, CSPLINE,
     :           X, Y, W, KEY, WORKSIZE, WORK1, WORK2, IFAIL )

*
* Fits spline to data and performs reject cycles with
* no user interaction.
*
*  Inputs:
*       NDATA   = number of data values
*       XDATA   = X data values
*       YDATA   = Y data values
*       YSIGMA  = Y uncertainties (1-sigma) (negative to reject)
*       NSPLINE = Number of spline nodes
*       NCYCLE  = Max number of reject cycles
*       THRHI   = high threshold for sigma clipping
*       THRLO   = low threshold for sigma clipping
*
*  Output:
*       YFIT    = Y FIT VALUES
*       IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.
*
* Oct 1986 KDH @ STScI
*
      INTEGER WORKSIZE
      DOUBLE PRECISION XDATA(ndata), YDATA(ndata), YFIT(ndata)
      REAL YSIGMA(ndata)

      DOUBLE PRECISION X(ndata+2), Y(ndata+2), W(ndata+2)
      INTEGER KEY(ndata+2)  ! Key to sorted data
      DOUBLE PRECISION WORK1(ndata+2)
      DOUBLE PRECISION WORK2(2*ncoeff)

      DOUBLE PRECISION XKNOT(ncoeff/2)
      DOUBLE PRECISION CSPLINE(ncoeff/2)

      INTEGER LASTREJ     ! Reject count from previous fit.
      INTEGER NREJ        ! Count of rejected pixels.
      INTEGER ICYCLE      ! Count of fit-reject cycles.
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3

      CHARACTER*80 REPORT_STRING
      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2
      CHARACTER*16 REF_STR3

*  Test inputs.
      IF ( NDATA .LE. 0 ) THEN
         GO TO 999
      END IF

      NREJ = 0
      ICYCLE = -1
  100 ICYCLE = ICYCLE + 1
      LASTREJ = NREJ

*  Fit the spline.
      CALL SPLFIT2( NDATA, XDATA, YDATA, YSIGMA, YFIT, NCOEFF, XKNOT,
     :     CSPLINE, X, Y, W, KEY, WORKSIZE, WORK1, WORK2, IFAIL )
      IF ( IFAIL .NE. 0 ) GO TO 999

* Evaluate the fit
* Do not restore any points
      NREJ = -1

      IF ( NCYCLE .GT. 0 ) THEN

*     Evaluate the fit and reject large outliers.
         CALL REJECT2( NDATA, YDATA, YSIGMA, YFIT, THRHI, THRLO, RMS,
     :        NREJ )

*     Report results of this reject.
         CALL CHR_ITOC( ICYCLE, REF_STR1, NCHAR1 )
         CALL CHR_RTOC( RMS, REF_STR2, NCHAR2 )
         CALL CHR_ITOC( NREJ, REF_STR3, NCHAR3 )
         REPORT_STRING = ' Cycle ' //
     :         REF_STR1( :NCHAR1 ) // ': RMS=' //
     :         REF_STR2( :NCHAR2 ) // ', (sigma) rejects: ' //
     :         REF_STR3( :NCHAR3 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

*  Next fit-reject cycle.
      IF ( ICYCLE .LT. NCYCLE .AND. NREJ .NE. LASTREJ ) GO TO 100

*  Normal return.
      IFAIL = 0
      RETURN

*  Error return.
  999 IFAIL = 1
      RETURN

      END


      SUBROUTINE SHELLSORT(NDATA, XDATA, KEY)
C
C MIKE IRWIN ROUTINE SUBSTANTIALLY MODIFIED BY T.R. MARSH
C SHELL SORT ROUTINE
C INPUT:
C       NDATA = NUMBER OF DATA VALUES
C       XDATA = DATA VALUES
C OUTPUT:
C       KEY   = KEY TO SORTED DATA VALUES (I.E. XDATA(KEY(I)) ASCENDS)
C
      DOUBLE PRECISION XDATA(ndata),XX
      INTEGER KEY(ndata)
C
C EVALUATE JUMP STEP TO BE USED IN SHELL SORT
C
      JUMP = 2
   10 JUMP = 2*JUMP
      IF( JUMP .LT. NDATA ) GO TO 10
      JUMP = MIN0( NDATA, (3*JUMP)/4-1 )
C
C INITIALISE KEY ARRAY
C
      DO I = 1, NDATA
          KEY(I) = I
      END DO
      IF(NDATA.EQ.1) RETURN
      DO WHILE(JUMP .GT. 1)
          JUMP = JUMP/2
          IEND = NDATA - JUMP
          DO I = 1, IEND
            I1 = I
            I2 = I + JUMP
C
C COMPARE VALUES "JUMP" APART IN THE CURRENT SORTED ARRAY
C A VALUE IS MOVED IN THE ARRAY IF IT IS LESS THAN THE
C VALUE JUMP BINS BEFORE IT. IT WILL CARRY ON JUMPING
C UP THE ARRAY UNTIL IT MEETS A SMALLER VALUE OR RUNS OUT
C OF SPACE
C
            IF( XDATA(KEY(I1)) .GT. XDATA(KEY(I2)) ) THEN
              KEEP = KEY(I2)
              XX = XDATA(KEEP)
   20         KEY(I2) = KEY(I1)
              I2 = I1
              I1 = I1 - JUMP
              IF( I1.LE.0 ) GO TO 30
              IF( XDATA(KEY(I1)) .GT. XX ) GO TO 20
   30         KEY(I2) = KEEP
            END IF
          END DO
      END DO

      END


      DOUBLE PRECISION FUNCTION ECH_DBVALU( T, A, N, IDERIV, X, INBV,
     :      WORK, STATUS)
*+
*  Name:
*     ECHOMOP - ECH_DBVALU

*  Notes:
*     Pinched from PDA library by MJC - modified to speed up splines
*     as they are all cubics and boundary conditions are tested by
*     higher-level ECHOMOP routines.

*-
      INTEGER STATUS
      INTEGER I,IDERIV,IDERP1,IHI,IHMKMJ,ILO,IMK,IMKPJ, INBV, IPJ,
     : IP1, IP1MJ, J, JJ, J1, J2, KMJ, MFLAG, N
      DOUBLE PRECISION A, FKMJ, T, WORK, X
      DIMENSION T(*), A(*), WORK(*)

C***FIRST EXECUTABLE STATEMENT  ECH_DBVALU
      ECH_DBVALU = 0.0D0
C
C *** FIND *I* IN (K,N) SUCH THAT T(I) .LE. X .LT. T(I+1)
C     (OR, .LE. T(I+1) IF T(I) .LT. T(I+1) = T(N+1)).
      CALL PDA_DINTRV(T, N+1, X, INBV, I, MFLAG)
      IF (MFLAG.EQ.0) GO TO 20
   10 IF (I.EQ.4) GO TO 140
      I = I - 1
      IF (X.EQ.T(I)) GO TO 10
C
C *** DIFFERENCE THE COEFFICIENTS *IDERIV* TIMES
C
   20 IMK = I - 4
        IMKPJ = IMK + 1
        WORK(1) = A(IMKPJ)
        IMKPJ = IMK + 2
        WORK(2) = A(IMKPJ)
        IMKPJ = IMK + 3
        WORK(3) = A(IMKPJ)
        IMKPJ = IMK + 4
        WORK(4) = A(IMKPJ)
      IF (IDERIV.EQ.0) GO TO 60
      J = 1
        KMJ = 3
        FKMJ = KMJ
        DO 40 JJ=1,KMJ
          IHI = I + JJ
          IHMKMJ = IHI - KMJ
          WORK(JJ) = (WORK(JJ+1)-WORK(JJ))/(T(IHI)-T(IHMKMJ))*FKMJ
   40   CONTINUE
C
C *** COMPUTE VALUE AT *X* IN (T(I),(T(I+1)) OF IDERIV-TH DERIVATIVE,
C     GIVEN ITS RELEVANT B-SPLINE COEFF. IN AJ(1),...,AJ(K-IDERIV).
   60 IP1 = I + 1
      J1 = 5
      J2 = 9
      DO 70 J=1, 4 - IDERIV
        IPJ = I + J
        WORK(J1) = T(IPJ) - X
        IP1MJ = IP1 - J
        WORK(J2) = X - T(IP1MJ)
        J1 = J1 + 1
        J2 = J2 + 1
   70 CONTINUE
      IDERP1 = IDERIV + 1
      DO 90 J=IDERP1,3
        KMJ = 4 - J
        ILO = KMJ
        DO 80 JJ=1,KMJ
          WORK(JJ) = (WORK(JJ+1)*WORK(8+ILO)+WORK(JJ)
     1              *WORK(4+JJ))/(WORK(8+ILO)+WORK(4+JJ))
          ILO = ILO - 1
   80   CONTINUE
   90 CONTINUE
  100 ECH_DBVALU = WORK(1)
      RETURN
C
  140 CONTINUE
      CALL PDA_XERMSG ('SLATEC', 'PDA_DBVALU',
     +   'A LEFT LIMITING VALUE CANNOT BE OBTAINED AT T(K)', 2,
     +   1, STATUS)
      RETURN
      END
