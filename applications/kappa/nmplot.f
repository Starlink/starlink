      SUBROUTINE NMPLOT( VA, VB, NEL, AMIN, AMAX, NBIN, NITER, SIGLIM,
     :                   MINPIX, NDFA, NDFB, PLOT, TTL, TTLX, TTLY, 
     :                   MINTIC, MAJTIC, OUTTIC, PIC0, NSUM, ASUM,
     :                   BSUM, B2SUM, VARLIM, SLOPE, OFFSET, STATUS )
 
*+
*  Name:
*     NMPLOT

*  Purpose:
*     To normalize one vector to another similar vector by plotting
*     the intensities in each vector against each other.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NMPLOT( VA, VB, NEL, AMIN, AMAX, NBIN, NITER, SIGLIM,
*    :             MINPIX, NDF1, NDF2, PLOT, TTL, TTLX, TTLY, MINTIC,
*    :             MAJTIC, OUTTIC, PIC0, NSUM, ASUM, BSUM, B2SUM,
*    :             VARLIM, SLOPE, OFFSET, STATUS )


*  Description:
*     Intensities which are valid in each input vector and lie within
*     the data range to be used in vector A are binned according to the
*     intensity in vector A. A mean and standard deviation for the B
*     intensities are found for each bin. A straight line is fitted to
*     this binned data to determine the slope and intercept, from which
*     the B vector may be normalized to the A vector. Iterations are
*     performed to reject bad data by repeating the binning and line
*     fitting procedure, rejecting pixels whose B intensities deviate
*     by more than a specified number of standard deviations from the
*     line fitted in the previous iteration.
*
*     After the final iteration, a plot is produced showing the bin 
*     centres and error bars, and the best fitting straight line.


*  Arguments:
*     VA( NEL ) = REAL (Given)
*        First input vector, to which the second input vector is 
*        normalized.
*     VB( NEL) = REAL (Given)
*        Second input vector...the one to be normalized.
*     NEL = REAL (Given)
*        No. of elements in the input vectors.
*     AMIN = REAL (Given)
*        Upper limit on data values to use in vector A.
*     AMAX = REAL (Given)
*        Lower limit on data values to use in vector A.
*     NBIN = INTEGER (Given)
*        The nunber of bins to use in the fit.
*     NITER = INTEGER (Given)
*        The number of data rejection iterations to perform.
*     SIGLIM = REAL (Given)
*        The rejection threshold in standard deviations for aberrant
*        data values.
*     MINPIX = INTEGER (Given)
*        The minimum number of data values required before a bin is
*        used.
*     NDFA = INTEGER (Given)
*        The identifier for the NDF from which the first vector was 
*        taken. 
*     NDFB = INTEGER (Given)
*        The identifier for the NDF from which the second vector was 
*        taken. 
*     PLOT = LOGICAL (Given)
*        Set to .true. if a plot is required. 
*     TTL = CHARACTER (Given)
*        A title for the plot.
*     TTLX = CHARACTER (Given)
*        A title for the X axis of the plot.
*     TTLY = CHARACTER (Given)
*        A title for the Y axis of the plot.
*     MINTIC( 2 ) = REAL (Given)
*        The number of minor tick marks between each major tick mark
*        for the x and y axes.  A negative value forces the graphics
*        package to compute appropriate values.
*     MAJTIC( 2 ) = REAL (Given)
*        The parameter controlling the numbers of major tick marks
*        for the x and y axes.  (Number used is between MAJTIC+2 and
*        5*MAJTIC/2+4.) A negative value forces the graphics package
*        to compute appropriate values.
*     OUTTIC = LOGICAL (Given)
*        If true the axis tick marks are drawn outside the box.
*     PIC0 = INTEGER (Given)
*        The AGI identifier for the current picture.
*     NSUM( NBIN ) = INTEGER (Given and Returned)
*        Work space.
*     ASUM( NBIN ) = REAL (Given and Returned)
*        Work space.
*     BSUM( NBIN ) = REAL (Given and Returned)
*        Work space.
*     B2SUM( NBIN ) = DOUBLE (Given and Returned)
*        Work space.
*     VARLIM( NBIN ) = REAL (Given and Returned)
*        Work space.
*     SLOPE = REAL (Returned)
*        The slope of the line fitted in the expression B=SLOPE*A+OFFSET.
*     OFFSET = REAL (Returned)
*        The constant C in B=SLOPE*A+OFFSET.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R.F. Warren_Smith (STARLINK)
*     DSB: D.S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JUN-1990 (DSB):
*        Original version (based on EDRS routine NMPLOT).
*     22-JUN-1990 (DSB):
*        Graphics added.
*     1990 Oct 4 (MJC):
*        Added extra arguments MINTIC, MAJTIC and OUTTIC to be passed
*        onto the  NCRBCK routine. Removed tabs.  Corrected the variance
*        equations.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      
*  Type Definitions:

      IMPLICIT NONE              ! No implicit typing


*  Global Constants:

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants


*  Arguments Given:

      INTEGER     NEL            ! Size of input vectors
      REAL        VA( NEL )      ! Input vector A
      REAL        VB( NEL )      ! Input vector B
      REAL        AMIN           ! Min. vector A data value to use
      REAL        AMAX           ! Max. vector A data value to use
      INTEGER     NBIN           ! No. of bins in fit
      INTEGER     NITER          ! No. of rejection iterations
      REAL        SIGLIM         ! Rejection threshold
      REAL        MAJTIC( 2 )    ! Parameters controlling the numbers of
                                 ! major tick marks along x and y axes
                                 ! respectively
      INTEGER     MINPIX         ! Min. no. of good pixels in a bin
      REAL        MINTIC( 2 )    ! Numbers of minor tick marks along x
                                 ! and y axes respectively
      INTEGER     NDFA           ! NDF id for first input vector
      INTEGER     NDFB           ! NDF id for second input vector
      LOGICAL     OUTTIC         ! True if Axis tick marks are to be
                                 ! placed outside the box instead of
                                 ! inside
      LOGICAL     PLOT           ! True if plot required
      CHARACTER*(*)  TTL         ! Title for plot
      CHARACTER*(*)  TTLX        ! Title for X axis of plot
      CHARACTER*(*)  TTLY        ! Title for Y axis of plot
      INTEGER     PIC0           ! AGI identifier of current picture


*  Arguments Given and Returned:

      INTEGER           NSUM( NBIN )  ! Work space
      REAL              ASUM( NBIN )  ! Work space
      REAL              BSUM( NBIN )  ! Work space
      DOUBLE PRECISION  B2SUM( NBIN ) ! Work space
      REAL              VARLIM( NBIN )! Work space


*  Arguments Returned:

      REAL              SLOPE    ! Gradient of fit; B=SLOPE*A+OFFSET
      REAL              OFFSET   ! Offset of fit


*  Status:

      INTEGER STATUS             ! Global status


*  Local Variables:

      REAL        A              ! Vector A value
      REAL        AAMAX          ! Max. allowed value from vector A
      REAL        AAMIN          ! Min. allowed value from vector A
      DOUBLE PRECISION  ABOT     ! Lowest vector A value actually used
      DOUBLE PRECISION  ATOP     ! Highest vector A value actually used
      REAL        A0             ! Bin no. for zero A value
      REAL        A1             ! No. of bins in unit A value
      REAL        B              ! Vector B value
      REAL        BFIT           ! Expected vector B value
      REAL        DET            ! Denominator of normal equations
      INTEGER     I              ! Loop index
      INTEGER     IBIN           ! Current bin
      INTEGER     ITER           ! Iteration counter
      INTEGER     NDATA          ! No. of non-empty bins
      INTEGER     NEWPIC         ! AGI ID for the DATA picture holding 
                                 ! the plot
      INTEGER     NPIX           ! No. of vector B values used
      REAL        Q0             ! Min. possible variance for a bin
      DOUBLE PRECISION  WT       ! Weight for current bin
      REAL        WTMAX          ! Max. allowed bin weight
      DOUBLE PRECISION  WTSUM    ! Sum of bin weights
      DOUBLE PRECISION  X        ! Mean vector A value over a bin
      REAL        XMAX           ! Upper X axis limit for plot
      REAL        XMIN           ! Lower X axis limit for plot
      DOUBLE PRECISION  XSUM     ! Sum of X values
      DOUBLE PRECISION  XYSUM    ! Sum of X*Y values
      DOUBLE PRECISION  X2SUM    ! Sum of X*X values
      DOUBLE PRECISION  Y        ! Mean vector B value in a bin
      REAL        YMAX           ! Upper Y axis limit for plot
      REAL        YMIN           ! Lower Y axis limit for plot
      DOUBLE PRECISION  YSUM     ! Sum of Y values

*.


*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN


*  Set minimum expected variance for a bin.

      Q0 = 0.0
      SLOPE = 1.0
      OFFSET = 0.0
 

*  Set constants to convert intensity into bins.

      AAMIN = MIN( AMIN, AMAX )
      AAMAX = MAX( AMIN, AMAX )
      A0 = 1.5 - AAMIN * ( REAL( NBIN - 1 ) ) 
     :       / MAX( 1.0E-20, AAMAX - AAMIN  )
      A1 = REAL( NBIN - 1 ) / MAX( 1.0E-20, AAMAX - AAMIN )
 

*  Initialise the variance threshold for each bin.

      DO I = 1, NBIN
         VARLIM( I ) = VAL__MAXR
      END DO
 
 
*  Perform NITER iterations.

      DO ITER = 0, NITER


*  Initialise the bins
 
         DO I = 1, NBIN
            NSUM( I ) = 0
            ASUM( I ) = 0.0
            BSUM( I ) = 0.0
            B2SUM( I ) = 0.0D0
         END DO
 

*  Scan through the input vectors.
 
         DO I = 1, NEL
 

*  Use pixels which are good in both vectors.
 
            IF ( VA( I ) .NE. VAL__BADR .AND.
     :          VB( I ) .NE. VAL__BADR ) THEN
 

*  Check if the A intensity is in the required data range.

               A = VA( I )
               IF ( A .GE. AAMIN .AND. A .LE. AAMAX ) THEN
 

*  Calculate the bin number and form sums for this bin

                  IBIN = INT( A0 + A1 * A )
                  B = VB( I )
                  BFIT = A * SLOPE + OFFSET
 
                  IF ( ( B - BFIT )**2 .LE. VARLIM( IBIN ) ) THEN
                     NSUM( IBIN ) = NSUM( IBIN ) + 1
                     BSUM( IBIN ) = BSUM( IBIN ) + B
                     B2SUM( IBIN ) = B2SUM( IBIN ) + DBLE( B * B )
                     ASUM( IBIN ) = ASUM( IBIN ) + A
                  END IF
 
               END IF
 
            END IF
 
         END DO
 
 
*  Count the total number of pixels used.

         NPIX = 0
 
         DO I = 1, NBIN
            NPIX = NPIX + NSUM( I )
         END DO
 
 
*  Calculate the max. weight to be applied to any bin (this is the
*  number of points per bin if the points are uniformly distributed).

         WTMAX = REAL( NPIX ) / REAL( NBIN )
 

*  Initialise sums for the straight line fit.

         XSUM = 0.0D0
         X2SUM = 0.0D0
         YSUM = 0.0D0
         XYSUM = 0.0D0
         WTSUM = 0.0D0
         ABOT = VAL__MAXD
         ATOP = VAL__MIND
 

*  Scan those bins with at least the minimum number of pixels in.
 
         DO I = 1, NBIN
 
            IF ( NSUM( I ) .GE. MAX( 1, MINPIX ) ) THEN
 

*  Form the weight for this bin.

               WT = DBLE( MIN( WTMAX, REAL( NSUM( I ) ) ) )
 

*  Set the variance threshold in this bin for the next iteration.

               VARLIM( I ) = REAL( DBLE( SIGLIM**2 ) * 
     :                       ( B2SUM( I ) - DBLE( BSUM( I )**2 ) / 
     :                                      DBLE( NSUM( I ) )  ) )
     :                       / MAX( 1.0, REAL( NSUM( I ) - 1 ) )

               VARLIM( I ) = MAX( VARLIM( I ), Q0 * ( SIGLIM**2 ) )
 

*  Form the mean data values in each bin.

               X = DBLE( ASUM( I ) ) / DBLE( NSUM( I ) )
               Y = DBLE( BSUM( I ) ) / DBLE( NSUM( I ) )
 

*  Form weighted sums for fit.

               XSUM = XSUM + X * WT
               X2SUM = X2SUM + X * X * WT
               YSUM = YSUM + Y * WT
               XYSUM = XYSUM + Y * X * WT
               WTSUM = WTSUM + WT
 

*  Note the range of values used.

               ATOP = MAX( ATOP, X )
               ABOT = MIN( ABOT, X )
 
            ELSE
 

*  Set minimum variance for bins with too few points.

               VARLIM( I ) = ( SIGLIM**2 ) * Q0

            END IF
 
         END DO
 

*  If all bins are empty, abort with STATUS = SAI__ERROR

         IF ( ATOP .EQ. VAL__MIND ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('NORMALIZE_EMPTY',
     :        'NORMALIZE: All the bins in the fit are empty.', STATUS )
            GO TO 10
         END IF


*  If normal equations are singular, abort with STATUS = SAI__ERROR

         DET = REAL( WTSUM * X2SUM - XSUM * XSUM )
 
         IF ( ABS( DET ) .LE. VAL__SMLR ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('NORMALIZE_SINGULAR',
     :        'NORMALIZE: Unable to calculate normalization constants.',
     :         STATUS )
            GO TO 10
         END IF


*  Form the straight line parameters (B=SLOPE*A+OFFSET).

         SLOPE = REAL( WTSUM * XYSUM - XSUM * YSUM )
         OFFSET = REAL( X2SUM * YSUM - XSUM * XYSUM )
         SLOPE = SLOPE / DET
         OFFSET = OFFSET / DET
 

*  Print the results of this iteration.
 
         CALL MSG_OUT( 'REPORT', ' ', STATUS )

         CALL NDF_MSG( 'VA', NDFA )
         CALL MSG_SETI( 'ITER', ITER )      
         CALL MSG_SETI( 'NPIX', NPIX )      
         CALL MSG_SETR( 'ABOT', REAL( ABOT ) )
         CALL MSG_SETR( 'ATOP', REAL( ATOP ) )
         CALL MSG_OUT( 'REPORT',
     :    ' Iteration ^ITER used ^NPIX pixels '//
     :    'in the range ^ABOT and ^ATOP in ^VA', STATUS )

         CALL MSG_OUT( 'REPORT', ' ', STATUS )

         CALL MSG_SETR( 'SLOPE', SLOPE )
         CALL MSG_SETR( 'OFFSET', OFFSET )
         CALL NDF_MSG( 'VA', NDFA )
         CALL NDF_MSG( 'VB', NDFB )

         CALL MSG_OUT( 'REPORT',
     :    '   Fit gives:   ^VB = (^SLOPE) * ^VA + (^OFFSET)', STATUS )

         CALL MSG_OUT( 'REPORT', ' ', STATUS )
 

*  On the final iteration, plot the fit if required.
 
         IF ( ITER .EQ. NITER .AND. PLOT ) THEN
 

*  Remove bins with no data in and form the values to plot.
*  Also calculate the maximum and minimum extent of the plot in x and y
*  taking into account the length of the error bars.

            NDATA = 0
 
            XMAX = VAL__MINR
            XMIN = VAL__MAXR
            YMAX = VAL__MINR
            YMIN = VAL__MAXR

            DO I = 1, NBIN
 
               IF ( NSUM( I ).GE.MINPIX ) THEN

                  NDATA = NDATA + 1
                  ASUM( NDATA ) = ASUM( I ) / REAL( NSUM( I ) )
                  BSUM( NDATA ) = BSUM( I ) / REAL( NSUM( I ) )
                  VARLIM( NDATA ) = SQRT( MAX( 0.0,
     :                 ( REAL( B2SUM( I ) ) - ( BSUM( I )**2 ) * 
     :                   REAL( NSUM( I ) ) ) /
     :                   MAX( 1.0, REAL( NSUM( I ) - 1 ) ) ) )

                  XMAX = MAX( XMAX, ASUM( NDATA ) )
                  XMIN = MIN( XMIN, ASUM( NDATA ) )
                  YMAX = MAX( YMAX, BSUM( NDATA ) + VARLIM( NDATA ) )
                  YMIN = MIN( YMIN, BSUM( NDATA ) - VARLIM( NDATA ) )

               END IF
 
            END DO
 
 
*  Plot the axes. A new picture is stored in the AGI database 
*  corresponding to the data space of the plot.

            CALL NCRBCK( XMIN, XMAX, YMIN, YMAX, TTL, TTLX, TTLY,
     :                   MINTIC, MAJTIC, OUTTIC, .FALSE.,
     :                   'KAPPA - Normalize', PIC0, NEWPIC, STATUS )


*  Mark the bin centres and error bars.

            CALL DREBAR( ASUM, BSUM, VARLIM, NDATA, STATUS )


*  Draw a straight line to indicate the best fit.

            ASUM( 2 ) = ASUM( NDATA )
            BSUM( 1 ) = SLOPE * ASUM( 1 ) + OFFSET
            BSUM( 2 ) = SLOPE * ASUM( 2 ) + OFFSET
            CALL GPL( 2, ASUM, BSUM )

         END IF
 

*  Return for next iteration.

      END DO
 
   10 CONTINUE

      END
