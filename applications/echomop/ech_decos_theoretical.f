      SUBROUTINE ECH_DECOS_THEORETICAL(
     :           NX,
     :           IORD,
     :           PIXEL_COUNT,
     :           MCOUNT,
     :           MEAN, SIGMA,
     :           MEAN_ENERGY,
     :           PROB,
     :           DATA,
     :           DATA_SRT,
     :           XAXIS,
     :           YAXIS,
     :           GAUSSIAN,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_DECOS_THEORETICAL

*  Purpose:
*     Calculate theoretical CDF for automatic CR locator.

*  Description:
*     This routine calculates the expected CDF of a theoretical gaussian
*     distribution using the observed mean and sigma from the order being
*     processed. The mean and sigma apply to the set of factors by which
*     pixels deviate from their expected values. Eg. if a pixel DN=105
*     and its expected intensity is 100, then it would contribute a value
*     of 1.05 to the distribution. Thus the expected mean value will usually
*     be unity.

*  Invocation:
*     CALL ECH_DECOS_THEORETICAL(
*     :    NX,
*     :    IORD,
*     :    PIXEL_COUNT,
*     :    MCOUNT,
*     :    MEAN, SIGMA,
*     :    MEAN_ENERGY,
*     :    PROB,
*     :    DATA,
*     :    DATA_SRT,
*     :    XAXIS,
*     :    YAXIS,
*     :    GAUSSIAN,
*     :    STATUS
*     :    )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     IORD = INTEGER (Given)
*        Order number being processed.
*     PIXEL_COUNT = INTEGER (Given)
*        Count of pixels in CDF.
*     MCOUNT = INTEGER (Given)
*        Samples contributing to mean.
*     MEAN = REAL (Given)
*        Observed mean.
*     SIGMA = REAL (Given)
*        Observed sigma.
*     MEAN_ENERGY = REAL (Given)
*        Mean pixel intensity over order.
*     PROB = DOUBLE (Returned)
*        Probability of CDF fit.
*     DATA = REAL (Temporary Workspace)
*        Ratios of observed/predicted energy.
*     DATA_SRT = REAL (Temporary Workspace)
*        Sorted Ratios of observed/predicted energy.
*     YAXIS = REAL (Temporary Workspace)
*        Theoretical CDF values.
*     XAXIS = REAL (Temporary Workspace)
*        Theoretical data points.
*     GAUSSIAN = REAL (Temporary Workspace)
*        Theoretical gaussian values.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     If mean energy (ie counts per pixel ) less than 4 then
*        Count number of samples in data set with energy of  0,1,2,3,4....
*        Calculate theoretical values using a poissonian distribution, and
*               difference between observed/theoretical
*     Else
*        Sort data set into ascending order
*        Calculate a gaussian distribution using observed characteristics
*        Calculate cumulative distribution function using gaussian
*        Normalise CDF to peak value of 1.0
*        Loop thru ratios in data set
*           Take observed data value as reference
*           If past start of set of values (n>2) then
*              Set starting index to last one used
*           Else
*              Start at beginning of set
*           Endif
*           Loop thru theoretical data set until we reach a point > reference
*              Increment index into theoretical data array
*           End loop
*           If a theoretical entry is present near the reference value then
*              Save its index (next time round we can start searching here
*                              instead of at the beginning of the theoretical
*                              data array )
*              Calculate fraction of theoretical bin corresponding to
*                        reference value position
*              Calculate theoretical value corresponding to reference
*              Calculate the absolute difference between the two distributions
*                        at the reference point
*              If difference is greatest so far then
*                  Remember it, and calculate its probability of chance occurence
*              Endif
*           Else default is to search whole theoretical array
*           Endif
*        End loop
*     Endif

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'

*  Arguments Given:
      INTEGER nx
      INTEGER iord
      DOUBLE PRECISION prob

*  Arguments Returned:
      INTEGER mcount
      REAL mean
      REAL sigma
      REAL mean_energy
      INTEGER pixel_count

*  Workspace:
      REAL DATA( NX * MAX_SLICE_PIXELS )
*          ! Ratios of observed/predicted energy.
      REAL DATA_SRT( NX * MAX_SLICE_PIXELS )
*          ! Sorted Ratios of observed/predicted energy.
      REAL YAXIS( NX * MAX_SLICE_PIXELS )
*          ! Theoretical CDF values.
      REAL XAXIS( NX * MAX_SLICE_PIXELS )
*          ! Theoretical data points.
      REAL GAUSSIAN( NX * MAX_SLICE_PIXELS )
*          ! Theoretical gaussian values.

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAX_POISSON_BINS
      PARAMETER ( MAX_POISSON_BINS = 20 )

*  Local Variables:
      REAL PDIFF( 0:MAX_POISSON_BINS )
      REAL XPAXIS( 0:MAX_POISSON_BINS )
      REAL THEO_POS( 0:MAX_POISSON_BINS )
      REAL OBS_POS( 0:MAX_POISSON_BINS )
      REAL THEO_PLOT( 0:MAX_POISSON_BINS )
      REAL PMEAN
      REAL AFRACTION
      REAL XH
      REAL XM
      REAL YH
      REAL YM
      REAL MAX
      REAL PROB4
      REAL DIST_MAX
      REAL XV
      REAL GTOTAL
      REAL REFERENCE
      REAL VALUE
      REAL DISTANCE
      REAL SQPC
      REAL ANARG

      INTEGER POISS_COUNT( 0 : MAX_POISSON_BINS )
      INTEGER INDEX
      INTEGER OPTIONS
      INTEGER LAST_THEORETICAL
      INTEGER IX
      INTEGER NCHAR1
      INTEGER NCHAR2

      CHARACTER*32 REF_STR1
      CHARACTER*32 REF_STR2

*  Functions CALLED:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      REAL POISSON
      REAL PROBKS
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  If mean energy (ie counts per pixel ) less than 4 then
      IF ( mean_energy .LT. 4.0 ) THEN

*        Count number of samples in data set with energy of  0,1,2,3,4....
            pmean = mean_energy * FLOAT( pixel_count ) /
     :              FLOAT( mcount )
            DO ix = 0, max_poisson_bins
               poiss_count ( ix ) = 0
            END DO
            DO ix = 1, pixel_count
               poiss_count( INT( MAX( 0., data( ix ) ) ) ) =
     :             poiss_count( INT( MAX( 0., data( ix ) ) ) ) + 1
            END DO
            mcount = 0
            DO ix = 1, max_poisson_bins
               mcount = mcount + poiss_count( ix )
            END DO

*        Calculate theoretical values using a poissonian distribution, and
*        difference between observed/theoretical.
            DO ix = 0, 12
               xpaxis( ix ) = FLOAT( ix )
               theo_pos( ix ) = POISSON( pmean, FLOAT( ix ) )
               obs_pos( ix ) = FLOAT( poiss_count( ix ) ) /
     :               FLOAT( pixel_count )
               pdiff( ix ) = ABS( obs_pos( ix ) - theo_pos( ix ) )
               theo_plot( ix ) = theo_pos( ix )
               theo_pos( ix ) = theo_pos( ix ) * FLOAT( pixel_count )
            END DO
            prob = 0.0
            DO ix = 1, 12
               IF ( prob .LT. ABS( pdiff( ix ) ) )
     :            prob = ABS( pdiff( ix ) )
            END DO

            options = 0
            yh=1.0
            ym=0.0
            xh = 10.0
            xm = -1.0
            CALL ECH_PLOT_GRAPH( 13, xpaxis, theo_plot, xm, xh, ym, yh,
     :           'Count', 'Frequency', 'Poissonian', 0.0, 0.0,
     :           options, 'LINES', status )

            options = grph_overlay
            CALL ECH_PLOT_GRAPH( 13, xpaxis, obs_pos, xm, xh, ym, yh,
     :           'Count', 'Frequency', 'Poissonian', 0.0, 0.0,
     :           options, 'LINES', status )
            CALL CHR_ITOC( IORD, REF_STR1, NCHAR1 )
            CALL CHR_RTOC( PROB, REF_STR2, NCHAR2 )
            REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :            ': Maximum delta prob. from poissonian: ' //
     :            REF_STR2( :NCHAR2 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            PROB = 1.0 - PROB

      ELSE

*        Sort data set into ascending order
            DO ix = 1, pixel_count
               data_srt( ix ) = data( ix )
            END DO
            CALL SORT1( data_srt, pixel_count )

*        Calculate a gaussian distribution using observed characteristics
            yh=1.0
            ym=0.0
            xh = mean + 3 * sigma
            xm = mean - 3 * sigma
            gtotal = float( pixel_count ) * 6.0 * sigma / 50.0
            DO ix = 1, pixel_count
               xv = xm + (float(ix)-0.5)*6*sigma/pixel_count
               xaxis ( ix ) = xv
               yaxis ( ix ) = FLOAT ( ix ) / FLOAT ( pixel_count )
               gaussian ( ix ) = 1.0/SQRT(2.0*3.1415926535*sigma)
     :                          * EXP ( -0.5*((xv-mean)/sigma)**2.)
     :                          * gtotal
            END DO

*        Calculate cumulative distribution function using gaussian
            DO ix = 2, pixel_count
               gaussian ( ix ) = gaussian ( ix-1 ) +
     :                                  gaussian ( ix )
            END DO

*        Normalise CDF to peak value of 1.0
            DO ix = 1, pixel_count
               gaussian( ix ) = gaussian( ix ) /
     :               gaussian( pixel_count )
            END DO

*        Loop thru ratios in data set
            dist_max = 0.0
            prob = 0.0
            prob4 = 0.0
            sqpc = SQRT ( FLOAT ( pixel_count ) )
            last_theoretical = 1
            DO ix = 2, pixel_count

*           Take observed data value as reference
               reference = data_srt ( ix )

*           If past start of set of values (n>2) then
*              Set starting index to last one used
*           Else
*              Start at beginning of set
*           Endif
               IF ( ix .GT. 2 ) THEN
                  index = last_theoretical

               ELSE
                  index = 1
               END IF

*           Loop thru theoretical data set until we reach a point > reference
               DO WHILE ( xaxis ( index ) .LE. reference .AND.
     :                    index .LT. pixel_count  )

*              Increment index into theoretical data array
                  index = index + 1
               END DO

*           If a theoretical entry is present near the reference value then
               IF ( index .GT. 1 .AND. index .LT. pixel_count ) THEN

*              Save its index (next time round we can start searching here
*                              instead of at the beginning of the theoretical
*                              data array )
                  last_theoretical = index - 1

*              Calculate fraction of theoretical bin corresponding to
*              reference value position
*              We have located the nearest point in the theoretical distribution
*              for which a value was calculated. We now want to interpolate
*              to find the theoretical value at the point where our
*              reference (observed) value lies. So we work out what
*              fraction of a bin (theoretical) further along the reference
*              value lies.
                  afraction = ( xaxis ( index ) - reference )   /
     :                         ( xaxis ( index ) - xaxis ( index-1 ) )

*              Calculate theoretical value corresponding to reference
                  value = gaussian ( index-1 ) +
     :                        ( gaussian ( index ) -
     :                          gaussian ( index-1 ) ) * afraction

*              Calculate the absolute difference between the two distributions
*              at the reference point
                  distance = ABS ( value - yaxis ( ix ) )

*              If difference is greatest so far then
                  IF ( distance .GT. dist_max ) THEN

*                  Remember it, and calculate its probability of chance occurence
                      dist_max = distance
                      anarg=sqpc*dist_max
                      prob4 = PROBKS ( anarg )
                      prob = prob4
                  END IF

*           Else default is to search whole theoretical array
               ELSE
                  last_theoretical = 1
               END IF
          END DO
       END IF

       END
