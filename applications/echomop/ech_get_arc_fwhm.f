      SUBROUTINE ECH_GET_ARC_FWHM(
     :           INIM,
     :           NNX,
     :           NNY,
     :           N_ORDERS,
     :           POLYNOMIALS,
     :           LINE_WIDTH
     :          )
*+
*  Name:
*     ECHOMOP - ECH_GET_ARC_FWHM

*  Purpose:
*     Calculates the average FWHM of all arc lines

*-

*  Type Defintions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_INIT_RDCTN.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_WORKSPACE.INC'

*  Arguments:
      INTEGER NNX
      INTEGER NNY
      REAL INIM( NNX, NNY )
      INTEGER N_ORDERS
      DOUBLE PRECISION POLYNOMIALS( MAX_POLY, NUM_ORDERS )
      INTEGER LINE_WIDTH

*  Local Constants:
      REAL FLAG
      PARAMETER ( FLAG = -1.0E20 )

*  Local Variables:
      REAL ARC_PROFILE( -100 : 100 )
      REAL XAXIS( 201 )
      REAL X_AT_MAX
      REAL DUM_MAX
      REAL LINE_PEAK
      REAL MEDIAN
      REAL XH
      REAL YH
      REAL XM
      REAL YM
      REAL PEAK_VALUE

      INTEGER OPTIONS
      INTEGER STATUS
      INTEGER I
      INTEGER II
      INTEGER IIX
      INTEGER ISTAT
      INTEGER IORD
      INTEGER Y_DELTA
      INTEGER NEAREST_Y
      INTEGER HMAX_HI_AT
      INTEGER HMAX_LOW_AT

      INTEGER IORD_LOW
      INTEGER IORD_HI

      CHARACTER*80 TITLE
*.
      IORD = N_ORDERS / 2
      IORD_LOW = 2
      IORD_HI = N_ORDERS - 1
      IF ( N_ORDERS .LT. 3 ) THEN
         IORD_LOW = 1
         IORD_HI = 1
      END IF

      DO iord = iord_low, iord_hi
        IF ( polynomials ( 1, iord ) .GT. flag/2.0 ) THEN
          CALL ECH_CALC_TRACE ( nnx, polynomials(1,iord) )
          DO i = 1, nnx
             work_real1 ( i ) = 0.0
             work_real2 ( i ) = 0.0
          END DO
          DO i = 1, nnx
             nearest_y  = INT ( y_coord ( i ) + 0.5 )
             work_real2 ( i ) = inim ( i, nearest_y )
             DO y_delta = -1, 1
               IF ( nearest_y+y_delta .GT. 0 .AND.
     :              nearest_y+y_delta .LE. nny ) THEN
                 work_real1 ( i ) = work_real1 ( i ) +
     :                           inim ( i, nearest_y+y_delta )
               END IF
             END DO
             work_real1 ( i ) = work_real1 ( i ) / 3.0
          END DO
C
        END IF
       END DO
C
C
          DO i = 10, nnx-10
           IF ( work_real1 ( i ) .GT. work_real1 ( i-1 ) .AND.
     :        work_real1 ( i-1 ) .GT. work_real1 ( i-2 ) .AND.
     :        work_real1 ( i ) .GT. work_real1 ( i+1 ) .AND.
     :        work_real1 ( i+1 ) .GT. work_real1 ( i+2 )
     :              ) THEN
             line_peak = work_real2 ( i )
             x_at_max = 10.0
             CALL GEN_CENTROID ( work_real2(i-9),
     :                           19, 1.0,
     :                           x_at_max, dum_max, istat )
             IF ( istat .NE. 0 )
     :               CALL ECH_REPORT(0,'bad return in profile ')
             x_at_max = x_at_max - 10.0
             DO ii = -90, -1
                arc_profile ( ii ) = arc_profile ( ii ) +
     :               work_real2 ( i+(ii-5+INT(x_at_max*10))/10 ) /
     :               line_peak
             END DO
             DO ii = 0, 90
                arc_profile ( ii ) = arc_profile ( ii ) +
     :               work_real2 ( i+(ii+5+INT(x_at_max*10))/10 ) /
     :               line_peak
             END DO
           END IF
          END DO
          peak_value = arc_profile( 0 )
          DO ii = -50, 50
             IF ( arc_profile ( ii ) .GT. peak_value ) THEN
                peak_value = arc_profile ( ii )
             END IF
          END DO
          CALL ECH_MEAN_MEDIAN( 101, ARC_PROFILE( -50 ), .TRUE., MEDIAN,
     :         .FALSE., STATUS )
          peak_value = arc_profile ( 0 ) - median
          hmax_low_at = 0
          hmax_hi_at = 0
          DO WHILE ( arc_profile ( hmax_low_at ) - median
     :                                    .GT. peak_value/2 )
             hmax_low_at = hmax_low_at - 1
          END DO
          DO WHILE ( arc_profile ( hmax_hi_at ) - median
     :                                    .GT. peak_value/2 )
             hmax_hi_at = hmax_hi_at + 1
          END DO
          line_width =  ( hmax_hi_at - hmax_low_at + 1 ) / 10
C
          ym=0.0
          yh=(peak_value+median)*1.1
          xm=-10.
          xh=10.
          do iix=1,201
               xaxis(iix) = float(iix-101) / 10.0
          end do
          WRITE ( TITLE,'( A, I3 )') 'Arc line FWHM =', line_width
          options = 0
          CALL ECH_PLOT_GRAPH ( 201, xaxis, arc_profile(-100),
     :                          xm, xh, ym, yh,
     :                          'Pixels', 'Sum', title,
     :                          0.0, 0.0, options, 'BINS',
     :                          status )

      END
