*+  APADSB - bins up pixels within a specified circle

      SUBROUTINE APADSB ( INARR, DIM1, DIM2, XCEN, YCEN, PXDIAM, NUMPIX,
     :                    OLNOIZ, TOTAL, MEAN, NWNOIZ, STATUS )
*
*    Description :
*
*     This routine takes an input array and bins up all the pixels
*     that lie within a user-specified circle.
*     Returned are the pixel standard deviation before binning, the
*     integrated value over the aperture, and the calculated mean
*     signal level and noise after binning.
*
*    Invocation :
*
*     CALL APADSB( INARR, DIM1, DIM2, XCEN, YCEN, PXDIAM, NUMPIX,
*                  OLNOIZ, TOTAL, MEAN, NWNOIZ, STATUS )
*
*    Arguments :
*
*     INARR( DIM1, DIM2 ) = REAL( READ )
*         Array containing input data
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     XCEN = REAL( READ )
*         X co-ordinate of the centre of the circle
*     YCEN = REAL( READ )
*         Y co-ordinate of the centre of the circle
*     PXDIAM = REAL( READ )
*         Diameter of circle in pixels
*     NUMPIX = INTEGER( WRITE )
*         Number of pixels included in the circle
*     OLNOIZ = REAL( WRITE )
*         Noise found before binning
*     TOTAL = REAL( WRITE )
*         Sum of pixel values within the circle
*     MEAN = REAL( WRITE )
*         Mean of pixel values within the circle
*     NWNOIZ = REAL( WRITE )
*         Noise found after binning
*     STATUS = INTEGER( READ )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Constructs a box around the outside of the circle and for all
*     valid pixels within this box and on the array, the distance of
*     the pixel from the circle centre is found. If this distance is
*     less than or equal to the radius of the circle, the point and
*     its intensity is included in the subtotals.
*     On exit from the loop, the mean, old noise and new noise are
*     formed. The old noise is formed from the equation for standard
*     deviation (sigma), and is formed after only one run through the
*     array by an agebraic manipulation of the equation. The value for
*     the new noise is formed by reducing the old noise by a factor
*     equal to the square root of the number of pixels added.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     22-10-1985 : First implementation (REVA::MJM)
*     17-01-1986 : Removed error which caused arithmetic divide by
*                : zero if only one pixel binned up (REVA::MJM)
*     1986 Aug 13: Renamed from APERADDSUB, completed the prologue and
*                  nearly conformed to Starlink programming standards
*                  (RL.STAR::CUR).
*     1986 Sep 3 : Renamed parameters section to arguments, applied
*                  bad-pixel handling (RL.STAR::CUR).
*     1988 Apr 30: More error checking (RL.STAR::CUR).
*     1989 Jul 27: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'           ! SSE global definitions
      INCLUDE 'PRM_PAR'           ! PRIMDAT public constants

*    Import :

      INTEGER
     :     DIM1, DIM2

      REAL
     :     INARR( DIM1, DIM2 ),
     :     XCEN,
     :     YCEN,
     :     PXDIAM

*    Export :

      INTEGER
     :     NUMPIX

      REAL
     :     TOTAL,
     :     MEAN,
     :     OLNOIZ,
     :     NWNOIZ

*    Status :

      INTEGER  STATUS

*    Local variables :

      INTEGER
     :     BLXPIX,           ! x pixel at bottom left of box
     :     BLYPIX,           ! y   "    "    "     "   "  "
     :     TRXPIX,           ! x   "    " top right of box
     :     TRYPIX,           ! y   "    "  "    "    "  "
     :     I, J              ! counters

      REAL
     :     RADIUS,           ! radius of circle
     :     XOFFS,            ! squared x distance of pixel centre from
                             ! circle's centre
     :     YOFFS,            ! squared y distance of pixel centre from
                             ! circle's centre
     :     DISTNC,           ! distance of current pixel from centre
     :     TOTSQ,            ! sum of intensities squared
     :     VARINC,           ! variance of points in circle
     :     STDDEV            ! std.dev.  "    "    "    "

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    start by working out the circle radius

      RADIUS = PXDIAM / 2.0

*    now work out the bottom left and top right co-ordinates of
*    a square box that fully encompasses the circle. The input
*    co-ordinates (real) refer to the top right corner of any
*    given pixel (integer) - i.e. the co-ordinates of the pixel
*    1,1 are 0.0,0.0 at the bottom left, and 1.0,1.0 at the top right.
*    Thus the pixel in which the co-ordinate XCEN,YCEN sits is
*    NINT(XCEN+1,YCEN+1).

*    Derive the bottom-left and top-right pixels which are to be
*    included in the box

      BLXPIX = NINT( XCEN - RADIUS ) - 1
      BLYPIX = NINT( YCEN - RADIUS ) - 1
      TRXPIX = NINT( XCEN + RADIUS ) + 1
      TRYPIX = NINT( YCEN + RADIUS ) + 1

*    make sure that the box does not run off the array

      BLXPIX = MAX( 1, BLXPIX )
      BLYPIX = MAX( 1, BLYPIX )
      TRXPIX = MIN( TRXPIX, DIM1 )
      TRYPIX = MIN( TRYPIX, DIM2 )

*    initialise statistics variables

      NUMPIX = 0
      TOTAL = 0.0
      TOTSQ = 0.0

*    now loop round all the pixels in the box

      DO  J = BLYPIX, TRYPIX
         YOFFS = ( ABS( REAL( J ) - 0.5 - YCEN ) ) ** 2
         DO  I = BLXPIX, TRXPIX

*          test for valid pixels

            IF ( INARR( I, J ) .NE. VAL__BADR ) THEN

*             work out distance of current pixel from circle centre

               XOFFS = ( ABS( REAL( I ) - 0.5 - XCEN ) ) ** 2
               DISTNC = SQRT( MAX( XOFFS + YOFFS, 0.0 ) )

*             test for inclusion of current pixel

               IF ( DISTNC .LE. RADIUS ) THEN

*                include this point

                  NUMPIX  = NUMPIX + 1 
                  TOTAL   = TOTAL + INARR( I, J )
                  TOTSQ = TOTSQ + ( INARR( I, J )**2 )

               END IF

*          end of bad-pixel-handling condition
            END IF

         END DO
      END DO               

*    now form returned quantities - the standard deviation (sigma)
*    is formed from the equation for the variance (sigma squared) :
*                                               _ 
*     variance =  Sum over n pixels ( x   -   x )**2
*                                        i
*                   ----------------------------------
*
*                               ( n - 1 )
*
*    this can be algebraically manipulated to the following :
*                                       _                _
*     variance =  Sum(( x )**2)  -  2.x.Sum( x )  +  n.x**2
*                          i                    i
*                   -----------------------------------------
*                  
*                                  ( n - 1 )
*
*    and then standard deviation equals sqrt( variance )
*

*    find the mean and variance - check for situation where only one
*    pixel was included - could give divide by zero

      IF( NUMPIX .LE. 1 ) THEN
         VARINC = VAL__BADR
         MEAN = VAL__BADR
         IF ( NUMPIX .EQ. 1 ) MEAN = TOTAL
      ELSE
         MEAN = TOTAL / REAL( NUMPIX )
         VARINC = ( TOTSQ - 2*MEAN*TOTAL + REAL( NUMPIX )*MEAN**2 )
         VARINC = VARINC / ( REAL( NUMPIX ) - 1.0 )
      END IF

*    the standard deviation is sqrt( variance )

      IF ( VARINC .GE. 0.0 ) THEN
         STDDEV  = SQRT( VARINC )
      ELSE
         STDDEV = VAL__BADR
      END IF

*    set the value for the old noise equal to this, and the value
*    for the new noise as sqrt(NUMPIX) times lower - check for zero
*    pixels again

      IF( NUMPIX .LT. 1 ) THEN
         OLNOIZ = VAL__BADR
         NWNOIZ = VAL__BADR
      ELSE
         OLNOIZ = STDDEV
         NWNOIZ = STDDEV / ( SQRT( REAL( NUMPIX ) ) )
      END IF

 999  CONTINUE

*    end

      END
