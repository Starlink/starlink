*+  APERADDSUB - bins up pixels within a specified circle

      SUBROUTINE APERADDSUB ( INARRAY, IDIMS1, IDIMS2, XCEN, YCEN,
     :                        PIXDIAM, NUMPIX, OLDNOISE, TOTAL,
     :                        MEAN, NEWNOISE, STATUS )

*    Description :
*
*     This routine takes an input array and bins up all the pixels
*     that lie within a user specified circle.
*     Returned are the pixel standard deviation before binning, the
*     integrated value over the aperture, and the calculated mean
*     signal level and noise after binning.
*
*    Invocation :
*
*     CALL APERADDSUB( INARRAY, IDIMS, XCEN, YCEN, PIXDIAM,
*                      NUMPIX, OLDNOISE, TOTAL, MEAN, NEWNOISE,
*                      STATUS )
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Constructs a box around the outside of the circle and for all
*     pixels within this box and on the array, the distance of the
*     pixel from the circle centre is found. If this distance is
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
*
*    History :
*
*     22-10-1985 : First implementation (REVA::MJM)
*     17-01-1986 : Removed error which caused arithmetic divide by
*                : zero if only one pixel binned up (REVA::MJM)
*     02-11-1987 : added check of value before sqrt (UKTH::CAA)
*     15-AUG-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :     IDIMS1, IDIMS2        ! input array dimensions

      REAL
     :     INARRAY( IDIMS1, IDIMS2 ),  ! input array
     :     XCEN,             ! x coord of circle centre
     :     YCEN,             ! y   "    "    "      "
     :     PIXDIAM           ! circle diameter in pixels

*    Export :

      INTEGER
     :     NUMPIX            ! number of pixels included in circle

      REAL
     :     TOTAL,            ! total intensity in circle
     :     MEAN,             ! mean      "      "    "
     :     OLDNOISE,         ! noise before binning
     :     NEWNOISE          !   "   after     "

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :     IRADIUS,          ! integerised radius
     :     XPIX,             ! pixel containing circle centre x coord
     :     YPIX,             !   "        "        "      "   y   "
     :     BLXPIX,           ! x pixel at bottom left of box
     :     BLYPIX,           ! y   "    "    "     "   "  "
     :     TRXPIX,           ! x   "    " top right of box
     :     TRYPIX,           ! y   "    "  "    "    "  "
     :     I, J              ! counters

      REAL
     :     RADIUS,           ! radius of circle
     :     DISTANCE,         ! distance of current pixel from centre
     :     TOTALSQ,          ! sum of intensities**2
     :     VARIANCE,         ! variance of points in circle
     :     STANDEV,          ! std.dev.  "    "    "    "
     :	   JUNK

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    start by working out the circle radius
      RADIUS  =  PIXDIAM / 2.0

*    now work out the bottom left and top right coordinates of
*    a square box that fully encompasses the circle. The input
*    coordinates (real) refer to the top right corner of any
*    given pixel (integer) - i.e. the coordinates of the pixel
*    1,1 are 0.0,0.0 at bottom left, and 1.0,1.0. Thus the pixel
*    in which the coordinate XCEN,YCEN sits is NINT(XCEN+1,YCEN+1).
      XPIX  =  NINT( XCEN + 1.0 )
      YPIX  =  NINT( YCEN + 1.0 )

*    now the bottom left and top right pixels which are to be
*    included in the box
      IRADIUS =  NINT( RADIUS + 1.0 )
      BLXPIX  =  XPIX - IRADIUS
      BLYPIX  =  YPIX - IRADIUS
      TRXPIX  =  XPIX + IRADIUS
      TRYPIX  =  YPIX + IRADIUS

*    make sure that the box does not run off the array
      BLXPIX  =  MAX( 1, BLXPIX )
      BLYPIX  =  MAX( 1, BLYPIX )
      TRXPIX  =  MIN( TRXPIX, IDIMS1 )
      TRYPIX  =  MIN( TRYPIX, IDIMS2 )

*    initialise statistics variables
      NUMPIX  =  0
      TOTAL   =  0.0
      TOTALSQ =  0.0

*    now loop round all the pixels in the box
      DO  J  =  BLYPIX, TRYPIX
         DO  I  =  BLXPIX, TRXPIX

*          work out distance of current pixel from circle centre
            JUNK =  REAL( (I-XPIX)**2 + (J-YPIX)**2 )
	    IF( JUNK .GE. 0.0) THEN
              DISTANCE  =  SQRT( JUNK )
	    ELSE
	      DISTANCE = 1.0E37
	    END IF

*          test for inclusion of current pixel
            IF( DISTANCE .LE. RADIUS ) THEN

*             include this point
               NUMPIX   =  NUMPIX + 1
               TOTAL    =  TOTAL + INARRAY( I, J )
               TOTALSQ  =  TOTALSQ + ( INARRAY( I, J )**2 )

            END IF

         END DO
      END DO

*    now form returned quantities - the standard deviation (sigma)
*    is formed from the equation for the variance (sigma squared) :
*                                               _
*     variance  =   Sum over n pixels ( x   -   x )**2
*                                        i
*                   ----------------------------------
*
*                               ( n - 1 )
*
*    this can be algebraically manipulated to the following :
*                                       _                _
*     variance  =   Sum(( x )**2)  -  2.x.Sum( x )  +  n.x**2
*                          i                    i
*                   -----------------------------------------
*
*                                  ( n - 1 )
*
*    and then standard deviation equals sqrt( variance )
*
*    first the mean
      MEAN  =  TOTAL / REAL( NUMPIX )

*    now the variance - check for situation where only one pixel was
*    included - could give divide by zero
      IF( NUMPIX .LE. 1 ) THEN
         VARIANCE  =  0.0
      ELSE
         VARIANCE  =  ( TOTALSQ - 2*MEAN*TOTAL + REAL(NUMPIX)*MEAN**2 )
         VARIANCE  =  VARIANCE / ( REAL( NUMPIX ) - 1.0 )
      END IF

*    the standard deviation is sqrt( variance )
      IF( VARIANCE .GE. 0.0) THEN
        STANDEV   =  SQRT( VARIANCE )
      ELSE
        STANDEV   =  1.0E37
      END IF

*    set the value for the old noise equal to this, and the value
*    for the new noise as sqrt(NUMPIX) times lower - check for zero
*    pixels again
      IF( NUMPIX .LT. 1 ) THEN
         OLDNOISE  =  0.0
         NEWNOISE  =  0.0
      ELSE
         OLDNOISE  =  STANDEV
	 JUNK = REAL( NUMPIX)
	 IF( JUNK .GE. 0.0) THEN
           NEWNOISE  =  STANDEV / ( SQRT( JUNK) )
         ELSE
           NEWNOISE  =  1.0E37
         END IF
      END IF

*    end

      END
