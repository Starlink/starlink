*+  STATSSUB - works out image statistics

      SUBROUTINE MOSSTATSSUB ( ARRAY, DIMSX, DIMSY, XSTART, YSTART,
     :                         XSIZE, YSIZE, NUMPIX, MAXIMUM, MINIMUM,
     :                         TOTAL, MEAN, STDDEV, STATUS )

*    Description :
*
*     This routine returns statistics on a defined sub-array of
*     an input image.
*
*    Invocation :
*
*     CALL MOSSTATSSUB ( ARRAY, DIMSX, DIMSY, XSTART, YSTART, XSIZE,
*    :                YSIZE, NUMPIX, MAXIMUM, MINIMUM,
*    :                TOTAL, MEAN, STDDEV, STATUS )
*
*    Method :
*
*     Looping around the requested sub-array (which is assumed to
*     be valid) just once, the maximum and minimum pixel values are
*     found, along with the total of all the pixel values, and the
*     total of all the squared pixel values. On exit from the loop,
*     the quantities mean and standard deviation are formed from
*     algebraic manipulations.
*     All internal arithmetic is done in with double precision reals
*     for accuracy, although the input and output remain in single
*     precision.
*
*    Bugs :
*
*     None known.
*
*    Deficiencies :
*
*     Uses awfully clunky fixes to make sure that non-zero standard
*     deviations do not sneak through when every pixel has an identical
*     large and non-trivial value. Another way of fixing the root of
*     these problems (rounding errors it would appear), is to use
*     REAL*16 variables for the maths - this is Vax specific, and also
*     takes longer to do the arithmetic.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     09-12-1985 : First implementation (UKTH::MARK)
*     20-03-1986 : Changed to double precision arithmetic (REVA::MJM)
*     01-07-1986 : More fixes to stop stupid std.devs getting out
*                : (REVA::MJM)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :     DIMSX,        ! dimensions of input array
     :     DIMSY,        ! dimensions of input array
     :     XSTART,           ! x coord of start of sub-array
     :     YSTART,           ! y   "    "   "    "  "    "
     :     XSIZE,            ! x size of sub-array
     :     YSIZE             ! y   "   "  "    "

      REAL
     :     ARRAY( DIMSX, DIMSY )  ! input image array

*    Export :

      INTEGER
     :     NUMPIX            ! number of pixels in sub-array

      REAL
     :     MAXIMUM,          ! maximum pixel value in sub-array
     :     MINIMUM,          ! minimum   "     "    "  "    "
     :     TOTAL,            ! total of pixels in sub-array
     :     MEAN,             ! mean of pixels in sub-array
     :     STDDEV            ! standard deviation of pixels in sub-array

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local Constants :

      REAL*8
     :     STARTMAX,              ! initialising value for maximum
     :     STARTMIN               !       "        "    "  minimum
      PARAMETER( STARTMAX  =  -1.0D30 )
      PARAMETER( STARTMIN  =  +1.0D30 )

*    Local variables :

      REAL*8
     :     VALUE,                 ! current array pixel value
     :     DMEAN,                 ! double precision mean
     :     DTOTAL,                !    "       "     total
     :     DNUMPIX,               !    "       "     number of pixels
     :     DMAXIMUM,              !    "       "     maximum
     :     DMINIMUM,              !    "       "     minimum
     :     DTOTALSQ,              !    "       "     sum of pixels squared
     :     DSTDDEV,               !    "       "     standard deviation
     :     VARIANCE               !    "       "     variance

      INTEGER
     :     I, J                   ! array counter variables

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

!	type *, 'mosstatssub:', xstart, ystart, xsize, ysize
!	type *, dimsx, dimsy

*    initialise variables
      NUMPIX    =  XSIZE * YSIZE
      DNUMPIX   =  DBLE( NUMPIX )
      DTOTAL    =  0.0D0
      DTOTALSQ  =  0.0D0
      DMAXIMUM  =  STARTMAX
      DMINIMUM  =  STARTMIN

*    loop round the rows in the requested sub-array
      DO  J  =  YSTART, ( YSTART + YSIZE - 1 )

*       loop round the pixels in the current row
         DO  I  =  XSTART, ( XSTART + XSIZE - 1 )

            VALUE     =  DBLE( ARRAY( I, J ) )
            DTOTAL    =  DTOTAL + VALUE
            DTOTALSQ  =  DTOTALSQ + VALUE**2
            DMAXIMUM  =  DMAX1( DMAXIMUM, VALUE )
            DMINIMUM  =  DMIN1( DMINIMUM, VALUE )

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
*                                       _
*     variance  =   Sum(( x )**2)  -  n.x**2
*                          i
*                   ------------------------
*
*                           ( n - 1 )
*
*    and then standard deviation equals sqrt( variance )
*
*    first the mean
      DMEAN  =  DTOTAL / DNUMPIX

*    now the variance
      VARIANCE  =  ( DTOTALSQ - DNUMPIX*DMEAN**2 )

*    error check for case of 1x1 box - will divide by zero - and
*    for rounding errors - non-zero variances can be generated by
*    rounding errors even when all the pixels are identical - if
*    this is so then the variance should be zero - also check for
*    negative variances which can arise through rounding errors
      IF( NUMPIX .EQ. 1 .OR. DMAXIMUM .EQ. DMINIMUM .OR.
     :                           VARIANCE .LT. 0.0D0 ) THEN
         VARIANCE  =  0.0D0
      ELSE
         VARIANCE  =  VARIANCE / ( DNUMPIX - 1.0D0 )
      ENDIF

*    calculate the standard deviation = sqrt( variance )
      DSTDDEV  =  DSQRT( VARIANCE )

*    convert returned values back to single precision
      TOTAL    =  SNGL( DTOTAL )
      MEAN     =  SNGL( DMEAN )
      MAXIMUM  =  SNGL( DMAXIMUM )
      MINIMUM  =  SNGL( DMINIMUM )
      STDDEV   =  SNGL( DSTDDEV )


*    that's it - return
      END
