*+  STATSB - works out image statistics

      SUBROUTINE STATSB ( INARR, DIM1, DIM2, XSTART, YSTART, XSIZE,
     :                    YSIZE, NUMPIX, MAXMUM, MINMUM, TOTAL, MEAN,
     :                    STDDEV, NINVAL, MAXPOS, MINPOS, STATUS )
*
*    Description :
*
*     This routine returns statistics on a defined sub-array of
*     an input image.
*
*    Invocation :
*
*     CALL STATSB ( INARR, DIM1, DIM2, XSTART, YSTART, XSIZE, YSIZE,
*    :              NUMPIX, MAXMUM, MINMUM, TOTAL, MEAN, STDDEV, NINVAL,
*    :              MAXPOS, MINPOS, STATUS )
*
*    Arguments :
*
*     INARR( DIM1, DIM2 ) = REAL( READ )
*         The input data array containing the sub-array for which
*           statistical parameters are required
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     XSTART = INTEGER( READ )
*         The x start co-ordinate of the sub-array
*     YSTART = INTEGER( READ )
*         The y start co-ordinate of the sub-array
*     XSIZE = INTEGER( READ )
*         The x size in pixels of the sub-array
*     YSIZE = INTEGER( READ )
*         The y size in pixels of the sub-array
*     NUMPIX = INTEGER( WRITE )
*         Number of pixels in sub-array
*     MAXMUM = REAL( WRITE )
*         Maximum pixel value in the sub-array
*     MINMUM = REAL( WRITE )
*         Minimum pixel value in the sub-array
*     TOTAL = REAL( WRITE )
*         Total pixel value in the sub-array
*     MEAN = REAL( WRITE )
*         Mean pixel value in the sub-array
*     STDDEV = REAL( WRITE )
*         Standard deviation of the pixel values in the sub-array
*     NINVAL = INTEGER( WRITE )
*         Number of invalid pixels in the sub-array
*     MAXPOS( 2 )  =  INTEGER ( WRITE )
*         X,Y co-ordinates of the pixel where the maximum value is
*           (first) found.
*     MINPOS( 2 )  =  INTEGER ( WRITE )
*         X,Y co-ordinates of the pixel where the minimum value is
*           (first) found.
*     STATUS  =  INTEGER( READ )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Looping around the requested sub-array (including only valid
*     pixels) just once, the maximum and minimum pixel values are
*     found, along with the total of all the pixel values, and the
*     total of all the squared pixel values. On exit from the loop,
*     the quantities mean and standard deviation are formed from
*     algebraic manipulations.
*     All internal arithmetic is done in with double-precision reals
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
*     Malcolm Currie  STARLINK ( RAL::CUR )
*
*    History :
*
*     09-12-1985 : First implementation (UKTH::MARK)
*     20-03-1986 : Changed to double precision arithmetic (REVA::MJM)
*     01-07-1986 : More fixes to stop stupid std.devs getting out
*                : (REVA::MJM)
*     1986 Aug 16: Renamed from STATSSUB, completed prologue, nearly
*                  conformed to Starlink programming standards
*                  (RAL::CUR).
*     1986 Sep 5 : Renamed parameters section to arguments, applied
*                  bad-pixel handling (RAL::CUR).
*     1988 Feb 19: Number of invalid pixels computed and returned
*                  (RAL::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RAL::CUR).
*     1990 Mar  8: Added arguments for the position of maximum and
*                  minimum pixels (RAL::CUR).
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'PRM_PAR'          ! Magic-value definitions

*    Import :

      INTEGER
     :  DIM1, DIM2,
     :  XSTART,
     :  YSTART,
     :  XSIZE,
     :  YSIZE

      REAL
     :  INARR( DIM1, DIM2 )

*    Export :

      INTEGER
     :  NUMPIX,
     :  NINVAL,
     :  MAXPOS( 2 ),
     :  MINPOS( 2 )


      REAL
     :  MAXMUM,
     :  MINMUM,
     :  TOTAL,
     :  MEAN,
     :  STDDEV

*    Status :

      INTEGER  STATUS

*    Local variables :

      DOUBLE PRECISION
     :  VALUE,                 ! Current array pixel value
     :  DMEAN,                 ! Double precision mean
     :  DTOTAL,                !    "       "     total
     :  DNMPIX,                !    "       "     number of pixels
     :  DMAXIM,                !    "       "     maximum
     :  DMINIM,                !    "       "     minimum
     :  DTOTSQ,                !    "       "     sum of pixels squared
     :  DSTDDV,                !    "       "     standard deviation
     :  VARNCE                 !    "       "     variance

      INTEGER
     :  I, J,                  ! Array counter variables
     :  NVALPX                 ! Number of valid pixels in sub-array

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    initialise variables

      NUMPIX  =  XSIZE * YSIZE
      DTOTAL  =  0.0D0
      DTOTSQ  =  0.0D0
      DMAXIM  =  VAL__MIND
      DMINIM  =  VAL__MAXD

*    Positions are undefined because any other value could be a
*    co-ordinate.

      MAXPOS( 1 ) = VAL__BADI
      MAXPOS( 2 ) = VAL__BADI
      MINPOS( 1 ) = VAL__BADI
      MINPOS( 2 ) = VAL__BADI

      NVALPX  =  0

*    loop round the lines in the requested sub-array

      DO  J  =  YSTART, ( YSTART + YSIZE - 1 )

*       loop round the pixels in the current line

         DO  I  =  XSTART, ( XSTART + XSIZE - 1 )

*          only include valid pixel sin the statistics

            IF ( INARR( I, J ) .NE. VAL__BADR ) THEN

               NVALPX  =  NVALPX  +  1
               VALUE   =  DBLE( INARR( I, J ) )
               DTOTAL  =  DTOTAL + VALUE
               DTOTSQ  =  DTOTSQ + VALUE * VALUE

*             check current maximum against current pixel value

               IF ( VALUE .GT. DMAXIM ) THEN
                  DMAXIM  =  VALUE
                  MAXPOS( 1 )  =  I
                  MAXPOS( 2 )  =  J
               END IF

*             check current minimum against current pixel value

               IF ( VALUE .LT. DMINIM ) THEN
                  DMINIM  =  VALUE
                  MINPOS( 1 )  =  I
                  MINPOS( 2 )  =  J
               END IF
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
*                                       _ 
*     variance  =   Sum(( x )**2)  -  n.x**2
*                          i                
*                   ------------------------
*                  
*                           ( n - 1 )
*
*    and then standard deviation equals sqrt( variance )
*
*    first check that there is at least one valid pixel

      IF ( NVALPX .GT. 0 ) THEN

*       compute the mean

         DNMPIX  =  DBLE( NVALPX )
         DMEAN  =  DTOTAL / DNMPIX

*       now the variance

         VARNCE  =  ( DTOTSQ - DNMPIX * DMEAN * DMEAN )

*       error check for case of 1x1 box - will divide by zero - and
*       for rounding errors - non-zero variances can be generated by
*       rounding errors even when all the pixels are identical - if
*       this is so then the variance should be zero - also check for
*       negative variances which can arise through rounding errors

         IF ( NUMPIX .EQ. 1 .OR. DMAXIM .EQ. DMINIM .OR.
     :        VARNCE .LT. 0.0D0 ) THEN      
            VARNCE  =  0.0D0
         ELSE
            VARNCE  =  VARNCE / ( DNMPIX - 1.0D0 )
         ENDIF

*       calculate the standard deviation = sqrt( variance )

         DSTDDV  =  SQRT( VARNCE )

*       convert returned values back to single precision

         TOTAL   =  REAL( DTOTAL )
         MEAN    =  REAL( DMEAN )
         MAXMUM  =  REAL( DMAXIM )
         MINMUM  =  REAL( DMINIM )
         STDDEV  =  REAL( DSTDDV )
 
      ELSE

*       No valid data so the statistics are undefined.

         TOTAL  =  0
         MEAN   = VAL__BADR
         MAXMUM = VAL__BADR
         MINMUM = VAL__BADR
         STDDEV = VAL__BADR
         MAXPOS( 1 )  =  VAL__BADI
         MAXPOS( 2 )  =  VAL__BADI
         MINPOS( 1 )  =  VAL__BADI
         MINPOS( 2 )  =  VAL__BADI
      END IF

*    number of invalid pixels is number in sub-array less the valid ones

      NINVAL = NUMPIX - NVALPX

 999  CONTINUE

*    that's it - return

      END
