      SUBROUTINE CHVALW( DIM, INARR, OLDVAL, NEWVAL, OUTARR, STATUS )
*
*    Description :
*
*     This routine replaces a specified pixel with an input
*     value and returns the old value of the pixel.
*
*    Invocation :
*
*     CALL CHVALW( DIM, INARR, OLDVAL, NEWVAL, OUTARR, STATUS )
*
*    Arguments :
*
*     DIM = INTEGER( READ )
*         The dimension of the input array
*     INARR( DIM ) = INTEGER*2( READ )
*         The input data array
*     OLDVAL = INTEGER*2( READ )
*         Value to be replaced
*     NEWVAL = INTEGER*2( READ )
*         New value to be substituted for the %OLDVAL.
*     OUTARR( DIM ) = INTEGER*2( READ )
*         The output data array containing the modified pixel values
*     STATUS  =  INTEGER( READ )
*         Global status value
*
*    Method :
*
*     If global status is ok. then
*     For all pixels
*        If pixel value is the value to be removed then
*           Replace old value in data array with the new value
*        Endif
*     Endfor
*     End
*
*    Bugs :
*
*     None are known at this time.
*
*    Authors :
*
*     Malcolm J. Currie STARLINK (RAL::CUR)
*
*    History :
*
*     1988 Oct 26 : Original (RAL::CUR).
*     1990 Nov 24 : Used double precision for processing large
*                   integer values (RAL::CUR).
*
*    Type Definitions :
 
      IMPLICIT  NONE           ! No default typing allowed
 
*    Global constants :
 
      INCLUDE  'SAE_PAR'       ! Global SSE definitions
      INCLUDE  'PRM_PAR'       ! Machine-precision constant
 
*    Import :
 
      INTEGER
     :  DIM
 
      INTEGER*2
     :  INARR( DIM ),
     :  NEWVAL,
     :  OLDVAL
 
*    Export :
 
      INTEGER*2
     :  OUTARR( DIM )
 
*    Status :
 
      INTEGER  STATUS
 
*    External references :
 
      DOUBLE PRECISION
     :  VAL_WTOD             ! Data-conversion external function
 
*    Local variables :
 
      INTEGER
     :  I                      ! Loop counter
 
      DOUBLE PRECISION
     :  DIFF,                  ! Normalised maximum difference between
                               ! the data value and the value to change
                               ! for them to be regarded as identical
     :  FPOLD                  ! Floating-point value to be replaced
 
*-
 
*    Error check on entry - return if not ok.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*    To avoid testing two floating-point values for equality, they
*    are tested to be different by less than a small fraction of
*    the value to be replaced.  The machine precision defines the
*    minimum detectable difference.
 
      FPOLD = 0.5 * VAL_WTOD( .FALSE., OLDVAL, STATUS )
      DIFF = ABS( FPOLD * VAL__EPSD )
      DO  I = 1, DIM, 1
 
*       The halving is done to prevent overflows.
 
         IF ( ABS( ( VAL_WTOD( .FALSE., INARR( I ), STATUS ) * 0.5 )
     :        - FPOLD ) .LE. DIFF ) THEN
 
*          A match has been found so replace the value in the output
*          array.
 
            OUTARR( I ) = NEWVAL
         ELSE
 
*          There is no match so copy the value to the output array.
 
            OUTARR( I ) = INARR( I )
         END IF
      END DO
 
*    end and return
 
      END
