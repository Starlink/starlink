
*+  NUMBS - counts up the number of elements with absolute values > a limit

      SUBROUTINE  NUMBS ( ARRIN, DIMS1, DIMS2, VALUE, NUMBER, STATUS )

*    Description :
*
*     This routine returns the number of points in the input array
*     that have an absolute value greater than the input value.
*
*    Invocation :
*
*     CALL NUMBS( ARRIN, DIMS, VALUE, NUMBER, STATUS )
*
*    Method :
*
*     The routine loops around the whole input array, incrementing
*     the value of NUMBER by one each time the current pixel has
*     an absolute value greater than the specified VALUE. On exit,
*     NUMBER is returned.
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Mark McCaughrean (REVA::MJM)
*
*    History :
*
*     15/09/1983 : Original version (ROE::ASOC5)
*     21-10-1985 : Tidied up (REVA::MJM)
*     15-AUG-1994  Changed DIM arguments so routine will compile (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE             ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'         ! global SSE parameters

*    Status :

      INTEGER STATUS            ! global status parameter

*    Local Constants :

      INTEGER NDIMS
      PARAMETER ( NDIMS = 2 )   ! 2 dimensional arrays

*    Local variables :

      INTEGER
     :  DIMS1, DIMS2,          ! input array dimensions
     :  NUMBER,                 ! number of pixels greater than value
     :  X, Y                    ! counters

      REAL

     :  ARRIN( DIMS1, DIMS2 ),   ! input data array
     :  VALUE                        ! value to be tested against

*-
*    check status on entry - return if not o.k.
      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    initialise counter
      NUMBER = 0

*    loop round all pixels in input array
      DO Y = 1, DIMS2
         DO X = 1, DIMS1

*          test absolute pixel value against input value
            IF( ABS( ARRIN( X, Y ) ) .GT. VALUE ) THEN
*             if former greater than latter, add 1 to counter
               NUMBER = NUMBER + 1
            ENDIF

         ENDDO
      ENDDO

*    return and end

      END
