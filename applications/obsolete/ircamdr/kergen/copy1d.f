*+  COPY1D - copy one 1-D array to another
      SUBROUTINE COPY1D( DIM, ARRIN, ARROUT, STATUS )
*    Description :
*     The input 1-D array, ARRIN, of dimension DIM, is copied into the output
*     1-D array, ARROUT, of the same dimension.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL COPY1D( DIM, ARRIN, ARROUT, STATUS )
*    Parameters :
*     DIM = INTEGER( READ )
*           Dimension of the input and output arrays.
*     ARRIN( DIM ) = REAL( READ )
*           1-D array to be copied.
*     ARROUT( DIM ) = REAL( WRITE )
*           Output array is returned as a copy of the input array.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur.
*    Method :
*     If no error on entry then
*        For all points in the input array
*           Output array point is set to value of input array point.
*        Endfor
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     02/12/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  DIM ! dimension for input/output arrays
      REAL
     :  ARRIN( DIM ) ! the input array, data to be copied
*    Export :
      REAL
     :  ARROUT( DIM ) ! output array, copy of ARRIN
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  X ! index to input/output array elements
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       for all points in input/output arrays
         DO X = 1, DIM

*          output array point is set to value of input array point
            ARROUT( X ) = ARRIN( X )
         ENDDO
      ENDIF

      END
