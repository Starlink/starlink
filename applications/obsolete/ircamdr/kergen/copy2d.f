*+  COPY2D - copy one 2-D array into another
      SUBROUTINE COPY2D( DIMS1, DIMS2, ARRIN, ARROUT, STATUS )
*    Description :
*     The input 2-D array, ARRIN, of dimensions given in DIMS, is copied into
*     the output 2-D array, ARROUT, of the same dimensions.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL COPY2D( DIMS, ARRIN, ARROUT, STATUS )
*    Parameters :
*     DIMS( 2 ) = INTEGER( READ )
*           Dimensions of the input and output arrays.
*     ARRIN( DIMS(1), DIMS(2) ) = REAL( READ )
*           Array to be copied.
*     ARROUT( DIMS(1), DIMS(2) ) = REAL( WRITE )
*           Will be returned containing a copy of the input array.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur.
*    Method :
*     If no error on entry then
*        For all rows of the input and output arrays
*           For all the points in a row
*              Output array point is set to the value of input array point
*           Endfor
*        Endfor
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     01/12/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     15-JULY-1994 Changed arguments to input DIMS separately
*                  so that routine will still compile (SKL@JACH)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  DIMS1, ! dimensions for input/output arrays
     :  DIMS2  ! dimensions for input/output arrays
      REAL
     :  ARRIN( DIMS1, DIMS2 ) ! the input array, data to be copied
*    Export :
      REAL
     :  ARROUT( DIMS1, DIMS2 ) ! output array, copy of ARRIN
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  X, ! index to input/output array elements, 1st dimension
     :  Y  !   "    "      "         "       "     2nd     "
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       for all rows of input/output arrays
         DO Y = 1, DIMS2

*          for all points in row
            DO X = 1, DIMS1

*             output array point is set to value of input array point
               ARROUT( X, Y ) = ARRIN( X, Y )
            ENDDO
         ENDDO
      ENDIF

      END
