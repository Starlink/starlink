*+  COPY3D - copy one 3-D array into another
      SUBROUTINE COPY3D( DIMS1, DIMS2, DIMS3, ARRIN, ARROUT, STATUS )
*    Description :
*     The input 3-D array, ARRIN, of dimensions given in DIMS, is copied into
*     the output 3-D array, ARROUT, of the same dimensions.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL COPY3D( DIMS, ARRIN, ARROUT, STATUS )
*    Parameters :
*     DIMS( 3 ) = INTEGER( READ )
*           Dimensions of the input and output arrays.
*     ARRIN( DIMS(1), DIMS(2), DIMS(3) ) = REAL( READ )
*           Array to be copied.
*     ARROUT( DIMS(1), DIMS(2), DIMS(3) ) = REAL( WRITE )
*           Will be returned containing a copy of the input array.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur.
*    Method :
*     If no error on entry then
*        For all planes of the input and output arrays
*           For all the rows of a plane
*              For all the points in a row
*                 Output array point is set to value of input array point
*              Endfor
*           Endfor
*        Endfor
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     02/12/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     15_JULY-1994 Changed arguments to input DIMS separately
*                  so that routine will still compile  (SKL@JACH)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  DIMS1, ! dimensions of the input/output arrays
     :  DIMS2, ! dimensions of the input/output arrays
     :  DIMS3  ! dimensions of the input/output arrays
      REAL
     :  ARRIN( DIMS1, DIMS2, DIMS3 ) ! the input array, data to be copied
*    Export :
      REAL
     :  ARROUT( DIMS1, DIMS2, DIMS3 ) ! output array, copy of ARRIN
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  X, ! index to input/output array elements, 1st dimension
     :  Y, !   "    "      "         "       "     2nd     "
     :  Z  !   "    "      "         "       "     3rd     "
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       for all planes of input/output arrays
         DO Z = 1, DIMS3

*          for all rows of plane
            DO Y = 1, DIMS2

*             for all points in row
               DO X = 1, DIMS1

*                output array point is set to value of input array point
                  ARROUT( X, Y, Z ) = ARRIN( X, Y, Z )
               ENDDO
            ENDDO
         ENDDO
      ENDIF

      END
