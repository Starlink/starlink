*+  FLIPS - reverses input array horizontally or vertically into output array
      SUBROUTINE FLIPS( TYPE, DIMS1, DIMS2, ARRIN, ARROUT, STATUS )
*    Description :
*     The output array, ARROUT, will contain :
*      The input array, ARRIN, flipped by reversing the order of the first
*      dimension elements if the first character of TYPE is 'H' or 'h'.
*      The input array, ARRIN, flipped by reversing the order of the second
*      dimension elements if the first character of TYPE is 'V' or 'v'.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL FLIPS( TYPE, DIMS, ARRIN, ARROUT, STATUS )
*    Parameters :
*     TYPE = CHAR*(*)( READ )
*           If the first character of TYPE is 'H" or 'h' then the reversal is
*           performed in the horizontal direction, if it is 'V' or 'v' then the
*           reversal is performed in the vertical direction.
*     DIMS( 2 ) = INTEGER( READ )
*           Dimensions of the input and output arrays.
*     ARRIN( DIMS(1), DIMS(2) ) = REAL( READ )
*           Array of data to be reversed.
*     ARROUT( DIMS(1), DIMS(2) ) = REAL( WRITE )
*           Array to contain the data after reversal.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur.
*    Method :
*     If no error on entry then
*        If TYPE is 'H' then
*           Perform left to right reversal
*           ARROUT( X, Y ) = ARRIN( XREV, Y )
*           where X = 1 to DIMS(1), Y = 1 to DIMS(2) and XREV = DIMS(1)+1-X
*        Else
*           Perform top to bottom reversal
*           ARROUT( X, Y ) = ARRIN( X, YREV )
*           where X = 1 to DIMS(1), Y = 1 to DIMS(2) and YREV = DIMS(2)+1-Y
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     27/07/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     12-Aug-1994  Changed DIMS arguments so that routine will compile(SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  DIMS1, DIMS2 ! dimensions of input/output images
      CHARACTER*(*)
     :  TYPE ! type of flip to be performed H or V
      REAL
     :  ARRIN( DIMS1, DIMS2 ) ! input image, array to be flipped
*    Export :
      REAL
     :  ARROUT( DIMS1, DIMS2 ) ! output image, array after being flipped
*    Status :
      INTEGER STATUS
*    External References :
      LOGICAL CHR_SIMLR
*    Local variables :
      INTEGER
     :  X,     ! index to first dimension elements of input/output images
     :  Y,     !   "    " second dimension elements of input/output images
     :  XFLIP, !   "    " flipped input array point, first dimension
     :  YFLIP  !   "    "    "      "     "     "    second dimension
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

         IF( CHR_SIMLR( 'H', TYPE ) ) THEN

*          horizontal (left - right) flip
*          for all rows of input/output arrays
            DO Y = 1, DIMS2

*             for all points in a row
               DO X = 1, DIMS1

*                calculate position of flipped point in input array
                  XFLIP = DIMS1 + 1 - X

*                put flipped position input array point into output array
                  ARROUT( X, Y ) = ARRIN( XFLIP, Y )
               ENDDO
            ENDDO
         ELSE

*          vertical (top - bottom) flip
*          for all rows of input/output arrays
            DO Y = 1, DIMS2

*             calculate flipped position of point in input array
               YFLIP = DIMS2 + 1 - Y

*             for all points in a row
               DO X = 1, DIMS1

*                put flipped position input array point into output array
                  ARROUT( X, Y ) = ARRIN( X, YFLIP )
               ENDDO
            ENDDO
         ENDIF
      ENDIF

      END
