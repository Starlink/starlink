*+  MEDREF - expands ARRIN into ARROUT by reflection about edges of ARRIN
      SUBROUTINE MEDREF( STEP, IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2,
     :                   ARROUT, STATUS )
*    Description :
*     The input array, ARRIN, is copied into the central section of the output
*     array, ARROUT. The edges of ARROUT are then padded by reflection about
*     the edge pixels of ARRIN. For example, with STEP = 2 one corner of the
*     input and output arrays would appear as follows :
*
*                                              3 2 1 2 3 3 3
*                                              2 2 1 2 2 2 2
*     corner of     1 1 1 1 1   corresponding  1 1 1 1 1 1 1
*     input array : 1 2 2 2 2   corner of      2 2 1 2 2 2 2
*                   1 2 3 3 3   output array : 3 2 1 2 3 3 3
*                   1 2 3 4 4                  3 2 1 2 3 4 4
*                   1 2 3 4 5                  3 2 1 2 3 4 5
*
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL MEDREF( STEP, IDIMS, ARRIN, ODIMS, ARROUT, STATUS )
*    Parameters :
*     STEP = INTEGER( READ )
*           The number of rows/columns in the output array which will be padded
*           by reflection.
*     IDIMS( 2 ) = INTEGER( READ )
*           Dimensions of the input data array ARRIN.
*     ARRIN( IDIMS(1), IDIMS(2) ) = REAL( READ )
*           Data to be copied into the central section of the output array.
*     ODIMS( 2 ) = INTEGER( READ )
*           Dimensions of the output array ARROUT.
*     ARROUT( ODIMS(1), ODIMS(2) ) = REAL( WRITE )
*           Will contain input data array in central section and will be padded
*           at the edge of the array by a reflection about the edges of the
*           input array.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value
*           on entry then an immediate return will occur.
*    Method :
*     If no error then
*        Copy input array into central area of output array
*        For the first STEP rows of the output array
*           Calculate the index to the row in the output array which is
*             the same number of rows away from the row corresponding to
*             the first row of the input array but on the opposite side of it
*           Copy all the points in this row corresponding to an input array
*             row into the current row
*        Endfor
*        For the last STEP rows of the output array
*           Calculate the index to the row in the output array which is
*             the same number of rows away from the row corresponding to
*             the last row of the input array but on the opposite side of it
*           Copy all the points in this row corresponding to an input array
*             row into the current row
*        Endfor
*        For all rows of output array
*           For the first STEP points in the row
*              Copy the value of the point which is equidistant from the point
*                corresponding to the first point of an input array row but
*                on the opposite side of it
*           Endfor
*           For the last STEP points in the row
*              Copy the value of the point which is equidistant from the point
*                corresponding to the last point of an input array row but
*                on the opposite side of it
*           Endfor
*        Endfor
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     26/10/83 : Original version                     (ROE::ASOC5)
*     17/02/84 : Documentation brought up to standard ( ROE::ASOC5)
*     12-Aug-1994 Changed DIM arguments so that routine will compile (SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS1, IDIMS2, ! dimensions of input array
     :  ODIMS1, ODIMS2, !     "       "   "     "
     :  STEP        !
      REAL
     :  ARRIN( IDIMS1, IDIMS2 ) ! input array
*    Export :
      REAL
     :  ARROUT( ODIMS1, ODIMS2 ) ! output array
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  FIRST, ! position in output array of first row/column of input array
     :  LASTX, !     "     "    "     "    " last column of input array
     :  LASTY, !     "     "    "     "    " last row of input array
     :  XREF,  ! X index to array elements for 'reflection'
     :  YREF,  ! Y   "    "   "       "     "        "
     :  XIN,   ! X   "    " input array elements
     :  YIN,   ! Y   "    "   "     "       "
     :  X,     ! X   "    " output  "       "
     :  Y      ! Y   "    "   "     "       "
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       FIRST is position in output array of row or column corresponding
*       to the first row or column of input array
         FIRST = STEP + 1

*       LASTX is position of column in output array corresponding to
*       last column of input array
         LASTX = ODIMS1 - STEP

*       LASTY, position in output array of row corresponding to last row
*       of input array
         LASTY = ODIMS2 - STEP

*       copy input frame into central section of output frame
         DO Y = FIRST, LASTY

            YIN = Y - STEP
            DO X = FIRST, LASTX

               XIN = X - STEP
               ARROUT( X, Y ) = ARRIN( XIN, YIN )
            ENDDO
         ENDDO

*       pad bottom STEP rows of output array by a reflection about the row
*       corresponding to the first row of input array
         DO Y = 1, STEP

            YREF = ( 2 * ( FIRST ) ) - Y
            DO X = FIRST, LASTX

               ARROUT( X, Y ) = ARROUT( X, YREF )
            ENDDO
         ENDDO

*       pad top STEP rows of output array by a reflection about the row
*       corresponding to the last row of input array
         DO Y = LASTY+1, ODIMS2

            YREF = ( 2 * LASTY ) - Y
            DO X = FIRST, LASTX

               ARROUT( X, Y ) = ARROUT( X, YREF )
            ENDDO
         ENDDO

*       pad out ends of all rows of output image
         DO Y = 1, ODIMS2

*          pad first STEP points of output array row by a reflection about the
*          point corresponding to first point of input image row
            DO X = 1, STEP

               XREF = ( 2 * FIRST ) - X
               ARROUT( X, Y ) = ARROUT( XREF, Y )
            ENDDO

*          pad last STEP points of output array row by a reflection about the
*          point corresponding to the last point of input image row
            DO X = LASTX+1, ODIMS1

               XREF = ( 2 * LASTX ) - X
               ARROUT( X, Y ) = ARROUT( XREF, Y )
            ENDDO
         ENDDO
      ENDIF

      END
