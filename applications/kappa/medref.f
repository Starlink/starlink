*+  MEDREF - expands 2-D array by reflection about its edges to form new
*            array

      SUBROUTINE MEDREF( STEP, IDIM1, IDIM2, ARRIN, ODIM1, ODIM2,
     :                   ARROUT, STATUS )
*
*    Description :
*
*     The input array, ARRIN, is copied into the central section of the 
*     output array, ARROUT. The edges of ARROUT are then padded by
*     reflection about the edge pixels of ARRIN. For example, with
*     STEP = 2 one corner of the input and output arrays would appear
*     as follows :
*
*                                              3 2 1 2 3 3 3
*                                              2 2 1 2 2 2 2
*     corner of     1 1 1 1 1   corresponding  1 1 1 1 1 1 1
*     input array : 1 2 2 2 2   corner of      2 2 1 2 2 2 2
*                   1 2 3 3 3   output array : 3 2 1 2 3 3 3
*                   1 2 3 4 4                  3 2 1 2 3 4 4
*                   1 2 3 4 5                  3 2 1 2 3 4 5
*
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*    Invocation :
*
*     CALL MEDREF( STEP, IDIM1, IDIM2, ARRIN, ODIM1, ODIM2, ARROUT,
*                  STATUS )
*
*    Arguments :
*
*     STEP = INTEGER( READ )
*         The number of lines/columns in the output array which will
*           be padded by reflection.
*     IDIM1 = INTEGER( READ )
*         The first dimension of the input 2-d array.
*     IDIM2 = INTEGER( READ )
*         The second dimension of the input 2-d array.
*     ARRIN( IDIM1, IDIM2 ) = REAL( READ )
*         Data to be copied into the central section of the output
*           array.
*     ODIM1 = INTEGER( READ )
*         The first dimension of the output 2-d array.
*     ODIM2 = INTEGER( READ )
*         The second dimension of the output 2-d array.
*     ARROUT( ODIM1, ODIM2 ) = REAL( WRITE )
*         Will contain input data array in central section and will
*           be padded at the edge of the array by a reflection about
*           the edges of the input array.
*     STATUS = INTEGER( READ )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.
*
*    Method :
*
*     If no error then
*        Copy input array into central area of output array
*        For the first STEP lines of the output array
*           Calculate the index to the line in the output array which is
*             the same number of lines away from the line corresponding
*             to the first line of the input array but on the opposite
*             side of it
*           Copy all the points in this line corresponding to an input
*             array line into the current line
*        Endfor
*        For the last STEP lines of the output array
*           Calculate the index to the line in the output array which is
*             the same number of lines away from the line corresponding
*             to the last line of the input array but on the opposite
*             side of it
*           Copy all the points in this line corresponding to an input
*             array line into the current line
*        Endfor
*        For all lines of output array
*           For the first STEP points in the line
*              Copy the value of the point which is equidistant from the
*                point corresponding to the first point of an input
*                array line but on the opposite side of it
*           Endfor
*           For the last STEP points in the line
*              Copy the value of the point which is equidistant from the
*                point corresponding to the last point of an input
*                array line but on the opposite side of it
*           Endfor
*        Endfor
*     Endif
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*     26/10/83 : Original version                     (ROE::ASOC5)
*     17/02/84 : Documentation brought up to standard ( ROE::ASOC5)
*     1986 Sep 9 : Renamed parameters section to arguments and tidied
*                  (RL.STAR::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE 'SAE_PAR'

*    Import :

      INTEGER
     :  IDIM1, IDIM2,          ! Dimensions of input array
     :  ODIM1, ODIM2,          ! Dimensions of output array
     :  STEP

      REAL
     :  ARRIN( IDIM1, IDIM2 )  ! Input array

*    Export :

      REAL
     :  ARROUT( ODIM1, ODIM2 ) ! Output array

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  FIRST,                 ! Position in output array of first line
                               ! /column of input array
     :  LASTX,                 ! Position in output array of last column
                               ! of input array
     :  LASTY,                 ! Position in output array of last line
                               ! of input array
     :  XREF,                  ! X index to array elements for 
                               ! 'reflection'
     :  YREF,                  ! Y index to array elements for 
                               ! 'reflection'
     :  XIN,                   ! X   "    " input array elements
     :  YIN,                   ! Y   "    "   "     "       "
     :  X,                     ! X   "    " output  "       "
     :  Y                      ! Y   "    "   "     "       "
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       FIRST is position in output array of line or column
*       corresponding to the first line or column of input array

         FIRST = STEP + 1

*       LASTX is position of column in output array corresponding to
*       last column of input array

         LASTX = ODIM1 - STEP

*       LASTY, position in output array of line corresponding to last
*       line of input array

         LASTY = ODIM2 - STEP

*       copy input frame into central section of output frame

         DO  Y = FIRST, LASTY

            YIN = Y - STEP
            DO  X = FIRST, LASTX
               XIN = X - STEP
               ARROUT( X, Y ) = ARRIN( XIN, YIN )
            END DO
         END DO
 
*       pad bottom STEP lines of output array by a reflection about the
*       line corresponding to the first line of input array

         DO  Y = 1, STEP

            YREF = ( 2 * ( FIRST ) ) - Y
            DO  X = FIRST, LASTX
               ARROUT( X, Y ) = ARROUT( X, YREF )
            END DO
         END DO

*       pad top STEP lines of output array by a reflection about the
*       line corresponding to the last line of input array

         DO  Y = LASTY+1, ODIM2

            YREF = ( 2 * LASTY ) - Y
            DO  X = FIRST, LASTX
               ARROUT( X, Y ) = ARROUT( X, YREF )
            END DO
         END DO

*       pad out ends of all lines of output image

         DO  Y = 1, ODIM2

*          pad first STEP points of output array line by a reflection
*          about the point corresponding to first point of input image
*          line

            DO  X = 1, STEP
               XREF = ( 2 * FIRST ) - X
               ARROUT( X, Y ) = ARROUT( XREF, Y )
            END DO

*          pad last STEP points of output array line by a reflection
*          about point corresponding to the last point of input image
*          line

            DO  X = LASTX+1, ODIM1
               XREF = ( 2 * LASTX ) - X
               ARROUT( X, Y ) = ARROUT( XREF, Y )
            END DO
         END DO
      END IF

      END
