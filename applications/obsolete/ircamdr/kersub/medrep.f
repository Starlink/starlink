*+  MEDREP - expands ARRIN into ARROUT by replication of edge pixels of ARRIN
      SUBROUTINE MEDREP( STEP, IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2,
     :                   ARROUT, STATUS )
*    Description :
*     The input array, ARRIN, is copied into the central section of the
*     output array, ARROUT. The edges of ARROUT are then padded by replication
*     of the edge pixels of ARRIN. For example, with STEP = 2 one corner
*     of the input and output arrays would appear as follows :
*
*                                              1 1 1 1 1 1 1
*                                              1 1 1 1 1 1 1
*     corner of     1 1 1 1 1   corresponding  1 1 1 1 1 1 1
*     input array : 1 2 2 2 2   corner of      1 1 1 2 2 2 2
*                   1 2 3 3 3   output array : 1 1 1 2 3 3 3
*                   1 2 3 4 4                  1 1 1 2 3 4 4
*                   1 2 3 4 5                  1 1 1 2 3 4 5
*
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL MEDREP( STEP, IDIMS, ARRIN, ODIMS, ARROUT, STATUS )
*    Parameters :
*     STEP = INTEGER( READ )
*           The number of rows/columns of the output array which will be padded
*           by replication.
*     IDIMS( 2 ) = INTEGER( READ )
*           Dimensions of the input array ARRIN.
*     ARRIN( IDIMS(1), IDIMS(2) ) = REAL( READ )
*           Data to be copied into central section of the output array.
*     ODIMS( 2 ) = INTEGER( READ )
*           Dimensions of the output array ARROUT.
*     ARROUT( ODIMS(1), ODIMS(2) ) = REAL( WRITE )
*           Will contain a copy of the input array in its central section and
*           will be padded at the edges by replicating the edge pixels of the
*           input array.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value
*           on entry then an immediate return will occur.
*    Method :
*     If no error then
*        Copy input array into central area of output array
*        Replicate section of output array row corresponding to first row
*          of input array into corresponding section of first STEP rows
*          of output array
*        Replicate section of output array row corresponding to last row
*          of input array into corresponding section of last STEP rows of
*          output array
*        For all rows of output array
*           Replicate point in output array row corresponding to first point
*             of input array row into first STEP points of output array row
*           Replicate point in output array row corresponding to last point
*             of input array row into last STEP points of output array row
*        Endfor
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     24/10/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     12-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS1, IDIMS2, ! dimensions of input array
     :  ODIMS1, ODIMS2, !      "      " output  "
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
     :  XIN,   ! X index to input array elements
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

*       copy input array into central section of output array
         DO Y = FIRST, LASTY

            YIN = Y - STEP
            DO X = FIRST, LASTX

               XIN = X - STEP
               ARROUT( X, Y ) = ARRIN( XIN, YIN )
            ENDDO
         ENDDO

*       for first STEP rows of output array
         DO Y = 1, STEP

*          replicate section of row corresponding to 1st row of input array
            DO X = FIRST, LASTX

               ARROUT( X, Y ) = ARROUT( X, FIRST )
            ENDDO
         ENDDO

*       for last STEP rows of output array
         DO Y = LASTY+1, ODIMS2

*          replicate section of row corresponding to last row of input array
            DO X = FIRST, LASTX

               ARROUT( X, Y ) = ARROUT( X, LASTY )
            ENDDO
         ENDDO

*       for all rows of output array
         DO Y = 1, ODIMS2

*          for 1st STEP points in row
            DO X = 1, STEP

*             replicate point corresponding to 1st in input array row
               ARROUT( X, Y ) = ARROUT( FIRST, Y )
            ENDDO

*          for last STEP points in row
            DO X = LASTX+1, ODIMS1

*             replicate point corresponding to last in input array row
               ARROUT( X, Y ) = ARROUT( LASTX, Y )
            ENDDO
         ENDDO
      ENDIF

      END
