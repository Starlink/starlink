*+  MEDREP - expands 2-d array by replication of edge pixels into new
*            array

      SUBROUTINE MEDREP( STEP, IDIM1, IDIM2, ARRIN, ODIM1, ODIM2,
     :                   ARROUT, STATUS )
*
*    Description :
*
*     The input array, ARRIN, is copied into the central section of the
*     output array, ARROUT. The edges of ARROUT are then padded by
*     replication of the edge pixels of ARRIN. For example, with
*     STEP = 2 one corner of the input and output arrays would appear
*     as follows :
*
*                                              1 1 1 1 1 1 1
*                                              1 1 1 1 1 1 1
*     corner of     1 1 1 1 1   corresponding  1 1 1 1 1 1 1
*     input array : 1 2 2 2 2   corner of      1 1 1 2 2 2 2
*                   1 2 3 3 3   output array : 1 1 1 2 3 3 3
*                   1 2 3 4 4                  1 1 1 2 3 4 4
*                   1 2 3 4 5                  1 1 1 2 3 4 5
*
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*    Invocation :
*
*     CALL MEDREP( STEP, IDIM1, IDIM2, ARRIN, ODIM1, ODIM2, ARROUT,
*    :             STATUS )
*
*    Arguments :
*
*     STEP = INTEGER( READ )
*         The number of lines/columns of the output array which will
*           be padded by replication.
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
*         Will contain a copy of the input array in its central
*           section and will be padded at the edges by replicating the
*           edge pixels of the input array.
*     STATUS = INTEGER( READ )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.
*
*    Method :
*
*     If no error then
*        Copy input array into central area of output array
*        Replicate section of output array line corresponding to first
*          line of input array into corresponding section of first STEP 
*          lines of output array
*        Replicate section of output array line corresponding to last
*          line of input array into corresponding section of last STEP
*          lines of output array
*        For all lines of output array
*           Replicate point in output array line corresponding to first
*             point of input array line into first STEP points of
*             output array line
*           Replicate point in output array line corresponding to last
*             point of input array line into last STEP points of output
*             array line
*        Endfor
*     Endif
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     24/10/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
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

*       copy input array into central section of output array

         DO  Y = FIRST, LASTY

            YIN = Y - STEP
            DO  X = FIRST, LASTX
               XIN = X - STEP
               ARROUT( X, Y ) = ARRIN( XIN, YIN )
            END DO
         END DO

*       for first STEP lines of output array

         DO  Y = 1, STEP

*          replicate section of line corresponding to first line of
*          input array

            DO  X = FIRST, LASTX
               ARROUT( X, Y ) = ARROUT( X, FIRST )
            END DO
         END DO

*       for last STEP lines of output array

         DO  Y = LASTY+1, ODIM2

*          replicate section of line corresponding to last line of
*          input array

            DO  X = FIRST, LASTX
               ARROUT( X, Y ) = ARROUT( X, LASTY )
            END DO
         END DO

*       for all lines of output array

         DO  Y = 1, ODIM2

*          for 1st STEP points in line

            DO  X = 1, STEP

*             replicate point corresponding to 1st in input array line

               ARROUT( X, Y ) = ARROUT( FIRST, Y )
            END DO

*          for last STEP points in line

            DO  X = LASTX+1, ODIM1

*             replicate point corresponding to last in input array line

               ARROUT( X, Y ) = ARROUT( LASTX, Y )
            END DO
         END DO
      END IF

      END
