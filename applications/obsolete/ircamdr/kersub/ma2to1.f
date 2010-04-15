*+  MA2TO1 - Subroutine to extract a 1-D array from a 2-D array
      SUBROUTINE MA2TO1( MODE, IDIMS1, IDIMS2, ARRIN, ODIMS, ARROUT,
     :                   STATUS )
*    Description :
*     The 1-dimensional output array, ARROUT, is extracted from the
*     2-dimensional input array, ARRIN, according to the value of MODE.
*     If MODE = 1 the n'th point of ARROUT is formed from the sum of the
*     points in the n'th column of ARRIN. If MODE = 2 the n'th point of
*     ARROUT is formed from the sum of the points in the n'th row of ARRIN.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL MA2TO1( MODE, IDIMS, ARRIN, ODIMS, ARROUT, STATUS )
*    Parameters :
*     MODE = INTEGER( READ )
*           If MODE is 1 then the output 1-D array is formed from the sum of
*           the points in the corresponding columns of the input array.
*           If MODE is 2 then the output 1-D array is formed from the sum of
*           the points in the corresponding rows in the input array.
*     IDIMS( DAT__MXDIM ) = INTEGER( READ )
*           Contains dimensions of the input array.
*     ARRIN( IDIMS(1), IDIMS(2) ) = REAL( READ )
*           Data to be summed to create the output array.
*     ODIMS( DAT__MXDIM ) = INTEGER( READ )
*           Contains dimension of the output array.
*     ARROUT( ODIMS(1) ) = REAL( WRITE )
*           Data generated from input array according to the value of MODE.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value
*           on entry then an immediate return will occur. If MODE has a value
*           other than 1 or 2 then STATUS will be set to SAI__ERROR and an
*           error reported.
*    Method :
*     If no error on entry then
*        Set all output array elements to zero
*        If MODE = 1 then
*           For all points in output array
*              Output array point is sum of points in corresponding
*                input array column
*           Endfor
*        Elseif MODE = 2 then
*           For all points in output array
*              Output array point is sum of points in corresponding
*                input array row
*           Endfor
*        Else
*           Value of MODE not allowed in this routine, set STATUS to SAI__ERROR
*             and report an error
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     01/11/1983 : Original version                     (ROE::ASOC5)
*     20/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     15-July-1994 Changed arguments to input DIMS separately
*                  so that routine will still compile (SKL@JACH)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS1, ! dimensions of input array
     :  IDIMS2, ! dimensions of input array
     :  ODIMS, !      "      " output  "
     :  MODE ! determines whether extraction takes place in X or Y
      REAL
     :  ARRIN( IDIMS1, IDIMS2 ) ! input array
*    Export :
      REAL
     :  ARROUT( ODIMS ) ! output array
*    Status :
      INTEGER STATUS
*    External references :

*    Local variables :
      INTEGER
     :  X, ! index to input array elements, first dimension
     :  Y  !   "    "   "     "       "     second    "
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       set all elements of the output array to zero
         CALL ZERO1D( ODIMS, ARROUT, STATUS )

*       create output array according to value of mode
         IF( MODE .EQ. 1 ) THEN

            DO Y = 1, IDIMS2

               DO X = 1, IDIMS1

*                output array point is sum of points in input array column
                  ARROUT( X ) = ARROUT( X ) + ARRIN( X, Y )
               ENDDO
            ENDDO

         ELSEIF( MODE .EQ. 2 ) THEN

            DO Y = 1, IDIMS2

               DO X = 1, IDIMS1

*                output array point is sum of points in input array row
                  ARROUT( Y ) = ARROUT( Y ) + ARRIN( X, Y )
               ENDDO
            ENDDO

         ELSE

*          value of MODE is not allowed so set STATUS and report error
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MODE', MODE )
            CALL ERR_REP( 'ERR_MA2TO1',
     :        'MODE = ^MODE not allowed in routine MA2TO1', STATUS )

         ENDIF
      ENDIF

      END
