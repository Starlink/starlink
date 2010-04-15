*+  MA1TO2 - Subroutine to grow a 1-D image into a 2-D image
      SUBROUTINE MA1TO2( MODE, IDIMS1, ARRIN, ODIMS1, ODIMS2, ARROUT,
     :                   STATUS )
*    Description :
*     The 1-dimensional input array, ARRIN, is "grown" into the 2-dimensional
*     output array, ARROUT, according to the value of MODE. If MODE = 1 then
*     ARRIN will be taken as lying along the X-axis of ARROUT and replicated
*     in the Y-axis directon and if MODE = 2 ARRIN will be taken as lying along
*     the Y-axis of ARROUT and replicated in the X-axis direction.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL MA1TO2( MODE, IDIMS, ARRIN, ODIMS, ARROUT, STATUS )
*    Parameters :
*     MODE = INTEGER( READ )
*           If MODE is 1 then the input 1-D array is copied into each row
*           of the output array. If MODE is 2 then the input 1-D array is
*           copied into each column of the output array.
*     IDIMS( DAT__MXDIM ) = INTEGER( READ )
*           Contains dimension of the input array.
*     ARRIN( IDIMS(1) ) = REAL( READ )
*           Data to be copied in creating output array.
*     ODIMS( DAT__MXDIM ) = INTEGER( READ )
*           Contains dimensions of the output 2-D array.
*     ARROUT( ODIMS(1), ODIMS(2) ) = REAL( WRITE )
*           Data generated from input array according to the value of MODE.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value
*           on entry then an immediate return will occur. If MODE has a value
*           other than 1 or 2 then STATUS will be set to SAI__ERROR and an
*           error reported.
*    Method :
*     If no error on entry then
*        If MODE = 1 then
*           For each row in output array
*              Output array row is a copy of input array
*           Endfor
*        Elseif MODE = 2 then
*           For each column in output array
*              Output array column is a copy of input array
*           Endfor
*        Else
*           Value of MODE not allowed so set STATUS and report error
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     28/10/1983 : Original Version                     (ROE::ASOC5)
*     20/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     15-JULY-1994 Changed arguments so that DIMS input separately
*                  so that routine will still compile (SKL@JACH)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS1, ! dimensions of input image
     :  ODIMS1, !      "      " output  "
     :  ODIMS2, !      "      " output  "
     :  MODE ! determines along which axis replication will occur
      REAL
     :  ARRIN( IDIMS1 ) ! input array
*    Export :
      REAL
     :  ARROUT( ODIMS1, ODIMS2 ) ! output array
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  X,    ! index to output array elements along X-axis
     :  Y     !   "    "   "      "       "      "   Y-axis
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       extract output values and grow to 2 dimensions for both cases
         IF( MODE .EQ. 1 ) THEN

*          input array forms X-axis of output and is replicated in Y direction
            DO Y = 1, ODIMS2

               DO X = 1, ODIMS1

                  ARROUT( X, Y ) = ARRIN( X )
               ENDDO
            ENDDO

         ELSEIF( MODE .EQ. 2 ) THEN

*          input array forms Y-axis of output and is replicated in X direction
            DO Y = 1, ODIMS2

               DO X = 1, ODIMS1

                  ARROUT( X, Y ) = ARRIN( Y )
               ENDDO
            ENDDO
         ELSE

*       value of MODE not allowed
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MODE', MODE )
         CALL ERR_REP( 'ERR_MA1TO2',
     :     'MODE = ^MODE not allowed in routine MA1TO2', STATUS )
         ENDIF
      ENDIF

      END
