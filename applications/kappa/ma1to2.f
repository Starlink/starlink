*+  MA1TO2 - Subroutine to grow a 1-D image into a 2-D image

      SUBROUTINE MA1TO2( MODE, IDIM, ARRIN, ODIM1, ODIM2, ARROUT,
     :                   STATUS )
*
*    Description :
*
*     The 1-dimensional input array, ARRIN, is "grown" into the
*     2-dimensional output array, ARROUT, according to the value of
*     MODE. If MODE = 1 then ARRIN will be taken as lying along the
*     X-axis of ARROUT and replicated in the Y-axis directon and if
*     MODE = 2, ARRIN will be taken as lying along the Y-axis of
*     ARROUT and replicated in the X-axis direction. An immediate
*     return will occur if STATUS has an error value on entry.
*
*    Invocation :
*
*     CALL MA1TO2( MODE, IDIM, ARRIN, ODIM1, ODIM2, ARROUT, STATUS )
*
*    Arguments :
*
*     MODE = INTEGER( READ )
*         If MODE is 1 then the input 1-D array is copied into each
*           row of the output array. If MODE is 2 then the input 1-D
*           array is copied into each column of the output array.
*     IDIM = INTEGER( READ )
*         The dimension of the input array.
*     ARRIN( IDIM ) = REAL( READ )
*         Data to be copied in creating output array.
*     ODIM1 = INTEGER( READ )
*         The first dimension of the output 2-d array.
*     ODIM2 = INTEGER( READ )
*         The second dimension of the output 2-d array.
*     ARROUT( ODIM1, ODIM2 ) = REAL( WRITE )
*         Data generated from input array according to the value of
*           MODE.
*     STATUS = INTEGER( UPDATE )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur. If MODE 
*           has a value other than 1 or 2 then STATUS will be set to
*           SAI__ERROR and an error reported.
*
*    Method :
*
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
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     28/10/1983 : Original Version                     (ROE::ASOC5)
*     20/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     1986 Sep  8: Renamed parameters section to arguments and tidied
*                  (RL.STAR::CUR).
*     1988 Jun 22: Added identification to error reporting
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
     :  IDIM,                  ! Dimension of input array
     :  ODIM1, ODIM2,          ! Dimensions of output array
     :  MODE                   ! Determines along which axis replication
                               ! will occur

      REAL
     :  ARRIN( IDIM )          ! Input array

*    Export :

      REAL
     :  ARROUT( ODIM1, ODIM2 ) ! Output array

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  X,                     ! Index to output array elements along x
                               ! axis
     :  Y                      ! Index to output array elements along y
                               ! axis
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       extract output values and grow to 2 dimensions for both cases

         IF ( MODE .EQ. 1 ) THEN

*          input array forms X-axis of output and is replicated in Y
*          direction

            DO  Y = 1, ODIM2

               DO  X = 1, ODIM1

                  ARROUT( X, Y ) = ARRIN( X )
               END DO
            END DO

         ELSE IF ( MODE .EQ. 2 ) THEN

*          input array forms Y-axis of output and is replicated in X
*          direction

            DO  Y = 1, ODIM2

               DO  X = 1, ODIM1

                  ARROUT( X, Y ) = ARRIN( Y )
               END DO
            END DO
         ELSE

*       value of MODE not allowed

         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MODE', MODE )
         CALL ERR_REP( 'ERR_MA1TO2_WMODE',
     :     'MA1TO2: MODE = ^MODE not allowed.', STATUS )
         END IF
      END IF

      END
