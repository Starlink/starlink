*+  MA2TO3 - Subroutine to grow a 2-D array into a 3-D array

      SUBROUTINE MA2TO3( MODE, IDIM1, IDIM2, ARRIN, ODIM1, ODIM2, ODIM3,
     :                   ARROUT, STATUS )
*
*    Description :
*
*     The two dimensional input array, ARRIN, is "grown" into the
*     three-dimensional output array, ARROUT. If MODE is 1 ARRIN will
*     form the XY plane of ARROUT and will be replicated along the
*     Z-axis. If MODE is 2 ARRIN will form the XZ plane and will be
*     replicated along the Y-axis. If MODE is 3 ARRIN will form the YZ
*     plane and will be replicated along the X-axis.
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*    Invocation :
*
*     CALL MA2TO3( MODE, IDIM1, IDIM2, ARRIN, ODIM1, ODIM2, ODIM3,
*    :             ARROUT, STATUS )
*
*    Arguments :
*
*     MODE = INTEGER( READ )
*         If MODE is 1 then the input 2-D array forms the XY plane of
*           the output 3-D array and is replicated in the Z-axis
*           direction.
*           If MODE is 2 then the input 2-D array forms the XZ plane of
*           the output 3-D array and is replicated in the Y-axis
*           direction.
*           If MODE is 3 then the input 2-D array forms the YZ plane of
*           the output 3-D array and is replicated in the X-axis
*           direction.
*     IDIM1 = INTEGER( READ )
*         The first dimension of the input 2-d array.
*     IDIM2 = INTEGER( READ )
*         The second dimension of the input 2-d array.
*     ARRIN( IDIM1, IDIM2 ) = REAL( READ )
*         Data to be summed to create output array.
*     ODIM1 = INTEGER( READ )
*         The first dimension of the output 3-d array.
*     ODIM2 = INTEGER( READ )
*         The second dimension of the output 3-d array.
*     ODIM3 = INTEGER( READ )
*         The third dimension of the output 3-d array.
*     ARROUT( ODIM1, ODIM2, ODIM3 ) = REAL( WRITE )
*         Data generated from input array according to the value of
*           MODE.
*     STATUS = INTEGER( UPDATE )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur. If MODE
*           has a value other than 1,2 or 3 then STATUS will be set to
*           SAI__ERROR and an error reported.
*
*    Method :
*
*     If no error on entry then
*        If MODE is 1 then
*           For all planes of the output array
*              For all rows of a plane
*                 For all points in a row
*                    The output array point is set to the value of the
*                      input array point indexed by the first and
*                      second dimension indices to the output array
*                      point
*                 Endfor
*              Endfor
*           Endfor
*        Elseif MODE is 2 then
*           For all planes of the output array
*              For all rows of a plane
*                 For all points in a row
*                    The output array point is set to the value of the
*                      input array point indexed by the first and third 
*                      dimension indices to the output array point
*                 Endfor
*              Endfor
*           Endfor
*        Elseif MODE is 3 then
*           For all planes of the output array
*              For all rows of a plane
*                 For all points in a row
*                    The output array point is set to the value of the
*                      input array point indexed by the second and third
*                       dimension indices to the output array point
*                 Endfor
*              Endfor
*           Endfor
*        Else
*           Value of MODE is not allowed, set STATUS to SAI__ERROR and
*             report an error
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
*     30/10/1983 : Original version                     (ROE::ASOC5)
*     20/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     1986 Sep 8 : Renamed parameters section to arguments and tidied
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
     :  IDIM1, IDIM2,          ! Dimensions of input array
     :  ODIM1, ODIM2, ODIM3,   ! Dimensions of output array
     :  MODE                   ! Determines how input to output
                               ! conversion will be performed

      REAL
     :  ARRIN( IDIM1, IDIM2 ) ! input array

*    Export :

      REAL
     :  ARROUT( ODIM1, ODIM2, ODIM3 ) ! output array

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  X,                     ! Index to output array elements, 1st dim
     :  Y,                     !   "    "    "     "       "     2nd  "
     :  Z                      !   "    "    "     "       "     3rd  "
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       create output array from input array according to value of MODE

         IF ( MODE .EQ. 1 ) THEN

*          input array forms X,Y plane of the output array and is
*          replicated in the Z-axis direction

            DO  Z = 1, ODIM3

               DO  Y = 1, ODIM2

                  DO  X = 1, ODIM1

                     ARROUT( X, Y, Z ) = ARRIN( X, Y )
                  END DO
               END DO
            END DO

         ELSE IF ( MODE .EQ. 2 ) THEN

*          input array forms X,Z plane of the output array and is
*          replicated in the Y-axis direction

            DO  Z = 1, ODIM3

               DO  Y = 1, ODIM2

                  DO  X = 1, ODIM1

                     ARROUT( X, Y, Z ) = ARRIN( X, Z )
                  END DO
               END DO
            END DO

         ELSE IF ( MODE .EQ. 3 ) THEN

*          input array forms Y,Z  plane of the output array and is
*          replicated in the X-axis direction

            DO  Z = 1, ODIM3

               DO  Y = 1, ODIM2

                  DO  X = 1, ODIM1

                     ARROUT( X, Y, Z ) = ARRIN( Y, Z )
                  END DO
               END DO
            END DO
         ELSE

*          value of MODE is not allowed, report as error

            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MODE', MODE )
            CALL ERR_REP( 'ERR_MA2TO3_WMODE',
     :        'MA2TO3: MODE = ^MODE not allowed.', STATUS )

         END IF
      END IF

      END
