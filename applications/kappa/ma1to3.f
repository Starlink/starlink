*+  MA1TO3 - Subroutine to grow a 1-D array into a 3-D array

      SUBROUTINE MA1TO3( MODE, IDIM, ARRIN, ODIM1, ODIM2, ODIM3, ARROUT,
     :                   STATUS )
*
*    Description :
*
*     The 1-dimensional input array, ARRIN, is "grown" into the
*     3-dimensional output array, ARROUT, according to the value of
*     MODE. If MODE is 1 then ARRIN is taken as lying along the X-axis
*     of ARROUT and replicated in the Y and Z-axis directions, if MODE
*     is 2 ARRIN is taken as lying along the Y-axis of ARROUT and
*     replicated in the X and Z-axis directions and if MODE is 3, ARRIN
*     is taken as lying along the Z-axis of ARROUT and replicated in
*     the X and Y-axis directions. An immediate return will occur if
*     STATUS has an error value on entry.
*
*    Invocation :
*
*     CALL MA1TO3( MODE, IDIM, ARRIN, ODIM1, ODIM2, ODIM3, ARROUT,
*    :             STATUS )
*
*    Arguments :
*
*     MODE = INTEGER( READ )
*         If MODE is 1 then the input 1-D array forms the X-axis of
*           the output array and is replicated in the Y and Z-axis
*           directions. If MODE is 2 then the input 1-D array forms
*           the Y-axis of the output array and is replicated in the Z
*           and Z-axis directions. If MODE is 3 then the input 1-D
*           array forms the Z-axis of the output array and is 
*           replicated in the X and Y axis directions.
*     IDIM = INTEGER( READ )
*         The dimension of the input array.
*     ARRIN( IDIM ) = REAL( READ )
*         Data to be copied in creating output array.
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
*        If mode is 1 then
*           For all output array planes
*              For all rows in plane
*                 For all points in row
*                    Output array point is set to value of input array
*                      point indexed by output array's third dimension
*                      index
*                 Endfor
*              Endfor
*           Endfor
*        Elseif mode is 2 then
*           For all output array planes
*              For all rows in plane
*                 For all points in row
*                    Output array point is set to value of input array
*                      point indexed by output array's second dimension 
*                      index
*                 Endfor
*              Endfor
*           Endfor
*        Elseif mode is 3 then
*           For all output array planes
*              For all rows in plane
*                 For all points in row
*                    Output array point is set to value of input array
*                      point indexed by output array's first dimension
*                      index
*                 Endfor
*              Endfor
*           Endfor
*        Else
*           Value of MODE not allowed so set STATUS to SAI__ERROR and
*             report error
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
*     28/10/1983 : Original version                     (ROE::ASOC5)
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
     :  IDIM,                  ! Dimension of input array
     :  ODIM1, ODIM2, ODIM3,   ! Dimensions of output array
     :  MODE                   ! Determines how the input is to be
                               ! "grown" into the output

      REAL
     :  ARRIN( IDIM ) ! input array

*    Export :

      REAL
     :  ARROUT( ODIM1, ODIM2, ODIM3 ) ! output array

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  X,                     ! Index to output array elements along x
                               ! axis
     :  Y,                     ! Index to output array elements along y
                               ! axis
     :  Z                      ! Index to output array elements along z
                               ! axis
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( MODE .EQ. 1 )   THEN

*          input array taken as output array X-axis and replicated in
*          Y-axis and Z-axis directions

            DO  Z = 1, ODIM3

               DO  Y = 1, ODIM2

                  DO  X = 1, ODIM1

                     ARROUT( X, Y, Z ) = ARRIN( X )
                  END DO
               END DO
            END DO

         ELSE IF ( MODE .EQ. 2 ) THEN

*          input array taken as output array Y-axis and replicated in
*          X-axis and Z-axis directions

            DO  Z = 1, ODIM3

               DO  Y = 1, ODIM2

                  DO  X = 1, ODIM1

                     ARROUT( X, Y, Z ) = ARRIN( Y )
                  END DO
               END DO
            END DO

         ELSE IF ( MODE .EQ. 3 ) THEN

*          input array taken as output array Z-axis and replicated in
*          X-axis and Y-axis directions

            DO  Z = 1, ODIM3

               DO  Y = 1, ODIM2

                  DO  X = 1, ODIM1

                     ARROUT( X, Y, Z ) = ARRIN( Z )
                  END DO
               END DO
            END DO
         ELSE

*          value of MODE not allowed, set STATUS and report error

            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MODE', MODE )
            CALL ERR_REP( 'ERR_MA1TO3_WMODE',
     :        'MA1TO3: MODE = ^MODE not allowed.', STATUS )

         END IF
      END IF

      END
