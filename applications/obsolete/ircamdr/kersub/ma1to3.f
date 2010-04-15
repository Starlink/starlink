*+  MA1TO3 - Subroutine to grow a 1-D array into a 3-D array
      SUBROUTINE MA1TO3( MODE, IDIMS, ARRIN, ODIMS1, ODIMS2, ODIMS3,
     :                   ARROUT, STATUS )
*    Description :
*     The 1-dimensional input array, ARRIN, is "grown" into the 3-dimensional
*     output array, ARROUT, according to the value of MODE. If MODE is 1 then
*     ARRIN is taken as lying along the X-axis of ARROUT and replicated in the
*     Y and Z-axis directions, if MODE is 2 ARRIN is taken as lying along the
*     Y-axis of ARROUT and replicated in the X and Z-axis directions and if
*     MODE is 3 ARRIN is taken as lying along the Z-axis of ARROUT and
*     replicated in the X and Y-axis directions.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL MA1TO3( MODE, IDIMS, ARRIN, ODIMS, ARROUT, STATUS )
*    Parameters :
*     MODE = INTEGER( READ )
*           If MODE is 1 then the input 1-D array forms the X-axis of the
*           output array and is replicated in the Y and Z axis directions.
*           If MODE is 2 then the input 1-D array forms the Y-axis of the
*           output array and is replicated in the Z and Z axis directions.
*           If MODE is 3 then the input 1-D array forms the Z-axis of the
*           output array and is replicated in the X and Y axis directions.
*     IDIMS( DAT__MXDIM ) = INTEGER( READ )
*           Contains dimension of the input array.
*     ARRIN( IDIMS(1) ) = REAL( READ )
*           Data to be copied in creating output array.
*     ODIMS( DAT__MXDIM ) = INTEGER( READ )
*           Contains dimensions of the output 3-D array.
*     ARROUT( ODIMS(1), ODIMS(2), ODIMS(3) ) = REAL( WRITE )
*           Data generated from input array according to the value of MODE.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value
*           on entry then an immediate return will occur. If MODE has a value
*           other than 1,2 or 3 then STATUS will be set to SAI__ERROR and an
*           error reported.
*    Method :
*     If no error on entry then
*        If mode is 1 then
*           For all output array planes
*              For all rows in plane
*                 For all points in row
*                    Output array point is set to value of input array point
*                      indexed by output array third dimension index
*                 Endfor
*              Endfor
*           Endfor
*        Elseif mode is 2 then
*           For all output array planes
*              For all rows in plane
*                 For all points in row
*                    Output array point is set to value of input array point
*                      indexed by output array second dimension index
*                 Endfor
*              Endfor
*           Endfor
*        Elseif mode is 3 then
*           For all output array planes
*              For all rows in plane
*                 For all points in row
*                    Output array point is set to value of input array point
*                      indexed by output array first dimension index
*                 Endfor
*              Endfor
*           Endfor
*        Else
*           Value of MODE not allowed so set STATUS to SAI__ERROR and report
*             error
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     28/10/1983 : Original version                     (ROE::ASOC5)
*     20/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     11-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS, ! input array dimensions
     :  ODIMS1, ODIMS2, ODIMS3, ! output  "        "
     :  MODE ! determines how the input is to be "grown" into the output
      REAL
     :  ARRIN( IDIMS ) ! input array
*    Export :
      REAL
     :  ARROUT( ODIMS1, ODIMS2, ODIMS3 ) ! output array
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  X, ! index to output array elements, first dimension
     :  Y, !   "    "    "     "       "     second    "
     :  Z  !   "    "    "     "       "     third     "
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

         IF( MODE .EQ. 1 )   THEN

*          input array taken as output array X-axis and replicated in Y-axis
*          and Z-axis directions
            DO Z = 1, ODIMS3

               DO Y = 1, ODIMS2

                  DO X = 1, ODIMS1

                     ARROUT( X, Y, Z ) = ARRIN( X )
                  ENDDO
               ENDDO
            ENDDO

         ELSEIF( MODE .EQ. 2 ) THEN

*          input array taken as output array Y-axis and replicated in X-axis
*          and Z-axis directions
            DO Z = 1, ODIMS3

               DO Y = 1, ODIMS2

                  DO X = 1, ODIMS1

                     ARROUT( X, Y, Z ) = ARRIN( Y )
                  ENDDO
               ENDDO
            ENDDO

         ELSEIF( MODE .EQ. 3 ) THEN

*          input array taken as output array Z-axis and replicated in X-axis
*          and Y-axis directions
            DO Z = 1, ODIMS3

               DO Y = 1, ODIMS2

                  DO X = 1, ODIMS1

                     ARROUT( X, Y, Z ) = ARRIN( Z )
                  ENDDO
               ENDDO
            ENDDO
         ELSE

*          value of MODE not allowed, set STATUS and report error
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MODE', MODE )
            CALL ERR_REP( 'ERR_MA1TO3',
     :        'MODE = ^MODE not allowed in routine MA1TO3', STATUS )

         ENDIF
      ENDIF

      END
