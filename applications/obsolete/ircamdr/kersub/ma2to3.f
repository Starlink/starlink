*+  MA2TO3 - Subroutine to grow a 2-D array into a 3-D array
      SUBROUTINE MA2TO3( MODE, IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2,
     :                   ODIMS3, ARROUT, STATUS )
*    Description :
*     The two dimensional input array, ARRIN, is "grown" into the
*     three dimensional output array, ARROUT. If MODE is 1 ARRIN will
*     form the XY plane of ARROUT and will be replicated along the Z-axis
*     If MODE is 2 ARRIN will form the XZ plane and will be replicated
*     along the Y-axis. If MODE is 3 ARRIN will form the YZ plane and will
*     be replicated along the X-axis.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL MA2TO3( MODE, IDIMS, ARRIN, ODIMS, ARROUT, STATUS )
*    Parameters :
*     MODE = INTEGER( READ )
*           If MODE is 1 then the input 2-D array forms the XY plane of the
*           output 3-D array and is replicated in the Z-axis direction.
*           If MODE is 2 then the input 2-D array forms the XZ plane of the
*           output 3-D array and is replicated in the Y-axis direction.
*           If MODE is 3 then the input 2-D array forms the YZ plane of the
*           output 3-D array and is replicated in the X-axis direction.
*     IDIMS( DAT__MXDIM ) = INTEGER( READ )
*           Contains the dimensions of the input array.
*     ARRIN( IDIMS(1), IDIMS(2) ) = REAL( READ )
*           Data to be copied in creating output array.
*     ODIMS( DAT__MXDIM ) = INTEGER( READ )
*           Contains dimensions of the output array.
*     ARROUT( ODIMS(1), ODIMS(2), ODIMS(3) ) = REAL( WRITE )
*           Data generated from input array according to the value of MODE.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value
*           on entry then an immediate return will occur. If MODE has a value
*           other than 1,2 or 3 then STATUS will be set to SAI__ERROR and an
*           error reported.
*    Method :
*     If no error on entry then
*        If MODE is 1 then
*           For all planes of the output array
*              For all rows of a plane
*                 For all points in a row
*                    The output array point is set to the value of the input
*                      array point indexed by the first and second dimension
*                      indices to the output array point
*                 Endfor
*              Endfor
*           Endfor
*        Elseif MODE is 2 then
*           For all planes of the output array
*              For all rows of a plane
*                 For all points in a row
*                    The output array point is set to the value of the input
*                      array point indexed by the first and third dimension
*                      indices to the output array point
*                 Endfor
*              Endfor
*           Endfor
*        Elseif MODE is 3 then
*           For all planes of the output array
*              For all rows of a plane
*                 For all points in a row
*                    The output array point is set to the value of the input
*                      array point indexed by the second and third dimension
*                      indices to the output array point
*                 Endfor
*              Endfor
*           Endfor
*        Else
*           Value of MODE is not allowed, set STATUS to SAI__ERROR and report
*             an error
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     30/10/1983 : Original version                     (ROE::ASOC5)
*     20/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     15-July-1994 Changed arguments to input DIMS separately
*                  so that routine will still compile  (SKL@JACH)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS1, ! dimensions of input array
     :  IDIMS2, ! dimensions of input array
     :  ODIMS1, !      "      " output array
     :  ODIMS2, !      "      " output array
     :  ODIMS3, !      "      " output array
     :  MODE ! determines how input to output conversion will be performed
      REAL
     :  ARRIN( IDIMS1, IDIMS2 ) ! input array
*    Export :
      REAL
     :  ARROUT( ODIMS1, ODIMS2, ODIMS3 ) ! output array
*    Status :
      INTEGER STATUS
*    External references :

*    Local variables :
      INTEGER
     :  X, ! index to output array elements, 1st dimension
     :  Y, !   "    "   "      "       "     2nd     "
     :  Z  !   "    "   "      "       "     3rd     "
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       create output array from input array according to value of MODE
         IF( MODE .EQ. 1 ) THEN

*          input array forms X,Y plane of the output array and is replicated
*          in the Z-axis direction
            DO Z = 1, ODIMS3

               DO Y = 1, ODIMS2

                  DO X = 1, ODIMS1

                     ARROUT( X, Y, Z ) = ARRIN( X, Y )
                  ENDDO
               ENDDO
            ENDDO

         ELSEIF( MODE .EQ. 2 ) THEN

*          input array forms X,Z plane of the output array and is replicated
*          in the Y-axis direction
            DO Z = 1, ODIMS3

               DO Y = 1, ODIMS2

                  DO X = 1, ODIMS1

                     ARROUT( X, Y, Z ) = ARRIN( X, Z )
                  ENDDO
               ENDDO
            ENDDO

         ELSEIF( MODE .EQ. 3 ) THEN

*          input array forms Y,Z  plane of the output array and is replicated
*          in the X-axis direction
            DO Z = 1, ODIMS3

               DO Y = 1, ODIMS2

                  DO X = 1, ODIMS1

                     ARROUT( X, Y, Z ) = ARRIN( Y, Z )
                  ENDDO
               ENDDO
            ENDDO
         ELSE

*          value of MODE is not allowed, report as error
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MODE', MODE )
            CALL ERR_REP( 'ERR_MA2TO3',
     :        'MODE = ^MODE not allowed in routine MA2TO3.', STATUS )

         ENDIF
      ENDIF

      END
