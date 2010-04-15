*+  MA3TO2 - Subroutine to extract a 2-D image from a 3-D image
      SUBROUTINE MA3TO2( MODE, IDIMS1, IDIMS2, IDIMS3, ARRIN, ODIMS1,
     :                   ODIMS2, ARROUT, STATUS )
*    Description :
*     The 2-dimensional output array, ARROUT, is created from the 3-dimensional
*     input array, ARRIN, according to the value of MODE. If MODE is 1 the
*     output array is created from the sum of all the X,Y planes of the input
*     array, if MODE is 2 then the output array is the sum of all the X,Z
*     planes of the input array and if MODE is 3 then the output array is the
*     sum of all the Y,Z planes of the input array.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL MA3TO2( MODE, IDIMS, ARRIN, ODIMS, ARROUT, STATUS )
*    Parameters :
*     MODE = INTEGER( READ )
*           If MODE is 1 then each point in the output array is formed from
*           the sum over the Z-axis of the corresponding point in the XY plane
*           of the input 3-D array.
*           If MODE is 2 then each point in the output array is formed from
*           the sum over the Y-axis of the corresponding point in the XZ plane
*           of the input 3-D array.
*           If MODE is 3 then each point in the output array is formed from
*           the sum over the X-axis of the corresponding point in the YZ plane
*           of the input 3-D array.
*     IDIMS( DAT__MXDIM ) = INTEGER( READ )
*           Contains dimensions of the input array.
*     ARRIN( IDIMS(1), IDIMS(2), IDIMS(3) ) = REAL( READ )
*           Data to be summed to create output array.
*     ODIMS( DAT__MXDIM ) = INTEGER( READ )
*           Contains dimensions of the output array.
*     ARROUT( ODIMS(1), ODIMS(2) ) = REAL( WRITE )
*           Data generated from input array according to the value of MODE.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value
*           on entry then an immediate return will occur. If MODE has a value
*           other than 1,2 or 3 then STATUS will be set to SAI__ERROR and an
*           error reported.
*    Method :
*     If no error on entry then
*        Set all elements of the output array to zero
*        If MODE is 1 then
*           For all planes of the input array
*              For all rows of a plane
*                 For all points in a row
*                    The value of the current input array point is added
*                      to the value of the point in the output array indexed
*                      by the first and second dimension indices to the input
*                      array point
*                 Endfor
*              Endfor
*           Endfor
*        Else if MODE is 2 then
*           For all planes of the input array
*              For all rows of a plane
*                 For all points in a row
*                    The value of the current input array point is added
*                      to the value of the point in the output array indexed
*                      by the first and third dimension indices to the input
*                      array point
*                 Endfor
*              Endfor
*           Endfor
*        Else if MODE is 3 then
*           For all planes of the input array
*              For all rows of a plane
*                 For all points in a row
*                    The value of the current input array point is added
*                      to the value of the point in the output array indexed
*                      by the second and third dimension indices to the input
*                      array point
*                 Endfor
*              Endfor
*           Endfor
*        Else
*           Value of MODE not allowed, set STATUS to SAI__ERROR and report an
*             error
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     30/10/1983 : Original version                     (ROE::ASOC5)
*     20/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     15-July-1994 Changed arguments to input dimensions separately
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
     :  IDIMS3, ! dimensions of input array
     :  ODIMS1, !      "      " output  "
     :  ODIMS2, !      "      " output  "
     :  MODE ! determines how input to output conversion is performed
      REAL
     :  ARRIN( IDIMS1, IDIMS2, IDIMS3 ) ! input array
*    Export :
      REAL
     :  ARROUT( ODIMS1, ODIMS2 ) ! output array
*    Status :
      INTEGER STATUS
*    External references :

*    Local variables :
      INTEGER
     :  X, ! index to input array elements, 1st dimension
     :  Y, !   "    "   "     "       "     2nd     "
     :  Z  !   "    "   "     "       "     3rd     "
*-

*    check for error on entry
      IF( STATUS .EQ.SAI__OK ) THEN

*       set all elements of output image to zero
         CALL ZERO2D( ODIMS1, ODIMS2, ARROUT, STATUS )

*       create output array from input array according to MODE
         IF( MODE .EQ. 1 )  THEN

*          output array is the sum of all the X,Y planes of the input array
            DO Z = 1, IDIMS3

               DO Y = 1, IDIMS2

                  DO X = 1, IDIMS1

                     ARROUT( X, Y ) = ARROUT( X, Y ) + ARRIN( X, Y, Z )
                  ENDDO
               ENDDO
            ENDDO

         ELSEIF( MODE .EQ. 2 ) THEN

*          output array is the sum of all the X,Z planes of the input array
            DO Z = 1, IDIMS3

               DO Y = 1, IDIMS2

                  DO X = 1, IDIMS1

                     ARROUT( X, Z ) = ARROUT( X, Z ) + ARRIN( X, Y, Z )
                  ENDDO
               ENDDO
            ENDDO

         ELSEIF( MODE .EQ. 3 ) THEN

*          output array is the sum of all the Y,Z planes of the input array
            DO Z = 1, IDIMS3

               DO Y = 1, IDIMS2

                  DO X = 1, IDIMS1

                     ARROUT( Y, Z ) = ARROUT( Y, Z ) + ARRIN( X, Y, Z )
                  ENDDO
               ENDDO
            ENDDO
         ELSE

*          value of MODE not allowed, set STATUS and report as error
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MODE', MODE )
            CALL ERR_REP( 'ERR_MA3TO2',
     :        'MODE = ^MODE not allowed in routine MA3TO2.', STATUS )

         ENDIF
      ENDIF

      END
