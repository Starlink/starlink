*+  MA3TO1 - Subroutine to extract a 1-D array from a 3-D array
      SUBROUTINE MA3TO1( MODE, IDIMS1, IDIMS2, IDIMS3, ARRIN, ODIMS,
     :                   ARROUT, STATUS )
*    Description :
*     The 1-dimensional output array, ARROUT, is extracted from the
*     3-dimensional input array, ARRIN, according to the value of MODE.
*     For values of MODE of 1,2 and 3  each point in the output array will
*     be formed from the sum of all the points in the input array with the
*     first, second and third dimension index respectively given by the index
*     to the output array point.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL MA3TO1( MODE, IDIMS, ARRIN, ODIMS, ARROUT, STATUS )
*    Parameters :
*     MODE = INTEGER( READ )
*           If MODE is 1 each point in the output 1-D array is formed from the
*           sum over the Y and Z axes of the corresponding points of the input
*           3-D arrays X-axis.
*           If MODE is 2 each point in the output 1-D array is formed from the
*           sum over the X and Z axes of the corresponding points of the input
*           3-D arrays Y-axis.
*           If MODE is 3 each point in the output 1-D array is formed from the
*           sum over the X and Y axes of the corresponding points of the input
*           3-D arrays Z-axis.
*     IDIMS( DAT__MXDIM ) = INTEGER( READ )
*           Contains the dimensions of the input array.
*     ARRIN( IDIMS(1), IDIMS(2), IDIMS(3) ) = REAL( READ )
*           Data to be summed to create output array.
*     ODIMS( DAT__MXDIM ) = INTEGER( READ )
*           Contains the dimension of the output array.
*     ARROUT( ODIMS(1) ) = REAL( WRITE )
*           Data generated from input array according to the value of MODE.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value
*           on entry then an immediate return will occur. If MODE has a value
*           other than 1,2 or 3 then STATUS will be set to SAI__ERROR and an
*           error reported.
*    Method :
*     If no error on entry then
*        Set all elements of output array to zero
*        If MODE is 1 then
*           For all planes of the input array
*              For all lines of a plane
*                 For all points of a line
*                     The value of the input array point is added to the value
*                       of the output array point indexed by the input array
*                       points first dimension index.
*                 Endfor
*              Endfor
*           Endfor
*        Else if MODE is 2 then
*           For all planes of the input array
*              For all lines of a plane
*                 For all points of a line
*                     The value of the input array point is added to the value
*                       of the output array point indexed by the input array
*                       points second dimension index.
*                 Endfor
*              Endfor
*           Endfor
*        Else if MODE is 3 then
*           For all planes of the input array
*              For all lines of a plane
*                 For all points of a line
*                     The value of the input array point is added to the value
*                       of the output array point indexed by the input array
*                       points third dimension index.
*                 Endfor
*              Endfor
*           Endfor
*        Else
*           Value of MODE not allowed so set STATUS to SAI__ERROR and report
*             an error
*        End if
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     28/10/1983 : Original version                     (ROE::ASOC5)
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
     :  IDIMS1, ! input array dimensions
     :  IDIMS2, ! input array dimensions
     :  IDIMS3, ! input array dimensions
     :  ODIMS, ! output  "       "
     :  MODE ! determines how extraction is carried out
      REAL
     :  ARRIN( IDIMS1, IDIMS2, IDIMS3 ) ! input array
*    Export :
      REAL
     :  ARROUT( ODIMS ) ! output array
*    Status :
      INTEGER STATUS
*    External references :

*    Local variables :
      INTEGER
     :  X,    ! index to input array elements, 1st dimension
     :  Y,    !   "    "   "     "       "     2nd     "
     :  Z     !   "    "   "     "       "     3rd     "
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       set all elements of the output array to zero
         CALL ZERO1D( ODIMS, ARROUT, STATUS )

         IF( MODE .EQ. 1 ) THEN

*          create output array by summation of all first dimension points over
*          the other two dimensions
            DO Z = 1, IDIMS3

               DO Y = 1, IDIMS2

                  DO X = 1, IDIMS1

                     ARROUT( X ) = ARROUT( X ) + ARRIN( X, Y, Z )
                  ENDDO
               ENDDO
            ENDDO

         ELSEIF( MODE .EQ. 2 ) THEN

*          create output array by summation of all second dimension points over
*          the other two dimensions
            DO Z = 1, IDIMS3

               DO Y = 1, IDIMS2

                  DO X = 1, IDIMS1

                     ARROUT( Y ) = ARROUT( Y ) + ARRIN( X, Y, Z )
                  ENDDO
               ENDDO
            ENDDO

         ELSEIF( MODE .EQ. 3 ) THEN

*          create output array by summation of all third dimension points over
*          the other two dimensions
            DO Z = 1, IDIMS3

               DO Y = 1, IDIMS2

                  DO X = 1, IDIMS1

                     ARROUT( Z ) = ARROUT( Z ) + ARRIN( X, Y, Z )
                  ENDDO
               ENDDO
            ENDDO

         ELSE

*          value of MODE not allowed so set STATUS and report error
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MODE', MODE )
            CALL ERR_REP( 'ERR_MA3TO1',
     :        'MODE = ^MODE not allowed in routine MA3TO1', STATUS )

         ENDIF
      ENDIF

      END
