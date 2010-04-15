*+  STRY - stretches the input array into the output array in the Y-direction
      SUBROUTINE STRY( IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2, ARROUT,
     :                 YPIX, YWT, STATUS )
*    Description :
*     The input array, ARRIN, is stretched in the Y ( second dimension )
*     direction by bi-linear interpolation into the output array, ARROUT.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL STRY( IDIMS, ARRIN, ODIMS, ARROUT, YPIX, YWT, STATUS )
*    Parameters :
*     IDIMS( 2 ) = INTEGER( READ )
*           Dimensions of the input array ARRIN.
*     ARRIN( IDIMS(1), IDIMS(2) ) = REAL( READ )
*           Array of data which will be interpolated to produce stretched
*           output array.
*     ODIMS( 2 ) = INTEGER( READ )
*           Dimensions of the output array ARROUT.
*     ARROUT( ODIMS(1), ODIMS(2) ) = REAL( WRITE )
*           Array to hold stretched version of input data.
*     YPIX( ODIMS(2), 2 ) = INTEGER( WRITE )
*           Workspace array for pixel limits used in interpolation.
*     YWT( ODIMS(2), 2 ) = REAL( WRITE )
*           Workspace array for weights corresponding to pixel limits.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value on
*           then an immediate return will occur.
*    Method :
*     If no error on entry then
*        For second to last but one pixels in column of output image
*           Set up interpolation limits and weights
*        Endfor
*        Set first row of output image equal to first row of input image
*        Set last row of output image equal to last row of input image
*        For second to last but one rows of output image
*           For all pixels of row
*              Output image pixel is interpolated from input image pixels
*                using calculated pixel limits and weights
*           Endfor
*        Endfor
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     18/08/1983 : Original version                     (ROE::ASOC5)
*     19/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     12-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS1, IDIMS2, ! dimensions of input array
     :  ODIMS1, ODIMS2, !      "      " output array
     :  YPIX( ODIMS2, 2 ) ! array of pixel limits for interpolation
      REAL
     :  ARRIN( IDIMS1, IDIMS2 ),  ! input array
     :  YWT( ODIMS2, 2 )            ! array of weights for interpolation
*    Export :
      REAL
     :  ARROUT( ODIMS1, ODIMS2 ) ! output array
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  X,    ! index to output array element, 1st dimension
     :  Y     !   "    "   "      "      "   , 2nd     "
      REAL
     :  FACTY, ! relation between output and input image pixels
     :  YPOS   ! position of output image pixel in input image
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       set up relation between input and output image pixel positions such
*       that position in input image = (FACTY*(position in output image-1))+1
         FACTY = REAL( IDIMS2 - 1 ) / REAL( ODIMS2 - 1 )

*       set up interpolation limits and weights for the second to last but one
*       pixels of output image row
         DO Y  = 2, ODIMS2-1

*          get position of output image pixel in input image
            YPOS = FACTY * REAL( Y - 1 ) + 1.0

*          set interpolation limits to pixels either side of this position
            YPIX( Y, 1 ) = INT( YPOS )
            YPIX( Y, 2 ) = YPIX( Y, 1 ) + 1

*          set up interpolation weights
            YWT( Y, 2 ) = YPOS - REAL( YPIX( Y, 1 ) )
            YWT( Y, 1 ) = 1.0 - YWT( Y, 2 )
         ENDDO

         DO X = 1, ODIMS1

*          first row of output image is first row of input image
            ARROUT( X, 1 ) = ARRIN( X, 1 )

*          last row of output image is last row of input image
            ARROUT( X, ODIMS2 ) = ARRIN( X, IDIMS2 )
         ENDDO

*       do second to last but one rows of output image
         DO Y = 2, ODIMS2-1

*          do all points of row
            DO X = 1, ODIMS1

*             output image pixel is interpolated from input image pixels
               ARROUT( X, Y ) =
     :           ( YWT( Y, 1 ) * ARRIN( X, YPIX( Y, 1 ) ) ) +
     :           ( YWT( Y, 2 ) * ARRIN( X, YPIX( Y, 2 ) ) )
            ENDDO
         ENDDO
      ENDIF

      END
