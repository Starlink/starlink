*+  STRX - stretches the input array into the output array in the X-direction
      SUBROUTINE STRX( IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2, ARROUT,
     :                 XPIX, XWT, STATUS )
*    Description :
*     The input array, ARRIN, is stretched in the X ( first dimension )
*     direction by bi-linear interpolation into the output array, ARROUT.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL STRX( IDIMS, ARRIN, ODIMS, ARROUT, XPIX, XWT, STATUS )
*    Parameters :
*     IDIMS( 2 ) = INTEGER( READ )
*           Dimensions of input array ARRIN.
*     ARRIN( IDIMS(1), IDIMS(2) ) = REAL( READ )
*           Array of data which will be interpolated to produce stretched
*           output array.
*     ODIMS( 2 ) = INTEGER( READ )
*           Dimensions of the output array.
*     ARROUT( ODIMS(1), ODIMS(2) ) = REAL( WRITE )
*           Array to contain stretched version of the input array.
*     XPIX( ODIMS(1), 2 ) = INTEGER( WRITE )
*           Workspace array for pixel limits used in the interpolation.
*     XWT( ODIMS(1), 2 ) = REAL( WRITE )
*           Workspace array for weights corresponding to the pixel limits.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur.
*    Method :
*     If no error on entry then
*        For second to last but one pixels in row of output image
*           Set up interpolation limits and weights
*        Endfor
*        For all rows of output image
*           First point of row in output image is first point of row in
*             input image
*           For second to last but one pixels of row of output image
*              Output image pixel is interpolated from input image
*                using calculated pixel limits and weights
*           Endfor
*           Last point of row in output image is last point of row in
*             input image
*        Endfor
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     18/08/1983 : Original version                 (ROE::ASOC5)
*     19/02/1984 : Documentation brought up to date (ROE::ASOC5)
*     12-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS1, IDIMS2, ! dimensions of input array
     :  ODIMS1, ODIMS2, !      "      " output array
     :  XPIX( ODIMS1, 2 )  ! array of pixel limits for interpolation
      REAL
     :  ARRIN( IDIMS1, IDIMS2 ),  ! input array
     :  XWT( ODIMS1, 2 )            ! array of weights for interpolation
*    Export :
      REAL
     :  ARROUT( ODIMS1, ODIMS2 ) ! output array
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  X,    ! index to output array element, 1st dimension
     :  Y     !   "    "    "     "      "   , 2nd     "
      REAL
     :  FACTX, ! relation between input and output image pixels
     :  XPOS   ! position of output image pixel in input image
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       define relaton between input and output image pixels such that
*       position in input image = (FACTX*( position in output image - 1 )) + 1
         FACTX = REAL( IDIMS1-1 ) / REAL( ODIMS1-1 )

*       set up interpolation limits and weights for second to last but one
*       pixels of output image row
         DO X = 2, ODIMS1-1

*          get position corresponding to output image pixel in input image
            XPOS = ( FACTX * REAL( X - 1 ) ) + 1.0

*          set interpolation limits to pixels either side of this position
            XPIX( X, 1 ) = INT( XPOS )
            XPIX( X, 2 ) = XPIX( X, 1 ) + 1

*          set up the interpolation weights
            XWT( X, 2 ) = XPOS - REAL( XPIX( X, 1 ) )
            XWT( X, 1 ) = 1.0 - XWT( X, 2 )
         ENDDO

*       do all rows of output image
         DO Y = 1, ODIMS2

*          first pixel of row for output image is first pixel of row in
*          input image
            ARROUT( 1, Y ) = ARRIN( 1, Y )

*          do all points in row between second and last but one
            DO X = 2, ODIMS1-1

*             output image point is interpolated from input image
               ARROUT( X, Y ) =
     :           ( XWT( X, 1 ) * ARRIN( XPIX( X, 1 ), Y ) ) +
     :           ( XWT( X, 2 ) * ARRIN( XPIX( X, 2 ), Y ) )
            ENDDO

*          last pixel of row for output image is last pixel of row in
*          input image
            ARROUT( ODIMS1, Y ) = ARRIN( IDIMS1, Y )
         ENDDO
      ENDIF

      END
