*+  STRX - stretches the input array into the output array in the
*          X-direction

      SUBROUTINE STRX( IDIM1, IDIM2, ARRIN, ODIM1, ODIM2, ARROUT, XPIX, 
     :                 XWT, STATUS )
*
*    Description :
*
*     The input array, ARRIN, is stretched in the X ( first dimension )
*     direction by bi-linear interpolation into the output array,
*     ARROUT.
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*    Invocation :
*
*     CALL STRX( IDIM1, IDIM2, ARRIN, ODIM1, ODIM2, ARROUT, XPIX, XWT,
*                STATUS )
*
*    Arguments :
*
*     IDIM1 = INTEGER( READ )
*         The first dimension of the input 2-d array.
*     IDIM2 = INTEGER( READ )
*         The second dimension of the input 2-d array.
*     ARRIN( IDIM1, IDIM2 ) = REAL( READ )
*         Array of data which will be interpolated to produce
*           stretched output array.
*     ODIM1 = INTEGER( READ )
*         The first dimension of the output 2-d array.
*     ODIM2 = INTEGER( READ )
*         The second dimension of the output 2-d array.
*     ARROUT( ODIM1, ODIM2 ) = REAL( WRITE )
*         Array to contain stretched version of the input array.
*     XPIX( ODIM1, 2 ) = INTEGER( WRITE )
*         Workspace array for pixel limits used in the interpolation.
*     XWT( ODIM1, 2 ) = REAL( WRITE )
*         Workspace array for weights corresponding to the pixel
*           limits.
*     STATUS = INTEGER( READ )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.
*
*    Method :
*
*     If no error on entry then
*        For second to last but one pixels in row of output image
*           Set up interpolation limits and weights
*        Endfor
*        For all rows of output image
*           First point of row in output image is first point of row in
*             input image
*           For second to last but one pixels of row of output image
*              If both input-pixel limits are invalid then
*                 Set output pixel to be invalid
*              Else if left input-pixel limit is invalid
*                 Set output pixel to be the right-hand limiting pixel 
*                   value
*              Else if right input-pixel limit is invalid
*                 Set output pixel to be the left-hand limiting pixel 
*                   value
*              Else
*                 Output image pixel is interpolated from input image
*                   using calculated pixel limits and weights
*              Endif
*           Endfor
*           Last point of row in output image is last point of row in
*             input image
*        Endfor
*     Endif
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR)
*
*    History :
*
*     18/08/1983 : Original version                 (ROE::ASOC5)
*     19/02/1984 : Documentation brought up to date (ROE::ASOC5)
*     1986 Sep 10: Renamed parameters section to arguments, added
*                  invalid-pixel handling and tidied (RL.STAR::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE  'SAE_PAR'      ! SSE global definitions
      INCLUDE 'PRM_PAR'       ! PRIMDAT public constants

*    Import :

      INTEGER
     :  IDIM1, IDIM2,
     :  ODIM1, ODIM2,
     :  XPIX( ODIM1, 2 )

      REAL
     :  ARRIN( IDIM1, IDIM2 ),
     :  XWT( ODIM1, 2 )

*    Export :

      REAL
     :  ARROUT( ODIM1, ODIM2 )

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  X,                     ! Index to output array element, 1st dim
     :  Y                      !   "    "    "     "      "   , 2nd   "

      REAL
     :  FACTX,                 ! Relation between input and output
                               ! arrays' pixels
     :  XPOS                   ! Position of output image pixel in input
                               ! array
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       define relationship between input and output image pixels such
*       that the position in input image = 
*          (FACTX*( position in output image- 1 )) + 1

         FACTX = REAL( IDIM1-1 ) / REAL( ODIM1-1 )

*       set up interpolation limits and weights for second to last but
*       one pixels of output image row

         DO  X = 2, ODIM1-1

*          get position corresponding to output image pixel in input
*          image

            XPOS = ( FACTX * REAL( X - 1 ) ) + 1.0

*          set interpolation limits to pixels either side of this
*          position

            XPIX( X, 1 ) = INT( XPOS )
            XPIX( X, 2 ) = XPIX( X, 1 ) + 1

*          set up the interpolation weights

            XWT( X, 2 ) = XPOS - REAL( XPIX( X, 1 ) )
            XWT( X, 1 ) = 1.0 - XWT( X, 2 )
         END DO

*       do all rows of output image

         DO  Y = 1, ODIM2

*          first pixel of row for output image is first pixel of row in
*          input image

            ARROUT( 1, Y ) = ARRIN( 1, Y )

*          do all points in row between second and last but one

            DO  X = 2, ODIM1-1

*              should both input-pixel limits be invalid then set output
*              pixel to be invalid also

                IF ( ARRIN ( XPIX( X, 1 ), Y ) .EQ. VAL__BADR .AND.
     :               ARRIN ( XPIX( X, 2 ), Y ) .EQ. VAL__BADR ) THEN

                   ARROUT( X, Y ) = VAL__BADR

                ELSE IF ( ARRIN ( XPIX( X, 1 ), Y ) .EQ. VAL__BADR )
     :            THEN

*             left-hand input-pixel limit is bad so cannot interpolate
*             - use a constant right-hand pixel value for output pixel

                   ARROUT( X, Y ) = ARRIN ( XPIX( X, 2 ), Y )

                ELSE IF ( ARRIN ( XPIX( X, 2 ), Y ) .EQ. VAL__BADR )
     :            THEN

*             right-hand input-pixel limit is bad so cannot interpolate
*             - use a constant left-hand pixel value for output pixel

                   ARROUT( X, Y ) = ARRIN ( XPIX( X, 1 ), Y )
                ELSE

*                output image point is interpolated from input image

                  ARROUT( X, Y ) =
     :                     ( XWT( X, 1 ) * ARRIN( XPIX( X, 1 ), Y ) ) +
     :                     ( XWT( X, 2 ) * ARRIN( XPIX( X, 2 ), Y ) )
               END IF
            END DO

*          last pixel of row for output image is last pixel of row in
*          input image

            ARROUT( ODIM1, Y ) = ARRIN( IDIM1, Y )
         END DO
      END IF

      END
