*+  STRY - stretches the input array into the output array in the
*          Y-direction

      SUBROUTINE STRY( IDIM1, IDIM2, ARRIN, ODIM1, ODIM2, ARROUT, YPIX, 
     :                 YWT, STATUS )
*
*    Description :
*
*     The input array, ARRIN, is stretched in the Y ( second dimension )
*     direction by bi-linear interpolation into the output array,
*     ARROUT.
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*    Invocation :
*
*     CALL STRY( IDIM1, IDIM2, ARRIN, ODIM1, ODIM2, ARROUT, YPIX, YWT,
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
*     YPIX( ODIM2, 2 ) = INTEGER( WRITE )
*         Workspace array for pixel limits used in the interpolation.
*     YWT( ODIM2, 2 ) = REAL( WRITE )
*         Workspace array for weights corresponding to the pixel
*           limits.
*     STATUS = INTEGER( READ )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.
*
*    Method :
*
*     If no error on entry then
*        For second to last but one pixels in column of output image
*           Set up interpolation limits and weights
*        Endfor
*        Set first row of output image equal to first row of input image
*        Set last row of output image equal to last row of input image
*        For second to last but one rows of output image
*           For all pixels of row
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
     :  YPIX( ODIM2, 2 )

      REAL
     :  ARRIN( IDIM1, IDIM2 ),
     :  YWT( ODIM2, 2 )

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
     :  FACTY,                 ! Relation between input and output
                               ! arrays' pixels
     :  YPOS                   ! Position of output image pixel in input
                               ! array
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       define relationship between input and output image pixels such
*       that the position in input image = 
*          (FACTY*( position in output image- 1 )) + 1

         FACTY = REAL( IDIM2-1 ) / REAL( ODIM2-1 )

*       set up interpolation limits and weights for second to last but
*       one pixels of output image column

         DO  Y = 2, ODIM2-1

*          get position corresponding to output image pixel in input
*          image

            YPOS = ( FACTY * REAL( Y - 1 ) ) + 1.0

*          set interpolation limits to pixels either side of this
*          position

            YPIX( Y, 1 ) = INT( YPOS )
            YPIX( Y, 2 ) = YPIX( Y, 1 ) + 1

*          set up the interpolation weights

            YWT( Y, 2 ) = YPOS - REAL( YPIX( Y, 1 ) )
            YWT( Y, 1 ) = 1.0 - YWT( Y, 2 )
         END DO

*       do all columns of output image

         DO  X = 1, ODIM1

*          first of row for output image is first row of input image
*          last row of output image is last row of input image

            ARROUT( X, 1 ) = ARRIN( X, 1 )
            ARROUT( X, ODIM2 ) = ARRIN( X, IDIM2 )
         END DO

*       do second to last but one rows of output image

         DO Y = 2, ODIM2-1

*          do all points in row between second and last but one

            DO  X = 1, ODIM1

*              should both input-pixel limits be invalid then set output
*              pixel to be invalid also

                IF ( ARRIN ( X, YPIX( Y, 1 ) ) .EQ. VAL__BADR .AND.
     :               ARRIN ( X, YPIX( Y, 2 ) ) .EQ. VAL__BADR ) THEN

                   ARROUT( X, Y ) = VAL__BADR

                ELSE IF ( ARRIN ( X, YPIX( Y, 1 ) ) .EQ. VAL__BADR )
     :            THEN

*             left-hand input-pixel limit is bad so cannot interpolate
*             - use a constant right-hand pixel value for output pixel

                   ARROUT( X, Y ) = ARRIN ( X, YPIX( Y, 2 ) )

                ELSE IF ( ARRIN ( X, YPIX( Y, 2 ) ) .EQ. VAL__BADR )
     :            THEN

*             right-hand input-pixel limit is bad so cannot interpolate
*             - use a constant left-hand pixel value for output pixel

                   ARROUT( X, Y ) = ARRIN ( X, YPIX( Y, 1 ) )
                ELSE

*                output image point is interpolated from input image

                  ARROUT( X, Y ) =
     :                     ( YWT( Y, 1 ) * ARRIN( X, YPIX( Y, 1 ) ) ) +
     :                     ( YWT( Y, 2 ) * ARRIN( X, YPIX( Y, 2 ) ) )
               END IF
            END DO
         END DO
      END IF

      END
