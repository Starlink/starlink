*+  SHIFTX - input array is shifted into the output array in the X direction
      SUBROUTINE SHIFTX( XNEG, XWHOLE, INTXS, FRACX, DIMSX, DIMSY,
     :  ARRIN, ARROUT, STATUS )
*    Description :
*     The output array, ARROUT, will contain the input array, ARRIN, shifted
*     in the X direction. The following arguments determine how the shift is
*     performed :
*     XNEG   - if .true. then shift is a negative shift
*     XWHOLE - if .true. then shift is through a whole number of pixels
*     INTXS  - integer pixel shift. This will also be the number of columns of
*              the output array which will be set to zeros.
*     FRACX  - fractional pixel shift, only used if XWHOLE is .FALSE.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL SHIFTX( XNEG, XWHOLE, INTXS, FRACX, DIMS, ARRIN, ARROUT, STATUS )
*    Parameters :
*     XNEG = LOGICAL( READ )
*           Will be .TRUE. if shift is in the negative direction.
*     XWHOLE = LOGICAL( READ )
*           Will be .TRUE. if shift is a whole number of pixels.
*     INTXS = INTEGER( READ )
*           Number of whole pixels through which the input data will be
*           shifted.
*     FRACX = REAL( READ )
*           Fractional part of the pixel shift, will only be needed if XWHOLE
*           is set to .FALSE.
*     DIMS( 2 ) = INETEGER( READ )
*           Dimensions of the input and output arrays.
*     ARRIN( DIMS(1), DIMS(2) ) = REAL( READ )
*           Data to be shifted.
*     ARROUT( DIMS(1), DIMS(2) ) = REAL( WRITE )
*           Will contain the shifted data.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur.
*    Method :
*     If no error on entry then
*        Set all elements of output array equal to zero
*        If shift is through whole number of pixels then
*           If negative shift then
*              For all rows of output image
*                 For all pixels in row up to pixel corresponding to last
*                   pixel in input image row
*                    Output image pixel is shifted input image pixel
*                 Endfor
*              Endfor
*           Else
*              For all rows of output image
*                 For all pixels in row from pixel corresponding to first
*                   pixel in input image row
*                    Output image pixel is shifted input image pixel
*                 Endfor
*              Endfor
*           Endif
*        Else shift is fractional pixel shift
*           Calculate second interpolation weight as 1.0 - FRACX
*           If negative shift then
*              For all rows of output image
*                 For all pixels in row up to pixel corresponding to last
*                   pixel in input image row
*                    Set up interpolation limits
*                    Output image pixel is interpolated from input image pixels
*                 Endfor
*              Endfor
*           Else
*              For all rows of output image
*                 For all pixels in row from pixel corresponding to first
*                   pixel in input image row
*                    Set up interpolation limits
*                    Output image pixel is interpolated from input image pixels
*                 Endfor
*              Endfor
*           Endif
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     18/08/1983 : Original version                     (ROE::ASOC5)
*     19/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  DIMSX, ! dimensions of input/output arrays
     :  DIMSY, ! dimensions of input/output arrays
     :  INTXS      ! integer pixel shift
      REAL
     :  ARRIN( DIMSX, DIMSY ),  ! input array
     :  FRACX                       ! fractional pixel shift
      LOGICAL
     :  XNEG,   ! .true. if negative shift
     :  XWHOLE  ! .true. if shift through whole number of pixels
*    Export :
      REAL
     :  ARROUT( DIMSX, DIMSY ) ! output array
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  X,  ! index to elements in output array, 1st dimension
     :  Y,  !   "    "     "     " input/output arrays, 2nd dim.
     :  X1, !   "    "     "     " input array for interpolation
     :  X2  !   "    "     "     "   "     "    "        "
      REAL
     :  FRACX1 ! 1.0 - fractional pixel shift
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       set all values in the output array to zero
         CALL ZERO2D( DIMSX, DIMSY, ARROUT, STATUS )

         IF( XWHOLE ) THEN

            IF( XNEG ) THEN

*             shift in X is a negative whole number of pixels
*             do all rows of output image
               DO Y = 1, DIMSY

*                do all pixels in row up to pixel corresponding to last pixel
*                of input image
                  DO X = 1, DIMSX-INTXS

*                   output image pixel is shifted input image pixel
                     ARROUT( X, Y ) = ARRIN( X+INTXS, Y )
                  ENDDO
               ENDDO
            ELSE

*             shift in X is positive whole number of pixels
*             do all rows of output image
               DO Y = 1, DIMSY

*                do all pixels in row from pixel corresponding to first pixel
*                of input image
                  DO X = INTXS+1, DIMSX

*                   output image pixel is shifted input image pixel
                     ARROUT( X, Y ) = ARRIN( X-INTXS, Y )
                  ENDDO
               ENDDO
            ENDIF
         ELSE

*          calculate second interpolation weight
            FRACX1 = 1.0 - FRACX

            IF( XNEG ) THEN

*             shift in X is a negative fractional pixel shift
*             do all rows of output image
               DO Y = 1, DIMSY

*                do all pixels in row up to pixel corresponding to last pixel
*                in input image
                  DO X = 1, DIMSX-INTXS

*                   set up pixel limits for interpolation
                     X2 = X + INTXS
                     X1 = X2 - 1

*                   output image pixel is interpolated from input image pixels
                     ARROUT( X, Y ) = ( FRACX1 * ARRIN( X1, Y ) ) +
     :                 ( FRACX * ARRIN( X2, Y ) )
                  ENDDO
               ENDDO
            ELSE

*             shift in X is a positive fractional pixel shift
*             do all rows of output image
               DO Y = 1, DIMSY

*                do all pixels in row from pixel corresponding to first pixel
*                in input image
                  DO X = INTXS+1, DIMSX

*                   set up interpolation limits
                     X1 = X - INTXS
                     X2 = X1 + 1

*                   output image pixel is interpolated from input image
                     ARROUT( X, Y ) = ( FRACX * ARRIN( X1, Y ) ) +
     :                 ( FRACX1 * ARRIN( X2, Y ) )
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
      ENDIF

      END
