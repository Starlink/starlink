*+  SHIFTY - shifts the input array into the output array in the Y-direction
      SUBROUTINE SHIFTY( YNEG, YWHOLE, INTYS, FRACY, DIMSX, DIMSY,
     :  ARRIN, ARROUT, STATUS )
*    Description :
*     The output array, ARROUT, will contain the input array, ARRIN, shifted
*     in the Y direction. The following arguments determine how the shift is
*     performed :
*     YNEG   - if .true. then shift is a negative shift
*     YWHOLE - if .true. then shift is through a whoe number of pixels
*     INTYS  - integer pixel shift, this will also be the number of columns
*              of the output array which will be set to zeros.
*     FRACY  - fractional pixel shift, only used if YWHOLE is .FALSE.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL SHIFTY( YNEG, YWHOLE, INTYS, FRACY, DIMS, ARRIN, ARROUT, STATUS )
*    Parameters :
*     YNEG = LOGICAL( READ )
*           Should be .TRUE. if shift is in the negative direction.
*     YWHOLE = LOGICAL( READ )
*           Should be .true. if shift is through a whole number of pixels.
*     INTYS = INTEGER( READ )
*           Number of whole pixels through which the input array will be
*           shifted.
*     FRACY = REAL( READ )
*           Fractional part of the shift, will only be used if YWHOLE is
*           .FALSE.
*     DIMS( 2 ) = INTEGER( READ )
*           Dimensions of input and output arrays.
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
*              For all rows of output image up to row corresponding to
*                last row of input image
*                 For all pixels in row
*                    Output image pixel is shifted input image pixel
*                 Endfor
*              Endfor
*           Else
*              For all rows of output image from row corresponding to
*                first row in input image
*                 For all pixels in row
*                    Output image pixel is shifted input image pixel
*                 Endfor
*              Endfor
*           Endif
*        Else shift is fractional pixel shift
*           Calculate second interpolation weight as 1.0 - FRACY
*           If negative shift then
*              For all rows of output image up to row corresponding to
*                last row of input image
*                 For all pixels in row
*                    Set up interpolation limits
*                    Output image pixel is interpolated from input image pixels
*                 Endfor
*              Endfor
*           Else
*              For all rows of output image from row corresponding to
*                first row in input image
*                 For all pixels in row
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
*     19/08/1983 : Original version                     (ROE::ASOC5)
*     19/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  DIMSX, ! dimensions of input/output arrays
     :  DIMSY, ! dimensions of input/output arrays
     :  INTYS      ! integer pixel shift
      LOGICAL
     :  YNEG,  ! .true. if negative shift
     :  YWHOLE ! .true. if shift through a whole number of pixels
      REAL
     :  ARRIN( DIMSX, DIMSY ), ! input array
     :  FRACY                      ! fractional pixel shift
*    Export :
      REAL
     :  ARROUT( DIMSX, DIMSY ) ! output array
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  X,  ! pointer to elements in input/output arrays, 1st dim.
     :  Y,  !    "     "    "      " output arrays, 2nd dimension
     :  Y1, !    "     "    "      " input array for interpolation
     :  Y2  !    "     "    "      "   "     "    "        "
      REAL
     :  FRACY1 ! 1.0 - fractional pixel shift
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       set all values in output array equal to zero
         CALL ZERO2D( DIMSX, DIMSY, ARROUT, STATUS )

         IF( YWHOLE ) THEN

            IF( YNEG ) THEN

*             shift in Y is a negative whole number of pixels
*             do all rows of output image up to row corresponding to last
*             row of input image
               DO Y = 1, DIMSY-INTYS

*                do all points in row
                  DO X = 1, DIMSX

*                   output image pixel is shifted input image pixel
                     ARROUT( X, Y ) = ARRIN( X, Y+INTYS )
                  ENDDO
               ENDDO
            ELSE

*             shift in Y is a positive whole number of pixels
*             do all rows in output image from row corresponding to first
*             row in input image
               DO Y = INTYS+1, DIMSY

*                do all points in row
                  DO X = 1, DIMSX

*                   output image pixel is shifted input image pixel
                     ARROUT( X, Y ) = ARRIN( X, Y-INTYS )
                  ENDDO
               ENDDO
            ENDIF
         ELSE

*          calculate second interpolation weight
            FRACY1 = 1.0 - FRACY

            IF( YNEG ) THEN

*             shift in Y is a negative fractional pixel shift
*             do all rows of output image up to row corresponding to last
*             row of input image
               DO Y = 1, DIMSY-INTYS

*                set up interpolation limits
                  Y2 = Y + INTYS
                  Y1 = Y2 - 1

*                do all points in row
                  DO X = 1, DIMSX

*                   output image pixel is interpolated from input image pixels
                     ARROUT( X, Y ) = ( FRACY1 * ARRIN( X, Y1 ) ) +
     :                 ( FRACY * ARRIN( X, Y2 ) )
                  ENDDO
               ENDDO
            ELSE

*             shift in Y is a positive fractional pixel shift
*             do all rows in output image from row corresponding to first
*             row in input image
               DO Y = INTYS+1, DIMSY

*                set up interpolation limits
                  Y1 = Y - INTYS
                  Y2 = Y1 + 1

*                do all points in row
                  DO X = 1, DIMSX

*                   output image pixel is interpolated from input image pixels
                     ARROUT( X, Y ) = ( FRACY * ARRIN( X, Y1 ) ) +
     :                 ( FRACY1 * ARRIN( X, Y2 ) )
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
      ENDIF

      END
