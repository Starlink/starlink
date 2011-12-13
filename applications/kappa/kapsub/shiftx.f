      SUBROUTINE SHIFTX( XNEG, XWHOLE, INTXS, FRACX, DIM1, DIM2, ARRIN,
     :                   ARROUT, STATUS )
*+
*  Name:
*     SHIFTX

*  Purpose:
*     Input array is shifted into the output array in the X
*     direction

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*     CALL SHIFTX( XNEG, XWHOLE, INTXS, FRACX, DIM1, DIM2, ARRIN,
*    :             ARROUT, STATUS )

*  Description:
*     The output array, ARROUT, will contain the input array, ARRIN,
*     shifted in the X direction. The following arguments determine
*     how the shift is performed:
*     XNEG   - if .true. then shift is a negative shift
*     XWHOLE - if .true. then shift is through a whole number of pixels
*     INTXS  - integer pixel shift. This will also be the number of
*              columns of the output array which will be set to zeros.
*     FRACX  - fractional pixel shift, only used if XWHOLE is .FALSE.
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*     Undefined pixels are given the magic value.

*  Arguments:
*     XNEG = LOGICAL( READ )
*           Will be .TRUE. if shift is in the negative direction.
*     XWHOLE = LOGICAL( READ )
*           Will be .TRUE. if shift is a whole number of pixels.
*     INTXS = INTEGER( READ )
*           Number of whole pixels through which the input data will be
*           shifted.
*     FRACX = REAL( READ )
*           Fractional part of the pixel shift, will only be needed if
*           XWHOLE is set to .FALSE.
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d arrays.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d arrays.
*     DIM2 = INTEGER( READ )
*           Dimensions of the input and output arrays.
*     ARRIN( DIM1, DIM2 ) = REAL( READ )
*           Data to be shifted.
*     ARROUT( DIM1, DIM2 ) = REAL( WRITE )
*           Will contain the shifted data.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.

*  Algorithm:
*     If no error on entry then
*        Set all elements of output array equal to bad value
*        If shift is through whole number of pixels then
*           If negative shift then
*              For all lines of output image
*                 For all pixels in line up to pixel corresponding to
*                   last
*                   pixel in input image line
*                    Output image pixel is shifted input image pixel
*                 Endfor
*              Endfor
*           Else
*              For all lines of output image
*                 For all pixels in line from pixel corresponding to
*                   first
*                   pixel in input image line
*                    Output image pixel is shifted input image pixel
*                 Endfor
*              Endfor
*           Endif
*        Else shift is fractional pixel shift
*           Calculate second interpolation weight as 1.0 - FRACX
*           If negative shift then
*              For all lines of output image
*                 For all pixels in line up to pixel corresponding to
*                   last pixel in input image line
*                    Set up interpolation limits
*                    If either input pixel is invalid then
*                       Output image pixel is invalid
*                    Else
*                       Output image pixel is interpolated from input
*                         image pixels
*                    Endif
*                 Endfor
*              Endfor
*           Else
*              For all lines of output image
*                 For all pixels in line from pixel corresponding to
*                   first pixel in input image line
*                    Set up interpolation limits
*                    If either input pixel is invalid then
*                       Output image pixel is invalid
*                    Else
*                       Output image pixel is interpolated from input
*                         image pixels
*                    Endif
*                 Endfor
*              Endfor
*           Endif
*        Endif
*     Endif

*  Copyright:
*     Copyright (C) 1983-1984, 1986, 1989, 1992 Science & Engineering
*     Research Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1983 (ROE::ASOC5):
*        : Original version
*     19-FEB-1984 (ROE::ASOC5):
*        : Documentation brought up to standard
*     1986 Sep 9 : Renamed parameters section to arguments and tidied
*                  (RL.STAR::CUR).
*     1986 Oct 27: Added bad-pixel handling (RL.STAR::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*     1992 Aug 15: Initialise output array with bad values rather than
*                  zero (RL.STAR::CUR).
*     {enter_further_changes_here}

*-

*  Type Definitions:

      IMPLICIT NONE

*  Global Constants:

      INCLUDE  'SAE_PAR'      ! SSE global definitions
      INCLUDE 'PRM_PAR'       ! PRIMDAT public constants

*  Arguments Given:

      INTEGER
     :  DIM1, DIM2,
     :  INTXS

      REAL
     :  ARRIN( DIM1, DIM2 ),
     :  FRACX

      LOGICAL
     :  XNEG,
     :  XWHOLE

*  Arguments Returned:

      REAL
     :  ARROUT( DIM1, DIM2 )

*  Status:

      INTEGER STATUS

*  Local Variables:

      INTEGER
     :  X,          ! index to elements in output array, 1st dimension
     :  Y,          !   "    "     "     " input/output arrays, 2nd dim.
     :  X1,         !   "    "     "     " input array for interpolation
     :  X2          !   "    "     "     "   "     "    "        "

      REAL
     :  FRACX1      ! 1.0 - fractional pixel shift
*.

*    check for error on entry

      IF( STATUS .EQ. SAI__OK ) THEN

*       set all values in the output array to the bad value

         CALL KPG1_FILLR( VAL__BADR, DIM1 * DIM2, ARROUT, STATUS )

         IF( XWHOLE ) THEN

            IF( XNEG ) THEN

*             shift in X is a negative whole number of pixels
*             do all lines of output image

               DO  Y = 1, DIM2

*                do all pixels in line up to pixel corresponding to
*                last pixel of input image

                  DO  X = 1, DIM1 - INTXS

*                   output image pixel is shifted input image pixel

                     ARROUT( X, Y ) = ARRIN( X+INTXS, Y )
                  ENDDO
               ENDDO
            ELSE

*             shift in X is positive whole number of pixels
*             do all lines of output image

               DO  Y = 1, DIM2

*                do all pixels in line from pixel corresponding to first
*                pixel of input image

                  DO  X = INTXS + 1, DIM1

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
*             do all lines of output image

               DO  Y = 1, DIM2

*                do all pixels in line up to pixel corresponding to last
*                pixel in input image

                  DO  X = 1, DIM1 - INTXS

*                   set up pixel limits for interpolation

                     X2 = X + INTXS
                     X1 = X2 - 1

*                   should either input pixel be invalid the output
*                   is invalid

                     IF ( ARRIN( X1, Y ) .EQ. VAL__BADR .OR.
     :                    ARRIN( X2, Y ) .EQ. VAL__BADR ) THEN

                        ARROUT( X, Y ) = VAL__BADR
                     ELSE

*                      output image pixel is interpolated from input
*                      image  pixels

                        ARROUT( X, Y ) = ( FRACX1 * ARRIN( X1, Y ) ) +
     :                                   ( FRACX * ARRIN( X2, Y ) )
                     END IF
                  ENDDO
               ENDDO
            ELSE

*             shift in X is a positive fractional pixel shift
*             do all lines of output image

               DO  Y = 1, DIM2

*                do all pixels in line from pixel corresponding to first
*                pixel in input image

                  DO  X = INTXS + 1, DIM1

*                   set up interpolation limits

                     X1 = X - INTXS
                     X2 = X1 + 1

*                   should either input pixel be invalid the output
*                   is invalid

                     IF ( ARRIN( X1, Y ) .EQ. VAL__BADR .OR.
     :                    ARRIN( X2, Y ) .EQ. VAL__BADR ) THEN

                        ARROUT( X, Y ) = VAL__BADR
                     ELSE

*                      output image pixel is interpolated from input
*                      image

                        ARROUT( X, Y ) = ( FRACX * ARRIN( X1, Y ) ) +
     :                                   ( FRACX1 * ARRIN( X2, Y ) )
                     ENDIF
                  ENDDO
               ENDDO

            ENDIF
         ENDIF
      ENDIF

      END
