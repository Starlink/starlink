      SUBROUTINE SHIFTY( YNEG, YWHOLE, INTYS, FRACY, DIM1, DIM2, ARRIN,
     :                   ARROUT, STATUS )
*+
*  Name:
*     SHIFTY

*  Purpose:
*     Shifts the input array into the output array in the Y
*     direction

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*     CALL SHIFTY( YNEG, YWHOLE, INTYS, FRACY, DIM1, DIM2, ARRIN,
*    :             ARROUT, STATUS )

*  Description:
*     The output array, ARROUT, will contain the input array, ARRIN,
*     shifted in the Y direction. The following arguments determine how
*     the shift is performed:
*     YNEG   - if .true. then shift is a negative shift
*     YWHOLE - if .true. then shift is through a whoe number of pixels
*     INTYS  - integer pixel shift, this will also be the number of
*              columns of the output array which will be set to zeros.
*     FRACY  - fractional pixel shift, only used if YWHOLE is .FALSE.
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*     Undefined pixels are given the magic value.

*  Arguments:
*     YNEG = LOGICAL( READ )
*           Should be .TRUE. if shift is in the negative direction.
*     YWHOLE = LOGICAL( READ )
*           Should be .true. if shift is through a whole number of
*           pixels.
*     INTYS = INTEGER( READ )
*           Number of whole pixels through which the input array will be
*           shifted.
*     FRACY = REAL( READ )
*           Fractional part of the shift, will only be used if YWHOLE is
*           .FALSE.
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d arrays.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d arrays.
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
*              For all lines of output image up to line corresponding to
*                last line of input image
*                 For all pixels in line
*                    Output image pixel is shifted input image pixel
*                 Endfor
*              Endfor
*           Else
*              For all lines of output image from line corresponding to
*                first line in input image
*                 For all pixels in line
*                    Output image pixel is shifted input image pixel
*                 Endfor
*              Endfor
*           Endif
*        Else shift is fractional pixel shift
*           Calculate second interpolation weight as 1.0 - FRACY
*           If negative shift then
*              For all lines of output image up to line corresponding to
*                last line of input image
*                 For all pixels in line
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
*              For all lines of output image from line corresponding to
*                first line in input image
*                 For all pixels in line
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
*     19-AUG-1983 (ROE::ASOC5):
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
     :  INTYS

      LOGICAL
     :  YNEG,
     :  YWHOLE

      REAL
     :  ARRIN( DIM1, DIM2 ),
     :  FRACY

*  Arguments Returned:

      REAL
     :  ARROUT( DIM1, DIM2 )

*  Status:

      INTEGER STATUS

*  Local Variables:

      INTEGER
     :  X,        ! pointer to elements in input/output arrays, 1st dim.
     :  Y,        !    "     "    "      " output arrays, 2nd dimension
     :  Y1,       !    "     "    "      " input array for interpolation
     :  Y2        !    "     "    "      "   "     "    "        "

      REAL
     :  FRACY1    ! 1.0 - fractional pixel shift
*.

*    check for error on entry

      IF( STATUS .EQ. SAI__OK ) THEN

*       set all values in output array equal to the bad value

         CALL KPG1_FILLR( VAL__BADR, DIM1 * DIM2, ARROUT, STATUS )

         IF( YWHOLE ) THEN

            IF( YNEG ) THEN

*             shift in Y is a negative whole number of pixels
*             do all lines of output image up to line corresponding to
*             last line of input image

               DO  Y = 1, DIM2 - INTYS

*                do all points in line

                  DO  X = 1, DIM1

*                   output image pixel is shifted input image pixel

                     ARROUT( X, Y ) = ARRIN( X, Y+INTYS )
                  ENDDO
               ENDDO
            ELSE

*             shift in Y is a positive whole number of pixels
*             do all lines in output image from line corresponding to
*             first line in input image

               DO  Y = INTYS + 1, DIM2

*                do all points in line

                  DO  X = 1, DIM1

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
*             do all lines of output image up to line corresponding to
*             last line of input image

               DO  Y = 1, DIM2 - INTYS

*                set up interpolation limits

                  Y2 = Y + INTYS
                  Y1 = Y2 - 1

*                do all points in line

                  DO  X = 1, DIM1

*                   should either input pixel be invalid the output
*                   is invalid

                     IF ( ARRIN( X, Y1 ) .EQ. VAL__BADR .OR.
     :                    ARRIN( X, Y2 ) .EQ. VAL__BADR ) THEN

                        ARROUT( X, Y ) = VAL__BADR
                     ELSE

*                      output image pixel is interpolated from input
*                      image pixels

                        ARROUT( X, Y ) = ( FRACY1 * ARRIN( X, Y1 ) ) +
     :                                   ( FRACY * ARRIN( X, Y2 ) )
                     END IF
                  ENDDO
               ENDDO
            ELSE

*             shift in Y is a positive fractional pixel shift
*             do all lines in output image from line corresponding to
*             first line in input image

               DO  Y = INTYS + 1, DIM2

*                set up interpolation limits

                  Y1 = Y - INTYS
                  Y2 = Y1 + 1

*                do all points in line

                  DO  X = 1, DIM1

*                   should either input pixel be invalid the output
*                   is invalid

                     IF ( ARRIN( X, Y1 ) .EQ. VAL__BADR .OR.
     :                    ARRIN( X, Y2 ) .EQ. VAL__BADR ) THEN

                        ARROUT( X, Y ) = VAL__BADR
                     ELSE

*                      output image pixel is interpolated from input
*                      image pixels

                        ARROUT( X, Y ) = ( FRACY * ARRIN( X, Y1 ) ) +
     :                                   ( FRACY1 * ARRIN( X, Y2 ) )
                     END IF
                  ENDDO
               ENDDO

            ENDIF
         ENDIF
      ENDIF

      END
