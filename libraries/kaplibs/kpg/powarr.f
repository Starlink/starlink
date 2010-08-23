      SUBROUTINE POWARR ( INARR, DIMS, POWER, OUTARR, STATUS )
*+
*  Name:
*     POWARR

*  Purpose:
*     raise an array, pixel by pixel, to a given power.

*  Language:
*     Starlink

*  Invocation:
*     CALL POWARR( INARR, DIMS, POWER, OUTARR, STATUS )

*  Description:
*     This routine fills the output array pixels with the results
*     of raising the pixels of the input array to the power
*     specified, i.e. New value = Old value ** Power.
*     If the result is bigger than the maximum allowed Vax real,
*     then a bad pixel value is used. If the result is smaller than
*     minimum allowed real set output pixel to be zero. (This underflow
*     check is not necessary on some machines, e.g. Vax.)
*     Negative input values are also converted to a bad pixel value
*     in the output.

*  Arguments:
*     INARR( DIMS )  =  REAL( READ )
*        Array containing input image data
*     DIMS  =  INTEGER( READ )
*        Dimension of input and output arrays
*     POWER  =  REAL( READ )
*        Power to be used
*     OUTARR( DIMS )  =  REAL( WRITE )
*        Array containing results of raising input data to the given power
*     STATUS  =  INTEGER( READ, WRITE )
*        Global status value

*  Algorithm:
*     Check for error on entry - if not o.k. return immediately
*     If input power is less than negative unity then
*        Very small pixel values may cause us to overflow - work out
*         minimum limit and set large checking flag false
*        Compute maximum limit to prevent underflow
*     Else input power is greater than unity
*        Large positive pixels may cause us to overflow - work out
*         maximum limit and set large checking flag true
*        Compute minimum limit to prevent underflow
*     Else
*        Minimum and maximum limits are the real-number range limits
*     Endif
*     If large checking then
*        For all pixels of the output array
*           If Inarray pixel has value greater than specified
*             maximum or it is invalid then
*              Outarray pixel = Bad value
*           Else if Inarray pixel is negative
*              If power is even then
*                 Outarray pixel = |Inarray pixel| ** Power
*              Else
*                 Outarray pixel = Bad value
*              Endif
*           Else if inarray pixel value lower than specified minimum
*              Outarray pixel = 0.0
*           Else
*              Outarray pixel = Inarray pixel ** Power
*           Endif
*        Endfor
*     Else small checking required
*        For all pixels of the output array
*           If Inarray pixel or has value less than specified
*             minimum or it is invalid then
*              Outarray pixel = Bad value
*           Else if Inarray pixel is negative
*              If power is even then
*                 Outarray pixel = |Inarray pixel| ** Power
*              Else
*                 Outarray pixel = Bad value
*              Endif
*           Else if inarray pixel value greater than specified maximum
*              Outarray pixel = 0.0
*           Else
*              Outarray pixel = Inarray pixel ** Power
*           Endif
*        Endfor
*     Endif
*     Return

*  Copyright:
*     Copyright (C) 1986, 1988 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*     {enter_new_authors_here}

*  History:
*     03-07-1986 :  First implementation (REVA::MJM)
*     1986 Aug 12:  Renamed from POWARR2D, generalised to any array,
*        completed prologue and nearly conformed to
*        Starlink standards (RL.STAR::CUR).
*     1986 Sep 2 :  Renamed parameters -> arguments section in prologue,
*        corrected bad-pixel handling and tidied
*        (RL.CUR::STAR).
*     1986 Oct 27:  Extended the protection to prevent underflows and
*        overflows when the power lies between -1 and 1
*        (RL.CUR::STAR).
*     1988 May 24:  Allow for raising negative numbers to even powers
*        (RL.CUR::STAR).
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE          ! no implicit typing allowed


*  Global Constants:
      INCLUDE  'SAE_PAR'      ! SSE global definitions
      INCLUDE 'PRM_PAR'       ! PRIMDAT public constants


*  Arguments Given:
      INTEGER
     :    DIMS

      REAL
     :    INARR( DIMS ),
     :    POWER


*  Arguments Returned:
      REAL
     :    OUTARR( DIMS )


*  Status:
      INTEGER  STATUS


*  Local Variables:
      INTEGER
     :    J                   ! counter variable

      REAL
     :    MAXVAL,             ! maximum valid pixel value allowed
     :    MINVAL,             ! minimum   "     "     "      "
     :    X                   ! work variable

      LOGICAL                 ! true if :
     :    LRGCHK              ! we are in danger from large pixel values
                              ! as opposed to small ones


*.

*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

      X = LOG( ABS( VAL__BADR ) )

*    work out pixel value checking limits according to input value of
*    power

      IF ( POWER .LT. -1.0 ) THEN

*       when the power is negative, the danger pixel values are small
*       positive ones - work out the limit

         MINVAL  =  EXP( X / POWER )

*       find the maximum value to prevent underflows

         MAXVAL  =  EXP( -X / POWER )

*       set large checking flag false, i.e. we will be checking for
*       numbers which are too small

         LRGCHK  =  .FALSE.

      ELSE IF ( POWER .GT. 1.0 ) THEN

*       large positive pixel values will cause problems - work out limit

         MAXVAL  =  EXP( X / POWER )

*       find the minimum value to prevent underflows

         MINVAL  =  EXP( -X / POWER )

*       set large checking flag true

         LRGCHK  =  .TRUE.

*    else power is zero

      ELSE

*       any pixel value is o.k.

         MAXVAL  =  VAL__MAXR
         MINVAL  =  VAL__MINR

         LRGCHK  =  .TRUE.

*    end of if-power-value-less-than-one check

      END IF


*    depending on value of limit checking flag continue

      IF ( LRGCHK ) THEN

*       we will be checking for pixel values larger than the calculated
*       positive maximum
*       now loop round all pixels of the output array

         DO  J  =  1, DIMS

*          check pixel value for an value greater than the
*          specified maximum or for an undefined value

            IF ( INARR( J ) .GE. MAXVAL .OR.
     :           INARR( J ) .EQ. VAL__BADR ) THEN

*             too large a pixel value - stick the 'bad pixel'
*             value into the output pixel

               OUTARR( J )  =  VAL__BADR

*          check for negative pixel value

            ELSE IF ( INARR( J ) .LE. 0.0 ) THEN

*             result is undefined unless power is an even integer

               IF ( IFIX( POWER/2.0 ) * 2 .EQ. IFIX( POWER ) ) THEN

*                value and power ok so raise to power

                  OUTARR( J ) = ABS( INARR( J ) ) ** POWER

               ELSE

*                stick the 'bad pixel' value into the output pixel

                  OUTARR( J )  =  VAL__BADR

	       END IF

*          pixel value is too small, but positive

            ELSE IF ( INARR( J ) .LE. MINVAL ) THEN

               OUTARR( J )  =  0.0

*          else a valid calculation may take place

            ELSE

*             set output pixel to input pixel value raised to the power

               OUTARR( J )  =  INARR( J ) ** POWER

*          end of if-pixel-value-too-large check

            END IF

         END DO

*    else small limit checking is required

      ELSE

*       now loop round all rows of the output array

         DO  J  =  1, DIMS

*          check pixel value for an value smaller than the
*          specified minimum or for an undefined value

            IF ( INARR( J ) .LE. MINVAL .OR.
     :           INARR( J ) .EQ. VAL__BADR ) THEN

*             too small a pixel value - stick the 'bad pixel'
*             value into the output pixel

               OUTARR( J )  =  VAL__BADR

*          check for negative pixel value

            ELSE IF ( INARR( J ) .LE. 0.0 ) THEN

*             result is undefined unless power is an even integer

               IF ( IFIX( POWER/2.0 ) * 2 .EQ. IFIX( POWER ) ) THEN

*                value and power ok so raise to power

                  OUTARR( J ) = ABS( INARR( J ) ) ** POWER

               ELSE

*                stick the 'bad pixel' value into the output pixel

                  OUTARR( J )  =  VAL__BADR

	       END IF

*          pixel value is too large

            ELSE IF ( INARR( J ) .GE. MAXVAL ) THEN

               OUTARR( J )  =  0.0

*          else a valid calculation may take place

            ELSE

*             set output pixel to input pixel value raised to the power

               OUTARR( J )  =  INARR( J ) ** POWER

*          end of if-pixel-value-too-small check

            END IF

         END DO

*    end of if-large-limit-checking-required check

      END IF

 999  CONTINUE

*    return

      END
