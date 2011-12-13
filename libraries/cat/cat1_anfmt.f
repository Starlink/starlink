      SUBROUTINE CAT1_ANFMT (ANGLE, SEP, PLUS, LEAD, UNITS, NSGDIV,
     :  DECPL, FANGLE, STATUS)
*+
*  Name:
*     CAT1_ANFMT
*  Purpose:
*     Convert angle to sexagesimal hours or degrees.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ANFMT (ANGLE, SEP, PLUS, LEAD, UNITS, NSGDIV, DECPL;
*       FANGLE; STATUS)
*  Description:
*     Convert an angle in radians into hours or degrees and format it
*     as a sexagesimal value.  The angle is returned in a left
*     justified character string.  A set of format descriptors prescribe
*     how the angle is to be represented as a sexagesimal value.
*  Arguments:
*     ANGLE  =  DOUBLE PRECISION (Given)
*        The angle, in radians, to be represented as a sexagesimal
*        value.
*     SEP  =  INTEGER (Given)
*        The separator to be used between the hours or degrees, minutes
*        and seconds.  The options are:
*        CAT1__ISO    -  ISO standard; a colon (':'),
*        CAT1__BLANK  -  a blank,
*        CAT1__LETTR  -  a letter (h, d, m, s, as appropriate).
*     PLUS  =  LOGICAL (Given)
*        Is a plus character ('+') to precede positive angles, coded
*        as follows:
*        .TRUE.  -  insert plus character,
*        .FALSE. -  do not insert plus character.
*     LEAD  =  LOGICAL (Given)
*        Insert leading zeroes before the hours or degrees, coded as
*        follows,
*        .TRUE.  -  insert leading zeroes,
*        .FALSE. -  do not insert leading zeroes.
*     UNITS  =  INTEGER (Given)
*        Units in which the angle is to be represented.  The options
*        are:
*        CAT1__HOUR  -  hours,
*        CAT1__DEG   -  degrees,
*        CAT1__TIMIN -  minutes of time,
*        CAT1__ARMIN -  minutes of arc,
*        CAT1__TISEC -  seconds of time,
*        CAT1__ARSEC -  seconds of arc.
*     NSGDIV  =  INTEGER (Given)
*        Number of sexagesimal subdivisions into which the angle is
*        to be divided.  For example, an angle to represented as hours,
*        minutes and seconds would have two sexagesimal subdivisions.
*        An angle to be represented as just hours and minutes, or a
*        (usually small) angle to be represented as minutes and seconds
*        would have just one.
*     DECPL  =  INTEGER (Given)
*        The number of decimal places to be included in the sexagesimal
*        representation.
*     FANGLE  =  CHARACTER*(*) (Returned)
*        The angle converted to hours or degrees and represented as a
*        sexagesimal value.  The string is left justified.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the sign of the angle.
*     Convert the absolute value of the angle to the output units.
*     Compute the integer part, any sexagesimal subdivisions and any
*     remainder for the angle.
*     seconds and the remainder, as required.
*     If the required separators are letters then assemble the
*     particular string of letters required.
*     Initialise the buffer to contain the working copy of the formatted
*     angle.
*     Insert any sign that is required into the buffer.
*     If leading zeroes are required then
*       append any leading zeroes that are needed.
*     end if
*     Append the integer part.
*     If a sexagesimal subdivision is required then
*       Append the appropriate specifier.
*       Append any leading zeroes.
*       Append the sexagesimal value.
*       If a second sexagesimal subdivision is required then
*         Append the appropriate specifier.
*         Append any leading zeroes.
*         Append the sexagesimal value.
*       end if
*     end if
*     If the number of decimal places is greater than zero then
*       Append the remainder, expressed to the specified number of
*       decimal places.
*     end if
*     If the separator is a letter then
*       Append the final appropriate trailing letter.
*     end if
*     Copy the buffer to the output string, right justified.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     9/3/94  (ACD): Original version.
*     2/10/95 (ACD): Fixed bug in the rounding.
*     10/9/00 (ACD): Avoided errors due to exponentiation when checking
*        for rounding with a large number of decimal places.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Arguments Given:
      DOUBLE PRECISION
     :  ANGLE
      INTEGER
     :  SEP,
     :  UNITS,
     :  NSGDIV,
     :  DECPL
      LOGICAL
     :  PLUS,
     :  LEAD
*  Arguments Returned:
      CHARACTER
     :  FANGLE*(*)
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      DOUBLE PRECISION
     :  VALUE,   ! Value of the angle in hours or degrees.
     :  REM,     ! Remainder.
     :  DSG1,    ! DOUBLE PRECISION first  sexagesimal subdivision.
     :  DSG2     !   "        "     second     "            "     .
      INTEGER
     :  IVALUE,  ! INTEGER value
     :  ISG1,    !    "    first  sexagesimal subdivision.
     :  ISG2,    !    "    second     "            "     .
     :  IREM,    !    "    remainder.
     :  LSTAT,   ! Local status.
     :  BUFPOS,  ! Current position in BUFFER (excl. trail. blanks).
     :  REMPOS,  !    "       "     "  REMBUF ( "  .   "  .   "   ).
     :  FORPOS,  !    "       "     "  FORMAT ( "  .   "  .   "   ).
     :  FANLEN   ! Declared length of FANGLE.
      CHARACTER
     :  BUFFER*25,  ! Buffer for assembling the formatted string.
     :  REMBUF*25,  ! Buffer holding the remainder as a character string.
     :  FORMAT*10,  ! Format for writing the buffer.
     :  LSEP(3)*1   ! List of 'letter' separators.
      LOGICAL
     :  POSTVE,  ! Flag: is the angle positive (or zero)?
     :  ROUND    ! Flag: has rounding occurred?
*.

      IF (STATUS .EQ. CAT__OK) THEN
C        print2000, angle, sep, plus, lead, units, nsgdiv, decpl
C2000    format(1x, 'cat1_anfmt on entry: ',
C    :     3x, 'angle: ', d12.6 /
C    :     3x, 'sep: ', i5 /
C    :     3x, 'plus: ', l5 /
C    :     3x, 'lead: ', l5 /
C    :     3x, 'units: ', i5 /
C    :     3x, 'nsgdiv: ', i5 /
C    :     3x, 'decpl: ', i5 )

*
*       Determine the sign of the angle.  Note that zero is regarded as
*       positive.

         IF (ANGLE .LT. 0.0D0) THEN
            POSTVE = .FALSE.
         ELSE
            POSTVE = .TRUE.
         END IF

*
*       Convert the absolute value of the angle to hours or degrees,
*       as appropriate.

         IF (UNITS .EQ. CAT1__HOUR) THEN
            VALUE = ABS(ANGLE) * 1.2D1 / CAT1__PI
         ELSE IF (UNITS .EQ. CAT1__DEG) THEN
            VALUE = ABS(ANGLE) * 1.8D2 / CAT1__PI
         ELSE IF (UNITS .EQ. CAT1__TIMIN) THEN
            VALUE = ABS(ANGLE) * 1.2D1 * 6.0D1 / CAT1__PI
         ELSE IF (UNITS .EQ. CAT1__ARMIN) THEN
            VALUE = ABS(ANGLE) * 1.8D2 * 6.0D1 / CAT1__PI
         ELSE IF (UNITS .EQ. CAT1__TISEC) THEN
            VALUE = ABS(ANGLE) * 1.2D1 * 6.0D1 * 6.0D1 / CAT1__PI
         ELSE IF (UNITS .EQ. CAT1__ARSEC) THEN
            VALUE = ABS(ANGLE) * 1.8D2 * 6.0D1 * 6.0D1 / CAT1__PI
         END IF

*
*       Compute the integer value, and sexagesimal subdivisions
*       and the remainder, as required.  In outline the algorithm
*       is: compute the required quantities; round up the remainder;
*       check that rounding up the remainder has not caused it to
*       become greater than one and then 'traverse up' the remaining
*       quantitities checking that each has not exceeded its
*       permitted range.
*
*       No such check is performed on the value; that is, hours or
*       degrees are not forced to lie in the range 0 - 23 or 0 - 359
*       respectively, nor are minutes or seconds of arc or time, when
*       these are the units (rather than sexagesimal subdivisions)
*       forced to lie in the range 0 - 60.

*
*       First compute the integer value, and sexagesimal subdivisions
*       and the remainder, as required.

         IVALUE = INT(VALUE)
         REM = VALUE - DBLE(IVALUE)

         IF (NSGDIV .GE. 1) THEN
            DSG1 = REM * 6.0D1
            ISG1 = INT(DSG1)
            REM = DSG1 - DBLE(ISG1)

            IF (NSGDIV .EQ. 2) THEN
               DSG2 = REM * 6.0D1
               ISG2 = INT(DSG2)
               REM = DSG2 - DBLE(ISG2)
            END IF
         END IF

C        print2001, rem
C2001    format(1x, 'rem: ', 1pd12.6)

*
*       If a remainder is required then round it to the required number
*       of decimal places.  Otherwise, if no remainder is required then
*       check if it rounds up.
*
*       Note that if more than six decimal places are specified then
*       the rounding is forced to .FALSE. to avoid rounding errors
*       with the exponentiation.

         ROUND = .FALSE.

         IF (DECPL .GT. 0) THEN
            IF (DECPL .LE. 6) THEN
               REM = REM * (1.0D1**DECPL)
               IREM = NINT(REM)
               REM = (DBLE(IREM)) / (1.0D1**DECPL)

               IF (REM .GE. 1.0D0) THEN
                  REM = 0.0D0
                  ROUND = .TRUE.
               END IF
            ELSE
               ROUND = .FALSE.
            END IF

         ELSE
            IREM = NINT(REM)

            IF (IREM .GE. 1) THEN
               ROUND = .TRUE.
            END IF

         END IF

C        print2002, rem, round
C2002    format(1x, 'rem, round: ', 1pd12.6, l6)

*
*       If rounding occurred then increment the next higher subdivision.
*       Check that each successive unit in the hierarchy has not exceeded
*       its range.

         IF (ROUND) THEN

            IF (NSGDIV .EQ. 2) THEN

*
*             A second sexagesimal subdivision is required.

               ISG2 = ISG2 + 1

               IF (ISG2 .GT. 59) THEN
                  ISG2 = 0
                  ISG1 = ISG1 + 1

                  IF (ISG1 .GT. 59) THEN
                     ISG1 = 0
                     IVALUE = IVALUE + 1
                  END IF
               END IF

            ELSE

               IF (NSGDIV .GE.1) THEN

*
*                A single sexagesimal subdivision is required.

                  ISG1 = ISG1 + 1

                  IF (ISG1 .GT. 59) THEN
                     ISG1 = 0
                     IVALUE = IVALUE + 1
                  END IF

               ELSE

*
*                Only the value is required, without sexagesimal
*                subdivision.

                  IVALUE = IVALUE + 1
               END IF

            END IF

         END IF

*
*       If the required separators are letters then assemble the
*       particular string of letters required.

         IF (SEP .EQ. CAT1__LETTR) THEN
            IF (UNITS .EQ. CAT1__HOUR) THEN
               LSEP(1) = 'h'
               LSEP(2) = 'm'
               LSEP(3) = 's'

            ELSE IF (UNITS .EQ. CAT1__DEG) THEN
               LSEP(1) = 'd'
               LSEP(2) = 'm'
               LSEP(3) = 's'

            ELSE IF (UNITS .EQ. CAT1__TIMIN  .OR.
     :        UNITS .EQ. CAT1__ARMIN) THEN
               LSEP(1) = 'm'
               LSEP(2) = 's'
               LSEP(3) = ' '

            ELSE IF (UNITS .EQ. CAT1__TISEC  .OR.
     :        UNITS .EQ. CAT1__ARSEC) THEN
               LSEP(1) = 's'
               LSEP(2) = ' '
               LSEP(3) = ' '
            END IF
         END IF

*
*       Initialise the buffer to contain the working copy of the
*       sexagesimal angle.

         BUFFER = ' '
         BUFPOS = 0

*
*       Insert any sign that is required into the buffer.  If the angle
*       is negative then a minus sign is always inserted.  If it is
*       positive then a plus sign is only inserted if the appropriate
*       format descriptor is set.

         IF (.NOT. POSTVE) THEN
            CALL CHR_PUTC ('-', BUFFER, BUFPOS)
         ELSE
            IF (PLUS) THEN
               CALL CHR_PUTC ('+', BUFFER, BUFPOS)
            END IF
         END IF

*
*       If leading zeroes are required then append the appropriate
*       number.  Hours are assumed to be two-digit numbers and degrees
*       three-digit.  Similarly when hours or minutes of arc or time
*       are the units, rather than sexagesimal subdivisions, they are
*       assumed to be two-digit numbers (0 - 60).  Thus, all units
*       two-digit, except degrees which are three-digit.

         IF (LEAD) THEN
            IF (UNITS .EQ. CAT1__DEG) THEN
               IF (IVALUE .LT. 100) THEN
                  CALL CHR_PUTC ('0', BUFFER, BUFPOS)
               END IF
               IF (IVALUE .LT. 10) THEN
                  CALL CHR_PUTC ('0', BUFFER, BUFPOS)
               END IF
            ELSE
               IF (IVALUE .LT. 10) THEN
                  CALL CHR_PUTC ('0', BUFFER, BUFPOS)
               END IF
            END IF
         END IF

*
*       Append the integer part.

         CALL CHR_PUTI (IVALUE, BUFFER, BUFPOS)

*
*       Check if first sexagesimal subdivision is required.

         IF (NSGDIV .GE. 1) THEN

*
*          Append the appropriate specifier.

            IF (SEP .EQ. CAT1__ISO) THEN
               CALL CHR_PUTC (':', BUFFER, BUFPOS)
            ELSE IF (SEP .EQ. CAT1__BLANK) THEN
               CALL CHR_PUTC (' ', BUFFER, BUFPOS)
            ELSE
               CALL CHR_PUTC (LSEP(1)(1 : 1), BUFFER, BUFPOS)
            END IF

*
*          Append any leading zeroes.  Note that leading zeroes are
*          always included for sexagesimal subdivisions.

            IF (ISG1 .LT. 10) THEN
               CALL CHR_PUTC ('0', BUFFER, BUFPOS)
            END IF

*
*          Append the sexagesimal subdivision.

            CALL CHR_PUTI (ISG1, BUFFER, BUFPOS)

*
*          Check if a second sexagesimal subdivision is required.

            IF (NSGDIV .EQ. 2) THEN

*
*             Append the appropriate specifier.

               IF (SEP .EQ. CAT1__ISO) THEN
                  CALL CHR_PUTC (':', BUFFER, BUFPOS)
               ELSE IF (SEP .EQ. CAT1__BLANK) THEN
                  CALL CHR_PUTC (' ', BUFFER, BUFPOS)
               ELSE
                  CALL CHR_PUTC (LSEP(2)(1 : 1), BUFFER, BUFPOS)
               END IF

*
*             Append any leading zeroes.  Note that leading zeroes are
*             always included for sexagesimal subdivisions.

               IF (ISG2 .LT. 10) THEN
                  CALL CHR_PUTC ('0', BUFFER, BUFPOS)
               END IF

*
*             Append the second sexagesimal subdivision.

               CALL CHR_PUTI (ISG2, BUFFER, BUFPOS)
            END IF
         END IF

*       If more than zero decimal places are required then append the
*       remainder, converted to a character string and expressed to the
*       specified number of decimal places.

         IF (DECPL .GT. 0) THEN
            FORMAT = ' '
            FORPOS = 0

            CALL CHR_PUTC ('(F', FORMAT, FORPOS)
            CALL CHR_PUTI (DECPL+2, FORMAT, FORPOS)
            CALL CHR_PUTC ('.', FORMAT, FORPOS)
            CALL CHR_PUTI (DECPL, FORMAT, FORPOS)
            CALL CHR_PUTC (')', FORMAT, FORPOS)

            REMBUF = ' '
            WRITE(REMBUF, FORMAT(1 : FORPOS), IOSTAT=LSTAT) REM
C           print2003, rem, format, rembuf
C2003       format(1x, 'rem: ', 1pd12.6 /
C    :        1x, 'format:', a /
C    :        1x, 'rembuf:', a / )

            IF (LSTAT .EQ. 0) THEN
               CALL CHR_LDBLK (REMBUF)

*
*             Remove the zero that must be before the decimal point.

               REMBUF(1 : 1) = ' '

               CALL CHR_LDBLK (REMBUF)

               REMPOS = CHR_LEN(REMBUF)

               CALL CHR_PUTC (REMBUF(1 : REMPOS), BUFFER, BUFPOS)
            END IF
         END IF

*
*       If the separator is 'letter' then append the appropriate
*       trailing letter.

         IF (SEP .EQ. CAT1__LETTR) THEN
            IF (NSGDIV .EQ. 2) THEN
               CALL CHR_PUTC (LSEP(3)(1 : 1), BUFFER, BUFPOS)
            ELSE IF (NSGDIV .EQ. 1) THEN
               CALL CHR_PUTC (LSEP(2)(1 : 1), BUFFER, BUFPOS)
            ELSE
               CALL CHR_PUTC (LSEP(1)(1 : 1), BUFFER, BUFPOS)
            END IF
         END IF

*
*       Copy the buffer to the output string, left justified.

         FANLEN = LEN(FANGLE)

         FANGLE = ' '

         IF (FANLEN .GT. BUFPOS) THEN
            FANGLE(1 : BUFPOS) = BUFFER(1 : BUFPOS)
         ELSE
            FANGLE(1 : FANLEN) = BUFFER(1 : FANLEN)
         END IF

      END IF

      END
