      SUBROUTINE CAT1_ANGDC (ANGLEC, ANCODE, ANGLER, CONVOK, STATUS)
*+
*  Name:
*     CAP_ANGDC
*  Purpose:
*     Decode an angle from a CHARACTER string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ANGDC (ANGLEC, ANCODE; ANGLER, CONVOK; STATUS)
*  Description:
*     Decode an angle from a CHARACTER string.  Unless the units
*     are explicitly set the string is interpretted as follows.  If the
*     first non-blank character is '+' or '-' the angle is deemed to
*     have units of degrees; otherwise it is deemed to have units of hours
*     (this behaviour is based on the supposition that hours only occur in
*     the range 0 to 24).  A consequence is that any angle in degrees must
*     always be preceded  by a sign, even if it is positive.
*
*     The hours or degrees, minutes and seconds must be separated by a
*     colon (':').  Either the seconds or the minutes and the seconds
*     may be omitted.  Only the least significant unit (hours or
*     degrees, minutes or seconds, as appropriate) may contain a fractional
*     part; the more significant items must be integer numbers.
*
*     If the units are minutes of arc or time then they may optionally
*     be subdivided into sexagemsimal seconds.  If so the minutes and
*     seconds must be separated by a colon (':').
*  Arguments:
*     ANGLEC  =  CHARACTER*(*) (Given)
*        String containing the angle expressed in a sexagesimal format.
*     ANCODE  =  INTEGER (Given)
*        Code specifying the units, as follows:
*        CAT1__HOUR  -  hours,
*        CAT1__DEG   -  degrees,
*        CAT1__SDEG  -  if explicitly signed degrees, otherwise hours,
*        CAT1__ARMIN -  minutes of arc,
*        CAT1__TIMIN -     "    "  time,
*        CAT1__ARSEC -  seconds of arc,
*        CAT1__TISEC -     "    "  time.
*     ANGLER  =  DOUBLE PRECISION (Returned)
*        Corresponding angle in radians.
*     CONVOK  =  LOGICAL (Returned)
*        Flag indicating whether the conversion was ok, coded as
*        follows:
*        .TRUE.  -  converted ok,
*        .FALSE. -  failed to convert.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the string is not entirely blank then
*       Take a copy of the angle.
*       Remove any leading blanks.
*       Determine whether the units of the angle are hours or degrees,
*       using the presence or absence of a sign if appropriate.
*       Determine the length of the string.
*       Replace any colons with spaces.
*       Attempt to convert the angle to radians.
*       If the conversion is ok then
*         If the original angle was in hours then
*           Multiply the result by 15.
*         end if
*       else
*         Set the conversion failed flag.
*       end if
*     else (the string is blank)
*       Set the result to zero.
*     end if
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     30/7/96 (ACD): Original version (based onn CAP_ANGDC).
*     3/6/97  (ACD): Added options for angles in minutes and seconds of
*       arc and time (again based on CAP_ANGDC).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal  "      "    .
*  Arguments Given:
      CHARACTER
     :  ANGLEC*(*)
      INTEGER
     :  ANCODE
*  Arguments Returned:
      DOUBLE PRECISION
     :  ANGLER
      LOGICAL
     :  CONVOK
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  ANGCPY*75   ! Copy of ANGLEC.
      INTEGER
     :  LANGCP,     ! Length of ANGCPY (excl. trail. blanks).
     :  LOOP,       ! Loop index.
     :  ANGPOS,     ! Current position in ANGCPY.
     :  LSTAT       ! Local status decoding value from string.
      LOGICAL
     :  HOURS       ! Flag; is the angle in hours?
*.

      IF (STATUS .EQ. CAT__OK) THEN
         CONVOK = .TRUE.

*
*       Check that the string is not completely blank.

         IF (ANGLEC .NE. ' ') THEN

*
*          Take a copy of the angle (this copy will be modified).

            ANGCPY = ANGLEC

*
*          Remove any leading blanks.

            CALL CHR_LDBLK (ANGCPY)

*
*          Determine whether the angle is in hours or degrees,
*          using the presence of absence of a sign if approipriate.

            IF (ANCODE .EQ. CAT1__SDEG) THEN
               IF (ANGCPY(1 : 1) .EQ. '+'  .OR.
     :             ANGCPY(1 : 1) .EQ. '-') THEN
                  HOURS = .FALSE.
               ELSE
                  HOURS = .TRUE.
               END IF

            ELSE IF (ANCODE .EQ. CAT1__HOUR) THEN
               HOURS = .TRUE.

            ELSE
               HOURS = .FALSE.

            END IF


*
*          Determine the length of the string.

            LANGCP = CHR_LEN(ANGCPY)

*
*          Replace any colons with spaces (to get the string in the
*          format required by SLA_DAFIN).

            DO LOOP = 1, LANGCP
               IF (ANGCPY(LOOP : LOOP) .EQ. ':') THEN
                  ANGCPY(LOOP : LOOP) = ' '
               END IF
            END DO

*
*          Attempt to convert the angle to radians (the conversion is
*          performed implicitly assuming it is in degrees).

            ANGPOS = 1

            CALL SLA_DAFIN (ANGCPY(1 : LANGCP), ANGPOS, ANGLER, LSTAT)

*
*          Proceed if the conversion is was successful.

            IF (LSTAT .EQ. 0) THEN

*
*             Multiply the appropriate conversion factor to compensate
*             for the original units not being degrees.

               IF (ANCODE .EQ. CAT1__HOUR) THEN
                  ANGLER = ANGLER * 1.5D1
               ELSE IF (ANCODE .EQ. CAT1__DEG) THEN
                  CONTINUE
               ELSE IF (ANCODE .EQ. CAT1__SDEG) THEN
                  IF (HOURS) THEN
                     ANGLER = ANGLER * 1.5D1
                  ELSE
                     CONTINUE
                  END IF
               ELSE IF (ANCODE .EQ. CAT1__ARMIN) THEN
                  ANGLER = ANGLER / 6.0D1
               ELSE IF (ANCODE .EQ. CAT1__ARSEC) THEN
                  ANGLER = ANGLER / (6.0D1 * 6.0D1)
               ELSE IF (ANCODE .EQ. CAT1__TIMIN) THEN
                  ANGLER = ANGLER * 1.5D1 / 6.0D1
               ELSE IF (ANCODE .EQ. CAT1__TISEC) THEN
                  ANGLER = ANGLER * 1.5D1 / (6.0D1 * 6.0D1)
               ELSE
                  CONVOK = .FALSE.
                  ANGLER = 0.0D0
               END IF

            ELSE

*
*             The conversion failed; set the return flag.

               CONVOK = .FALSE.
               ANGLER = 0.0D0

            END IF

         ELSE

*
*          The string is completely blank; set the angle to zero.

            ANGLER = 0.0D0

         END IF

      END IF

      END
