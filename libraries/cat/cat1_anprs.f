      SUBROUTINE CAT1_ANPRS (FORMAT, SEP, PLUS, LEAD, UNITS, NSGDIV,
     :  DECPL, STATUS)
*+
*  Name:
*     CAT1_ANPRS
*  Purpose:
*     Parse a CAT angle format specifier.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ANPRS (FORMAT; SEP, PLUS, LEAD, UNITS, NSGDIV, DECPL;
*       STATUS)
*  Description:
*     Parse a CAT angle format specifier.  The specifier is decoded into
*     a set of descriptors, each of which describes one aspect of the angular
*     format.  The set of descriptors can be used to format an angle in
*     radians into a sexagesimal representation in hours or degrees.
*
*     A CAT angle format specifier has the form:
*
*        RADIANS{sexagesimal_format_description}
*
*     The sexagesimal_format_description is described in full in
*     SUN/181, the CAT Programmer's Manual.  An example is 'L+ZHMS.2',
*     which corresponds to output in hours, minutes and seconds, with
*     the seconds expressed to two places of decimals.  Letters (h, m, s)
*     are used as separators, a plus sign is included in positive angles
*     and leading zeroes are shown.
*  Arguments:
*     FORMAT  =  CHARACTER*(*) (Given)
*        The given CAT angle format specifier to be decoded.
*     SEP  =  INTEGER (Returned)
*        The separator to be used between the hours or degrees, minutes
*        and seconds.  The options are:
*        CAT1__ISO    -  ISO standard; a colon (':'),
*        CAT1__BLANK  -  a blank,
*        CAT1__LETTR  -  a letter (h, d, m, s, as appropriate).
*     PLUS  =  LOGICAL (Returned)
*        Is a plus character ('+') to precede positive angles, coded
*        as follows:
*        .TRUE.  -  insert plus character,
*        .FALSE. -  do not insert plus character.
*     LEAD  =  LOGICAL (Returned)
*        Insert leading zeroes before the hours or degrees, coded as
*        follows,
*        .TRUE.  -  insert leading zeroes,
*        .FALSE. -  do not insert leading zeroes.
*     UNITS  =  INTEGER (Returned)
*        Units in which the angle is to be represented.  The options
*        are:
*        CAT1__HOUR  -  hours,
*        CAT1__DEG   -  degrees,
*        CAT1__TIMIN -  minutes of time,
*        CAT1__ARMIN -  minutes of arc,
*        CAT1__TISEC -  seconds of time,
*        CAT1__ARSEC -  seconds of arc.
*     NSGDIV  =  INTEGER (Returned)
*        Number of sexagesimal subdivisions into which the angle is
*        be divided.  For example, an angle to be represented as hours,
*        minutes and seconds would have two sexagesimal subdivisions.
*        An angle to be represented as just hours and minutes, or a
*        (usually small) angle to be represented as minutes and seconds
*        would have just one.
*     DECPL  =  INTEGER (Returned)
*        The number of decimal places to follow the smallest subdivision
*        of the angle.  For example, for an angle to be represented as
*        hours, minutes and seconds, it would be the number of decimal
*        places of the seconds.  For an angle simply represented as
*        (decimal) degrees it would be the number of decimal places of
*        the degrees.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Take a working copy of the given format specifier.
*     Convert the specifier to upper case.
*     Remove the leading string 'RADIANS{'.
*     Determine the length of the string.
*     Remove any trailing '}'.
*     Replace any simple synonym specifiers with their real equivalents.
*     Set the default values for the descriptors and flags.
*     Examine every element of the format specifier and set the
*     descriptors and flags as appropriate.
*     Determine the required units from the flags.
*     Determine the number of sexagesimal subdivisions required from
*     the flags.
*     If there is a decimal point then
*       Determine the number of decimal points required.
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
*     ACD: A C Davenhall (Leicester, Edinburgh)
*  History:
*     8/3/94  (ACD): Original version.
*     29/2/96 (ACD): Re-written to accommodate minutes and seconds
*        of arc and time as the units.
*     6/3/98  (ACD): Modified so that a specifier comprising of a
*        'T' without any other unit specifiers is taken to signify
*        hours.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Arguments Given:
      CHARACTER
     :  FORMAT*(*)
*  Arguments Returned:
      INTEGER
     :  SEP,
     :  UNITS,
     :  NSGDIV,
     :  DECPL
      LOGICAL
     :  PLUS,
     :  LEAD
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  FRMWRK*20   ! Working copy of the format specifier.
      INTEGER
     :  LOOP,       ! Loop index.
     :  LENGTH,     ! Length of FRMWRK (excl. trail. blanks).
     :  DECPOS,     ! Position of any decimal point.
     :  LSTAT       ! Local status.
      LOGICAL
     :  HFLAG,      ! Flag; 'H' present in FRMWRK?
     :  DFLAG,      !  "  ; 'D'    "    "    "   ?
     :  MFLAG,      !  "  ; 'M'    "    "    "   ?
     :  SFLAG,      !  "  ; 'S'    "    "    "   ?
     :  TFLAG       !  "  ; 'T'    "    "    "   ?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Take a working copy of the given format specifier.  This copy
*       will be modified.

         FRMWRK = FORMAT

*
*       Convert the specifier to upper case.

         CALL CHR_UCASE (FRMWRK)

*
*       The first eight characters of the specifier should be
*       'RADIANS{'; remove them.

         FRMWRK(1 : 8) = '        '

         CALL CHR_LDBLK (FRMWRK)

*
*       Determine the length of the string.

         IF (FRMWRK .NE. ' ') THEN
            LENGTH = CHR_LEN(FRMWRK)
         ELSE
            LENGTH = 1
         END IF

*
*       Remove any trailing '}'.

         IF (FRMWRK(LENGTH : LENGTH) .EQ. '}') THEN
            FRMWRK(LENGTH : LENGTH) = ' '
         END IF

*
*       If the remaining specifier is one of the simple synonyms then
*       replace it with the equivalent real specifier.  The simple
*       synonyms are:
*
*          HOURS
*          DEGREES
*          ARCMIN
*          TIMEMIN
*          ARCSEC
*          TIMESEC
*
*       Note that the 'small angle' synonyms (ARCMIN, TIMEMIN, ARCSEC
*       and TIMESEC) can optionally be followed by a dot ('.') and a
*       number of decimal places; HOURS and DEGREES cannot.

         IF (FRMWRK .EQ. 'DEGREES'  .OR.  FRMWRK .EQ. ' ') THEN
            FRMWRK = 'IDMS'
         ELSE IF (FRMWRK .EQ. 'HOURS') THEN
            FRMWRK = 'IHMS.1'
         ELSE IF (FRMWRK(1 : 6) .EQ. 'ARCMIN') THEN
            FRMWRK(1 : 6) = 'M     '
         ELSE IF (FRMWRK(1 : 7) .EQ. 'TIMEMIN') THEN
            FRMWRK(1 : 7) = 'TM     '
         ELSE IF (FRMWRK(1 : 6) .EQ. 'ARCSEC') THEN
            FRMWRK(1 : 6) = 'S     '
         ELSE IF (FRMWRK(1 : 7) .EQ. 'TIMESEC') THEN
            FRMWRK(1 : 7) = 'TS     '
         END IF

*
*       Set the default values for the descriptors and flags.

         SEP = CAT1__ISO
         PLUS = .FALSE.
         LEAD = .FALSE.

         UNITS = CAT1__DEG
         NSGDIV = 0

         HFLAG = .FALSE.
         DFLAG = .FALSE.
         MFLAG = .FALSE.
         SFLAG = .FALSE.
         TFLAG = .FALSE.

         DECPL = 0
         DECPOS = 0

*
*       Examine every element of the format specifier and set the
*       descriptors as appropriate.

         LENGTH = CHR_LEN(FRMWRK)

         DO LOOP = 1, LENGTH
            IF (FRMWRK(LOOP : LOOP) .EQ. 'I') THEN
               SEP = CAT1__ISO

            ELSE IF (FRMWRK(LOOP : LOOP) .EQ. 'B') THEN
               SEP = CAT1__BLANK

            ELSE IF (FRMWRK(LOOP : LOOP) .EQ. 'L') THEN
               SEP = CAT1__LETTR

            ELSE IF (FRMWRK(LOOP : LOOP) .EQ. '+') THEN
               PLUS = .TRUE.

            ELSE IF (FRMWRK(LOOP : LOOP) .EQ. 'Z') THEN
               LEAD  = .TRUE.

            ELSE IF (FRMWRK(LOOP : LOOP) .EQ. 'H') THEN
               HFLAG = .TRUE.

            ELSE IF (FRMWRK(LOOP : LOOP) .EQ. 'D') THEN
               DFLAG = .TRUE.

            ELSE IF (FRMWRK(LOOP : LOOP) .EQ. 'M') THEN
               MFLAG = .TRUE.

            ELSE IF (FRMWRK(LOOP : LOOP) .EQ. 'S') THEN
               SFLAG = .TRUE.

            ELSE IF (FRMWRK(LOOP : LOOP) .EQ. 'T') THEN
               TFLAG = .TRUE.

            ELSE IF (FRMWRK(LOOP : LOOP) .EQ. '.') THEN
               DECPOS = LOOP

            END IF
         END DO

*
*       Determine the units from the flags.  The algorithm is as
*       follows:
*
*       if 'H' present then
*         units are hours
*       else if 'D' present then
*         units are degrees
*       else (neither H nor D are present)
*         if 'M' is present then
*           if 'T' is present then
*             units are minutes of time.
*           else
*             units are minutes of arc.
*           end if
*         else ('M' is absent)
*           if 'S' is present then
*             if 'T' is present then
*               units are seconds of time.
*             else
*               units are seconds of arc.
*             end if
*           else ('S' is absent)
*             if 'T' is present then
*               units are hours.
*             else (none of 'H', 'D', 'M', 'S' and 'T' are present)
*               units are degrees.
*             end if
*           end if
*         end if
*       end if

         IF (HFLAG) THEN
            UNITS = CAT1__HOUR
         ELSE IF (DFLAG) THEN
            UNITS = CAT1__DEG
         ELSE
            IF (MFLAG) THEN
               IF (TFLAG) THEN
                  UNITS = CAT1__TIMIN
               ELSE
                  UNITS = CAT1__ARMIN
               END IF
            ELSE
               IF (SFLAG) THEN
                  IF (TFLAG) THEN
                     UNITS = CAT1__TISEC
                  ELSE
                     UNITS = CAT1__ARSEC
                  END IF
               ELSE
                  IF (TFLAG) THEN
                     UNITS = CAT1__HOUR
                  ELSE
                     UNITS = CAT1__DEG
                  END IF
               END IF
            END IF
         END IF

*
*       Determine the number of sexagesimal subdivisions required.  The
*       algorithm is as follows:
*
*       If the units are hours or degrees then
*         if 'M' is present then
*           if 'S' is present then
*             2 sexagesimal subdivisions are required.
*           else
*             1 sexagesimal subdivision is required.
*           end if
*         else ('M' is absent)
*           no sexagesimal subdivisions are required.
*         end if
*       else the units are minutes of arc or time
*         if 'S' is present then
*           1 sexagesimal subdivision is required.
*         else
*           no sexagesimal subdivisions are required.
*         end if
*       else the units are seconds of arc or time
*         no sexagesimal subdivisions are required.
*       end if

         IF (UNITS .EQ. CAT1__HOUR  .OR.  UNITS .EQ. CAT1__DEG) THEN
            IF (MFLAG) THEN
               IF (SFLAG) THEN
                  NSGDIV = 2
               ELSE
                  NSGDIV = 1
               END IF
            ELSE
               NSGDIV = 0
            END IF
         ELSE IF (UNITS .EQ. CAT1__TIMIN  .OR.  UNITS .EQ. CAT1__ARMIN)
     :     THEN
            IF (SFLAG) THEN
               NSGDIV = 1
            ELSE
               NSGDIV = 0
            END IF
         ELSE IF (UNITS .EQ. CAT1__TISEC  .OR.  UNITS .EQ. CAT1__ARSEC)
     :     THEN
            NSGDIV = 0
         END IF

*
*       If there is a decimal point then determine the number of decimal
*       places required.

         IF (DECPOS .GT. 0) THEN

            LSTAT = CAT__OK
            CALL CHR_CTOI (FRMWRK(DECPOS+1 : LENGTH), DECPL, LSTAT)

            IF (LSTAT .NE. CAT__OK) THEN
               DECPL = 0
            END IF
         END IF

      END IF

      END
