      SUBROUTINE CAP_ANGDC (UNITS, ANGLEC, ANGLER, STATUS)
*+
*  Name:
*     CAP_ANGDC
*  Purpose:
*     Decode an angle from a CHARACTER string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_ANGDC (UNITS, ANGLEC; ANGLER; STATUS)
*  Description:
*     Decode an angle from a CHARACTER string.
*
*     The hours or degrees, minutes and seconds must be separated by a
*     colon (':').  Either the seconds or the minutes and the seconds
*     may be omitted.  Only the least significant unit (hours or
*     degrees, minutes or seconds, as appropriate) may contain a fractional
*     part; the more significant items must be integer numbers.
*  Arguments:
*     UNITS  =  CHARACTER*(*) (Given)
*        Units in which the angle is expressed.  The options are:
*        ANGLE   - sexagesimal hours or degrees; an unsigned number is
*           assumed to be in hours, a signed one in degrees (thus a
*           an angle in degrees must always be signed, even if it is
*           positive),
*        HOURS   - sexagesimal hours,
*        DEGREES - sexagesimal degrees,
*        ARCMIN  - sexagesimal minutes of arc (optionally subdivided
*           into seconds),
*        ARCSEC  - seconds of arc.
*        TIMEMIN - sexagesimal minutes of time (optionally subdivided
*           into seconds),
*        TIMESEC - seconds of time.
*     ANGLEC  =  CHARACTER*(*) (Given)
*        String containing the angle expressed in a sexagesimal format.
*     ANGLER  =  DOUBLE PRECISION (Returned)
*        Corresponding angle in radians.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Take a local copy of the units.
*     Convert the local copy of the units to upper case.
*     If the string is not entirely blank then
*       Take a copy of the angle.
*       Remove any leading blanks.
*       If the units are 'ANGLE' then
*         If the first character is a sign then
*           Set the units to 'DEGREES'.
*         else
*           Set the units to 'HOURS'.
*         end if
*       end if
*       Determine the length of the string.
*       Replace any colons with spaces.
*       Attempt to convert the angle to radians (assuming it is in
*       degrees).
*       If the conversion is ok then
*         Multiply the angle by the appropriate conversion factor for
*         its units.
*       else
*         Set the status.
*         Report an error.
*       end if
*     else (the string is blank)
*       Set the result to zero.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     14/3/94 (ACD): Original version.
*     8/10/96 (ACD): Added the 'UNITS' argument.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      CHARACTER
     :  UNITS*(*),
     :  ANGLEC*(*)
*  Arguments Returned:
      DOUBLE PRECISION
     :  ANGLER
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  CUNITS*10,  ! Copy of UNITS.
     :  ANGCPY*75,  !  "   "  ANGLEC.
     :  ERRTXT*75   ! Text of error message.
      INTEGER
     :  LANGCP,     ! Length of ANGCPY (excl. trail. blanks).
     :  LOOP,       ! Loop index.
     :  ANGPOS,     ! Current position in ANGCPY.
     :  LSTAT,      ! Local status decoding value from string.
     :  ERRLEN      ! Current position in ERRTXT.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Take a local copy of the units and convert it to upper case.

         CUNITS = UNITS
         CALL CHR_UCASE (CUNITS)

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
*          If the units are 'ANGLE' then determine the actual units by
*          checking whether the first character is a sign ('+' or '-').
*          If it is then the angle is considered to be in degrees;
*          otherwise it is considered to be in hours.

            IF (CUNITS .EQ. 'ANGLE') THEN
               IF (ANGCPY(1 : 1) .EQ. '+'  .OR.  ANGCPY(1 : 1) .EQ. '-')
     :           THEN
                  CUNITS = 'DEGREES'
               ELSE
                  CUNITS = 'HOURS'
               END IF
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

               IF (CUNITS .EQ. 'HOURS') THEN
                  ANGLER = ANGLER * 1.5D1
               ELSE IF (CUNITS .EQ. 'DEGREES') THEN
                  CONTINUE
               ELSE IF (CUNITS .EQ. 'ARCMIN') THEN
                  ANGLER = ANGLER / 6.0D1
               ELSE IF (CUNITS .EQ. 'ARCSEC') THEN
                  ANGLER = ANGLER / (6.0D1 * 6.0D1)
               ELSE IF (CUNITS .EQ. 'TIMEMIN') THEN
                  ANGLER = ANGLER * 1.5D1 / 6.0D1
               ELSE IF (CUNITS .EQ. 'TIMESEC') THEN
                  ANGLER = ANGLER * 1.5D1 / (6.0D1 * 6.0D1)
               ELSE
                  CALL MSG_SETC ('CUNITS', CUNITS)
                  CALL CAP_INFO (.TRUE., ' ', 'Unknown units for an '/
     :              /'angle: ^CUNITS; degrees assumed.', STATUS)
               END IF

            ELSE

*
*             The conversion failed.  Set the status and report an error.

               STATUS = SAI__ERROR

               ERRTXT = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('Failed to decode angle (', ERRTXT,
     :           ERRLEN)

               IF (LSTAT .EQ. -1) THEN
                  CALL CHR_PUTC ('bad degrees', ERRTXT, ERRLEN)

               ELSE IF (LSTAT .EQ. -2) THEN
                  CALL CHR_PUTC ('bad minutes', ERRTXT, ERRLEN)

               ELSE IF (LSTAT .EQ. -3) THEN
                  CALL CHR_PUTC ('bad seconds', ERRTXT, ERRLEN)

               ELSE
                  ANGLER = 0.0D0

                  CALL CHR_PUTC ('bad angle', ERRTXT, ERRLEN)

               END IF

               CALL CHR_PUTC ('): ', ERRTXT, ERRLEN)

               CALL CHR_PUTC (ANGCPY(1 : LANGCP), ERRTXT, ERRLEN)

               CALL ERR_REP ('CAP_ANGDC_ERR', ERRTXT(1 : ERRLEN),
     :           STATUS)

            END IF

         ELSE

*
*          The string is completely blank; set the angle to zero.

            ANGLER = 0.0D0

         END IF

      END IF

      END
