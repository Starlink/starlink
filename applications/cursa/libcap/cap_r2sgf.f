      SUBROUTINE CAP_R2SGF (RANGLE, DUNITS, DECPL, SANGLE, STATUS)
*+
*  Name:
*     CAP_R2SGF
*  Purpose:
*     Convert an angle in radians to sexagesimal hours or degrees.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_R2SGF (RANGLE, DUNITS, DECPL; SANGLE; STATUS)
*  Description:
*     Convert an angle in radians to sexagesimal hours or degrees.  The
*     value is returned as a character string.
*  Arguments:
*     RANGLE  =  DOUBLE PRECISION (Given)
*        Angle in radians to be converted and formatted.
*     DUNITS  =  CHARACTER*(*) (Given)
*        Destination units for the angle, one of 'HOURS' or 'DEGREES'.
*     DECPL  =  INTEGER (Given)
*        Number of decimal places to which the seconds (or arc or time)
*        are to be expressed.
*     SANGLE  =  CHARACTER*(*) (Returned)
*        Angle in hours or degrees, formatted as a sexagesimal value.
*        If the attempt to format the angle as a sexagesimal value
*        fails an unformatted value in radians is returned.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the value to sexagismal hours or degrees, as appropriate.
*     If ok then
*       Attempt to write the value obtained to an internal character string.
*       If ok then
*         Replace any leading spaces in the minutes and seconds with
*         zeros.
*         Remove any spaces between the sign and value for the hours or
*         degrees.
*         Insert the appropriate number of leading spaces to compensate
*         for hours or degrees with less than the maximum possible
*         number of digits.
*         Copy the assembled string to the return argument.
*       else
*         Return an unformatted value.
*       end if
*     end if
*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     4/8/94   (ACD): Original version.
*     4/8/94   (ACD): First stable version.
*     24/6/96  (ACD): Created CAP version from SuperCOSMOS routine CPC_R2SGF.
*     20/12/96 (ACD): Removed non-standard Format statement revealed by
*       the port to Linux.
*     16/3/98  (ACD): Corrected bug in Format statement for angles with
*       a non-zero number of decimal places of seconds and allowed
*       angles to have three digits of degrees (though these will not
*       be alinged properly).
*     25/7/00  (ACD): Corrected spelling mistake.
*-
*  Type Definitions:
      IMPLICIT NONE       ! No implicit typing
*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
*  Arguments Given:
      DOUBLE PRECISION
     :  RANGLE
      CHARACTER
     :  DUNITS*(*)
      INTEGER
     :  DECPL
*  Arguments Returned:
      CHARACTER
     :  SANGLE*(*)
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  HDMSF(4),    ! Hours or degrees, minutes, seconds and fraction.
     :  LOOP,        ! Loop index.
     :  SANGLL,      ! Length of ANGLE  (excl. trail. blanks).
     :  BUFLEN,      !   "    "  BUFFER ( "  .   "  .   "   ).
     :  LSTAT,       ! Local internal Fortran I/O status.
     :  LASTAT       ! Local ADAM status.
      CHARACTER
     :  SIGN*1,      ! Sign of the angle.
     :  BUFFER*30,   ! Local buffer for the converted angle.
     :  RTFMT*80     ! Run-time format string.
      LOGICAL
     :  CONVOK       ! Flag; conversion to sexagesimal value ok?
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Convert the value to sexagismal hours or degrees, as appropriate.
*       The destination units, DUNITS, determines whether the value
*       is converted to hours, degrees, or returned as a simple value.

         CONVOK = .TRUE.

         IF (DUNITS(1 : 1) .EQ. 'H') THEN
            CALL SLA_DR2TF (DECPL, RANGLE, SIGN, HDMSF)

         ELSE IF (DUNITS(1 : 1) .EQ. 'D') THEN
            CALL SLA_DR2AF (DECPL, RANGLE, SIGN, HDMSF)

         ELSE
            CONVOK = .FALSE.

            CALL MSG_OUT (' ', 'Illegal units specified for '/
     :        /'formatting a sexagesimal value.', STATUS)

            SANGLE = ' '
            SANGLL = 0

            CALL CHR_PUTD (RANGLE, SANGLE, SANGLL)
         END IF

*
*       Proceed if the conversion succeeded.

         IF (CONVOK) THEN

*
*          Attempt to write the value obtained to an internal character
*          string and proceed if ok.

            BUFFER = ' '

            IF (DECPL .GT. 0) THEN
               WRITE(RTFMT, 2000, IOSTAT=LSTAT) DECPL
 2000          FORMAT('(A1, I3, '':'', I2, '':'', I2, ''.'', I', I1,
     :           ')')

               WRITE(BUFFER, RTFMT, IOSTAT=LSTAT) SIGN, HDMSF(1),
     :           HDMSF(2), HDMSF(3), HDMSF(4)
            ELSE
               WRITE(BUFFER, 2001, IOSTAT=LSTAT) SIGN, HDMSF(1),
     :           HDMSF(2), HDMSF(3)
 2001          FORMAT(A1, I3, ':', I2, ':', I2)
            END IF

            LASTAT = SAI__OK
            CALL FIO_SERR (LSTAT, LASTAT)

            IF (LASTAT .EQ. SAI__OK) THEN

*
*             Remove any leading spaces.

               CALL CHR_LDBLK (BUFFER)

*
*             Replace any leading spaces in the minutes and seconds with
*             zeros.

               BUFLEN = CHR_LEN(BUFFER)

               DO LOOP = 4, BUFLEN
                  IF (BUFFER(LOOP : LOOP) .EQ. ' ') THEN
                     BUFFER(LOOP : LOOP) = '0'
                  END IF
               END DO

*
*             Remove any remaining spaces (which can only be between the
*             sign and the degrees or hours).

               CALL CHR_RMBLK (BUFFER)

*
*             Insert the appropriate number of leading spaces to compensate
*             for hours or degrees with less than the maximum possible
*             number of digits.
*
*             Note that only numbers with an absolute value of less than
*             100 are handled.  That is, in practice it is assumed that
*             angles in degrees will be in the range -90 to + 90.

               SANGLE = ' '
               SANGLL = 0

               IF (HDMSF(1) .LT. 10) THEN
                  SANGLL = SANGLL + 1
               END IF

*
*             Copy the assembled string to the return argument.

               BUFLEN = CHR_LEN(BUFFER)
               CALL CHR_PUTC (BUFFER(1 : BUFLEN), SANGLE, SANGLL)

            ELSE
               SANGLE = ' '
               SANGLL = 0

               CALL CHR_PUTD (RANGLE, SANGLE, SANGLL)
            END IF

         END IF

      END IF

      END
