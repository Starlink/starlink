      SUBROUTINE SCULIB_DECODE_ANGLE (STRING, ANGLE, STATUS)
*+
*  Name:
*     SCULIB_DECODE_ANGLE

*  Purpose:
*     convert angle string to double precision angle

*  Description:
*     This routine converts an angle in dd:mm:ss.dd format into a
*     double precision number in radians. It assumes that the input
*     string specifies the angle in degrees. The process involves
*     removing the ':' delimiters, then calling SLA_DAFIN to perform
*     the conversion. An error will be returned if there are less than 2 ':'
*     delimiters in the string, or if the SLA routine errors.

*  Invocation:
*     CALL SCULIB_DECODE_ANGLE (STRING, ANGLE, STATUS)

*  Arguments:
*     STRING         = CHARACTER*(*) (Given)
*           angle in xx:mm:ss.ddd format
*     ANGLE          = DOUBLE PRECISION (Returned)
*           angle in radians
*     STATUS         = INTEGER (Given and returned)
*           Global status

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  Method:

*  Deficiencies:

*  Bugs:

*  History:
*     $Id$
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) STRING

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION ANGLE

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I                 !
      INTEGER CHR_STATUS        !
      INTEGER CPOS              !
      INTEGER NSTRT             !
      INTEGER SLA_STATUS        !
      CHARACTER*80 COPY         ! copy of input string

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  take local copy of string, watching out for truncation

      CALL CHR_COPY (STRING, .FALSE., COPY, CHR_STATUS)
      IF (CHR_STATUS .NE. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_DECODE_ANGLE: input string is '//
     :     'too long', STATUS)
      END IF


      IF (STATUS .EQ. SAI__OK) THEN

*  strip : delimiters from string

         DO I = 1, 2
            CPOS = INDEX (COPY, ':')
            IF (CPOS .NE. 0) THEN
               COPY(CPOS:CPOS) = ' '
            ELSE
               STATUS = SAI__ERROR
            END IF
         END DO

*  convert to radians

         IF (STATUS .EQ. SAI__OK) THEN
            NSTRT = 1
            CALL SLA_DAFIN (COPY, NSTRT, ANGLE, SLA_STATUS)
            IF (SLA_STATUS .NE. 0) THEN
               STATUS = SAI__ERROR
            END IF
         END IF

      END IF

*  report error

      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_SETC ('STRING', STRING)
         CALL ERR_REP (' ', 'SCULIB_DECODE_ANGLE: error decoding '//
     :     'angle - ^STRING', STATUS)
      END IF

      END
