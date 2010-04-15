      SUBROUTINE SCULIB_BOLDECODE (BOLCODE, ADC, CHANNEL, STATUS)
*+
*  Name:
*     SCULIB_BOLDECODE

*  Purpose:
*     decode a bolometer ID into ADC and channel number

*  Description:
*     Given a character string containing a bolometer ID, put ADC and
*     CHANNEL number into output variables. The syntax of the bolometer
*     ID is a3, A3 or 3. Allowed ADCs run from a-i. Channels
*     must be in range 1-16. Bolometer IDs outside these ranges will cause
*     an error to be returned.

*  Invocation:
*     CALL SCULIB_BOLDECODE (BOLCODE, ADC, CHANNEL, STATUS)

*  Arguments:
*     BOLCODE                     = CHARACTER*(*) (given)
*           bolometer ID
*     ADC                         = INTEGER (Returned)
*           ADC number
*     CHANNEL                     = INTEGER (Returned)
*           channel number
*     STATUS                      = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (REVAD::JFL), adapted from transputer routine by IAS.

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  History:
*     $Id$
*    endhistory

*  Method:

*  Deficiencies:

*  Bugs:


*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) BOLCODE

*  Arguments Returned:
      INTEGER ADC
      INTEGER CHANNEL

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN              ! CHR string length function

*  Local variables:
      INTEGER I                    !

*.

      IF (STATUS .NE. SAI__OK) RETURN

      ADC = 0
      CHANNEL = 0

      CALL CHR_UCASE (BOLCODE)

*  convert leading letter to ADC number

      IF (BOLCODE(1:1) .EQ. 'A') THEN
         ADC = 1
      ELSE IF (BOLCODE(1:1) .EQ. 'B') THEN
         ADC = 2
      ELSE IF (BOLCODE(1:1) .EQ. 'C') THEN
         ADC = 3
      ELSE IF (BOLCODE(1:1) .EQ. 'D') THEN
         ADC = 4
      ELSE IF (BOLCODE(1:1) .EQ. 'E') THEN
         ADC = 5
      ELSE IF (BOLCODE(1:1) .EQ. 'F') THEN
         ADC = 6
      ELSE IF (BOLCODE(1:1) .EQ. 'G') THEN
         ADC = 7
      ELSE IF (BOLCODE(1:1) .EQ. 'H') THEN
         ADC = 8
      ELSE IF (BOLCODE(1:1) .EQ. 'I') THEN
         ADC = 9
      ELSE

*  unless the first character is a number, an error has occured

         I = ICHAR (BOLCODE)
         IF ((I.LT.48) .OR. (I.GT.57)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('STRING', BOLCODE)
            CALL ERR_REP (' ', 'SCULIB: bad bolometer ID - ^STRING',
     :        STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN

*  decode channel

         IF (ADC .GT. 0) THEN
            I = 2
         ELSE
            I = 1
         END IF

         IF (CHR_LEN(BOLCODE(I:)) .LT. 1) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('STRING', BOLCODE)
            CALL ERR_REP (' ', 'SCULIB: bad bolometer ID - ^STRING',
     :        STATUS)
         ELSE

            CALL CHR_CTOI (BOLCODE(I:), CHANNEL, STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETC ('STRING', BOLCODE)
               CALL ERR_REP ( ' ', 'SCULIB: bad bolometer ID - ^STRING',
     :           STATUS)
               CHANNEL = 0
            ELSE

*  check that channel lies in allowed range

               IF ((CHANNEL .LT. 1) .OR. (CHANNEL .GT. 16)) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC ('STRING', BOLCODE)
                  CALL ERR_REP (' ', 'SCULIB: bad bolometer channel '//
     :              '- ^STRING', STATUS)
                  CHANNEL = 0
               END IF

            END IF
         END IF
      END IF

      END
