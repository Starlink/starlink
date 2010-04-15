      SUBROUTINE SCULIB_BOLNAME (ADC, CHANNEL, BOLCODE, STATUS)
*+
*  Name:
*     SCULIB_BOLNAME

*  Purpose:
*     generate bolometer name from ADC and channel number

*  Description:
*     Given bolometer ADC and channel, generate the bolometer name. E.g.
*     ADC=2, CHAN=5 gives B5. If the ADC number is outside the range 1-9, or
*     the channel number outside 1-16, and error will be reported and bad status
*     returned.

*  Invocation:
*     CALL SCULIB_BOLNAME (ADC, CHANNEL, BOLCODE, STATUS)

*  Arguments:
*     ADC                         = INTEGER (Given)
*           ADC number
*     CHANNEL                     = INTEGER (Given)
*           channel number
*     BOLCODE                     = CHARACTER*(*) (Returned)
*           bolometer ID
*     STATUS                      = INTEGER (Given and returned)
*           global status

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
*     25-MAY-1993: Original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER ADC
      INTEGER CHANNEL

*  Arguments Returned:
      CHARACTER*(*) BOLCODE

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN              ! CHR string length function

*  Local variables:
      INTEGER SLEN                 !
      CHARACTER*1 ADC_NAME (9)     !

*  Local data:
      DATA ADC_NAME / 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'/

*.

      IF (STATUS .NE. SAI__OK) RETURN

      BOLCODE = ' '

*  check ADC, channel numbers are valid

      IF ((ADC.LT.1) .OR. (ADC.GT.9)) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI ('ADC', ADC)
         CALL ERR_REP (' ', 'SCULIB_BOL_NAME: ADC number out of '//
     :     'range - ^ADC', STATUS)
      END IF

      IF ((CHANNEL.LT.1) .OR. (CHANNEL.GT.16)) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI ('CHAN', CHANNEL)
         CALL ERR_REP (' ', 'SCULIB_BOL_NAME: channel number out of '//
     :     'range - ^CHAN', STATUS)
      END IF


      IF (STATUS .EQ. SAI__OK) THEN

*  OK, generate name

         BOLCODE = ADC_NAME (ADC)
         SLEN = CHR_LEN (BOLCODE)
         CALL CHR_PUTI (CHANNEL, BOLCODE, SLEN)

      END IF

      END
