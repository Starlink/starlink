      SUBROUTINE SCULIB_SET_QUALITY (N_BOLS, N_POS, N_BEAM, QUALITY,
     :  START_BOL, END_BOL, START_POS, END_POS, START_BEAM, END_BEAM,
     :  BIT_POS, BIT_VALUE, STATUS)
*+
*  Name:
*     SCULIB_SET_QUALITY

*  Purpose:
*     set quality bits in a subset of a quality array

*  Description:
*     set quality bits in a subset of a quality array

*  Invocation:
*      CALL  SCULIB_SET_QUALITY (N_BOLS, N_POS, N_BEAM, QUALITY,
*    :  START_BOL, END_BOL, START_POS, END_POS, START_BEAM, END_BEAM,
*    :  BIT_POS, BIT_VALUE, STATUS)

*  Arguments:
*     N_BOLS                  = INTEGER (Given)
*           number of bolometers measured
*     N_POS                   = INTEGER (Given)
*           number of positions measured
*     N_BEAM                  = INTEGER (Given)
*           number of beams used
*     QUALITY (N_BOLS, N_POS, N_BEAM) = BYTE (Given and returned)
*           the quality array
*     START_BOL               = INTEGER (Given)
*           index of first bolometer to be set bad
*     END_BOL                 = INTEGER (Given)
*           index of last bolometer to be set bad
*     START_POS               = INTEGER (Given)
*           index of first position to be set bad
*     END_POS                 = INTEGER (Given)
*           index of last position to be set bad
*     START_BEAM              = INTEGER (Given)
*           index of first beam  to be set bad
*     END_BEAM                = INTEGER (Given)
*           index of last beam  to be set bad
*     BIT_POS                 = INTEGER (Given)
*           position of bit ot be set (0 - 7)
*     BIT_VALUE               = INTEGER (Given)
*           value to which bit is to be set (0 if zero, 1 otherwise)
*     STATUS                  = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)
*     T. Jenness  (timj@jach.hawaii.edu)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Log$
*     Revision 1.3  1999/08/19 03:37:25  timj
*     Header tweaks to ease production of SSN72 documentation.
*

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N_BOLS
      INTEGER N_POS
      INTEGER N_BEAM
      INTEGER START_BOL
      INTEGER END_BOL
      INTEGER START_POS
      INTEGER END_POS
      INTEGER START_BEAM
      INTEGER END_BEAM
      INTEGER BIT_POS
      INTEGER BIT_VALUE

*  Arguments Given & Returned:
      BYTE QUALITY (N_BOLS, N_POS, N_BEAM)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      BYTE SCULIB_BITOFF                ! clear a bit
      BYTE SCULIB_BITON                 ! set a bit

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER BEAM                      ! beam index in DO loop
      INTEGER BOL                       ! bolometer index in DO loop
      INTEGER POS                       ! measured position index in DO loop

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  check input parameters

      IF ((BIT_POS .LT.0) .OR. (BIT_POS .GT. 7)) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI ('BIT_POS', BIT_POS)
         CALL ERR_REP (' ', 'SCULIB_SET_QUALITY: bad bit position - '//
     :     '^BIT_POS', STATUS)
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((START_BOL.LT.1) .OR. (START_BOL.GT.N_BOLS)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('START', START_BOL)
            CALL ERR_REP (' ', 'SCULIB_SET_QUALITY: bad value for '//
     :        'START_BOL - ^START', STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((END_BOL.LT.1) .OR. (END_BOL.GT.N_BOLS)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('END', END_BOL)
            CALL ERR_REP (' ', 'SCULIB_SET_QUALITY: bad value for '//
     :        'END_BOL - ^END', STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((START_POS.LT.1) .OR. (START_POS.GT.N_POS)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('START', START_POS)
            CALL ERR_REP (' ', 'SCULIB_SET_QUALITY: bad value for '//
     :        'START_POS - ^START', STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((END_POS.LT.1) .OR. (END_POS.GT.N_POS)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('END', END_POS)
            CALL ERR_REP (' ', 'SCULIB_SET_QUALITY: bad value for '//
     :        'END_POS - ^END', STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((START_BEAM.LT.1) .OR. (START_BEAM.GT.N_BEAM)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('START', START_BEAM)
            CALL ERR_REP (' ', 'SCULIB_SET_QUALITY: bad value for '//
     :        'START_BEAM - ^START', STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((END_BEAM.LT.1) .OR. (END_BEAM.GT.N_BEAM)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('END', END_BEAM)
            CALL ERR_REP (' ', 'SCULIB_SET_QUALITY: bad value for '//
     :        'END_BEAM - ^END', STATUS)
         END IF
      END IF

*  OK, set the quality

      IF (STATUS .EQ. SAI__OK) THEN
         IF (BIT_VALUE .NE. 0) THEN

            DO BEAM = START_BEAM, END_BEAM
               DO POS = START_POS, END_POS
                  DO BOL = START_BOL, END_BOL
                     QUALITY (BOL,POS,BEAM) =
     :                 SCULIB_BITON (QUALITY(BOL,POS,BEAM), BIT_POS)
                  END DO
               END DO
            END DO

         ELSE

            DO BEAM = START_BEAM, END_BEAM
               DO POS = START_POS, END_POS
                  DO BOL = START_BOL, END_BOL
                     QUALITY (BOL,POS,BEAM) =
     :                 SCULIB_BITOFF (QUALITY(BOL,POS,BEAM), BIT_POS)
                  END DO
               END DO
            END DO

         END IF
      END IF

      END
