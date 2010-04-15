      SUBROUTINE SCULIB_READ_JIGGLE (JIGGLE_NAME, MAX_JIGGLE,
     :  JIGGLE_P_SWITCH, JIGGLE_REPEAT, EXP_PER_INT, JIGGLE_X, JIGGLE_Y,
     :  JIGGLE_COUNT, JIGGLE_X_MAX, JIGGLE_X_MIN, JIGGLE_Y_MAX,
     :  JIGGLE_Y_MIN, STATUS)
*+
*  Name:
*     SCULIB_READ_JIGGLE

*  Purpose:
*     read a jiggle pattern

*  Description:
*     This routine reads in a jiggle pattern and sets some variables associated
*     with it; the number of jiggles in the pattern, the number of jiggles to
*     be measured in each exposure, the number of times the pattern will be
*     repeated in each exposure, the maximum offsets in the pattern.
*
*        After checking status on entry, SCULIB_READ_NUMBERS is called to read
*     in the jiggle offsets from the file named in JIGGLE_NAME, an error will
*     be reported and bad status returned if no offsets are read. The maximum
*     and minimum x and y offsets in the pattern are calculated.
*
*        Next, the variables governing the way the jiggle pattern will be
*     divided among the exposures making up each integration is worked out.
*     An error will be reported and bad status returned if JIGGLE_P_SWITCH
*     is less than or equal to zero. Otherwise, the number of exposures
*     required to execute the whole jiggle pattern, each exposure containing
*     JIGGLE_P_SWITCH jiggles (except perhaps the last which will hold less),
*     is calculated. If JIGGLE_P_SWITCH is larger than the number of jiggles
*     in the pattern then the jiggle pattern will be repeated an integer
*     number of times during each switch of the exposure and JIGGLE_P_SWITCH
*     will be reset accordingly.

*  Invocation:
*     CALL SCULIB_READ_JIGGLE (JIGGLE_NAME, MAX_JIGGLE,
*    :  JIGGLE_P_SWITCH, JIGGLE_REPEAT, EXP_PER_INT, JIGGLE_X, JIGGLE_Y,
*    :  JIGGLE_COUNT, JIGGLE_X_MAX, JIGGLE_X_MIN, JIGGLE_Y_MAX,
*    :  JIGGLE_Y_MIN, STATUS)

*  Arguments:
*     JIGGLE_NAME            = CHARACTER*(*) (Given)
*           the name of the file containing the jiggle pattern
*     MAX_JIGGLE             = INTEGER (Given)
*           the maximum number of jiggles that can be read
*     JIGGLE_P_SWITCH        = INTEGER (Given and returned)
*           the requested (given) and actual (returned) number of jiggles
*           that will be performed in each exposure of the integration
*     JIGGLE_REPEAT          = INTEGER (Returned)
*           the number of times the jiggle pattern wil be repeated in each
*           exposure
*     EXP_PER_INT            = INTEGER (Returned)
*           the number of exposures required per integration
*     JIGGLE_X (MAX_JIGGLE)  = REAL (Returned)
*           the x jiggle offsets
*     JIGGLE_Y (MAX_JIGGLE)  = REAL (Returned)
*           the y jiggle offsets
*     JIGGLE_COUNT           = INTEGER (Returned)
*           the number of jiggles in the pattern
*     JIGGLE_X_MAX           = REAL (Returned)
*           the maximum jiggle offset in the x axis
*     JIGGLE_X_MIN
*           the minimum jiggle offset in the x axis
*     JIGGLE_Y_MAX           = REAL (Returned)
*           the maximum jiggle offset in the y axis
*     JIGGLE_Y_MIN
*           the minimum jiggle offset in the y axis
*     STATUS =       INTEGER (Given and returned)
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
*     14-AUG-1993: Original version
*      3-OCT-1994: Code-read and improvements incorporated (JFL).
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) JIGGLE_NAME
      INTEGER MAX_JIGGLE

*  Arguments Given & Returned:
      INTEGER JIGGLE_P_SWITCH

*  Arguments Returned:
      INTEGER JIGGLE_REPEAT
      INTEGER EXP_PER_INT
      REAL JIGGLE_X (MAX_JIGGLE), JIGGLE_Y (MAX_JIGGLE)
      INTEGER JIGGLE_COUNT
      REAL JIGGLE_X_MIN, JIGGLE_X_MAX
      REAL JIGGLE_Y_MIN, JIGGLE_Y_MAX

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I                              ! DO loop

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  read in jiggle offsets

      CALL SCULIB_READ_NUMBERS (JIGGLE_NAME, 2, MAX_JIGGLE,
     :  JIGGLE_X, JIGGLE_Y, 0, JIGGLE_COUNT, STATUS)

      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP (' ', 'SCULIB_READ_JIGGLE: error reading '//
     :     'jiggle pattern', STATUS)
      END IF

      IF (JIGGLE_COUNT .LE. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('VAL', JIGGLE_COUNT)
            CALL ERR_REP (' ', 'SCULIB_READ_JIGGLE: invalid number '//
     :        'of jiggles - ^VAL', STATUS)
         END IF
      END IF

*  calculate the maximum offsets

      IF (STATUS .EQ. SAI__OK) THEN
         JIGGLE_X_MAX = JIGGLE_X (1)
         JIGGLE_X_MIN = JIGGLE_X (1)
         JIGGLE_Y_MAX = JIGGLE_Y (1)
         JIGGLE_Y_MIN = JIGGLE_Y (1)
         DO I = 1, JIGGLE_COUNT
            JIGGLE_X_MAX = MAX (JIGGLE_X_MAX,JIGGLE_X (I))
            JIGGLE_X_MIN = MIN (JIGGLE_X_MIN,JIGGLE_X (I))
            JIGGLE_Y_MAX = MAX (JIGGLE_Y_MAX,JIGGLE_Y (I))
            JIGGLE_Y_MIN = MIN (JIGGLE_Y_MIN,JIGGLE_Y (I))
         END DO
      END IF

*  and calculate the number of exposures that will be required to execute all
*  the jiggle positions

      IF (STATUS .EQ. SAI__OK) THEN
         IF (JIGGLE_P_SWITCH .LE. 0) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('VAL', JIGGLE_P_SWITCH)
            CALL ERR_REP (' ', 'SCULIB_READ_JIGGLE: invalid number '//
     :        'of jiggles per switch - ^VAL', STATUS)
         ELSE
            IF (JIGGLE_P_SWITCH .GT. JIGGLE_COUNT) THEN
               EXP_PER_INT = 1
               IF (MOD(JIGGLE_P_SWITCH,JIGGLE_COUNT) .EQ. 0) THEN
                  JIGGLE_REPEAT = JIGGLE_P_SWITCH / JIGGLE_COUNT
               ELSE
                  JIGGLE_REPEAT = (JIGGLE_P_SWITCH / JIGGLE_COUNT) + 1
                  JIGGLE_P_SWITCH = JIGGLE_COUNT * JIGGLE_REPEAT
               END IF
            ELSE
               IF (MOD(JIGGLE_COUNT,JIGGLE_P_SWITCH) .EQ. 0) THEN
                  EXP_PER_INT = JIGGLE_COUNT / JIGGLE_P_SWITCH
                  JIGGLE_REPEAT = 1
               ELSE
                  EXP_PER_INT = (JIGGLE_COUNT / JIGGLE_P_SWITCH) + 1
                  JIGGLE_REPEAT = 1
               END IF
            END IF
         END IF
      END IF


      END
