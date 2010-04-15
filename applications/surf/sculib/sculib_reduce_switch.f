      SUBROUTINE SCULIB_REDUCE_SWITCH (CHOP_FUN, SWITCH_PER_EXP,
     :  N_DATA,
     :  SWITCH_1_DATA, SWITCH_1_VARIANCE, SWITCH_1_QUALITY,
     :  SWITCH_2_DATA, SWITCH_2_VARIANCE, SWITCH_2_QUALITY,
     :  SWITCH_3_DATA, SWITCH_3_VARIANCE, SWITCH_3_QUALITY,
     :  BEAM,
     :  EXP_DATA, EXP_VARIANCE, EXP_QUALITY, WEIGHT, STATUS)
*+
*  Name:
*     SCULIB_REDUCE_SWITCH

*  Purpose:
*     reduce the demodulated data from the switches of
*     an exposure into the exposure result.

*  Description:
*     This routine reduces the switches in an exposure to give the
*     exposure result, and returns the weight to be given to the
*     specified projected bolometer position when combining different
*     projected bolometers into the result for a sub-instrument.
*       Exposure results are calculated assuming that:-
*
*     For a SQUARE chop-function -
*      switch 1 has the object in the R beam of the `middle' projected
*      bolometer and in the L beam of the `right' bolometer.
*      switch 2 has the object in the L beam of the `middle' bolometer
*      and in R beam of the `left' bolometer.
*
*     For a TRIPOS (3-position) chop-function -
*      switch 1 has the object in the M beam of the `middle' bolometer,
*      in the L beam of the right and the R beam of the `left'.
*      switch 2 has the object in the R beam of the `middle' bolometer
*      and in the M beam of the `right'.
*      switch 3 has the object in the L beam of the `middle' bolometer
*      and in the M beam of the `left'.
*
*     The reduction method depends on the number of switches taken per
*     exposure, the chop function used and the projected beam that the
*     bolometers are assumed to be in.
*
*     SWITCH_PER_EXP = 1
*        CHOP_FUN = SQUARE (SCUBAWAVE or RAMPWAVE)
*           left bolometer = BAD (was never looking at source), weight =  0.0
*         middle bolometer = switch 1, weight = 1.0
*          right bolometer = - switch 1, weight = 1.0
*        CHOP_FUN = TRIPOS
*           left bolometer = - 2 * switch 1, weight = 0.5
*         middle bolometer = switch 1, weight = 1.0
*          right bolometer = - 2 * switch 1, weight = 0.5
*
*     SWITCH_PER_EXP = 2
*        CHOP_FUN = SQUARE
*           left bolometer = switch 2 - switch 1, weight = 0.5
*         middle bolometer = (switch 1 - switch 2) / 2, weight = 1.0
*          right bolometer = switch 2 - switch 1, weight = 0.5
*        CHOP_FUN = TRIPOS
*           left bolometer = - 2 * (switch 1 - switch 2), weight = 0.5
*         middle bolometer = 2/3 * (switch 1 - switch 2), weight = 1.5
*          right bolometer = -2/3 * (switch 1 - switch 2), weight = 1.5
*
*     SWITCH_PER_EXP = 3
*        CHOP_FUN = SQUARE
*           error
*        CHOP_FUN = TRIPOS
*           left bolometer = -1/2 * (2 * switch 1 - (switch 2 + switch 3)),
*                            weight = 2/3
*         middle bolometer = 1/3 * (2 * switch 1 - (switch 2 + switch 3)),
*                            weight = 1.0
*          right bolometer = -1/2 * (2 * switch 1 - (switch 2 + switch 3)),
*                            weight = 2/3
*
*     Any other combinations of parameters will give rise to an error report
*     and the routine will return with bad status.

*  Invocation:
*     CALL SCULIB_REDUCE_SWITCH (CHOP_FUN, SWITCH_PER_EXP,
*    :  N_DATA,
*    :  SWITCH_1_DATA, SWITCH_1_VARIANCE, SWITCH_1_QUALITY,
*    :  SWITCH_2_DATA, SWITCH_2_VARIANCE, SWITCH_2_QUALITY,
*    :  SWITCH_3_DATA, SWITCH_3_VARIANCE, SWITCH_3_QUALITY,
*    :  BEAM,
*    :  EXP_DATA, EXP_VARIANCE, EXP_QUALITY, WEIGHT, STATUS)

*  Arguments:
*     CHOP_FUN                              = CHARACTER*(*) (Given)
*           the chop function used
*     SWITCH_PER_EXP                        = INTEGER (Given)
*           the number of switches per exposure
*     N_DATA                                = INTEGER (Given)
*           the number of measurements
*     SWITCH_1_DATA (N_DATA)                = REAL (Given)
*           data for switch 1
*     SWITCH_1_VARIANCE (N_DATA)            = REAL (Given)
*           variance for switch 1
*     SWITCH_1_QUALITY (N_DATA)             = BYTE (Given)
*           quality for switch 1
*     SWITCH_2_DATA (N_DATA)                = REAL (Given)
*           likewise for switch 2
*     SWITCH_2_VARIANCE (N_DATA)            = REAL (Given)
*     SWITCH_2_QUALITY (N_DATA)             = BYTE (Given)
*     SWITCH_3_DATA (N_DATA)                = REAL (Given)
*           and switch 3
*     SWITCH_3_VARIANCE (N_DATA)            = REAL (Given)
*     SWITCH_3_QUALITY (N_DATA)             = BYTE (Given)
*     BEAM                                  = INTEGER (Given)
*           the projected beam assumed for the bolometers; 1 = LEFT
*           2 = MIDDLE, 3 = RIGHT
*     EXP_DATA (N_DATA)                     = REAL (Returned)
*           the exposure result
*     EXP_VARIANCE (N_DATA)                 = REAL (Returned)
*           the variance on the result
*     EXP_QUALITY (N_DATA)                  = BYTE (Returned)
*           the quality on the result
*     WEIGHT                                = REAL (Returned)
*           the relative weight to assign to this projected-beam compared
*           to the others when adding them together to give a final result
*     STATUS                                = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (REVAD::JFL)
*     TIMJ: Tim Jenness (t.jenness@jach.hawaii.edu)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     5-AUG-1993: Original version.
*     18-NOV-1994: Data arrays made 1-d (JFL).
*     $Log$
*     Revision 1.5  1999/08/19 03:37:21  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.4  1999/08/03 19:35:23  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.3  1998/07/23 23:11:20  timj
*     Check for bad pixels
*
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      CHARACTER*(*) CHOP_FUN
      INTEGER SWITCH_PER_EXP
      INTEGER N_DATA
      REAL SWITCH_1_DATA (N_DATA)
      REAL SWITCH_1_VARIANCE (N_DATA)
      BYTE SWITCH_1_QUALITY (N_DATA)
      REAL SWITCH_2_DATA (N_DATA)
      REAL SWITCH_2_VARIANCE (N_DATA)
      BYTE SWITCH_2_QUALITY (N_DATA)
      REAL SWITCH_3_DATA (N_DATA)
      REAL SWITCH_3_VARIANCE (N_DATA)
      BYTE SWITCH_3_QUALITY (N_DATA)
      INTEGER BEAM

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL EXP_DATA (N_DATA)
      REAL EXP_VARIANCE (N_DATA)
      BYTE EXP_QUALITY (N_DATA)
      REAL WEIGHT

*  Status:
      INTEGER STATUS

*  External references:
      BYTE SCULIB_BITOR

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I                    ! DO loop index

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  reduction method depends of chop function, number of switches and
*  projected beam assumed for bolometers

      IF (SWITCH_PER_EXP .EQ. 1) THEN

         IF (CHOP_FUN .EQ. 'SQUARE' .OR. CHOP_FUN.EQ.'RAMPWAVE'
     :        .OR. CHOP_FUN .EQ. 'SCUBAWAVE') THEN

            IF (BEAM .EQ. 1) THEN

               DO I = 1, N_DATA
                  WEIGHT = 0.0
                  EXP_QUALITY (I) = 1      ! Switch on INVALID QUAL flag
               END DO

            ELSE IF (BEAM .EQ. 2) THEN

               DO I = 1, N_DATA
                  WEIGHT = 1.0

                  EXP_DATA (I) = SWITCH_1_DATA (I)
                  EXP_VARIANCE (I) = SWITCH_1_VARIANCE (I)
                  EXP_QUALITY (I) = SWITCH_1_QUALITY(I)

               END DO

            ELSE IF (BEAM .EQ. 3) THEN

               DO I = 1, N_DATA
                  WEIGHT = 1.0

                  EXP_DATA (I) = - SWITCH_1_DATA (I)
                  EXP_VARIANCE (I) = SWITCH_1_VARIANCE (I)
                  EXP_QUALITY (I) = SWITCH_1_QUALITY(I)

               END DO

            ELSE

               CALL MSG_SETI ('BEAM', BEAM)
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_REDUCE_SWITCH: invalid '//
     :           'projected beam number - ^BEAM', STATUS)

            END IF

         ELSE IF (CHOP_FUN .EQ. 'TRIPOS') THEN

            IF ((BEAM .EQ. 1) .OR. (BEAM .EQ. 3)) THEN

               DO I = 1, N_DATA
                  WEIGHT = 0.5

                  EXP_DATA (I) = -2.0 * SWITCH_1_DATA (I)
                  EXP_VARIANCE (I) = SWITCH_1_VARIANCE (I)
                  EXP_QUALITY (I) = SWITCH_1_QUALITY(I)

               END DO

            ELSE IF (BEAM .EQ. 2) THEN

               DO I = 1, N_DATA
                  WEIGHT = 1.0

                  EXP_DATA (I) = SWITCH_1_DATA (I)
                  EXP_VARIANCE (I) = SWITCH_1_VARIANCE (I)
                  EXP_QUALITY (I) = SWITCH_1_QUALITY(I)

               END DO

            ELSE

               CALL MSG_SETI ('BEAM', BEAM)
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_REDUCE_SWITCH: invalid '//
     :           'projected beam number - ^BEAM', STATUS)

            END IF

         ELSE

            CALL MSG_SETC ('CHOP', CHOP_FUN)
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_REDUCE_SWITCH: invalid '//
     :        'chop function - ^CHOP', STATUS)

         END IF

      ELSE IF (SWITCH_PER_EXP .EQ. 2) THEN

         IF (CHOP_FUN .EQ. 'SQUARE' .OR. CHOP_FUN.EQ.'RAMPWAVE'
     :        .OR. CHOP_FUN .EQ. 'SCUBAWAVE') THEN

            IF ((BEAM .EQ. 1) .OR. (BEAM .EQ. 3)) THEN
               WEIGHT = 0.5

               DO I = 1, N_DATA

                  IF (SWITCH_1_DATA(I) .NE. VAL__BADR .AND.
     :                 SWITCH_2_DATA(I) .NE. VAL__BADR) THEN
                     EXP_DATA (I) = SWITCH_2_DATA (I) -
     :                    SWITCH_1_DATA (I)
                  ELSE
                     EXP_DATA(I) = VAL__BADR
                  END IF

                  IF (SWITCH_1_VARIANCE(I) .NE. VAL__BADR .AND.
     :                 SWITCH_2_VARIANCE(I) .NE. VAL__BADR) THEN
                     EXP_VARIANCE (I) =
     :                    SWITCH_2_VARIANCE (I) +
     :                    SWITCH_1_VARIANCE (I)
                  ELSE
                     EXP_VARIANCE(I) = VAL__BADR
                  END IF

                  EXP_QUALITY (I) = SCULIB_BITOR(SWITCH_1_QUALITY(I),
     :                 SWITCH_2_QUALITY(I))

               END DO

            ELSE IF (BEAM .EQ. 2) THEN
               WEIGHT = 1.0

               DO I = 1, N_DATA

                  IF (SWITCH_1_DATA(I) .NE. VAL__BADR .AND.
     :                 SWITCH_2_DATA(I) .NE. VAL__BADR) THEN
                     EXP_DATA (I) = (SWITCH_1_DATA (I) -
     :                    SWITCH_2_DATA (I)) / 2.0
                  ELSE
                     EXP_DATA(I) = VAL__BADR
                  END IF
                  IF (SWITCH_1_VARIANCE(I) .NE. VAL__BADR .AND.
     :                 SWITCH_2_VARIANCE(I) .NE. VAL__BADR) THEN
                     EXP_VARIANCE (I) =
     :                    (SWITCH_2_VARIANCE (I) +
     :                    SWITCH_1_VARIANCE (I)) / 4.0
                  ELSE
                     EXP_VARIANCE(I) = VAL__BADR
                  END IF
                  EXP_QUALITY (I) = SCULIB_BITOR(SWITCH_1_QUALITY(I),
     :                 SWITCH_2_QUALITY(I))

               END DO

            ELSE

               CALL MSG_SETI ('BEAM', BEAM)
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_REDUCE_SWITCH: invalid '//
     :           'projected beam number - ^BEAM', STATUS)

            END IF

         ELSE IF (CHOP_FUN .EQ. 'TRIPOS') THEN

            IF (BEAM .EQ. 1) THEN
               WEIGHT = 0.5

               DO I = 1, N_DATA

                  IF (SWITCH_1_DATA(I) .NE. VAL__BADR .AND.
     :                 SWITCH_2_DATA(I) .NE. VAL__BADR) THEN
                     EXP_DATA (I) = - 2.0 * (SWITCH_1_DATA (I) -
     :                    SWITCH_2_DATA (I))
                  ELSE
                     EXP_DATA(I) = VAL__BADR
                  END IF

                  IF (SWITCH_1_VARIANCE(I) .NE. VAL__BADR .AND.
     :                 SWITCH_2_VARIANCE(I) .NE. VAL__BADR) THEN
                     EXP_VARIANCE (I) = 4.0 * (SWITCH_2_VARIANCE (I) +
     :                    SWITCH_1_VARIANCE (I))
                  ELSE
                     EXP_DATA(I) = VAL__BADR
                  END IF

                  EXP_QUALITY (I) = SCULIB_BITOR(SWITCH_1_QUALITY(I),
     :                 SWITCH_2_QUALITY(I))

               END DO

            ELSE IF (BEAM .EQ. 2) THEN
               WEIGHT = 1.5

               DO I = 1, N_DATA
                  IF (SWITCH_1_DATA(I) .NE. VAL__BADR .AND.
     :                 SWITCH_2_DATA(I) .NE. VAL__BADR) THEN
                     EXP_DATA (I) = 2.0 * (SWITCH_1_DATA (I) -
     :                    SWITCH_2_DATA (I)) / 3.0
                  ELSE
                     EXP_DATA(I) = VAL__BADR
                  END IF

                  IF (SWITCH_1_VARIANCE(I) .NE. VAL__BADR .AND.
     :                 SWITCH_2_VARIANCE(I) .NE. VAL__BADR) THEN
                     EXP_VARIANCE (I) = 4.0 *
     :                    (SWITCH_2_VARIANCE (I) +
     :                    SWITCH_1_VARIANCE (I)) / 9.0
                  ELSE
                     EXP_DATA(I) = VAL__BADR
                  END IF

                  EXP_QUALITY (I) = SCULIB_BITOR(SWITCH_1_QUALITY(I),
     :                 SWITCH_2_QUALITY(I))
               END DO

            ELSE IF (BEAM .EQ. 3) THEN
               WEIGHT = 1.5

               DO I = 1, N_DATA
                  IF (SWITCH_1_DATA(I) .NE. VAL__BADR .AND.
     :                 SWITCH_2_DATA(I) .NE. VAL__BADR) THEN
                     EXP_DATA (I) = - 2.0 * (SWITCH_1_DATA (I) -
     :                    SWITCH_2_DATA (I)) / 3.0
                  ELSE
                     EXP_DATA(I) = VAL__BADR
                  END IF

                  IF (SWITCH_1_VARIANCE(I) .NE. VAL__BADR .AND.
     :                 SWITCH_2_VARIANCE(I) .NE. VAL__BADR) THEN
                     EXP_VARIANCE (I) = 4.0 * (SWITCH_2_VARIANCE (I) +
     :                    SWITCH_1_VARIANCE (I)) / 9.0
                  ELSE
                     EXP_VARIANCE(I) = VAL__BADR
                  END IF

                  EXP_QUALITY (I) = SCULIB_BITOR(SWITCH_1_QUALITY(I),
     :                 SWITCH_2_QUALITY(I))

               END DO

            ELSE

               CALL MSG_SETI ('BEAM', BEAM)
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_REDUCE_SWITCH: invalid '//
     :           'projected beam number - ^BEAM', STATUS)

            END IF

         ELSE

            CALL MSG_SETC ('CHOP', CHOP_FUN)
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_REDUCE_SWITCH: invalid '//
     :        'chop function - ^CHOP', STATUS)

         END IF

      ELSE IF (SWITCH_PER_EXP .EQ. 3) THEN

         IF (CHOP_FUN .EQ. 'SQUARE' .OR. CHOP_FUN.EQ.'RAMPWAVE'
     :        .OR. CHOP_FUN .EQ. 'SCUBAWAVE') THEN


            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_REDUCE_SWITCH: 3 switches '//
     :        'for a square chop is an invalid combination', STATUS)

         ELSE IF (CHOP_FUN .EQ. 'TRIPOS') THEN

            IF ((BEAM .EQ. 1) .OR. (BEAM .EQ. 3)) THEN
               WEIGHT = 2.0 / 3.0

               DO I = 1, N_DATA

                  EXP_DATA (I) = - (2.0 * SWITCH_1_DATA (I) -
     :                 (SWITCH_2_DATA (I) + SWITCH_3_DATA(I))) / 2.0
                  EXP_VARIANCE (I) =
     :                 (4.0 * SWITCH_1_VARIANCE (I) +
     :                 (SWITCH_2_VARIANCE (I) +
     :                 SWITCH_3_VARIANCE(I))) / 4.0
                  EXP_QUALITY (I) = SCULIB_BITOR(SWITCH_1_QUALITY(I),
     :                 SWITCH_2_QUALITY(I))
                  EXP_QUALITY (I) = SCULIB_BITOR(EXP_QUALITY(I),
     :                 SWITCH_3_QUALITY(I))

               END DO

            ELSE IF (BEAM .EQ. 2) THEN
               WEIGHT = 1.0

               DO I = 1, N_DATA

                  EXP_DATA (I) = - (2.0 * SWITCH_1_DATA (I) -
     :                 (SWITCH_2_DATA (I) + SWITCH_3_DATA(I))) / 3.0
                  EXP_VARIANCE (I) =
     :                 (4.0 * SWITCH_1_VARIANCE (I) +
     :                 (SWITCH_2_VARIANCE (I) +
     :                 SWITCH_3_VARIANCE(I))) / 9.0
                  EXP_QUALITY (I) = SCULIB_BITOR(SWITCH_1_QUALITY(I),
     :                 SWITCH_2_QUALITY(I))
                  EXP_QUALITY (I) = SCULIB_BITOR(EXP_QUALITY(I),
     :                 SWITCH_3_QUALITY(I))

               END DO

            ELSE

               CALL MSG_SETI ('BEAM', BEAM)
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_REDUCE_SWITCH: invalid '//
     :           'projected beam number - ^BEAM', STATUS)

            END IF

         ELSE

            CALL MSG_SETC ('CHOP', CHOP_FUN)
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_REDUCE_SWITCH: invalid '//
     :        'chop function - ^CHOP', STATUS)

         END IF

      ELSE

         CALL MSG_SETI ('NUMB', SWITCH_PER_EXP)
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_REDUCE_SWITCH: invalid number '//
     :     'of switches per exposure - ^SWITCH_PER_EXP', STATUS)

      END IF

      END
