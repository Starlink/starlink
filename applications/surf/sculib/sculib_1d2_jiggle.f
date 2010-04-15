      SUBROUTINE SCULIB_1D2_JIGGLE (BOL, N_BOLS, J_START, N_JIG,
     :  J_REPEAT, J_COUNT, J_DATA, J_VARIANCE, J_QUALITY, IDIM,
     :  JDIM, I_JIGGLE, J_JIGGLE, DATA_2D, VARIANCE_2D, QUALITY_2D,
     :  STATUS)
*+
*  Name:
*     SCULIB_1D2_JIGGLE

*  Purpose:
*     routine to unpack a 1-d jiggle dataset into a 2-d
*     image

*  Description:
*     This routine unpacks the data for a specified bolometer from a
*     dataset stored according to jiggle number into a 2-d dataset
*     with data stored according to jiggle offset.
*       The input dataset can contain data either from all or part of
*     ONE run through the jiggle pattern or for several REPEATS of
*     the entire pattern. In the first case, the routine copies the
*     input data into the appropriate part of the 2-d image. In the
*     second, the 2-d datum for a given pixel will be the average of the
*     input values for it, and the variance will calculated from the
*     spread of input points around the mean unless only one input point
*     contributed, in which case the input variance will be used.

*  Invocation:
*     CALL SCULIB_1D2_JIGGLE (BOL, N_BOLS, J_START, N_JIG,
*    :  J_REPEAT, J_COUNT, J_DATA, J_VARIANCE, J_QUALITY, IDIM,
*    :  JDIM, I_JIGGLE, J_JIGGLE, DATA_2D, VARIANCE_2D, QUALITY_2D,
*    :  STATUS)

*  Arguments:
*     BOL                          = INTEGER (Given)
*           the number of the bolometer whose data is to be unpacked
*     N_BOLS                       = INTEGER (Given)
*           the number of bolometers measured
*     J_START                      = INTEGER (Given)
*           the index within the pattern of the first jiggle position
*           measured
*     N_JIG                        = INTEGER (Given)
*           the total number of jiggle positions measured
*     J_REPEAT                     = INTEGER (Given)
*           the number of times the jiggle pattern was repeated in the
*           dataset
*     J_COUNT                      = INTEGER (Given)
*           the number of jiggles in the pattern
*     J_DATA (N_BOLS, N_JIG)
*                                  = REAL (Given)
*           the measured data at each jiggle position
*     J_VARIANCE (N_BOLS, N_JIG)
*                                  = REAL (Given)
*           the measured variance
*     J_QUALITY (N_BOLS, N_JIG)
*                                  = INTEGER (Given)
*           the quality on the measured data
*     IDIM                         = INTEGER (Given)
*           the x dimension of the 2-d map
*     JDIM                         = INTEGER (Given)
*           the y dimension of the 2-d map
*     I_JIGGLE (J_COUNT)           = INTEGER (Given)
*           the `i' index on the 2-d map of each jiggle position
*     J_JIGGLE (J_COUNT)           = INTEGER (Given)
*           the `j' index on the 2-d map of each jiggle position
*     DATA_2D (IDIM, JDIM)         = REAL (Returned)
*           the data plane of the 2-d map
*     VARIANCE_2D (IDIM, JDIM)     = REAL (Returned)
*           the variance plane
*     QUALITY_2D (IDIM, JDIM)      = INTEGER (Returned)
*           the quality plane
*     STATUS                       = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  History:
*     $Id$
*     10-AUG-1993: Original version.
*      8-JUN-1995: Checked.
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
      INTEGER BOL
      INTEGER N_BOLS
      INTEGER J_START
      INTEGER N_JIG
      INTEGER J_REPEAT
      INTEGER J_COUNT
      REAL J_DATA (N_BOLS, N_JIG)
      REAL J_VARIANCE (N_BOLS, N_JIG)
      INTEGER J_QUALITY (N_BOLS, N_JIG)
      INTEGER IDIM
      INTEGER JDIM
      INTEGER I_JIGGLE (J_COUNT)
      INTEGER J_JIGGLE (J_COUNT)

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL DATA_2D (IDIM, JDIM)
      REAL VARIANCE_2D (IDIM, JDIM)
      INTEGER QUALITY_2D (IDIM, JDIM)

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I
      INTEGER J
      INTEGER JPOS
      INTEGER JIG
      INTEGER REPEAT

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      IF ((BOL.LT.1) .OR. (BOL.GT.N_BOLS)) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI ('BOL', BOL)
         CALL MSG_SETI ('N_BOLS', N_BOLS)
         CALL ERR_REP (' ', 'SCULIB_1D2_JIGGLE: requested bolometer '//
     :     '^BOL is outside measured range 1 - ^N_BOLS', STATUS)
      END IF


      IF (STATUS .EQ. SAI__OK) THEN

         IF (J_REPEAT .EQ. 1) THEN

*  the dataset holds data for a section of the jiggle pattern measured
*  once

            IF ((J_START+N_JIG-1) .GT. J_COUNT) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_1D2_JIGGLE: too many '//
     :           'jiggles', STATUS)
            ELSE
               DO J = 1, JDIM
                  DO I = 1, IDIM
                     DATA_2D (I,J) = 0.0
                     VARIANCE_2D (I,J) = 0.0
                     QUALITY_2D (I,J) = 1
                  END DO
               END DO

               DO JIG = 1, N_JIG
                  J = JIG - 1 + J_START
                  DATA_2D (I_JIGGLE(J), J_JIGGLE(J)) =
     :              J_DATA (BOL, JIG)
                  VARIANCE_2D (I_JIGGLE(J), J_JIGGLE(J)) =
     :              J_VARIANCE (BOL, JIG)
                  QUALITY_2D (I_JIGGLE(J), J_JIGGLE(J)) =
     :              J_QUALITY (BOL, JIG)
               END DO
            END IF

         ELSE IF (J_REPEAT .GT. 1) THEN

*  the whole jiggle pattern was measured J_REPEAT times. The output data
*  should be the mean of the input for each pixel, the variance will be
*  calculated from the spread of input values about the mean unless there
*  is only one point contributing, in which case its variance will be
*  copied to the output. Note that the output quality array is
*  used to keep count of the number of input values that contribute to
*  each output pixel.

            IF ((J_REPEAT * J_COUNT) .NE. N_JIG) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_1D2_JIGGLE: wrong number '//
     :           'of jiggles in dataset', STATUS)
            ELSE
               DO J = 1, JDIM
                  DO I = 1,IDIM
                     DATA_2D (I,J) = 0.0
                     VARIANCE_2D (I,J) = 0.0
                     QUALITY_2D (I,J) = 0
                  END DO
               END DO

*  now cycle through the jiggle points

               DO JIG = 1, J_COUNT
                  I = I_JIGGLE (JIG)
                  J = J_JIGGLE (JIG)

*  and the repeats

                  DO REPEAT = 1, J_REPEAT
                     JPOS = JIG + (REPEAT-1) * J_COUNT

                     IF (J_QUALITY(BOL,JPOS) .EQ. 0) THEN
                        QUALITY_2D (I,J) = QUALITY_2D (I,J) + 1
                        IF (QUALITY_2D (I,J) .EQ. 1) THEN
                           VARIANCE_2D (I,J) = J_VARIANCE (BOL,JPOS)
                        ELSE IF (QUALITY_2D (I,J) .EQ. 2) THEN
                           VARIANCE_2D (I,J) = DATA_2D (I,J) **2 +
     :                       J_DATA (BOL,JPOS) **2
                        ELSE
                           VARIANCE_2D (I,J) = VARIANCE_2D (I,J) +
     :                       J_DATA (BOL,JPOS) **2
                        END IF
                        DATA_2D (I,J) = DATA_2D (I,J) +
     :                    J_DATA (BOL,JPOS)
                     END IF

                  END DO
               END DO

*  now calculate averages etc.

               DO J = 1, JDIM
                  DO I = 1,IDIM
                     IF (QUALITY_2D (I,J) .GT. 0) THEN
                        DATA_2D (I,J) = DATA_2D (I,J) /
     :                    REAL (QUALITY_2D (I,J))
                        IF (QUALITY_2D (I,J) .GT. 1) THEN
                           VARIANCE_2D (I,J) = (VARIANCE_2D (I,J) -
     :                       REAL (QUALITY_2D(I,J)) *
     :                       DATA_2D(I,J)**2) /
     :                       REAL (QUALITY_2D(I,J) *
     :                       (QUALITY_2D(I,J)-1))
                        END IF
                        QUALITY_2D (I,J) = 0
                     ELSE
                        QUALITY_2D (I,J) = 1
                     END IF
                  END DO
               END DO

            END IF

         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETI ('REPEAT', J_REPEAT)
            CALL ERR_REP (' ', 'SCULIB_1D2_JIGGLE: bad value for '//
     :        'J_REPEAT - ^REPEAT', STATUS)
         END IF
      END IF

      END
