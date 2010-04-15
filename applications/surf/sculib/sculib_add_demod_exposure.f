      SUBROUTINE SCULIB_ADD_DEMOD_EXPOSURE (N_BOLS, J_START, N_JIG,
     :  J_REPEAT, J_COUNT, EXP_DATA, EXP_VARIANCE, EXP_QUALITY,
     :  INT_DATA, INT_VARIANCE, INT_QUALITY, STATUS)
*+
*  Name:
*     SCULIB_ADD_DEMOD_EXPOSURE

*  Purpose:
*     add demodulated data for an exposure into the
*     integration result

*  Description:
*     This routine adds the reduced demodulated data for an exposure in a
*     jiggle-sampled observation into the integration result.
*       The exposure can contain data either for all or part of ONE run
*     through the jiggle pattern or for several REPEATS of the entire jiggle
*     pattern. In the first case, the routine copies the exposure data into
*     the appropriate part of the integration arrays. In the second, the
*     integration data will be the average of the input exposure values and the
*     integration variance will be set equal to the exposure value if there
*     was just one valid measurement, or calculated from the dispersion of
*     the exposure data about the mean otherwise.

*  Invocation:
*     CALL SCULIB_ADD_DEMOD_EXPOSURE (N_BOLS, J_START, N_JIG,
*    :  J_REPEAT, J_COUNT, EXP_DATA, EXP_VARIANCE, EXP_QUALITY,
*    :  INT_DATA, INT_VARIANCE, INT_QUALITY, STATUS)

*  Arguments:
*     N_BOLS                           = INTEGER (Given)
*           the number of bolometers measured
*     J_START                          = INTEGER (Given)
*           the index within the pattern of the first jiggle position measured
*     N_JIG                            = INTEGER (Given)
*           the number of jiggle positions measured
*     J_REPEAT                         = INTEGER (Given)
*           the number of times the measured pattern was repeated
*     J_COUNT                          = INTEGER (Given)
*           the number of jiggle positions in the entire pattern
*     EXP_DATA (N_BOLS, N_JIG)         = REAL (Given)
*           the data for the exposure
*     EXP_VARIANCE (N_BOLS, N_JIG)     = REAL (Given)
*           the exposure variance
*     EXP_QUALITY (N_BOLS, N_JIG)      = INTEGER (Given)
*           the exposure quality
*     INT_DATA (N_BOLS, J_COUNT)       = REAL (Returned)
*           the integration data
*     INT_VARIANCE (N_BOLS, J_COUNT)   = REAL (Returned)
*           the integration variance
*     INT_QUALITY (N_BOLS, J_COUNT)    = INTEGER (Returned)
*           the integration quality
*     STATUS                           = INTEGER (Given and returned)
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
*     9-AUG-1993: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N_BOLS
      INTEGER J_START
      INTEGER N_JIG
      INTEGER J_REPEAT
      INTEGER J_COUNT
      REAL EXP_DATA (N_BOLS, N_JIG)
      REAL EXP_VARIANCE (N_BOLS, N_JIG)
      INTEGER EXP_QUALITY (N_BOLS, N_JIG)

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL INT_DATA (N_BOLS, J_COUNT)
      REAL INT_VARIANCE (N_BOLS, J_COUNT)
      INTEGER INT_QUALITY (N_BOLS, J_COUNT)

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER BOL
      INTEGER J
      INTEGER JIG
      INTEGER REPEAT

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      IF (J_REPEAT .EQ. 1) THEN

*  the exposure measured a section of the jiggle pattern once, insert it into
*  the integration result

         IF ((J_START+N_JIG-1) .GT. J_COUNT) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_ADD_DEMOD_EXPOSURE: too many '//
     :        'jiggles', STATUS)
         ELSE

            DO JIG = 1, N_JIG
               J = JIG - 1 + J_START
               DO BOL = 1, N_BOLS
                  INT_DATA (BOL,J) = EXP_DATA (BOL,JIG)
                  INT_VARIANCE (BOL,J) = EXP_VARIANCE (BOL,JIG)
                  INT_QUALITY (BOL,J) = EXP_QUALITY (BOL,JIG)
               END DO
            END DO

         END IF

      ELSE IF (J_REPEAT .GT. 1) THEN

*  the whole jiggle pattern was measured J_REPEAT times. The integration
*  data should be the mean of the exposures, the variance will be calculated
*  from the distribution about the mean (note the integration quality array
*  is used to keep count of the number of exposures that have been coadded
*  for each jiggle)

         IF ((J_REPEAT*J_COUNT) .NE. N_JIG) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_ADD_DEMOD_EXPOSURE: wrong '//
     :        'number of jiggles in dataset', STATUS)

         ELSE

            DO BOL = 1, N_BOLS
               DO J = 1, J_COUNT

                  INT_DATA (BOL,J) = 0.0
                  INT_QUALITY (BOL,J) = 0

                  DO REPEAT = 1, J_REPEAT

                    JIG = (J_REPEAT - 1) * J_COUNT + J

                     IF (EXP_QUALITY (BOL,JIG) .EQ. 0) THEN
                        INT_QUALITY (BOL,J) = INT_QUALITY (BOL,J) + 1
                        IF (INT_QUALITY (BOL,J) .EQ. 1) THEN
                           INT_VARIANCE (BOL,J) = EXP_VARIANCE (BOL,JIG)
                        ELSE IF (INT_QUALITY (BOL,J) .EQ. 2) THEN
                           INT_VARIANCE (BOL,J) = INT_DATA (BOL,J) **2 +
     :                       EXP_DATA (BOL,JIG) **2
                        ELSE
                           INT_VARIANCE (BOL,J) = INT_VARIANCE (BOL,J) +
     :                       EXP_DATA (BOL,JIG) **2
                        END IF
                        INT_DATA (BOL,J) = INT_DATA (BOL,J) +
     :                    EXP_DATA (BOL,JIG)
                     END IF

                  END DO

                  IF (INT_QUALITY (BOL,J) .EQ. 0) THEN
                     INT_QUALITY (BOL,J) = 1
                  ELSE IF (INT_QUALITY (BOL,J) .EQ. 1) THEN
                     INT_QUALITY (BOL,J) = 0
                  ELSE
                     INT_DATA (BOL,J) = INT_DATA (BOL,J) /
     :                 REAL (INT_QUALITY(BOL,J))
                     INT_VARIANCE (BOL,J) = (INT_VARIANCE(BOL,J) -
     :                 REAL (INT_QUALITY(BOL,J)) *
     :                 INT_DATA(BOL,J) **2) /
     :                 REAL (INT_QUALITY(BOL,J) *
     :                 (INT_QUALITY(BOL,J) - 1))
                     INT_QUALITY (BOL,J) = 0
                  END IF

               END DO
            END DO

         END IF

      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETI ('REPEAT', J_REPEAT)
         CALL ERR_REP (' ', 'SCULIB_ADD_DEMOD_EXPOSURE: invalid '//
     :     'value for J_REPEAT - ^REPEAT', STATUS)

      END IF

      END
