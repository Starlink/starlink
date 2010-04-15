      SUBROUTINE JCMT_CORRECT_POINTING (N, RA, DEC, LST, LAT, NCORR,
     :   POINT_LST, POINT_DAZ, POINT_DALT, STATUS)
*+
*  Name:
*     JCMT_CORRECT_POINTING

*  Purpose:
*     Add alt/az pointing corrections to list of RA,DEC values, according to the
*     LST at which the values were measured

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE JCMT_CORRECT_POINTING (N, RA, DEC, LST, LAT, NCORR,
*    :   POINT_LST, POINT_DAZ, POINT_DALT, STATUS)

*  Description:
*     This routine linearly interpolates a pointing correct according
*     to the LST of the observed point, then adds the corrections to the
*     RA, dec of the point. If the LST of the observed point lies outside
*     the range covered by the description of the pointing correction
*     then the correction applied is that at the closest end of the
*     description.

*  Arguments:
*     N                  = INTEGER (Given)
*        The number of RA, Dec pairs in the input list
*     RA (N)             = DOUBLE PRECISION (Given and returned)
*        The RA of the observed points (radians)
*     DEC (N)            = DOUBLE PRECISION (Given and returned)
*        The dec of the observed points (radians)
*     LST (N)            = DOUBLE PRECISION (Given)
*        The local sidereal time at which the points were observed (radians)
*     LAT                = DOUBLE PRECISION (Given)
*        The mean geodetic latitude of the observer (Radians)
*     NCORR              = INTEGER (Given)
*        The number of points in the pointing correction list
*     POINT_LST (NCORR)  = DOUBLE PRECISION (Given)
*        The LST of the measured correction (radians)
*     POINT_DAZ (NCORR)  = REAL (Given)
*        The correction to be added in azimuth (arcsec)
*     POINT_DALT (NCORR) = REAL (Given)
*        The correction to be added in altitude (arcsec)
*     STATUS             = INTEGER (Given and returned)
*        Global status
*  [optional_subroutine_items]...
*
*  Authors:
*     REVAD::JFL: John Lightfoot (ROE)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1992: original version (REVAD::JFL)
*      5-OCT-1993: modified to use alt/az corrections rather than RA/dec
*                  (REVAD::JFL)
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'        ! standard astronomical constants

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION LST (N)
      DOUBLE PRECISION LAT
      INTEGER NCORR
      DOUBLE PRECISION POINT_LST (NCORR)
      REAL POINT_DAZ (NCORR)
      REAL POINT_DALT (NCORR)

*  Arguments Returned:
      DOUBLE PRECISION RA (N)
      DOUBLE PRECISION DEC (N)

*  Status:
      INTEGER STATUS                ! Global status

*  Local variables:
      LOGICAL DONE
      LOGICAL FAULT
      INTEGER I, J
      INTEGER IGNORE
      DOUBLE PRECISION AZ_CORR, ALT_CORR ! corrections to be added (arcsec)
      DOUBLE PRECISION HA                ! hour angle (radians)
      DOUBLE PRECISION ALT, AZ           ! altitude and azimuth (radians)
      DOUBLE PRECISION SIN_ALT           ! SIN (ALT)
      DOUBLE PRECISION SINAZ, COSAZ      ! SIN and COS (AZ)
      DOUBLE PRECISION ALT_CORRECTED, AZ_CORRECTED
                                         ! corrected altitude and azimuth
      DOUBLE PRECISION SIND              ! SIN (DEC)
      DOUBLE PRECISION SINHA, COSHA      ! SIN and COS (HA)

      IF (STATUS .NE. SAI__OK) RETURN

*  check that the LST array contains values in ascending order

      IF (NCORR .GT. 1) THEN
         FAULT = .FALSE.
         DO J = 1, NCORR - 1
            IF (POINT_LST(J+1) .LT. POINT_LST(J)) THEN
               FAULT = .TRUE.
            END IF
         END DO
         IF (FAULT) THEN
            IGNORE = 0
            CALL PAR_WRUSER ('JCMT_CORRECT_POINTING - the pointing '//
     :        'corrections are not in ascending LST order', IGNORE)
            STATUS = SAI__ERROR
         END IF
      END IF


      IF (STATUS .EQ. SAI__OK) THEN

*  cycle through the points

         DO I = 1, N

*  calculate correction..

            IF (LST(I) .LE. POINT_LST(1)) THEN
               AZ_CORR = DBLE (POINT_DAZ (1))
               ALT_CORR = DBLE (POINT_DALT (1))
            ELSE IF (LST(I) .GE. POINT_LST(NCORR)) THEN
               AZ_CORR = DBLE (POINT_DAZ (NCORR))
               ALT_CORR = DBLE (POINT_DALT (NCORR))
            ELSE

*  look for correction points that straddle LST of observed point

               DONE = .FALSE.
               J = 1
               DO WHILE (.NOT. DONE)
                  IF ((LST(I) .GT. POINT_LST(J)) .AND.
     :                (LST(I) .LE. POINT_LST(J+1))) THEN

*  linearly interpolate corrections

                     AZ_CORR = DBLE(POINT_DAZ (J)) +
     :                  (LST(I)-POINT_LST(J)) *
     :                  DBLE (POINT_DAZ(J+1) - POINT_DAZ(J)) /
     :                  (POINT_LST(J+1) - POINT_LST(J))

                     ALT_CORR = DBLE(POINT_DALT(J)) +
     :                  (LST(I)-POINT_LST(J)) *
     :                  DBLE (POINT_DALT(J+1) - POINT_DALT(J)) /
     :                  (POINT_LST(J+1) - POINT_LST(J))

                     DONE = .TRUE.

                  ELSE

                     J = J + 1
                     IF (J .EQ. NCORR) THEN
                        IGNORE = 0
                        CALL PAR_WRUSER ('JCMT_CORRECT_POINTING - '//
     :                    'an error has occured while interpolating '//
     :                    'the pointing correction', IGNORE)
                        STATUS = SAI__ERROR
                        DONE = .TRUE.
                     END IF

                  END IF
               END DO
            END IF

*  calculate the alt/az of the point, az measured from N to W (JCMT convention
*  would be increasing from N to E) as expected by SLA_DTP2S

            HA = LST (I) - RA (I)
            SIN_ALT = SIN (LAT) * SIN (DEC(I)) + COS (LAT) *
     :        COS (DEC(I)) * COS (HA)
            ALT = ASIN (SIN_ALT)

*  note also that there's a factor of cos(alt) missing from the denominator of
*  both sinaz and cosaz. It's their ratio that matters.

            SINAZ = SIN (HA) * COS (DEC(I))
            COSAZ = (SIN(DEC(I)) - SIN(ALT) * SIN(LAT)) / COS(LAT)
            AZ = ATAN2 (SINAZ, COSAZ)

*  assume pointing corrections are tangent plane alt/az offsets to calculate
*  corrected az, el of beam (remembering that JCMT az is minus what SLA_DTP2S)

            CALL SLA_DTP2S (-AZ_CORR * DAS2R, ALT_CORR * DAS2R, AZ,
     :        ALT, AZ_CORRECTED, ALT_CORRECTED)

*  calculate corrected RA and dec of point

            SIND = SIN (ALT_CORRECTED) * SIN (LAT) +
     :        COS (ALT_CORRECTED) * COS (LAT) * COS (AZ_CORRECTED)
            DEC (I) = ASIN (SIND)

*  again, there's afactor cos (dec) missing from the denominators of both
*  SINHA, COSHA

            SINHA = SIN(AZ_CORRECTED) * COS(ALT_CORRECTED)
            COSHA = (SIN(ALT_CORRECTED) - SIN(DEC(I)) * SIN(LAT)) /
     :        COS(LAT)
            HA = ATAN2 (SINHA, COSHA)

            RA (I) = LST (I) - HA
            IF (RA(I) .LT. 0.0D0) THEN
               RA(I) = RA(I) + 2.0D0 * DPI
            END IF

         END DO

      END IF

      END

