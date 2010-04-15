      SUBROUTINE SCULIB_GAUSSIAN_XISQ (XISQ, N, FIT, STATUS)
*+
*  Name:
*     SCULIB_GAUSSIAN_XISQ

*  Purpose:
*     calculate chi-squared of Gaussian fit

*  Description:
*     If entered with good status this routine calculates the chi-squared
*     between a dataset and a Gaussian function. Data points with bad quality
*     will be ignored, as will points with zero variance. A warning will be
*     issued if any data points with zero variance are encountered. If no
*     valid data points are found then an error will be reported and bad
*     status returned. The data are passed in via common.

*  Invocation:
*     CALL SCULIB_GAUSSIAN_XISQ (XISQ, N, FIT, STATUS)

*  Arguments:
*     XISQ                   = DOUBLE PRECISION (Returned)
*           the chi-squared of the current fit
*     N                      = INTEGER (Given)
*           the number of parameters being fit, should be 6
*     FIT(N)                 = DOUBLE PRECISION (Given)
*           the fit parameters:-
*              - FIT(1) = peak height
*              - FIT(2) = length of the `a' axis of the sigma ellipse
*              - FIT(3) = length of the `b' axis of the sigma ellipse
*              - FIT(4) = the angle between the `a' axis and
*                         the x axis (+ve anticlockwise, radians)
*              - FIT(5) = the x coord of the centre
*              - FIT(6) = the y coord of the centre
*     STATUS                 = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (JFL@ROE.AC.UK)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     23-MAR-1995: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INTEGER MAX_FIT_DATA            ! maximum number of measurements
      PARAMETER (MAX_FIT_DATA = 512)

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION FIT (N)

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION XISQ

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:
      INTEGER NDATA_FIT                   ! number of measurements
      REAL    X_FIT (MAX_FIT_DATA)        ! x position of measurements
      REAL    Y_FIT (MAX_FIT_DATA)        ! y position of measurements
      REAL    DATA_FIT (MAX_FIT_DATA)     ! value of measurements
      REAL    VARIANCE_FIT (MAX_FIT_DATA) ! variance on measurements
      INTEGER QUALITY_FIT (MAX_FIT_DATA)  ! quality on measurements
      COMMON /SCULIB_GAUSSIAN_FIT_DATA/ NDATA_FIT, X_FIT,
     :  Y_FIT, DATA_FIT, VARIANCE_FIT, QUALITY_FIT

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION A              ! = FIT(2)
      DOUBLE PRECISION ALPHA          ! angle between a line from the data
                                      ! point to the ellipse centre and the x
                                      ! axis
      DOUBLE PRECISION B              ! = FIT(3)
      DOUBLE PRECISION BETA           ! angle between a line from the data point
                                      ! to the ellipse centre and the `a' axis
                                      ! of the ellipse (beta = alpha - theta)
      DOUBLE PRECISION F              ! = FIT(1)
      DOUBLE PRECISION G              ! the value of the fitted Gaussian at
                                      ! a sample position
      INTEGER I                       ! DO loop index
      INTEGER N_ADDED                 ! number of points added into chi-squared
      DOUBLE PRECISION SIGMA2         ! sigma^2 of the Gaussian
      DOUBLE PRECISION THETA          ! = FIT(4)
      DOUBLE PRECISION XCENTRE        ! = FIT(5)
      DOUBLE PRECISION YCENTRE        ! = FIT(6)
      LOGICAL ZERO_VARS               ! .TRUE. if any data with zero variance
                                      ! are encountered

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  initialise variables

      F = FIT(1)
      A = FIT(2)
      B = FIT(3)
      THETA = FIT(4)
      XCENTRE = FIT(5)
      YCENTRE = FIT(6)

      XISQ = 0.0D0
      N_ADDED = 0
      ZERO_VARS = .FALSE.

*  loop through data points, looking out for bad quality and zero variance

      DO I = 1, NDATA_FIT
         IF (QUALITY_FIT(I) .EQ. 0) THEN
            IF (VARIANCE_FIT(I) .GT. 0.0) THEN
               ALPHA = ATAN2 (DBLE(Y_FIT(I))-YCENTRE,
     :           DBLE(X_FIT(I))-XCENTRE)
               BETA = ALPHA - THETA

               SIGMA2 = (A * COS(BETA))**2 + (B * SIN(BETA))**2
               G = F * EXP ((-(DBLE(X_FIT(I))-XCENTRE)**2 -
     :           (DBLE(Y_FIT(I))-YCENTRE)**2) / SIGMA2)

               XISQ = XISQ + (DBLE(DATA_FIT(I))-G)**2 /
     :           DBLE(VARIANCE_FIT(I))
               N_ADDED = N_ADDED + 1
            ELSE
               ZERO_VARS = .TRUE.
            END IF
         END IF
      END DO

      IF (N_ADDED .EQ. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_GAUSSIAN_XISQ: no valid data '//
     :     'points for chi-squared calculation', STATUS)
      ELSE IF (ZERO_VARS) THEN
         STATUS = SAI__WARN
         CALL ERR_REP (' ', 'SCULIB_GAUSSIAN_XISQ: warning - the '//
     :     'data contain point(s) with zero variance', STATUS)
      END IF

      END
