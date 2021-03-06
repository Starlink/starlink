      SUBROUTINE SCULIB_SKYDIP_VAR (RESIDUAL, VAR, N, FIT, NDEG, STATUS)
*+
*  Name:
*     SCULIB_SKYDIP_VAR

*  Purpose:
*     calculate residual and variance of skydip data points

*  Invocation:
*     CALL SCULIB_SKYDIP_VAR (RESIDUAL, VAR, N, FIT, NDEG, STATUS)

*  Language:
*     Starlink Fortran 77

*  Description:
*     If entered with good status this routine calculates the variance
*     between a SKYDIP dataset and the theoretical curve generated by
*     the sky and telescope parameters in FIT. Additionally, the residual
*     of the fit is also returned (absolute value of measured data mins
*     fitted values). Data points with bad
*     quality will be ignored. If no valid data points are found then an
*     error will be reported and bad status returned. The data are passed
*     in via common.
*
*     The FIT vector is composed as follows:-
*           - x(1) = ETA_TEL
*           - x(2) = B
*           - x(3) = TAU
*
*     For a given airmass:-
*
*         Jtheory = (1 - ETA_TEL) * J_TEL + ETA_TEL * J_ATM *
*                   (1 - B * EXP (-TAU * Airmass(i))
*
*
*           J_ATM = J_AMB * X_G
*
*             X_G = 1 + h1 * h2 * EXP (-TAU * Airmass(i) / X_Gconst)
*                .      -------
*                        J_AMB
*

*  Arguments:
*     RESIDUAL                = DOUBLE PRECISION (Returned)
*            residual of the fit (absolute difference between the
*            model and the data)
*     VAR                     = DOUBLE PRECISION (Returned)
*            variance of the residual of the current fit
*     N                        = INTEGER (Given)
*            the number of parameters being fit, should be 3
*     FIT (N)                  = DOUBLE PRECISION (Given)
*            the fit parameters:-
*                                - FIT (1) = ETA_TEL
*                                - FIT (2) = B
*                                - FIT (3) = TAUZ
*     NDEG                     = INTEGER (Given)
*            the number of degrees of freedom
*     STATUS                   = INTEGER (Given and returned)
*            global status


*  Authors:
*     J.Lightfoot (REVAD::JFL)
*     Nick Tothill (N.F.H.Tothill@qmw.ac.uk)
*     Tim Jenness (JACH)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Log$
*     Revision 1.6  2000/05/11 20:01:13  timj
*     Calculate RESIDUAL of fit
*
*     Revision 1.5  1999/08/19 03:37:27  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.4  1999/08/03 19:35:30  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.3  1999/03/24 00:28:17  timj
*     Increase the common-block for fitting to 500
*
*     Revision 1.2  1998/12/03 19:38:35  timj
*     Check for status on entry.
*     Update header.
*
*     Revision 1.1  1998/06/05 20:29:53  timj
*     Initial revision
*
*     11-MAR-1997: Original version of sculib_skydip_xisq.
*     18-MAY-1998: Rewritten as sculib_skydip_var by NFHT

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Local constants:
      INTEGER MAX_FIT_DATA               ! max number of measurements
      PARAMETER (MAX_FIT_DATA = 500)

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION FIT (N)
      INTEGER NDEG

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION RESIDUAL
      DOUBLE PRECISION VAR

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:
      DOUBLE PRECISION AIRMASS (MAX_FIT_DATA)
                                         ! airmasses at which measurements
                                         ! were made
      DOUBLE PRECISION B_HI              ! not used
      DOUBLE PRECISION B_LO              ! not used
      DOUBLE PRECISION ETA_TEL_HI        ! not used
      DOUBLE PRECISION ETA_TEL_LO        ! not used
      DOUBLE PRECISION J_AMB             ! brightness temp of air
      DOUBLE PRECISION J_MEAS (MAX_FIT_DATA)
                                         ! measured sky temperatures
      INTEGER          J_QUALITY (MAX_FIT_DATA)
                                         ! quality on J_MEAS
      DOUBLE PRECISION J_TEL             ! brightness temp of telescope
      DOUBLE PRECISION J_VARIANCE (MAX_FIT_DATA)
      INTEGER          M                 ! number of skydip measurements
      COMMON /SCULIB_SKYDIP_FIT_DATA_I/ J_QUALITY,
     :                                  M
      COMMON /SCULIB_SKYDIP_FIT_DATA_D/ AIRMASS,
     :                                  B_HI,
     :                                  B_LO,
     :                                  ETA_TEL_HI,
     :                                  ETA_TEL_LO,
     :                                  J_AMB,
     :                                  J_MEAS,
     :                                  J_TEL,
     :                                  J_VARIANCE

*  Local Constants:
      DOUBLE PRECISION H1                ! temperature drop / km
      PARAMETER (H1 = -6.5D0)
      DOUBLE PRECISION H2                ! scale height of absorber
      PARAMETER (H2 = 2.0D0)
      DOUBLE PRECISION X_GCONST          ! fudge constant
      PARAMETER (X_GCONST = 3.669383D0)

*  Local variables:
      DOUBLE PRECISION B                 ! b parameter of fit
      DOUBLE PRECISION ETA_TEL           ! telescope transmission of fit
      INTEGER          I                 ! DO loop index
      DOUBLE PRECISION J_ATM             ! brightness temperature of atmosphere
      DOUBLE PRECISION J_THEORY          ! theoretical sky brightness
                                         ! temperature at airmass measured
      INTEGER          N_ADDED           ! number of data elements added
                                         ! into chi-squared
      DOUBLE PRECISION RES               ! Difference between data and model
      DOUBLE PRECISION TAU               ! zenith optical depth of fit
      DOUBLE PRECISION X_G               ! fudge factor

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  initialise variables

      VAR = 0.0D0
      RESIDUAL = 0.0D0
      N_ADDED = 0

*  unpack FIT vector

      ETA_TEL = FIT (1)
      B = FIT (2)
      TAU = FIT (3)

*  calculate theoretical function for each airmass and work out variance
*  The data variance is added to the deviation for each point
*  - effectively, we add the (small) measurement error in quadrature

      DO I = 1, M
         IF (J_QUALITY(I) .EQ. 0) THEN
            IF (ABS(TAU * AIRMASS(I)) .LT. 20.0D0) THEN
               X_G = 1.0D0 + (H1 * H2 / J_AMB) *
     :           EXP (-TAU * AIRMASS(I) / X_GCONST)

               J_ATM = J_AMB * X_G

               J_THEORY = (1.0D0 - ETA_TEL) * J_TEL +
     :           ETA_TEL * J_ATM * (1.0D0 - B * EXP (-TAU * AIRMASS(I)))
            ELSE
               J_ATM = J_AMB

               J_THEORY = (1.0D0 - ETA_TEL) * J_TEL +
     :           ETA_TEL * J_ATM
            END IF

*     Calculate residual of this point
            RES = J_THEORY - J_MEAS(I)

*     Store variance
            IF (J_VARIANCE(I) .GT. 0.0D0) THEN
               VAR = VAR + (RES) **2 + J_VARIANCE(I)
            END IF

*     Increment residual
            RESIDUAL = RESIDUAL + ABS(RES)

*     Increment counter
            N_ADDED = N_ADDED + 1
         END IF
      END DO

*  And divide by number of degrees of freedom

      VAR = VAR / REAL(NDEG)

*  error?

      IF (N_ADDED .EQ. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_SKYDIP_VAR: no valid data '//
     :     'points for variance calculation', STATUS)
      ENDIF

      END






