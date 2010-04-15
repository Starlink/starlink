      SUBROUTINE SCULIB_SKYFUNC(F, X,P,M)
*+
*  Name:
*     SCULIB_SKYFUNC

*  Purpose:
*     Return the value of the skydip function

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_SKYFUNC(F, X, P, M)

*  Description:
*     This routine returns the value of the skydip function.
*     The parameters are passed as follows:
*         -  P(1) = ETA_TEL
*         -  P(2) = B
*         -  P(3) = TAU
*         -  P(4) = J_AMB
*         -  P(5) = J_TEL
*
*     This subroutine returns the theoretical value of the skydip
*     for the given input parameters.
*
*         Jtheory = (1 - ETA_TEL) * J_TEL + ETA_TEL * J_ATM *
*                   (1 - B * EXP (-TAU * Airmass(i))
*
*
*           J_ATM = J_AMB * X_G
*
*             X_G = 1 + h1 * h2 * EXP (-TAU * Airmass(i) / X_Gconst)
*         .             -------
*                        J_AMB
*
*  Arguments:
*     F = REAL (Returned)
*        The value of the skydip function
*     X = REAL (Given)
*        The airmass used to calculate the function
*     P = REAL (Given)
*        The parameter values
*     M = INTEGER (Given)
*        The number of parameters

*  Authors:
*     TIMJ: Tim Jenness (JAC: timj@jach.hawaii.edu)
*     JFL:  John Lightfoot (ROE/JAC: jfl@roe.ac.uk)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1996 (JFL):
*        Original version.
*     1997 February (TIMJ):
*        Changed completely so that works with LSQ_FIT
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER M
      REAL    P(M)
      REAL    X

*  Arguments Returned
      REAL    F

*  Local Constants :
      DOUBLE PRECISION H1                ! temperature drop / km
      PARAMETER (H1 = -6.5D0)
      DOUBLE PRECISION H2                ! scale height of absorber
      PARAMETER (H2 = 2.0D0)
      DOUBLE PRECISION X_GCONST          ! fudge constant
      PARAMETER (X_GCONST = 3.669383D0)

*  Local Variables:
      DOUBLE PRECISION AIRMASS_IN        ! airmass of measurement
      DOUBLE PRECISION B                 ! b parameter of fit
      DOUBLE PRECISION ETA_TEL           ! telescope transmission of fit
      DOUBLE PRECISION J_ATM             ! brightness temperature of atmosphere
      DOUBLE PRECISION J_AMB             ! brightness temp of ambient air
      DOUBLE PRECISION J_TEL             ! brightness temperature of telescope
      DOUBLE PRECISION J_THEORY          ! theoretical sky brightness
                                         ! temperature at airmass measured
      DOUBLE PRECISION TAU               ! zenith optical depth of fit
      DOUBLE PRECISION X_G               ! fudge factor
*.

*  calculate f(x) for given x

* First translate the parameters to useful names
      ETA_TEL = DBLE(P(1))
      B       = DBLE(P(2))
      TAU     = DBLE(P(3))
      J_AMB   = DBLE(P(4))
      J_TEL   = DBLE(P(5))

      AIRMASS_IN = DBLE(X)

      IF (ABS(TAU * AIRMASS_IN) .LT. 20.0D0) THEN
         X_G = 1.0D0 + (H1 * H2 / J_AMB) *
     :        EXP (-TAU * AIRMASS_IN / X_GCONST)

         J_ATM = J_AMB * X_G

         J_THEORY = (1.0D0 - ETA_TEL) * J_TEL +
     :        ETA_TEL * J_ATM * (1.0D0 - B * EXP (-TAU * AIRMASS_IN))
      ELSE
         J_ATM = J_AMB

         J_THEORY = (1.0D0 - ETA_TEL) * J_TEL +
     :        ETA_TEL * J_ATM
      END IF

      F = REAL(J_THEORY)

      END

