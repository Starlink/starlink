      SUBROUTINE SCULIB_SKYFUNCD(X, P, DR, M)
*+
*  Name:
*     SCULIB_SKYFUNCD

*  Purpose:
*     Return the value of the partial derivatives of the skydip function

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_SKYFUNCD(X, P, DR, M)

*  Description:
*     This routine returns the value of the skydip function.
*     The parameters are passed as follows:
*          - P(1) = ETA_TEL
*          - P(2) = B
*          - P(3) = TAU
*          - P(4) = J_AMB
*          - P(5) = J_TEL
*
*     The  theoretical value of the skydip
*     for the given input parameters is given by:
*
*         Jtheory = (1 - ETA_TEL) * J_TEL + ETA_TEL * J_ATM *
*                   (1 - B * EXP (-TAU * Airmass(i))
*
*
*           J_ATM = J_AMB * X_G
*
*             X_G = 1 + h1 * h2 * EXP (-TAU * Airmass(i) / X_Gconst)
*             .         -------
*                        J_AMB
*
*     The partial derivatives are:
*
*         d(F)/d(ETA_TEL) = DR (1) = (- J_TEL + J_ATM - B * J_ATM *
*     :                               EXP (-TAU * AIRMASS_IN))
*
*         d(F)/d(B) =  DR (2) = (- ETA_TEL * J_ATM * EXP (-TAU * AIRMASS_IN))
*
*         dX_G/dTAU = ((-H1 * H2 * AIRMASS_IN) / (J_AMB * X_GCONST)) *
*     :                EXP (-TAU * AIRMASS_IN / X_GCONST)
*
*         dJ_ATM/dTAU = J_AMB * dX_G/dTAU
*
*         d(F)/d(TAU) = DR (3) = ETA_TEL * dJ_ATM/dTAU - B * ETA_TEL *
*     :            EXP (-TAU * AIRMASS_IN) * (dJ_ATM/dTAU - J_ATM * AIRMASS_IN)
*
*     d(F)/d(J_AMB) = d(f)/d(J_TEL) = 0.0 (both are fixed parameters)

*  Arguments:
*     X = REAL (Given)
*        The airmass used to calculate the function
*     P = REAL (Given)
*        The parameter values
*     M = INTEGER (Given)
*        The number of parameters
*     DR = REAL (Returned)
*        The values of the partial derivatives

*  Authors:
*     TIMJ: Tim Jenness (JAC: timj@jach.hawaii.edu)
*     JFL:  John Lightfoot (ROE/JAC: jfl@roe.ac.uk)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.4  1999/08/19 03:37:28  timj
*     Header tweaks to ease production of SSN72 documentation.
*
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
      REAL P(M)
      REAL X

*  Arguments Returned:
      REAL DR(M)

*  Local Constants :
      DOUBLE PRECISION H1                ! temperature drop / km
      PARAMETER (H1 = -6.5D0)
      DOUBLE PRECISION H2                ! scale height of absorber
      PARAMETER (H2 = 2.0D0)
      DOUBLE PRECISION X_GCONST          ! fudge constant
      PARAMETER (X_GCONST = 3.669383D0)

*  Local Variables:
      DOUBLE PRECISION DJ_ATMDT          ! dJ_ATM / dTAU
      DOUBLE PRECISION DX_GDT            ! dX_G / dTAU
      DOUBLE PRECISION ETA_TEL           ! telescope transmission of fit
      DOUBLE PRECISION J_ATM             ! brightness temperature of atmosphere
      DOUBLE PRECISION TAU               ! zenith optical depth of fit
      DOUBLE PRECISION X_G               ! fudge factor
      DOUBLE PRECISION B
      DOUBLE PRECISION J_AMB
      DOUBLE PRECISION J_TEL
      DOUBLE PRECISION AIRMASS_IN
*.


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

         DR (1) = (- J_TEL + J_ATM - B * J_ATM *
     :        EXP (-TAU * AIRMASS_IN))
         DR (2) = (- ETA_TEL * J_ATM * EXP (-TAU * AIRMASS_IN))

         DX_GDT = ((-H1 * H2 * AIRMASS_IN) / (J_AMB * X_GCONST)) *
     :        EXP (-TAU * AIRMASS_IN / X_GCONST)

         DJ_ATMDT = J_AMB * DX_GDT

         DR (3) = ETA_TEL * DJ_ATMDT - B * ETA_TEL *
     :        EXP (-TAU * AIRMASS_IN) * (DJ_ATMDT - J_ATM * AIRMASS_IN)
      ELSE
         J_ATM = J_AMB

         DR (1) = (- J_TEL + J_ATM)
         DR (2) = 0.0D0
         DR (3) = 0.0D0
      END IF

*     The last two 'parameters' are actually constants and so are not
*     necessary (in for completeness).
      DR(4) = 1 - P(1)
      DR(5) = P(1) * (1 - P(2) * EXP(-P(3) * X))

      END

