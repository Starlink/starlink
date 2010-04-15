      SUBROUTINE SCULIB_SKYFUNC_1 (MODE, M, N, LDFJ, X, F, FJAC,
     :  NSTATE, IUSER, USER)
*+
*  Name:
*     SCULIB_SKYFUNC_1

*  Purpose:
*     calculate f(x) and its Jacobian for NAG non-linear
*     fitting routine E04UPF when fitting ETA_TEL, B
*     and TAU in SKYDIP analysis

*  Description:
*     E04UPF is a NAG routine that is used to fit a theoretical
*     sky-dip curve to the measured data by varying ETA_TEL, B and TAU.
*     This routine is called by E04UPF to calculate the M sub-functions
*     fi(x) at the current solution vector X, and/or their Jacobian
*     matrix. See the NAG manual if you need to know more about E04UPF.
*
*     The solution vector is composed as follows:-
*           - x(1) = ETA_TEL
*           - x(2) = B
*           - x(3) = TAU
*
*     For each of the M measured airmasses the subfunction f(x) is:-
*
*            f(x) = Jtheory (Airmass(i)) - Jmeas (Airmass(i))
*            .      -----------------------------------
*                         Jnoise (Airmass(i))
*
*     where:-
*
*         Jtheory = (1 - ETA_TEL) * J_TEL + ETA_TEL * J_ATM *
*                   (1 - B * EXP (-TAU * Airmass(i))
*
*
*           J_ATM = J_AMB * X_G
*
*             X_G = 1 + h1 * h2 * EXP (-TAU * Airmass(i) / X_Gconst)
*                 .      -------
*                        J_AMB
*
*
*     The corresponding Jacobian has component (i,j):-
*
*            df(x)   d Jtheory (Airmass(i))         1.0
*     .      -     = -                      *    ----------
*            dx(j)   dx(j)                       Jnoise (i)
*
*     =>
*            df(i)
*     .      -     = - J_TEL + J_ATM - B * J_ATM * EXP (-TAU * Airmass(i))
*            dx(1)     ---------------------------------------------------
*                                        Jnoise (i)
*
*            df(i)
*     .      -     = - ETA_TEL * J_ATM * EXP (-TAU * Airmass(i))
*            dx(2)     -----------------------------------------
*                                        Jnoise (i)
*
*            df(i)             dJ_ATM
*     .      -     = (ETA_TEL * ------  -
*            dx(3)              dTAU
*
*               ETA_TEL * B * EXP (-TAU * Airmass(i)) *
*
*                dJ_ATM
*               (------ - J_ATM * Airmass(i))) / Jnoise (i)
*                 dTAU
*
*     where:-
*
*           dJ_ATM           dX_G
*     .     ------ = J_AMB * ----
*            dTAU            dTAU
*
*            dX_G      H1 * H2 * Airmass(i)
*     .      ----  = - -------------------- * EXP (-TAU * Airmass(i)/ X_Gconst)
*            dTAU        J_AMB * X_Gconst
*
*     The array USER is used to get the necessary ancillary information into
*     the routine:-
*      - USER (1) = J_TEL
*      - USER (2) = J_AMB
*      - USER (3) = not used
*      - USER (4:M+3)      = the measured airmasses
*      - USER (M+4:2M+3)   = the measured sky temperatures
*      - USER (2M+4:3M+3)  = the errors on the measured sky temperatures
*

*  Invocation:
*     CALL SCULIB_SKYFUNC_1 (MODE, M, N, LDFJ, X, F, FJAC, NSTATE,
*    :    IUSER, USER)

*  Arguments:
*     See NAG manual description of OBJFUN parameter in E04UPF.


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
*     $Log$
*     Revision 1.3  1999/08/19 03:37:28  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     15-SEP-1993: Original version.
*      8-FEB-1996: modified to use J_ATM to J_AMB relation.
*     23-JUL-1996: modified to fit the correct function (JFL).
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:

*  Arguments Given:
      INTEGER MODE
      INTEGER M
      INTEGER N
      INTEGER LDFJ
      DOUBLE PRECISION X (N)
      INTEGER NSTATE
      INTEGER IUSER (*)
      DOUBLE PRECISION USER (3*M + 3)

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION F (M)
      DOUBLE PRECISION FJAC (LDFJ,N)

*  Status:

*  External references:

*  Global variables:

*  Local Constants:
      DOUBLE PRECISION H1                ! temperature drop / km
      PARAMETER (H1 = -6.5D0)
      DOUBLE PRECISION H2                ! scale height of absorber
      PARAMETER (H2 = 2.0D0)
      DOUBLE PRECISION X_GCONST          ! fudge constant
      PARAMETER (X_GCONST = 3.669383D0)

*  Local variables:
      DOUBLE PRECISION AIRMASS           ! airmass of measurement
      DOUBLE PRECISION B                 ! b parameter of fit
      DOUBLE PRECISION DJ_ATMDT          ! dJ_ATM / dTAU
      DOUBLE PRECISION DX_GDT            ! dX_G / dTAU
      DOUBLE PRECISION ETA_TEL           ! telescope transmission of fit
      INTEGER          I                 ! DO loop index
      DOUBLE PRECISION J_AMB             ! brightness temperature of ambient air
      DOUBLE PRECISION J_ATM             ! brightness temperature of atmosphere
      DOUBLE PRECISION J_ERROR           ! error on J_MEAS
      DOUBLE PRECISION J_MEAS            ! measured sky brightness
      DOUBLE PRECISION J_TEL             ! brightness temperature of telescope
      DOUBLE PRECISION J_THEORY          ! theoretical sky brightness
                                         ! temperature at airmass measured
      DOUBLE PRECISION TAU               ! zenith optical depth of fit
      DOUBLE PRECISION X_G               ! fudge factor

*  Internal References:

*  Local data:

*.

*  unpack X vector

      ETA_TEL = X (1)
      B = X (2)
      TAU = X (3)

*  and other numbers needed to calculate the theoretical sky temperature

      J_TEL = USER (1)
      J_AMB = USER (2)

      IF ((MODE .EQ. 0) .OR. (MODE .EQ. 2)) THEN

*  calculate f(x) for each airmass

         DO I = 1, M
            AIRMASS = USER (I + 3)
            J_MEAS = USER (I + 3 + M)
            J_ERROR = USER (I + 3 + 2*M)

            IF (ABS(TAU * AIRMASS) .LT. 20.0D0) THEN
               X_G = 1.0D0 + (H1 * H2 / J_AMB) *
     :           EXP (-TAU * AIRMASS / X_GCONST)

               J_ATM = J_AMB * X_G

               J_THEORY = (1.0D0 - ETA_TEL) * J_TEL +
     :           ETA_TEL * J_ATM * (1.0D0 - B * EXP (-TAU * AIRMASS))
            ELSE
               J_ATM = J_AMB

               J_THEORY = (1.0D0 - ETA_TEL) * J_TEL +
     :           ETA_TEL * J_ATM
            END IF

            F (I) = (J_THEORY - J_MEAS) / MAX (J_ERROR, 1.0D-5)
         END DO

      END IF


      IF ((MODE .EQ. 1) .OR. (MODE .EQ. 2)) THEN

*  calculate Jacobian matrix

         DO I = 1, M
            AIRMASS = USER (I + 3)
            J_MEAS = USER (I + 3 + M)
            J_ERROR = USER (I + 3 + 2*M)

            IF (ABS(TAU * AIRMASS) .LT. 20.0D0) THEN
               X_G = 1.0D0 + (H1 * H2 / J_AMB) *
     :           EXP (-TAU * AIRMASS / X_GCONST)

               J_ATM = J_AMB * X_G

               FJAC (I,1) = (- J_TEL + J_ATM - B * J_ATM *
     :           EXP (-TAU * AIRMASS)) / MAX (J_ERROR,1.0D-5)
               FJAC (I,2) = (- ETA_TEL * J_ATM *
     :           EXP (-TAU * AIRMASS)) / MAX (J_ERROR,1.0D-5)

               DX_GDT = ((-H1 * H2 * AIRMASS) / (J_AMB * X_GCONST)) *
     :           EXP (-TAU * AIRMASS / X_GCONST)

               DJ_ATMDT = J_AMB * DX_GDT

               FJAC (I,3) = (ETA_TEL * DJ_ATMDT - B * ETA_TEL *
     :           EXP (-TAU * AIRMASS) * (DJ_ATMDT - J_ATM * AIRMASS)) /
     :           MAX (J_ERROR,1.0D-5)
            ELSE
               J_ATM = J_AMB

               FJAC (I,1) = (- J_TEL + J_ATM) /
     :           MAX (J_ERROR,1.0D-5)
               FJAC (I,2) = 0.0D0
               FJAC (I,3) = 0.0D0
            END IF

         END DO

      END IF

      END
