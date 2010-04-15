      SUBROUTINE SCULIB_SKYFUNC_2 (MODE, M, N, LDFJ, X, F, FJAC,
     :  NSTATE, IUSER, USER)
*+
*  Name:
*     SCULIB_SKYFUNC_2

*  Purpose:
*     calculate f(x) and its Jacobian for NAG non-linear
*     fitting routine E04UPF when fitting B and TAU in
*     sky-dip analysis

*  Description:
*     E04UPF is a NAG routine that is used to fit a theoretical
*     sky-dip curve to the measured data by varying B and TAU.
*     This routine is called by E04UPF to calculate the M sub-functions
*     f(x) at the current solution vector X, and/or the Jacobian
*     matrix. Please see the NAG manual if you need to know more about
*     E04UPF.
*
*     The solution vector is composed as follows:-
*          -  x(1) = B
*          -  x(2) = TAU
*
*     For each of the M measured airmasses the subfunction f(x) is:-
*
*            f(x) = Jtheory (Airmass(i)) - Jmeas (Airmass(i))
*            .      -----------------------------------
*                         Jnoise (Airmass(i))
*
*     where:-
*
*         Jtheory = (1 - ETA_TEL) * J_TEL + ETA_TEL * J_ATM -
*                   B * ETA_TEL * J_ATM * EXP (-TAU * Airmass(i))
*
*
*     The corresponding Jacobian has component (i,j):-
*
*            df(x)   d Jtheory (Airmass(i))         1.0
*     .      -     = -                      *    ----------
*            dx(j)   dx(j)                       Jnoise (i)
*
*     ->
*
*            df(i)
*     .      -     = - ETA_TEL * J_ATM * EXP (-TAU * Airmass(i))
*            dx(1)
*
*            df(i)
*     .      -     = Airmass(i) * B * ETA_TEL * J_ATM * EXP (-TAU * Airmass(i))
*            dx(2)
*
*     The array USER is used to get the necessary ancillary information into
*     the routine:-
*      - USER (1) = J_TEL
*      - USER (2) = J_ATM
*      - USER (3) = ETA_TEL
*      - USER (4:M+3)     = the measured airmasses
*      - USER (M+4:2M+3)  = the measured sky temperatures
*      - USER (2M+4:3M+3) = the errors on the measured sky temperatures
*

*  Invocation:
*     CALL SCULIB_SKYFUNC_2 (MODE, M, N, LDFJ, X, F, FJAC, NSTATE,
*    :  IUSER, USER)

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
*     15-SEP-1993: Original version.
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

*  Local variables:
      INTEGER I
      DOUBLE PRECISION ETA_TEL
      DOUBLE PRECISION B
      DOUBLE PRECISION TAU
      DOUBLE PRECISION J_TEL
      DOUBLE PRECISION J_ATM
      DOUBLE PRECISION AIRMASS
      DOUBLE PRECISION J_THEORY
      DOUBLE PRECISION J_MEAS
      DOUBLE PRECISION J_ERROR

*  Internal References:

*  Local data:

*.

*  unpack X vector

      B = X (1)
      TAU = X (2)

*  and other numbers needed to calculate the theoretical sky temperature

      J_TEL = USER (1)
      J_ATM = USER (2)
      ETA_TEL = USER (3)

      IF ((MODE .EQ. 0) .OR. (MODE .EQ. 2)) THEN

*  calculate f(x) for each airmass

         DO I = 1, M

            AIRMASS = USER (I + 3)
            J_MEAS = USER (I + 3 + M)
            J_ERROR = USER (I + 3 + 2*M)

            IF (ABS(TAU * AIRMASS) .LT. 20.0D0) THEN
               J_THEORY = (1.0D0 - ETA_TEL) * J_TEL +
     :           ETA_TEL * J_ATM -
     :           B * ETA_TEL * J_ATM * EXP (-TAU * AIRMASS)
            ELSE
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
            J_ERROR = USER (I + 2 + 2*M)

            IF (ABS(TAU * AIRMASS) .LT. 20.0D0) THEN

               FJAC (I,1) = (- ETA_TEL * J_ATM *
     :           EXP (-TAU * AIRMASS)) / MAX (J_ERROR,1.0D-5)
               FJAC (I,2) = (AIRMASS * B * ETA_TEL * J_ATM *
     :           EXP (-TAU * AIRMASS)) / MAX (J_ERROR,1.0D-5)

            ELSE

               FJAC (I,1) = 0.0D0
               FJAC (I,2) = 0.0D0

            END IF

         END DO

      END IF

      END
