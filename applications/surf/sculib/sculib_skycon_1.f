      SUBROUTINE SCULIB_SKYCON_1 (MODE, NCNLN, N, LDCJ, NEEDC, X, C,
     :  CJAC, NSTATE, IUSER, USER)
*+
*  Name:
*     SCULIB_SKYCON_1

*  Purpose:
*     routine to calculate non-linear constraints for
*     NAG non-linear fitting routine E04UPF when fitting
*     ETA_TEL, B and TAU in sky-dip analysis

*  Description:
*     E04UPF is a NAG routine used to fit a theoretical SKYDIP
*     curve to the measured data by varying ETA_TEL, B and TAU. This
*     routine is called by E04UPF to calculate the 1 non-linear constraint
*     in the problem and/or it's Jacobian at the current solution vector
*     X. See the NAG manual if you need to know more about E04UPF.
*
*     The solution vector is composed as follows:-
*          - x(1) = ETA_TEL
*          - x(2) = B
*          - x(3) = TAU
*
*     The non-linear constraint limits ETA_TEL * B. So, evaluation
*     of the constraint gives:-
*
*          C(1) = ETA_TEL * B
*
*     and its Jacobian has components :-
*
*          dC     B
*     .    -    =
*          dx(1)
*
*          dC     ETA_TEL
*     .    -    =
*          dx(2)
*
*          dC     0.0
*     .    -    =
*          dx(3)
*

*  Invocation:
*     CALL SCULIB_SKYCON_1 (MODE, NCNLN, N, LDCJ, NEEDC, X, C,
*    :  CJAC, NSTATE, IUSER, USER)

*  Arguments:
*     See NAG manual description of CONFUN parameter in E04UPF.


*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1993-1999 Particle Physics and Astronomy
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
      INTEGER NCNLN
      INTEGER N
      INTEGER LDCJ
      INTEGER NEEDC (NCNLN)
      DOUBLE PRECISION X (N)
      INTEGER NSTATE

*  Arguments Given & Returned:
      INTEGER IUSER (*)
      DOUBLE PRECISION USER (*)

*  Arguments Returned:
      DOUBLE PRECISION C (NCNLN)
      DOUBLE PRECISION CJAC (LDCJ,NCNLN)

*  Status:

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION B                   ! b parameter in fitted function
      DOUBLE PRECISION ETA_TEL             ! telescope transmission

*  Internal References:

*  Local data:

*.

*  unpack X vector

      ETA_TEL = X (1)
      B = X (2)

      IF (NEEDC(1) .GT. 0) THEN

*  non-linear constraint needs to be evaluated

         IF ((MODE .EQ. 0) .OR. (MODE .EQ. 2)) THEN

*  evaluate the constraint

            C (1) = B * ETA_TEL

         END IF

         IF ((MODE .EQ. 1) .OR. (MODE .EQ. 2)) THEN

*  calculate the Jacobian of the constraint

            CJAC (1,1) = B
            CJAC (1,2) = ETA_TEL
            CJAC (1,3) = 0.0D0

         END IF
      END IF

      END
