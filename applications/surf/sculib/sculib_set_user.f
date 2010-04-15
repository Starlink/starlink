      SUBROUTINE SCULIB_SET_USER (J_TEL, J_ATM, N_MEASUREMENTS,
     :  AIRMASS, J_MEAS_D, J_MEAS_V, USER)
*+
*  Name:
*     SCULIB_SET_USER

*  Purpose:
*     set the USER array used by SCULIB_SKYFUNC_1

*  Description:
*     E04UPF is a NAG routine that is used to fit a theoretical
*     sky-dip curve to the measured data by varying ETA_TEL, B and TAU.
*     SCULIB_SKYFUNC_1 is called by E04UPF to calculate the M sub-functions
*     fi(x) at the current solution vector X, and/or their Jacobian
*     matrix (see NAG manual for further info). That routine obtains the
*     data needed for its calculations via the USER array passed into it,
*     and this routine fills USER with the necessary numbers:-
*      - USER (1) = J_TEL
*      - USER (2) = J_ATM
*      - USER (3) = not used
*      - USER (4:M+3)      = the measured airmasses
*      - USER (M+4:2M+3)   = the measured sky temperatures
*      - USER (2M+4:3M+3)  = the errors on the measured sky temperatures


*  Invocation:
*     CALL SCULIB_SET_USER (J_TEL, J_ATM, N_MEASUREMENTS,
*    :  AIRMASS, J_MEAS_D, J_MEAS_V, USER)

*  Arguments:
*     J_TEL                     = REAL (Given)
*             the temperature of the telescope
*     J_ATM                     = REAL (Given)
*             the temperature of the atmosphere
*     N_MEASUREMENTS            = INTEGER (Given)
*             the number of measurements taken in the sky-dip
*     AIRMASS (N_MEASUREMENTS)  = REAL (Given)
*             the airmass at each measurement
*     J_MEAS_D (N_MEASUREMENTS) = REAL (Given)
*             the brightness temperature of the atmosphere at each airmass
*     J_MEAS_V (N_MEASUREMENTS) = REAL (Given)
*             the variance on J_MEAS_D
*     USER (3 * N_MEASUREMENTS + 3) = DOUBLE PRECISION (Returned)
*             the USER array required

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
*     28-SEP-1993: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:

*  Arguments Given:
      REAL J_TEL
      REAL J_ATM
      INTEGER N_MEASUREMENTS
      REAL AIRMASS (N_MEASUREMENTS)
      REAL J_MEAS_D (N_MEASUREMENTS)
      REAL J_MEAS_V (N_MEASUREMENTS)

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION USER (3 * N_MEASUREMENTS + 3)

*  Status:

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I

*  Internal References:

*  Local data:

*.

      USER (1) = DBLE (J_TEL)
      USER (2) = DBLE (J_ATM)

      DO I = 1, N_MEASUREMENTS
         USER (I+3) = DBLE (AIRMASS(I))
         USER (I+3+N_MEASUREMENTS) = DBLE (J_MEAS_D(I))
         USER (I+3+2*N_MEASUREMENTS) = DBLE (J_MEAS_V(I))
      END DO

      END
