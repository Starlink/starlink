      SUBROUTINE SCULIB_J_THEORETICAL (TAUZ, AIRMASS, T_TEL, T_AMB,
     :  WAVELENGTH, ETA_TEL, B, J_THEORETICAL, STATUS)
*+
*  Name:
*     SCULIB_J_THEORETICAL

*  Purpose:
*     calculate the theoretical sky brightness temperature

*  Description:
*     This routine calculates the theoretical sky brightness temperature
*     according to Bill Duncan's model:-
*
*       Jth = (1 - eta_tel) * Jtel + eta_tel * Jatm *
*             (1 - b * exp (-tauz * airmass))
*
*     where Jatm is the effective brightness temperature of the sky, related
*     to the ambient air temperature at ground level by:-
*
*       Jatm = Jamb * Xg
*
*       Xg   = 1 + hscale * Tlapse * exp (-tauz * airmass / Xgconst)
*                 .---------------
*                      Tamb
*
*        hscale = 2km
*        Tlapse = temperature drop/km -6.5 K
*       Xgconst = 3.669383
*
*     If tauz * airmass exceeds 15 the following approximation will be
*     used:-
*
*       Jth = (1 - eta_tel) * Jtel + eta_tel * Jatm
*
*     The routine will return immediately if entered with bad status.
*     Errors will be reported and bad status returned if:-
*       - TAUZ is less than 0
*       - AIRMASS is less than 0
*       - WAVELENGTH is less than or equal to 0
*       - T_AMB is less than or equal to 0
*       - ETA_TEL is outside the range 0 to 1
*       - B is outside the range 0 to 1
*

*  Invocation:
*     CALL SCULIB_J_THEORETICAL (TAUZ, AIRMASS, T_TEL, T_AMB,
*    :  WAVELENGTH, ETA_TEL, B, J_THEORETICAL, STATUS)

*  Arguments:
*     TAUZ                = REAL (Given)
*           zenith sky optical depth
*     AIRMASS             = REAL (Given)
*           airmass
*     T_TEL               = REAL (Given)
*           temperature of the telescope (K)
*     T_AMB               = REAL (Given)
*           temperature of ambient air (K)
*     WAVELENGTH          = REAL (Given)
*           wavelength of interest (microns)
*     ETA_TEL             = REAL (Given)
*           telescope transmission
*     B                   = REAL (Given)
*           bandwidth factor
*     J_THEORETICAL       = REAL (Returned)
*           the theoretical sky brightness temperature
*     STATUS              = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (JFL@ROE)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     6-JAN-1995: Original version
*    24-JUL-1996: modified to use correct function (JFL).
*    $Log$
*    Revision 1.4  2001/03/30 20:22:38  timj
*    Use T_AMB rather than 273 for X_G
*
*    Revision 1.3  1999/08/19 03:37:16  timj
*    Header tweaks to ease production of SSN72 documentation.
*
*    Revision 1.2  1999/08/03 19:35:11  timj
*    Add copyright message to header.
*    Convert old header style to new.
*
*    Revision 1.1  1996/08/05 19:44:35  timj
*    Initial revision
*
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      REAL TAUZ
      REAL AIRMASS
      REAL T_TEL
      REAL T_AMB
      REAL WAVELENGTH
      REAL ETA_TEL
      REAL B

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL J_THEORETICAL

*  Status:
      INTEGER STATUS

*  External references:
      REAL SCULIB_JNU                ! brightness temperature function

*  Global variables:

*  Local Constants:
      REAL LIGHT                     ! velocity of light
      PARAMETER (LIGHT = 2.997929E8)

*  Local variables:
      REAL J_AMB                     ! brightness temperature of T_AMB
      REAL J_ATM                     ! effective brightness temperature of
                                     ! sky
      REAL J_TEL                     ! brightness temperature of T_TEL
      REAL NU                        ! frequency corresponding to wavelength
      REAL X_G                       ! Bill Duncan fudge factor relating
                                     ! J_ATM to J_AMB

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      IF (TAUZ .LT. 0.0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETR ('V', TAUZ)
         CALL ERR_REP (' ', 'SCULIB_J_THEORETICAL: bad TAUZ - ^V',
     :     STATUS)
      ELSE IF (AIRMASS .LT. 0.0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETR ('V', AIRMASS)
         CALL ERR_REP (' ', 'SCULIB_J_THEORETICAL: bad AIRMASS - ^V',
     :     STATUS)
      ELSE IF (WAVELENGTH .LE. 0.0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETR ('V', WAVELENGTH)
         CALL ERR_REP (' ', 'SCULIB_J_THEORETICAL: bad WAVELENGTH - ^V',
     :     STATUS)
      ELSE IF (T_AMB .LE. 0.0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETR ('V', T_AMB)
         CALL ERR_REP (' ', 'SCULIB_J_THEORETICAL: bad T_AMB - ^V',
     :     STATUS)
      ELSE IF ((ETA_TEL .LT. 0.0) .AND. (ETA_TEL .GT. 1.0)) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETR ('V', ETA_TEL)
         CALL ERR_REP (' ', 'SCULIB_J_THEORETICAL: bad ETA_TEL - ^V',
     :     STATUS)
      ELSE IF ((B .LT. 0.0) .AND. (B .GT. 1.0)) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETR ('V', B)
         CALL ERR_REP (' ', 'SCULIB_J_THEORETICAL: bad B - ^V', STATUS)
      ELSE

         NU = LIGHT / (WAVELENGTH * 1.0E-6)
         J_TEL = SCULIB_JNU (NU, T_TEL, STATUS)
         J_AMB = SCULIB_JNU (NU, T_AMB, STATUS)

         IF (ABS(TAUZ * AIRMASS) .LT. 15.0) THEN
            X_G = 1.0 - 2.0 * 6.5 * EXP (-TAUZ * AIRMASS / 3.669383) /
     :        T_AMB
            J_ATM = J_AMB * X_G
            J_THEORETICAL = (1.0 - ETA_TEL) * J_TEL + ETA_TEL * J_ATM *
     :        (1.0 - B * EXP (-TAUZ * AIRMASS))
         ELSE
            J_ATM = J_AMB
            J_THEORETICAL = (1.0 - ETA_TEL) * J_TEL + ETA_TEL * J_ATM
         END IF
      END IF

      END
