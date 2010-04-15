      SUBROUTINE FIND18( OBPS, OBPSR, OBST1D, OBST2D, OBTH,
     :               SLCRSZ, SLPSI, SLTH,
     :               TESTOK, OBPSHI, OBPSLO, SLCRHW, THDIFF, STATUS )
*+
*  Name:
*     FIND18

*  Purpose:
*     This is the second test used in FINDCRDD to determine whether a
*     scan passes sufficiently close to the source position that the
*     user might require it.
*     It calculates the satellite angles IRAS would have had to see the
*     source, and compares them with the actual satellite angle range
*     for the observation under consideration. All these figures are
*     approximate as the source position is that at epoch 1983.5, the
*     solar longitude and the observation satellite angles are at the
*     epoch of the start of the SOP. The acceptance test is made wider
*     to take account of these discrepancies.
*

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND18( OBPS, OBPSR, OBST1D, OBST2D, OBTH,
*     :               SLCRSZ, SLPSI, SLTH,
*     :               TESTOK, OBPSHI, OBPSLO, SLCRHW, STATUS )

*  Description:
*     This is the second test used in FINDCRDD to determine whether a
*     scan passes sufficiently close to the source position that the
*     user might require it.
*     It calculates the satellite angles IRAS would have had to see the
*     source, and compares them with the actual satellite angle range
*     for the observation under consideration. The observation has
*     associated with it a fixed angle theta, which is the angle from
*     the Sun at which the whole observation was taken. It also has a
*     range of angle psi which is the range of angle swept out by the
*     observation perpendicular to theta.
*     This subroutine first compares observation theta with the source
*     theta plus an allowance for the half focal plane width and
*     difference in epoch of data. It then finds out whether the source
*     psi is within the range of psi covered by the observation.
*     All these figures are approximate as the source position is that
*     at epoch 1983.5, the solar longitude and the observation
*     satellite angles are at the epoch of the start of the SOP.
*     The acceptance test is made wider to take account of these
*     discrepancies.
*
*     -  Calculate
*           the absolute value of ( source theta - scan theta )
*           and compare it with the crosscan half width.
*        ( The cross scan half width is calculated as half the required
*        cross scan extent + one degree allowance for the focal plane
*        half width, and allowance for errors due to lack of epoch
*        coincidence)
*
*     If this criterion is met then
*
*     - Compare the source psi with the range of psi covered by the
*     observation. This consists of three alternatives:-
*        i) The observation psi lo is greater than zero and
*        observation psi lo < source psi <= observation psi hi.
*        ii) The observation psi lo is less than zero and
*        observation psi lo + 2*pi < source psi <= 2*pi.
*        ii) The observation psi lo is less than zero and
*        0 < source psi <= observation psi hi.

*  Arguments:
*     OBPS = REAL (Given)
*        Psi at start of observation
*     OBPSR = REAL (Given)
*        Psi rate for observation
*     OBST1D = DOUBLE PRECISION (Given)
*        Satcal time of start of observation double precision version.
*     OBST2D = DOUBLE PRECISION (Given)
*        Satcal time of end of observation double precision version.
*     OBTH = REAL (Given)
*        Theta for observation
*     SLCRSZ = REAL (Given)
*        Source cross scan size in radians - this routine does not use
*        common so SOCRSZ( ISOURC ) is supplied in this argument.
*     SLPSI = REAL (Given)
*        Source psi
*     SLTH = REAL (Given)
*        Source theta
*     TESTOK = LOGICAL (Given and Returned)
*        .TRUE. if test is passed
*     OBPSHI = REAL (Returned)
*        Highest Psi value in observation.
*     OBPSLO = REAL (Returned)
*        Lowest Psi value in observation.
*     SLCRHW = REAL (Returned)
*        Source cross scan half width test size
*     THDIFF = REAL (Returned)
*         The absolute value of ( the difference between the theta of
*         the source and the observation theta )
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     None

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     21-JAN-1992 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL OBPS
      REAL OBPSR
      DOUBLE PRECISION OBST1D
      DOUBLE PRECISION OBST2D
      REAL OBTH
      REAL SLPSI
      REAL SLTH
      REAL SLCRSZ

*  Arguments Given and Returned:
      LOGICAL TESTOK

*  Arguments Returned:
      REAL OBPSHI
      REAL OBPSLO
      REAL SLCRHW
      REAL THDIFF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL DEGTOR                ! One degree in radians
      PARAMETER ( DEGTOR = 1.745329252e-02 )
      REAL TWOPIR                ! Two PI in radians
      PARAMETER ( TWOPIR = 6.2831853072 )
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the cross scan half width as half the required cross scan
*  extent + one degree allowance for the focal plane half width,
*  and for errors due to lack of epoch coincidence.
      SLCRHW = SLCRSZ / 2.0 + DEGTOR

*  Calculate the absolute value of ( source theta - observation theta )
      THDIFF = ABS( SLTH - OBTH )

*  Calculate the upper limit of the psi range of the observation as the
*  given start of observation psi
      OBPSHI = OBPS

*  Calculate the lower limit of the psi range of the observation as the
*  given start of the observation psi + ( the difference between the
*  start of observation time and the end time ) * psi rate
      OBPSLO = OBPS + REAL( OBST2D - OBST1D ) * OBPSR

*  Test whether the difference in theta is less than cross scan half
*  width test size
      IF ( THDIFF .LE. SLCRHW ) THEN

*  Test whether the source psi is within the range of observation psi
*  This consists of three alternatives but the first test is whether
*  the lower limit of the observation range is greater than zero
         IF ( OBPSLO .GE. 0.0 ) THEN

*  Given that the observation psi lo is greater than zero
*  the source psi is within the range if:-
*     observation psi lo < source psi <= observation psi hi.
            IF ( ( SLPSI .GE. OBPSLO ) .AND.
     :           ( SLPSI .LE. OBPSHI ) ) THEN
               TESTOK = .TRUE.
            ELSE

*  Source psi is not within observation psi range
               TESTOK = .FALSE.
            END IF
         ELSE

*  Given that the observation psi lo is less than zero
*  the source psi is within the range if either:-
*     observation psi lo + 2*pi < source psi <= 2*pi.
            IF ( ( SLPSI .GE. ( OBPSLO + TWOPIR) ) .AND.
     :           ( SLPSI .LE. TWOPIR)  ) THEN
               TESTOK = .TRUE.

*  or if:-
*     0 < source psi <= observation psi hi.
            ELSE IF ( ( SLPSI .GE. 0.0 ) .AND.
     :                ( SLPSI .LE. OBPSHI ) ) THEN
               TESTOK = .TRUE.
            ELSE

*  Source psi is not within observation psi range
               TESTOK = .FALSE.
            END IF
         END IF
      ELSE

*  Source theta is not within the half cross scan test size of the
*  observation theta.
         TESTOK = .FALSE.
      END IF

      END
