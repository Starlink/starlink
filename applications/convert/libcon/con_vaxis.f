      SUBROUTINE CON_VAXIS (SPECTM, DOPPLR, VCORR, FCEN, FINC, FREST,
     :  NPTS, AXIS, STATUS)
*+
*  Name:
*     CON_VAXIS
*  Purpose:
*     Compute the spectral axis.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CON_VAXIS (SPECTM, DOPPLR, VCORR, FCEN, FINC, FREST, NPTS;
*       AXIS; STATUS)
*  Description:
*     Compute the spectral axis.
*  Arguments:
*     SPECTM  =  CHARACTER*(*) (Given)
*        Flag indicating whether the spectral axis is to be expressed
*        as a frequency or a radial velocity.  The options are:
*        FREQUENCY  -  frequency in KHz,
*        VELOCITY   -  radial velocity in Km/sec.
*     DOPPLR  =  CHARACTER*(*) (Given)
*        Flag indicating whether the radial velocity is to be computed
*        using the classical or relativistic formula.  The options are:
*        CLASSICAL    -  classical,
*        RELATIVISTIC -  relativistic.
*        This flag is ignored if the spectrum is expressed as a
*        frequency.
*     VCORR  =  REAL (Given)
*        Computed radial velocity correction to the chosen standard of
*        rest (Km/sec).  Positive values indicate recession.
*     FCEN  =  REAL (Given)
*        Central frequency (Hz).
*     FINC  =  REAL (Given)
*        Frequency increment (Hz).
*     FREST  =  REAL (Given)
*        Rest frequency of the line (Hz).
*     NPTS  =  INTEGER (Given)
*        Number of points in the axis.
*     AXIS(NPTS)  =  REAL (Returned)
*        Central radial velocity of each point in the axis (Km/sec).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Compute the start frequency.
*     Compute the rest wavelength.
*     For each point in the axis
*       Compute the corresponding frequency.
*       If frequency units are required then
*         Set the axis value to the frequency.
*       else velocity units are required
*         Compute the wavelength.
*         If a classical doppler shift is required then
*           Compute the classical radial velocity.
*         else a relativistic doppler shift is required then
*           Compute the relativistic radial velocity.
*         end if
*         Set the axis value to the observed velocity correction minus
*         the correction to the standard of rest.
*       end if
*     end for
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     27/6/97 (ACD): Original version.
*     14/8/97 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      CHARACTER
     :  SPECTM*(*),
     :  DOPPLR*(*)
      REAL
     :  VCORR,
     :  FCEN,
     :  FINC,
     :  FREST
      INTEGER
     :  NPTS
*  Arguments Returned:
      REAL
     :  AXIS(NPTS)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      REAL C      ! Speed of light (m/sec).
      PARAMETER (C = 2.99792458E8)
*  Local Variables:
      INTEGER
     :  LOOP      ! Loop index.
      REAL
     :  FSTART,   ! Start frequency.
     :  WREST,    ! Rest wavelength of the line.
     :  FPT,      ! Frequency of the current point.
     :  WPT,      ! Wavelength of the current point.
     :  VOBSPT    ! Observed radial velocity (Km/sec).
      DOUBLE PRECISION
     :  RWRST2,   ! Squared, reciprocal rest   wavelength.
     :  RWOBS2    !    "         "      current    "     .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Compute the start frequency.

         FSTART = FCEN - (REAL( (NPTS/2) + 1 ) * FINC)

*
*       Compute the rest wavelength.

         WREST = C / FREST

*
*       Compute the value of each point in the axis.

         DO LOOP = 1, NPTS

*
*          Compute the frequency of the point.

            FPT = FSTART + (REAL(LOOP) * FINC)

*
*          If frequency units are required then set the axis value to
*          the frequency just computed.  Otherwise compute the radial
*          velocity.  In the former case the frequency is converted
*          from Hz to KHz.

            IF (SPECTM(1 : 1) .EQ. 'F') THEN
               AXIS(LOOP) = FPT / 1.0E3

            ELSE

*
*             First compute the corresponding wavelength.

               WPT = C / FPT

*
*             Compute the classical or relativistic radial velocity
*             (remembering to convert it from m/sec to Km/sec.

               IF (DOPPLR(1 : 1) .EQ. 'C') THEN
                  VOBSPT = C * (WPT - WREST) / (WPT * 1.0E3)
               ELSE
                  RWRST2 = 1.0D0 / (DBLE(WREST) * DBLE(WREST))
                  RWOBS2 = 1.0D0 / (DBLE(WPT) * DBLE(WPT))

                  VOBSPT = C * SNGL( 
     :              (RWRST2 - RWOBS2) / (RWRST2 + RWOBS2) ) / 1.0E3
               END IF

*
*             Set the axis value to the observed velocity correction
*             minus the correction to the standard of rest.

               AXIS(LOOP) = VOBSPT - VCORR

            END IF

         END DO

      END IF

      END
