      SUBROUTINE CON_VAXIS( SPECTM, DOPPLR, VCORR, FCEN, FINC, FREST,
     :                      NPTS, AXIS, STATUS)
*+
*  Name:
*     CON_VAXIS

*  Purpose:
*     Fills the spectral axis.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL CON_VAXIS( SPECTM, DOPPLR, VCORR, FCEN, FINC, FREST, NPTS;
*                     AXIS; STATUS)

*  Description:
*     Compute the spectral axis.

*  Arguments:
*     SPECTM  =  CHARACTER*(*) (Given)
*        Flag indicating whether the spectral axis is to be expressed
*        as a frequency or a radial velocity.  The options are:
*        FREQUENCY  -  frequency in KHz,
*        VELOCITY   -  radial velocity in km/sec.
*     DOPPLR  =  CHARACTER*(*) (Given)
*        Flag indicating whether the radial velocity is to be computed
*        using the classical or relativistic formula.  The options are:
*        CLASSICAL    -  classical,
*        RELATIVISTIC -  relativistic.
*        This flag is ignored if the spectrum is expressed as a
*        frequency.
*     VCORR  =  REAL (Given)
*        Computed radial velocity correction to the chosen standard of
*        rest (km/sec).  Positive values indicate recession.
*     FCEN  =  REAL (Given)
*        Central frequency (Hz).
*     FINC  =  REAL (Given)
*        Frequency increment (Hz).
*     FREST  =  REAL (Given)
*        Rest frequency of the line (Hz).
*     NPTS  =  INTEGER (Given)
*        Number of points in the axis.
*     AXIS(NPTS)  =  REAL (Returned)
*        Central radial velocity of each point in the axis (km/sec).
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

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     27/6/97 (ACD):
*        Original version.
*     14/8/97 (ACD):
*        First stable version.
*     2009 June 29 (MJC):
*        Used modern coding style.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.

*  Arguments Given:
      CHARACTER*(*) SPECTM
      CHARACTER*(*) DOPPLR
      REAL VCORR
      REAL FCEN
      REAL FINC
      REAL FREST
      INTEGER NPTS

*  Arguments Returned:
      REAL AXIS( NPTS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL C                     ! Speed of light (m/sec)
      PARAMETER (C = 2.99792458E8)

*  Local Variables:
      REAL FPT                   ! Frequency of the current point
      REAL FSTART                ! Start frequency
      INTEGER LOOP               ! Loop index
      DOUBLE PRECISION RWOBS2    ! Squared, reciprocal current
                                 ! wavelength
      DOUBLE PRECISION RWRST2    ! Squared, reciprocal rest wavelength
      REAL WPT                   ! Wavelength of the current point
      REAL WREST                 ! Rest wavelength of the line
      REAL VOBSPT                ! Observed radial velocity (kM/sec)

*.

*  Check the global inherited status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Compute the start frequency.
         FSTART = FCEN - ( REAL( ( NPTS / 2 ) + 1 ) * FINC )

*  Compute the rest wavelength.
         WREST = C / FREST

*  Compute the value of each point in the axis.
         DO LOOP = 1, NPTS

*  Compute the frequency of the point.
            FPT = FSTART +( REAL(LOOP) * FINC)

*  If frequency units are required then set the axis value to the
*  frequency just computed.  Otherwise compute the radial velocity.  In
*  the former case the frequency is converted from Hz to KHz.
            IF ( SPECTM(1 : 1) .EQ. 'F' ) THEN
               AXIS( LOOP ) = FPT / 1.0E3

            ELSE

*  First compute the corresponding wavelength.
               WPT = C / FPT

*  Compute the classical or relativistic radial velocity (remembering to
*  convert it from m/sec to km/sec.
               IF ( DOPPLR(1 : 1) .EQ. 'C' ) THEN
                  VOBSPT = C * ( WPT - WREST ) / ( WPT * 1.0E3 )
               ELSE
                  RWRST2 = 1.0D0 / (DBLE( WREST ) * DBLE( WREST ) )
                  RWOBS2 = 1.0D0 / (DBLE( WPT ) * DBLE( WPT ) )

                  VOBSPT = C * SNGL( ( RWRST2 - RWOBS2 ) /
     :                     ( RWRST2 + RWOBS2 ) ) / 1.0E3
               END IF

*  Set the axis value to the observed velocity correction minus the
*  correction to the standard of rest.
               AXIS( LOOP ) = VOBSPT - VCORR

            END IF

         END DO

      END IF

      END
