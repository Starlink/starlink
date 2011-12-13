      SUBROUTINE CON_SPHST( CUBID, SPECTM, DOPPLR, SOR, VCORR, STATUS )
*+
*  Name:
*     CON_SPHST

*  Purpose:
*     Adds history details describing the spectral axis.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL( CUBID, SPECTM, DOPPLR, SOR, VCORR; STATUS )

*  Description:
*     Add history details describing the spectral axis.

*  Arguments:
*     CUBID  =  INTEGER (Given)
*        Identifier for the data cube.
*     SPECTM  =  CHARACTER*(*) (Given)
*        Flag indicating whether the spectral axis is to be expressed
*        as frequencies or as radial velocities.  The options are:
*        FREQUENCY  -  frequency in kHz,
*        VELOCITY   -  radial velocity in km/sec.
*     DOPPLR  =  CHARACTER*(*) (Given)
*        Flag indicating whether the radial velocity is to be computed
*        using the classical or relativistic formula.  The options are:
*        CLASSICAL    -  classical,
*        RELATIVISTIC -  relativistic.
*        This flag is ignored if the spectrum is expressed as a
*        frequency.
*     SOR  =  CHARACTER*(*) (Given)
*        Flag indicating the standard of rest for which the correction
*        is to computed.  The permitted types are:
*        OBSERVED   - observed,
*        SOLAR      - solar (heliocentric),
*        KINLSR     - kinematical local standard of rest,
*        DYNLSR     - dynamical local standard of rest,
*        GALAXY     - centre of the Galaxy,
*        LOCALGROUP - local group.
*        This flag is ignored if the spectrum is expressed as a
*        frequency.
*     VCORR  =  REAL (Given)
*        Computed radial velocity correction to the required standard
*        of rest (km/sec).  Positive values indicate recession.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     If the spectral axis is in frequency then
*       Add a history line saying the axis in frequency.
*     else
*       Create a history line saying the axis is radial velocity.
*       Create a history line indicating whether the radial velocities
*       have been computed using the classical or relatavistic formula.
*       Create a history line indicating which standard of rest was
*       used.
*       If the observed radial velocity was not used then
*         Create a history line containing the radial velocity
*         adjustment to the standard of rest.
*       end if
*       Add the history lines.
*     end if
*     Report any error.

*  Copyright:
*     Copyright (C) 1997-1998 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     14/8/97 (ACD):
*        Original version.
*     8/1/98  (ACD):
*        Changed KHz to kHz and Km to km to conform to standard usage.
*     2009 June 29 (MJC):
*        Used modern coding style.  Correct more Km to km.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants
      INCLUDE 'NDF_PAR'          ! NDF parametric constants

*  Arguments Given:
      INTEGER CUBID
      CHARACTER*(*) SPECTM
      CHARACTER*(*) DOPPLR
      CHARACTER*(*) SOR
      REAL VCORR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LSTAT              ! Local fortran I/O status
      INTEGER TEXTS              ! Number of lines of history text
      CHARACTER*(NDF__SZHIS) TEXT( 4 ) ! Lines of history text
      CHARACTER*12 VCORRC        ! Character representation of
                                 ! radial-velocity correction

*.

*  Check the global inherited status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check whether the spectrum is expressed as frequencies or radial
*  velocities.
         IF ( SPECTM(1 : 1) .EQ. 'F' ) THEN

*  The axis is a set of frequencies.  Add an appropriate line to the
*  history.
            TEXT(1) = 'The spectral axis is a frequency in kHz.'

            CALL NDF_HPUT( 'NORMAL', ' ', .FALSE., 1, TEXT, .FALSE.,
     :                     .FALSE., .FALSE., CUBID, STATUS )

         ELSE

*  The axis is a set of radial velocities.  Assemble appropriate lines
*  for the history.  First the type of axis...
            TEXT( 1 ) = 'The spectral axis is a radial velocity in '/
     :                  /'km/sec.'

*  ... then the formula used for the Doppler shift.
            IF ( DOPPLR( 1:1 ) .EQ. 'C' ) THEN
               TEXT( 2 ) = 'These velocities were computed using the '/
     :                     /'classical Doppler formula.'
            ELSE IF ( DOPPLR( 1:1 ) .EQ. 'R' ) THEN
               TEXT( 2 ) = 'These velocities were computed using the '/
     :                     /'relativistic Doppler formula.'
            ELSE
               CALL MSG_SETC( 'DOPPLER', DOPPLR )
               TEXT( 2 ) = 'Illegal type of doppler shift given: '/
     :                     /'^DOPPLER.'
            END IF

*  ... the standard of rest used.
            IF ( SOR .EQ. 'OBSERVED' ) THEN
               TEXT( 3 ) = 'They are observed velocities, uncorrected '/
     :                     /'to any standard of rest.'
            ELSE IF ( SOR .EQ. 'SOLAR' ) THEN
               TEXT( 3 ) = 'They are relative to a solar '/
     :                     /'(heliocentric) standard of rest.'
            ELSE IF ( SOR .EQ. 'KINLSR' ) THEN
               TEXT( 3 ) = 'They are relative to a kinematical local '/
     :                     /'standard of rest.'
            ELSE IF ( SOR .EQ. 'DYNLSR' ) THEN
               TEXT( 3 ) = 'They are relative to the dynamical local '/
     :                     /'standard of rest.'
            ELSE IF ( SOR .EQ. 'GALAXY' ) THEN
               TEXT( 3 ) = 'They are relative to the centre of the '/
     :                     /'Galaxy.'
            ELSE IF ( SOR .EQ. 'LOCALGROUP' ) THEN
               TEXT( 3 ) = 'They are relative to the centre of the '/
     :                     /'Local Group of galaxies.'
            ELSE
               CALL MSG_SETC ('SOR', SOR)
               TEXT( 3 ) = 'Illegal type of standard of rest given: '/
     :                     /'^SOR.'
            END IF

*  ... finally, the velocity correction, if required.
            IF ( SOR .NE. 'OBSERVED' ) THEN
               TEXTS = 4

               WRITE( VCORRC, '(F12.2)', IOSTAT=LSTAT ) VCORR
               CALL CHR_LDBLK( VCORRC )

               CALL MSG_SETC( 'VCORRC', VCORRC )
               TEXT( 4 ) = 'The adjustment to this standard of rest '/
     :                     /'is ^VCORRC km/sec.'

            ELSE
               TEXTS = 3

            END IF

*  Add the assembled lines to the history.
            CALL NDF_HPUT( 'NORMAL', ' ', .FALSE., TEXTS, TEXT,
     :                     .TRUE., .FALSE., .FALSE., CUBID, STATUS )

         END IF

*  Report any error.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'CON_SPHST_ERR', 'CON_SPHST_ERR: failure '/
     :        /'adding details of the spectral axis to the history.',
     :        STATUS )
         END IF

      END IF

      END
