      SUBROUTINE KPG1_DSFR1( FRM, TEXT, NIND, FULL, STATUS )
*+
*  Name:
*     KPG1_DSFR1

*  Purpose:
*     Displays a textual description of the Current Frame in a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_DSFR1( FRM, TEXT, NIND, FULL, STATUS )

*  Description:
*     This routine displays a textual description of the supplied AST
*     Frame.  The displayed information does not include any
*     axis-specific details that are common to all classes of Frame
*     (such as axis units, labels, etc.).

*  Arguments:
*     FRM = INTEGER (Given)
*        An AST pointer to the Frame.
*     TEXT = CHARACTER * ( * ) (Given)
*        Text to display before the Frame description. May contain MSG
*        tokens.
*     NIND = INTEGER (Given)
*        Number of spaces to display at the start of each line.
*     FULL = LOGICAL (Given)
*        Display full information?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2003, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005, 2006 Particle Physics & Astronomy Research Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-FEB-2003 (DSB):
*        Original version.
*     11-AUG-2004 (DSB):
*        Report details of DSBSpecFrame class.
*     9-SEP-2005 (DSB):
*        Change GeoLon/Lat to ObsLon/Lat and move out of SpecFrame section.
*     14-SEP-2005 (DSB):
*        - Modify logic for choosing whether to display the observers position.
*        - Include the SkyFrame reference position in the report, if used.
*     30-JAN-2006 (DSB):
*        - Display source velocity in the current velocity system, if
*        possible.
*     28-JUN-2006 (TIMJ):
*        Add some TimeFrame attributes.
*     20-JUL-2006 (DSB):
*        Allow SideBand to be LO offset.
*     20-JUL-2006 (TIMJ):
*        Add some defensive programming to the SideBand string
*     24-AUG-2006 (DSB):
*        Display TimeOrigin to full precision.
*     5-OCT-2006 (DSB):
*        Clear TimeOrigin in the TimeFrame used to format the TimeOrigin
*        value so that the TimeOrigin value is formatted as an absolute
*        time.
*     11-OCT-2006 (DSB):
*        Ensure no units are displayed for redshift values.
*     5-DEC-2006 (DSB):
*        Ensure SourceVel is accessed as an apparent velocity.
*     6-DEC-2006 (DSB):
*        Display SourceVel in system specified by SourceSys unless
*        SourceSys is not set, in which case use main SpecFrame System if
*        possible.
*     24-AUG-2007 (DSB):
*        Display the SkyRef position even if it is ignored.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER FRM
      CHARACTER TEXT*(*)
      INTEGER NIND
      LOGICAL FULL

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN
      LOGICAL CHR_SIMLR
      DOUBLE PRECISION SLA_EPJ2D
      DOUBLE PRECISION SLA_EPB2D

*  Local Variables :
      CHARACTER FRMDMN*80        ! Frame domain
      CHARACTER FRMTTL*80        ! Frame title
      CHARACTER IND*80           ! Indentation string
      CHARACTER LABEL*40         ! A description of the spectral coord system
      CHARACTER MONTH( 12 )*3    ! Month names
      CHARACTER POSBUF*80        ! Buffer for position
      CHARACTER PRJ*50           ! Sky projection
      CHARACTER SIDEBN*10        ! SideBand value
      CHARACTER SIGN*1           ! Sign of day value
      CHARACTER SOR*30           ! Spectral standard of rest
      CHARACTER SREFIS*10        ! Value of SkyFrame SkyRefIs attribute
      CHARACTER SYS*30           ! Coordinate system
      CHARACTER SSYS*30          ! SourceSys value
      CHARACTER TSC*30           ! Time Scale of TimeFrame
      CHARACTER UNIT*20          ! Units string
      DOUBLE PRECISION EP        ! Epoch of observation
      DOUBLE PRECISION EQ        ! Epoch of reference equinox
      DOUBLE PRECISION FD        ! Fraction of day (+ve)
      DOUBLE PRECISION IFF       ! Intermediate frequency
      DOUBLE PRECISION MJD       ! Modified Julian Date corresponding to Epoch
      DOUBLE PRECISION SRCVEL    ! Source velocity
      DOUBLE PRECISION TIMEOR    ! Time Origin
      INTEGER FRM2               ! Modified copy of supplied Frame
      INTEGER IAT                ! Current length of a string
      INTEGER ID                 ! Day of month
      INTEGER IHMSF( 4 )         ! Hours, mins, secs, fraction of sec
      INTEGER IM                 ! Index of month
      INTEGER IY                 ! Year
      INTEGER J                  ! SLALIB status
      LOGICAL SHOWEP             ! Display the Epoch value?
      LOGICAL SSSET              ! SourceSys assigned a value?
      LOGICAL SHOWOB             ! Display the Observers position?

      DATA MONTH/ 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL',
     :            'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /,
     :     IND/ ' ' /
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Display any header text.
      IF( TEXT .NE. ' ' ) CALL MSG_OUT( 'KPG1_DSFR1_1', TEXT, STATUS )
      CALL MSG_BLANK( STATUS )

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Frame title and domain.
      FRMTTL = AST_GETC( FRM, 'TITLE', STATUS )
      FRMDMN = AST_GETC( FRM, 'DOMAIN', STATUS )

*  Remove any PGPLOT escape sequences from the title.
      CALL KPG1_PGESC( FRMTTL, STATUS )

*  Display the title (upto 45 characters), and domain.
      CALL MSG_SETC( 'TTL', FRMTTL( : 45 - NIND ) )
      IF( CHR_LEN( FRMTTL ) .GT. 45 - NIND ) CALL MSG_SETC( 'TTL',
     :                                                      '...' )

      CALL MSG_OUT( 'WCS_TITLE',
     :              IND( : NIND )//'Frame title         : "^TTL"',
     :              STATUS )

      CALL MSG_SETC( 'DOMAIN', FRMDMN )
      CALL MSG_OUT( 'WCS_DOMAIN',
     :     IND( : NIND )//'Domain              : ^DOMAIN', STATUS )

*  The rest is only displayed if a full listing is required.
      IF( FULL ) THEN

*  Initialise a flag to indicate that we do not have a SkyFrame or a
*  SpecFrame, and should therefore not display the EPOCH.
         SHOWEP = .FALSE.

*  Initialise a flag to indicate that we do not have a AzEl SkyFrame or a
*  SpecFrame, and should therefore not display the ObsLon/Lat.
         SHOWOB = .FALSE.

*  If the Frame is a SkyFrame, display the equinox, system and projection.
         IF( AST_ISASKYFRAME( FRM, STATUS ) ) THEN

*  Indicate that the EPOCH should be displayed.
            SHOWEP = .TRUE.

*  First get the equinox, system, projection, etc.
            EQ = AST_GETD( FRM, 'EQUINOX', STATUS )
            SYS = AST_GETC( FRM, 'SYSTEM', STATUS )
            PRJ = AST_GETC( FRM, 'PROJECTION', STATUS )
            SREFIS = AST_GETC( FRM, 'SKYREFIS', STATUS )

*  Construct a message token holding suitable description for each type of
*  system...
*  RA/DEC...
            IF( SYS .EQ. 'FK4' .OR. SYS .EQ. 'FK5' ) THEN
               CALL MSG_SETC( 'SYS', 'Equatorial (' )
               CALL MSG_SETC( 'SYS', SYS )
               CALL MSG_SETC( 'SYS', ' -' )
               IF( EQ .LT. 1984.0 ) THEN
                  CALL MSG_SETC( 'SYS', ' B' )
               ELSE
                  CALL MSG_SETC( 'SYS', ' J' )
               END IF
               CALL MSG_SETD( 'SYS', EQ )
               CALL MSG_SETC( 'SYS', ')' )

            ELSE IF( SYS .EQ. 'FK4-NO-E' ) THEN
               CALL MSG_SETC( 'SYS', 'Equatorial without '//
     :                        'E-terms (FK4 -' )
               IF( EQ .LT. 1984.0 ) THEN
                  CALL MSG_SETC( 'SYS', ' B' )
               ELSE
                  CALL MSG_SETC( 'SYS', ' J' )
               END IF
               CALL MSG_SETD( 'SYS', EQ )
               CALL MSG_SETC( 'SYS', ')' )

*  Geocentric apparent...
            ELSE IF( SYS .EQ. 'GAPPT' ) THEN
               CALL MSG_SETC( 'SYS', 'Equatorial (geocentric '//
     :                        'apparent)' )

*  Ecliptic...
            ELSE IF( SYS .EQ. 'ECLIPTIC' ) THEN
               CALL MSG_SETC( 'SYS', 'Ecliptic (' )
               IF( EQ .LT. 1984.0 ) THEN
                  CALL MSG_SETC( 'SYS', ' B' )
               ELSE
                  CALL MSG_SETC( 'SYS', ' J' )
               END IF
               CALL MSG_SETD( 'SYS', EQ )
               CALL MSG_SETC( 'SYS', ')' )

*  Galactic...
            ELSE IF( SYS .EQ. 'GALACTIC' ) THEN
               CALL MSG_SETC( 'SYS', 'Galactic' )

*  Supergalactic...
            ELSE IF( SYS .EQ. 'SUPERGALACTIC' ) THEN
               CALL MSG_SETC( 'SYS', 'Supergalactic' )

*  AzEl...
            ELSE IF( SYS .EQ. 'AZEL' ) THEN
               CALL MSG_SETC( 'SYS', 'Horizon' )
               SHOWOB = .TRUE.

*  Anything else..
            ELSE
               CALL MSG_SETC( 'SYS', SYS )
            END IF

*  Indicate if the system represents offsets or absolute coords
            IF( SREFIS .EQ. 'Pole' ) THEN
               CALL MSG_SETC( 'OFF', 'polar offsets' )
            ELSE IF( SREFIS .EQ. 'Origin' ) THEN
               CALL MSG_SETC( 'OFF', 'offsets' )
            ELSE
               CALL MSG_SETC( 'OFF', ' ' )
            END IF

*  Display the system.
            CALL MSG_OUT( 'WCS_SYS',
     :                 IND( : NIND )//'System              : ^SYS ^OFF',
     :                 STATUS )

*  Display the reference position, if it is being used.
            IF( SREFIS .NE. 'Ignored' ) THEN
               POSBUF = ' '
               IAT = 0
               CALL CHR_APPND( SREFIS, POSBUF, IAT )
               IAT = IAT + 1
               CALL CHR_APPND( 'position', POSBUF, IAT )
               IAT = 20
               CALL CHR_APPND( ':', POSBUF, IAT )

               CALL MSG_SETC( 'LAB', POSBUF( :IAT ) )
               CALL MSG_SETC( 'REF', AST_GETC( FRM, 'SkyRef', STATUS ) )

               CALL MSG_OUT( 'WCS_REF', IND( : NIND )//'^LAB ^REF',
     :                       STATUS )

            ELSE IF( AST_TEST( FRM, 'SkyRef', STATUS ) ) THEN
               POSBUF = ' '
               IAT = 0
               CALL CHR_APPND( 'Ref. position', POSBUF, IAT )
               IAT = 20
               CALL CHR_APPND( ':', POSBUF, IAT )

               CALL MSG_SETC( 'LAB', POSBUF( :IAT ) )
               CALL MSG_SETC( 'REF', AST_GETC( FRM, 'SkyRef', STATUS ) )

               CALL MSG_OUT( 'WCS_REF', IND( : NIND )//'^LAB ^REF',
     :                       STATUS )

            END IF

*  Display the projection.
            IF( PRJ .NE. ' ' ) THEN
               CALL MSG_SETC( 'PROJ', PRJ )
               CALL MSG_OUT( 'WCS_PROJ',
     :            IND( : NIND )//'Projection          : ^PROJ', STATUS )
            END IF

*  If the Frame is a SpecFrame, display SpecFrame specific information...
         ELSE IF( AST_ISASPECFRAME( FRM, STATUS ) ) THEN

*  Indicate that the EPOCH and ObsLon/Lat should be displayed.
            SHOWEP = .TRUE.
            SHOWOB = .TRUE.

*  System...
            SYS = AST_GETC( FRM, 'SYSTEM', STATUS )
            IF( SYS .EQ. 'FREQ' ) THEN
               LABEL = 'Frequency'
            ELSE IF( SYS .EQ. 'ENER' ) THEN
               LABEL = 'Energy'
            ELSE IF( SYS .EQ. 'WAVN' ) THEN
               LABEL = 'Wave number'
            ELSE IF( SYS .EQ. 'WAVE' ) THEN
               LABEL = 'Wavelength'
            ELSE IF( SYS .EQ. 'AWAV' ) THEN
               LABEL = 'Wavelength (in air)'
            ELSE IF( SYS .EQ. 'VRAD' ) THEN
               LABEL = 'Radio velocity'
            ELSE IF( SYS .EQ. 'VOPT' ) THEN
               LABEL = 'Optical velocity'
            ELSE IF( SYS .EQ. 'ZOPT' ) THEN
               LABEL = 'Redshift'
            ELSE IF( SYS .EQ. 'BETA' ) THEN
               LABEL = 'Beta factor'
            ELSE IF( SYS .EQ. 'VELO' ) THEN
               LABEL = 'Apparent radial velocity'
            ELSE
               LABEL = SYS
            END IF

            CALL MSG_SETC( 'SYS', LABEL )
            UNIT = AST_GETC( FRM, 'UNIT(1)', STATUS )
            IF( UNIT .NE. ' ' ) THEN
               CALL MSG_SETC( 'SYS', ' (' )
               CALL MSG_SETC( 'SYS', UNIT )
               CALL MSG_SETC( 'SYS', ')' )
            END IF
            CALL MSG_OUT( 'WCS_SYS',
     :                 IND( : NIND )//'System              : ^SYS',
     :                 STATUS )

*  Rest Frequency...
            IF( AST_TEST( FRM, 'RestFreq', STATUS ) ) THEN
               CALL MSG_SETD( 'RF', AST_GETD( FRM, 'RestFreq',
     :                        STATUS ) )
               CALL MSG_SETC( 'RF', ' GHz' )
            ELSE
               CALL MSG_SETC( 'RF', '<not defined>' )
            END IF
            CALL MSG_OUT( 'WCS_RF',
     :                 IND( : NIND )//'Rest frequency      : ^RF',
     :                 STATUS )

*  Standard of Rest...
            SOR = AST_GETC( FRM, 'STDOFREST', STATUS )
            IF( CHR_SIMLR( SOR, 'NONE' ) ) THEN
               CALL MSG_SETC( 'SOR', '<not defined>' )

            ELSE IF( CHR_SIMLR( SOR, 'LSRK' ) ) THEN
               CALL MSG_SETC( 'SOR', 'Kinematical Local '//
     :                                    'Standard of Rest' )

            ELSE IF( CHR_SIMLR( SOR, 'LSRD' ) ) THEN
               CALL MSG_SETC( 'SOR', 'Dynamical Local '//
     :                                    'Standard of Rest' )

            ELSE IF(  CHR_SIMLR( SOR, 'LOCAL_GROUP' ) ) THEN
               CALL MSG_SETC( 'SOR', 'Local group' )

            ELSE IF(  CHR_SIMLR( SOR, 'SOURCE' ) ) THEN
               CALL MSG_SETC( 'SOR', 'Source' )

            ELSE
               CALL MSG_SETC( 'SOR', SOR )
            END IF

            CALL MSG_OUT( 'WCS_SOR',
     :                    IND( : NIND )//'Standard of rest    : ^SOR',
     :                    STATUS )

*  Display source velocity if it is set, or if StdOfRest == source.
            IF( SOR .EQ. 'SOURCE' .OR.
     :          AST_TEST( FRM, 'SourceVel', STATUS ) ) THEN

*  If SourceSys is not set, temporarily set it to the same value as the
*  main SpecFrame, so long as it is a System supported by SourceSys.
               SSSET = AST_TEST( FRM, 'SourceSys', STATUS )
               IF( .NOT. SSSET .AND. ( SYS .EQ. 'VRAD' .OR.
     :                                 SYS .EQ. 'VOPT' .OR.
     :                                 SYS .EQ. 'ZOPT' .OR.
     :                                 SYS .EQ. 'BETA' .OR.
     :                                 SYS .EQ. 'VELO' ) ) THEN
                  CALL AST_SETC( FRM, 'SourceSys', SYS, STATUS )
               END IF

*  See what spectral system the SourceVel value is stored in.
               SSYS = AST_GETC( FRM, 'SourceSys', STATUS )

*  Get the source velocity in the above system.
               SRCVEL = AST_GETD( FRM, 'SourceVel', STATUS )

*  Set suitable label, title and unit strings.
               IF( SSYS .EQ. 'VRAD' ) THEN
                  CALL MSG_SETC( 'LABEL', 'Radio velocity' )
                  CALL MSG_SETC( 'TTL', 'velocity' )
                  CALL MSG_SETC( 'UNIT', 'km/s' )

               ELSE IF( SSYS .EQ. 'VOPT' ) THEN
                  CALL MSG_SETC( 'LABEL', 'Optical velocity' )
                  CALL MSG_SETC( 'TTL', 'velocity' )
                  CALL MSG_SETC( 'UNIT', 'km/s' )

               ELSE IF( SSYS .EQ. 'ZOPT' ) THEN
                  CALL MSG_SETC( 'LABEL', 'Redshift' )
                  CALL MSG_SETC( 'TTL', 'redshift' )
                  CALL MSG_SETC( 'UNIT', ' ' )

               ELSE IF( SSYS .EQ. 'BETA' ) THEN
                  CALL MSG_SETC( 'LABEL', 'Beta factor' )
                  CALL MSG_SETC( 'TTL', 'velocity' )
                  CALL MSG_SETC( 'UNIT', ' ' )

               ELSE IF( SSYS .EQ. 'VELO' ) THEN
                  CALL MSG_SETC( 'LABEL', 'Apparent radial velocity' )
                  CALL MSG_SETC( 'TTL', 'velocity' )
                  CALL MSG_SETC( 'UNIT', 'km/s' )

               ELSE
                  CALL MSG_SETC( 'LABEL', SSYS )
                  CALL MSG_SETC( 'UNIT', ' ' )
                  CALL MSG_SETC( 'TTL', 'velocity' )
               END IF

*  Set a suitable standard of rest string.
               SOR = AST_GETC( FRM, 'SOURCEVRF', STATUS )
               IF( CHR_SIMLR( SOR, 'NONE' ) ) THEN
                  CALL MSG_SETC( 'SOR', '<not defined>' )

               ELSE IF( CHR_SIMLR( SOR, 'LSRK' ) ) THEN
                  CALL MSG_SETC( 'SOR', 'Kinematical Local '//
     :                                       'Standard of Rest' )

               ELSE IF( CHR_SIMLR( SOR, 'LSRD' ) ) THEN
                  CALL MSG_SETC( 'SOR', 'Dynamical Local '//
     :                                       'Standard of Rest' )

               ELSE IF(  CHR_SIMLR( SOR, 'LOCAL_GROUP' ) ) THEN
                  CALL MSG_SETC( 'SOR', 'Local group' )

               ELSE
                  CALL MSG_SETC( 'SOR', SOR )
               END IF

*  Display the value
               CALL MSG_SETR( 'V', REAL( SRCVEL ) )

               CALL MSG_OUT( 'WCS_VELSOR',
     :            IND( : NIND )//'Source ^TTL     : ^V ^UNIT '//
     :            '(^SOR ^LABEL)', STATUS )
            END IF

*  If SourceSys was not set originally, clear it now.
            IF( .NOT. SSSET ) THEN
               CALL AST_CLEAR( FRM, 'SourceSys', STATUS )
            END IF

* Reference position...
            IF( AST_TEST( FRM, 'RefRA', STATUS ) ) THEN
               POSBUF = ' '
               IAT = 0
               CALL CHR_APPND( AST_GETC( FRM, 'RefRA', STATUS ), POSBUF,
     :                         IAT )
               CALL CHR_APPND( ',', POSBUF, IAT )
               IAT = IAT + 1
               CALL CHR_APPND( AST_GETC( FRM, 'RefDEC',
     :                                  STATUS ), POSBUF, IAT )
               IAT = IAT + 1
               CALL CHR_APPND( '(FK5 J2000)', POSBUF, IAT )
               CALL MSG_SETC( 'REF', POSBUF( : IAT ) )

            ELSE
               CALL MSG_SETC( 'REF', '<not defined>' )
            END IF
            CALL MSG_OUT( 'WCS_REF',
     :                    IND( : NIND )//'Reference (RA,Dec)  : ^REF',
     :                    STATUS )

*  Now display stuff specific to the DSBSpecFrame sub-class of SpecFrame.
            IF( AST_ISADSBSPECFRAME( FRM, STATUS ) ) THEN

*  Current sideband
               SIDEBN = AST_GETC( FRM, 'SIDEBAND', STATUS )
               IF( SIDEBN .EQ. 'USB') THEN
                  CALL MSG_SETC( 'SB',  'Upper' )
               ELSE IF( SIDEBN .EQ. 'LSB') THEN
                  CALL MSG_SETC( 'SB',  'Lower' )
               ELSE  IF ( SIDEBN .EQ. 'LO' ) THEN
                  CALL MSG_SETC( 'SB',  'Offset from local oscillator' )
               ELSE
*  Defensive
                  CALL MSG_SETC( 'SB', SIDEBN )
               END IF

               CALL MSG_OUT( 'WCS_SBND',
     :                IND( : NIND )//'Sideband            : ^SB',
     :                STATUS )

*  Intermediate Frequency...
               IFF = AST_GETD( FRM, 'IF', STATUS )
               CALL MSG_SETD( 'IF', IFF )
               CALL MSG_SETC( 'IF', ' GHz' )
               CALL MSG_OUT( 'WCS_IF',
     :                IND( : NIND )//'Intermediate frequency : ^IF',
     :                STATUS )

*  Observation centre...
               CALL MSG_SETD( 'CV', AST_GETD( FRM, 'DSBCentre',
     :                                        STATUS ) )
               CALL MSG_SETC( 'CU', AST_GETC( FRM, 'Unit(1)', STATUS ) )
               IF( IFF .LT. 0.0 ) THEN
                  CALL MSG_SETC( 'CU', ' (USB)' )
               ELSE
                  CALL MSG_SETC( 'CU', ' (LSB)' )
               END IF

               CALL MSG_OUT( 'WCS_CEN',
     :                IND( : NIND )//'Observation centre  : ^CV ^CU',
     :                STATUS )

            END IF

*  If the Frame is a TimeFrame, display its attributes
         ELSE IF ( AST_ISATIMEFRAME( FRM, STATUS ) ) THEN

*  Time System
            SYS = AST_GETC( FRM, 'SYSTEM', STATUS )
            IF ( SYS .EQ. 'MJD' ) THEN
               LABEL = 'Modified Julian Date'
            ELSE IF ( SYS .EQ. 'JD' ) THEN
               LABEL = 'Julian Date'
            ELSE IF ( SYS .EQ. 'JEPOCH' ) THEN
               LABEL = 'Julian Epoch'
            ELSE IF ( SYS .EQ. 'BEPOCH' ) THEN
               LABEL = 'Besselian Epoch'
            ELSE
               LABEL = SYS
            END IF

            CALL MSG_SETC( 'SYS', LABEL )
            CALL MSG_SETC( 'SYS', ' (' )
            CALL MSG_SETC( 'SYS', AST_GETC( FRM, 'UNIT(1)', STATUS ) )
            CALL MSG_SETC( 'SYS', ')' )
            CALL MSG_OUT( 'WCS_SYS',
     :                 IND( : NIND )//'System              : ^SYS',
     :                 STATUS )

*  Time Scale
            TSC = AST_GETC( FRM, 'TIMESCALE', STATUS )
            IF ( TSC .EQ. 'TAI' ) THEN
               LABEL = 'International Atomic Time (TAI)'
            ELSE IF ( TSC .EQ. 'UTC' ) THEN
               LABEL = 'Coordinated Universal Time (UTC)'
            ELSE IF ( TSC .EQ. 'UT1' ) THEN
               LABEL = 'Universal Time (UT1)'
            ELSE IF ( TSC .EQ. 'GMST' ) THEN
               LABEL = 'Greenwich Mean Sidereal Time (GMST)'
            ELSE IF ( TSC .EQ. 'LAST' ) THEN
               LABEL = 'Local Apparent Sidereal Time (LAST)'
            ELSE IF ( TSC .EQ. 'LMST' ) THEN
               LABEL = 'Local Mean Sidereal Time (LMST)'
            ELSE IF ( TSC .EQ. 'TT' ) THEN
               LABEL = 'Terrestrial Time (TT)'
            ELSE IF ( TSC .EQ. 'TDB' ) THEN
               LABEL = 'Barycentric Dynamical Time (TDB)'
            ELSE IF ( TSC .EQ. 'TCB' ) THEN
               LABEL = 'Barycentric Coordinate Time (TCB)'
            ELSE IF ( TSC .EQ. 'TCG' ) THEN
               LABEL = 'Geocentric Coordinate Time (TCG)'
            ELSE
               LABEL = TSC
            END IF

            CALL MSG_SETC( 'TSC', LABEL )
            CALL MSG_OUT( 'WCS_TSC',
     :                 IND( : NIND )//'Time Scale          : ^TSC',
     :                 STATUS )

*  TimeOrigin
            IF( AST_TEST( FRM, 'TimeOrigin', STATUS ) ) THEN
               TIMEOR = AST_GETD( FRM, 'TIMEORIGIN', STATUS )

               FRM2 = AST_COPY( FRM, STATUS )
               CALL AST_CLEAR( FRM2, 'TIMEORIGIN', STATUS )
               CALL AST_SETI( FRM2, 'Digits', VAL__SZD, STATUS )
               CALL MSG_SETC( 'TIMEOR',
     :              AST_FORMAT( FRM2, 1, TIMEOR, STATUS ) )
            ELSE
               CALL MSG_SETC( 'TIMEOR', '<not defined>' )
            END IF

            CALL MSG_OUT( 'WCS_TIMEOR',
     :                 IND( : NIND )//'Time Origin         : ^TIMEOR',
     :                 STATUS )

         END IF

*  Display the epoch for all Frames (as a Julian or Besselian epoch followed
*  by a Gregorian date). For SkyFrames and SpecFrames, always display the
*  epoch. For other Frames, only display it if set.
         IF( .NOT. SHOWEP ) SHOWEP = AST_TEST( FRM, 'EPOCH', STATUS )

         IF( SHOWEP ) THEN
            EP = AST_GETD( FRM, 'EPOCH', STATUS )
            IF( EP .LT. 1984.0 ) THEN
               CALL MSG_SETC( 'EPOCH', 'B' )
               MJD = SLA_EPB2D( EP )
            ELSE
               CALL MSG_SETC( 'EPOCH', 'J' )
               MJD = SLA_EPJ2D( EP )
            END IF

            CALL MSG_SETD( 'EPOCH', EP )

            CALL SLA_DJCL( MJD, IY, IM, ID, FD, J )
            CALL SLA_CD2TF( 0, REAL( FD ), SIGN, IHMSF )

            CALL MSG_SETI( 'DATE', ID )
            CALL MSG_SETC( 'DATE', '-' )
            CALL MSG_SETC( 'DATE', MONTH( IM ) )
            CALL MSG_SETC( 'DATE', '-' )
            CALL MSG_SETI( 'DATE', IY )
            CALL MSG_SETI( 'TIME', IHMSF( 1 ) )
            CALL MSG_SETC( 'TIME', ':' )
            CALL MSG_SETI( 'TIME', IHMSF( 2 ) )
            CALL MSG_SETC( 'TIME', ':' )
            CALL MSG_SETI( 'TIME', IHMSF( 3 ) )

            CALL MSG_OUT( 'WCS_EPOCH',
     :                 IND( : NIND )//'Epoch of observation: '//
     :                 '^EPOCH (^DATE ^TIME) TDB', STATUS )
         END IF

* Observers position...
         IF( .NOT. SHOWOB ) SHOWOB = AST_TEST( FRM, 'OBSLON', STATUS )
         IF( SHOWOB ) THEN

            IF( AST_TEST( FRM, 'OBSLON', STATUS ) ) THEN
               POSBUF = ' '
               IAT = 0
               CALL CHR_APPND( AST_GETC( FRM, 'ObsLon', STATUS ),
     :                          POSBUF, IAT )
               CALL CHR_APPND( ',', POSBUF, IAT )
               IAT = IAT + 1
               CALL CHR_APPND( AST_GETC( FRM, 'ObsLat',
     :                                  STATUS ), POSBUF, IAT )
               IAT = IAT + 1
               CALL MSG_SETC( 'OBS', POSBUF( : IAT ) )

            ELSE
               CALL MSG_SETC( 'OBS', '<not defined>' )
            END IF

            CALL MSG_OUT( 'WCS_REF',
     :                    IND( : NIND )//'Observer (Lon,Lat)  : ^OBS',
     :                    STATUS )

         END IF
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
