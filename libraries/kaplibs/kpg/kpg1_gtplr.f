      SUBROUTINE KPG1_GTPLR( PARAM, IWCS, NULL, POLE, PAORIG, CC, BC,
     :                       STATUS )
*+
*  Name:
*     KPG1_GTPLR

*  Purpose:
*     Gets a spatial position from the environment as polar co-ordinates

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GTPLR( PARAM, IWCS, NULL, POLE, PAORIG, CC, BC, STATUS )

*  Description:
*     This routine obtains a two-dimensional spatial position from the
*     environment, using a specified parameter.  The user supplies the
*     position in polar co-ordinates about a supplied centre within the
*     co-ordinate system of the Current Frame in the supplied WCS
*     FrameSet.  This FrameSet must have two axes, otherwise the routine
*     will exit with an error.

*     To be acceptable, the supplied position must correspond to a valid
*     position (on both axes) in the Base Frame of the supplied FrameSet.
*     If a Frame is supplied instead of a FrameSet this restriction is
*     not imposed, however polar co-ordinates cannot be supplied,
*     only regular co-ordinates along both axes.
*
*     If the polar position supplied in argument CC on entry is valid
*     (i.e. does not contain any AST__BAD values), then it is used as a
*     dynamic default for the parameter.  Otherwise, no dynamic default
*     is used.
*
*     The parameter is accessed as a single literal string containing a
*     space- or comma-separated list of radius and position-angle
*     values.  For SkyFrames the position angle is measured in degrees
*     from North via East; for other Frames, it is anticlockwise from
*     the origin defined by argument PAORIG.  The allowed formats for
*     the co-ordinates depends on the class of the Current Frame in the
*     supplied FrameSet, and are described in SUN/210.
*
*     If the string supplied for the parameter consists of a single
*     colon, then a description of the Current co-ordinate Frame is
*     displayed, together with an indication of the format required for
*     each axis value, and a new parameter value is then obtained.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     IWCS = INTEGER (Given)
*        A pointer to an AST FrameSet.  If a pointer to a Frame is
*        supplied instead the routine will issue a warning that it is
*        unable to handle polar co-ordinates and will expect spatial
*        positions like routine KPG1_GTPOS.  This can be aborted (!!) at
*        the parameter prompt.
*     NULL = LOGICAL (Given)
*        If TRUE, a null (!) parameter value will result in the dynamic
*        default value being used. If FALSE (or if there is no dynamic
*        default), a null parameter value will result in a PAR__NULL
*        error status.
*     PAORIG = DOUBLE PRECISION (Given)
*        This applies when the current Frame is not a SkyFrame.  It
*        specifies the origin for the position angle in degrees from
*        the the first WCS axis.  The normal convention is for this to
*        be zero (i.e. from X in a Cartesian co-ordinate system) but
*        another may be 90 for starting from up or Y.
*     POLE( 2 )  = DOUBLE PRECISION (Given)
*        The position of the pole of the polar co-ordinates measured
*        in the current co-ordinate Frame along each axis.  If any of
*        the co-ordinates are bad, the routine will issue a warning that
*        it is unable to handle polar co-ordinates and will expect two
*        spatial positions like routine KPG1_GTPOS.  This can be aborted
*        (!!) at the parameter prompt.
*     CC( 2 ) = DOUBLE PRECISION (Given and Returned)
*        On entry, holds the position to use as the dynamic default for
*        the parameter, in the Current Frame of the supplied FrameSet
*        (or Frame).  On exit, it holds the supplied position in the
*        Current Frame.  There should be one element for both axes.
*     BC( 2 ) = DOUBLE PRECISION (Returned)
*        Returned holding the Base Frame position corresponding to the
*        supplied Current Frame position.  If a Frame is supplied for
*        IWCS instead of a FrameSet, then BC will not be accessed.  The
*        returned values will be good on both axes.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  CC and BC are left unchanged if an error has already occurred.
*     -  Values of AST__BAD are returned in CC (and optionally BC) if an
*     error occurs during this routine.

*  Copyright:
*     Copyright (C) 1998, 1999, 2000 Central Laboratory of the Research
*     Councils
*     Copyright (c) 2007, 2011 Science and Technology Facilities Council
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 June 1 (MJC):
*        Original version based upon DSB's KPG1_GTPOS.
*     2007 June 12 (MJC):
*        Fixed incomplete coding for SkyFrame-to-polar mapping, in
*        particular transform radius to 90-latitude and ensure longitude
*        is measured in degrees North via East.
*     2011 May 10 (MJC):
*        Set mandatory bad status before calling ERR_REP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'AST_ERR'          ! AST error constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      CHARACTER*(*) PARAM
      INTEGER IWCS
      LOGICAL NULL
      DOUBLE PRECISION POLE( 2 )
      DOUBLE PRECISION PAORIG

*  Arguments Given and Returned:
      DOUBLE PRECISION CC( 2 )

*  Arguments Returned:
      DOUBLE PRECISION BC( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER NAX
      PARAMETER ( NAX = 2 )      ! Number WCS axes

      DOUBLE PRECISION PI
      PARAMETER ( PI = 3.1415926535898 )

      DOUBLE PRECISION HALFPI
      PARAMETER ( HALFPI = 0.5D0 * PI )

      DOUBLE PRECISION TWOPI
      PARAMETER ( TWOPI = 2.0D0 * PI )

*  Local Variables:
      CHARACTER*12 ATT           ! AST attribute name
      INTEGER BASFRM             ! Pointer to the Base Frame
      INTEGER BPMAP              ! Base-Polar mapping
      CHARACTER*125 BUFFER       ! Buffer for swapping expreessions
      INTEGER COPWCS             ! Deep copy of current FrameSet
      INTEGER CPMAP              ! Cartesian-Polar mapping
      INTEGER CURFRM             ! Pointer to the Current Frame
      CHARACTER*30 DOM           ! Domain of current frame
      CHARACTER*255 DPOS         ! Default position string
      INTEGER F                  ! Index of first non-blank character
      INTEGER FIAT               ! No. of characters in string FMT
      CHARACTER*100 FMT          ! List of axis format strings
      CHARACTER*125 FOREXP( NAX ) ! Forward expressions for
                                 ! MathMap
      LOGICAL GOOD               ! Is position good?
      LOGICAL GOODPO             ! Is pole position good?
      LOGICAL GOTFS              ! Was a FrameSet supplied?
      INTEGER I                  ! Axis index
      INTEGER IAT                ! No. of characters in a string
      INTEGER IBASIS             ! Index of basis Frame
      INTEGER ICURR              ! Index of the original Current Frame
      INTEGER IEND               ! End of field passed to AST_UNFORMAT
      CHARACTER*125 INVEXP( NAX ) ! Inverse expressions for
                                 ! MathMap
      INTEGER IPOS               ! Index of next character to be read
      LOGICAL ISSKY              ! Is the current Frame a SkyFrame?
      INTEGER L                  ! Index of last non-blank character
      CHARACTER*30 LAB( NAX )    ! Axis labels
      INTEGER LAT                ! Index to latitude axis in SkyFrame
      INTEGER LON                ! Index to longitude axis in SkyFrame
      LOGICAL LOOP               ! Get a new parameter value?
      INTEGER MAP                ! Simplified FrameSet Mapping
      INTEGER NBAXES             ! No. of axes in base Frame
      INTEGER NC                 ! No. of characters read from string
      INTEGER NCAXES             ! No. of axes in current Frame
      CHARACTER*1 NEXT           ! Next character to be read
      DOUBLE PRECISION PC( 2 )   ! Polar co-ordinates
      INTEGER POLFRM             ! Pointer to new polar Frame
      CHARACTER*255 POS          ! Position string
      LOGICAL SWITCH             ! Switch supplied polar co-ordinates?
      CHARACTER*10 SYM( NAX )    ! Axis symbols
      INTEGER UNITAX             ! Axis to be used for radius units
      CHARACTER*50 UNITS         ! Axis units

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access required frames and attributes, and mapping.
*  ===================================================

*  See if a FrameSet has been supplied.
      GOTFS = AST_ISAFRAMESET( IWCS, STATUS )

*  If so, get a pointer to the Current and Base Frames, and get the
*  number of axes in the Base Frame.  Also get a simplified Mapping for
*  the FrameSet.
      IF ( GOTFS ) THEN
         CURFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
         BASFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )
         IBASIS = AST_GETI( IWCS, 'CURRENT', STATUS )
         ICURR = AST_GETI( IWCS, 'Current', STATUS )
         NBAXES = AST_GETI( BASFRM, 'NAXES', STATUS )
         CALL AST_ANNUL( BASFRM, STATUS )

         MAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, STATUS )
         MAP = AST_SIMPLIFY( MAP, STATUS )

*  If a Frame was supplied, use a clone of the Frame as the "Current
*  Frame".
      ELSE
         CURFRM = AST_CLONE( IWCS, STATUS )
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL MSG_OUT( 'KPG1_GTPLR_M1', 'FrameSet not supplied. '//
     :                 'Parameter %^PARAM will interpret '//
     :                 'co-ordinates as regular axis values.  '//
     :                 '(Programming error)', STATUS )
      END IF

*  Get the number of axes in the Current Frame.  This routine demands
*  two axes.
      NCAXES = AST_GETI( CURFRM, 'NAXES', STATUS )

      IF ( NCAXES .NE. NAX ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTPLR_0', 'Current Frame does not '//
     :                 'have two axes.  Parameter %^PARAM will '//
     :                 'interpret co-ordinates as regular axis '//
     :                 'values.  (Programming error)', STATUS )
         GO TO 999
      END IF
      GOODPO = .TRUE.

*  Note the Domain of the Current Frame for use in messages.
      DOM = AST_GETC( CURFRM, 'DOMAIN', STATUS )

*  Is the current frame a SkyFrame?
      ISSKY = AST_ISASKYFRAME( CURFRM, STATUS )

*  Check and set co-ordinates of the supplied pole.
*  ================================================
      COPWCS = IWCS
      IF ( GOTFS .AND. GOODPO ) THEN

*  As we do not want to affect the supplied FrameSet's use beyond this
*  routine, we make a deep copy and modify that.
         COPWCS = AST_COPY( IWCS, STATUS )

*  See if a good position has been supplied in POLE.  At the same time,
*  set the pole position for a SkyFrame.
         GOODPO = .TRUE.
         DO I = 1, NCAXES
            IF ( POLE( I ) .EQ. AST__BAD .OR.
     :           POLE( I ) .EQ. VAL__BADD ) GOODPO = .FALSE.
            IF ( ISSKY .AND. GOODPO ) THEN
               ATT = 'SkyRef('
               IAT = 7
               CALL CHR_PUTI( I, ATT, IAT )
               CALL CHR_APPND( ')', ATT, IAT )
               CALL AST_SETD( COPWCS, ATT( : IAT ), POLE( I ), STATUS )
            END IF
         END DO

         IF ( .NOT. GOODPO ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'PARAM', PARAM )
            CALL ERR_REP( 'KPG1_GTPLR_M3', 'A co-ordinate of the '//
     :                    'pole is undefined.  Parameter %^PARAM '//
     :                    'wil interpret co-ordinates as regular '//
     :                    'axis values.  (Programming error)', STATUS )

*  Define mappings from Sky to user-supplied co-ordinates.
*  =======================================================
         ELSE
            IF ( ISSKY ) THEN

*  Switch the North pole to its new co-ordinates.
               CALL AST_SETC( COPWCS, 'SkyRefIs' , 'Pole', STATUS )

*  Determine which is the latitude and longitude axes.
               LAT = AST_GETI( CURFRM, 'LatAxis', STATUS )
               LON = AST_GETI( CURFRM, 'LonAxis', STATUS )
               SWITCH = LAT .GT. LON

*  Units will come from the latitude axis.
               UNITAX = LAT

*  Create the mappings.  We need the first co-ordinate to be radius
*  and the second to be a position angle.  Not sure if it's a bug in
*  this routine or in AST that generates angles 360-PA instead of PA
*  when the pole is rotated.
               FOREXP( 1 ) = 'R = '
               IAT = 4
               CALL CHR_PUTD( HALFPI, FOREXP( 1 ), IAT )
               CALL CHR_APPND( ' - LAT', FOREXP( 1 ), IAT )

               FOREXP( 2 ) = 'THETA = '
               IAT = 8
               CALL CHR_PUTD( TWOPI, FOREXP( 2 ), IAT )
               CALL CHR_APPND( ' - LON', FOREXP( 2 ), IAT )

               INVEXP( 1 ) = 'LAT = '
               IAT = 6
               CALL CHR_PUTD( HALFPI, INVEXP( 1 ), IAT )
               CALL CHR_APPND( ' - R', INVEXP( 1 ), IAT )

               INVEXP( 2 ) = 'LON = '
               IAT = 6
               CALL CHR_PUTD( TWOPI, INVEXP( 2 ), IAT )
               CALL CHR_APPND( ' - THETA', INVEXP( 2 ), IAT )

               IF ( SWITCH ) THEN
                  BUFFER = INVEXP( 1 )
                  INVEXP( 1 ) = INVEXP( 2 )
                  INVEXP( 2 ) = BUFFER
               END IF

*  Define mappings from 'Cartesian' to polar.
*  ===========================================
            ELSE

*  Units will come from the first axis by convention.
               UNITAX = 1

*  Create a MathMap of the conversion to polar with a pole at
*  supplied co-ordinates.  Note that offsets are in parentheses
*  within the expressions to avoid successive operators when an
*  offset is negaive.

*  Forward mapping
*  ---------------
               IAT = 0
               FOREXP( 1 ) = ' '
               CALL CHR_APPND( 'R = SQRT( ( X - (', FOREXP( 1 ), IAT )
               CALL CHR_PUTD( POLE( 1 ), FOREXP( 1 ), IAT )
               CALL CHR_APPND( ') ) * ( X - (', FOREXP( 1 ), IAT )
               CALL CHR_PUTD( POLE( 1 ), FOREXP( 1 ), IAT )
               CALL CHR_APPND( ') ) + ( Y - (', FOREXP( 1 ), IAT )
               CALL CHR_PUTD( POLE( 2 ), FOREXP( 1 ), IAT )
               CALL CHR_APPND( ') ) * ( Y - (', FOREXP( 1 ), IAT )
               CALL CHR_PUTD( POLE( 2 ), FOREXP( 1 ), IAT )
               CALL CHR_APPND( ') ) )', FOREXP( 1 ), IAT )

               IAT = 0
               FOREXP( 2 ) = ' '
               CALL CHR_APPND( 'THETA = MOD( ATAN2D( Y - (',
     :                         FOREXP( 2 ), IAT )
               CALL CHR_PUTD( POLE( 2 ), FOREXP( 2 ), IAT )
               CALL CHR_APPND( '),  X - (', FOREXP( 2 ), IAT )
               CALL CHR_PUTD( POLE( 1 ), FOREXP( 2 ), IAT )
               CALL CHR_APPND( ') ) - (', FOREXP( 2 ), IAT )
               CALL CHR_PUTD( PAORIG, FOREXP( 2 ), IAT )
               CALL CHR_APPND( '), 360.0D0 )', FOREXP( 2 ), IAT )

*  Inverse mapping
*  ---------------
               IAT = 0
               INVEXP( 1 ) = ' '
               CALL CHR_APPND( 'X = R * COSD( THETA + (', INVEXP( 1 ),
     :                         IAT )
               CALL CHR_PUTD( PAORIG, INVEXP( 1 ), IAT )
               CALL CHR_APPND( ') ) + (', INVEXP( 1 ), IAT )
               CALL CHR_PUTD( POLE( 1 ), INVEXP( 1 ), IAT )
               CALL CHR_APPND( ')', INVEXP( 1 ), IAT )

               IAT = 0
               INVEXP( 2 ) = ' '
               CALL CHR_APPND( 'Y = R * SIND( THETA + (',
     :                         INVEXP( 2 ), IAT )
               CALL CHR_PUTD( PAORIG, INVEXP( 2 ), IAT )
               CALL CHR_APPND( ') ) + (', INVEXP( 2 ), IAT )
               CALL CHR_PUTD( POLE( 2 ), INVEXP( 2 ), IAT )
               CALL CHR_APPND( ')', INVEXP( 2 ), IAT )
            END IF

*  Create new Frame in user-supplied polar co-ordinates.
*  =====================================================

*  For a transformed SkyFrame, the expected co-ordinates are still as
*  before, Lat,Long.  So a radius from the pole will be latitude
*  90-radius, and the angle must be in longitude hours.  For most cases
*  this means a flip of the axis order.

*  Create and simplify a MathMap for either new Frame.
            CPMAP = AST_MATHMAP( NAX, NAX, NAX, FOREXP, NAX, INVEXP,
     :                           'SimpFI=1,SimpIF=1', STATUS )
            CPMAP = AST_SIMPLIFY( CPMAP, STATUS )

*  Add the newly created polar Frame to the FrameSet.
            POLFRM = AST_COPY( CURFRM, STATUS )
            CALL AST_ADDFRAME( COPWCS, ICURR, CPMAP, POLFRM, STATUS )

*  Describe the new Frame in case the user enters ":" at the prompt.
            CALL AST_SETC( POLFRM, 'TITLE', 'Polar co-ordinates',
     :                     STATUS )
            CALL AST_SETC( POLFRM, 'LABEL(1)', 'Radius offset', STATUS )
            IF ( ISSKY ) THEN
               CALL AST_SETC( POLFRM, 'LABEL(2)',
     :                        'Position angle (North through East)',
     :                        STATUS )
            ELSE
               CALL AST_SETC( POLFRM, 'LABEL(2)',
     :                        'Position angle (from Y anticlockwise)',
     :                        STATUS )
            END IF

            ATT = 'Unit('
            IAT = 5
            CALL CHR_PUTI( UNITAX, ATT, IAT )
            CALL CHR_APPND( ')', ATT, IAT )
            UNITS = AST_GETC( POLFRM, ATT( : IAT ), STATUS )

            CALL AST_SETC( POLFRM, 'FORMAT(1)', 'dms', STATUS )
            CALL AST_SETC( POLFRM, 'FORMAT(2)', 'd', STATUS )

            CALL AST_SETC( POLFRM, 'UNIT(1)', UNITS, STATUS )
            CALL AST_SETC( POLFRM, 'UNIT(2)', 'deg', STATUS )

            IF ( ISSKY ) CALL AST_SET( POLFRM, 'AsTime(2)=0', STATUS )

*  Now we're make our new Frame current.
            CURFRM = POLFRM

*  Form a compound mapping between the Base Frame and the newly created
*  polar Frame.
            BPMAP = AST_GETMAPPING( COPWCS, AST__BASE, AST__CURRENT,
     :                              STATUS )
            BPMAP = AST_SIMPLIFY( BPMAP, STATUS )

*  Otherwise, create a default Frame of POLAR class.
         END IF
      END IF

*  Validate the dynamic default.
*  =============================

*  See if a good position has been supplied in CC.  At the same time,
*  save the axis labels and symbols for use in messages.  Also, form a
*  comma-separated list of the format strings for each axis.
      GOOD = .TRUE.

      FMT = ' '
      FIAT = 0

      DO I = 1, NCAXES
         IF ( CC( I ) .EQ. AST__BAD ) GOOD = .FALSE.

         ATT = 'LABEL('
         IAT = 6
         CALL CHR_PUTI( I, ATT, IAT )
         CALL CHR_APPND( ')', ATT, IAT )
         LAB( I ) = AST_GETC( CURFRM, ATT( : IAT ), STATUS )
         CALL CHR_LCASE( LAB( I ) )
         CALL KPG1_PGESC( LAB( I ), STATUS )

         ATT = 'SYMBOL('
         IAT = 7
         CALL CHR_PUTI( I, ATT, IAT )
         CALL CHR_APPND( ')', ATT, IAT )
         SYM( I ) = AST_GETC( CURFRM, ATT( : IAT ), STATUS )
         CALL KPG1_PGESC( SYM( I ), STATUS )

         IF ( I .GT. 1 ) CALL CHR_APPND( ',', FMT, FIAT )

         ATT = 'FORMAT('
         IAT = 7
         CALL CHR_PUTI( I, ATT, IAT )
         CALL CHR_APPND( ')', ATT, IAT )
         CALL CHR_APPND( AST_GETC( CURFRM, ATT( : IAT ), STATUS ), FMT,
     :                   FIAT )

      END DO

*  If a good position has been supplied in CC we use it as a dynamic
*  default for the parameter.
      IF ( GOOD ) THEN

*  Normalize it.
         CALL AST_NORM( CURFRM, CC, STATUS )

*  Construct a string holding the default position (a comma-separated
*  list of formatted axis values).
         DPOS = AST_FORMAT( CURFRM, 1, CC( 1 ), STATUS )
         IAT = CHR_LEN( DPOS )
         DO I = 2, NCAXES
            IAT = IAT + 1
            CALL CHR_APPND( AST_FORMAT( CURFRM, I, CC( I ), STATUS ),
     :                      DPOS, IAT )
         END DO

*  Use this string as the dynamic default for the parameter.
         CALL PAR_DEF0C( PARAM, DPOS, STATUS )

*  Clear the Digits value.
         CALL AST_CLEAR( CURFRM, 'DIGITS', STATUS )

*  Otherwise, use PAR_UNSET to ensure any previous dynamic default is
*  cancelled.
      ELSE
         CALL PAR_UNSET( PARAM, 'DEFAULT', STATUS )
         DPOS = ' '
      END IF

*  Obtain and validate user-supplied polar co-ordinates.
*  =====================================================

*  Loop until a valid position has been obtained from the user, or an
*  error occurs.
      LOOP = .TRUE.
      DO WHILE( LOOP .AND. STATUS .EQ. SAI__OK )

*  Get a value for the parameter.
         CALL PAR_GET0C( PARAM, POS, STATUS )

*  If a null parameter value has been given, annul the error and return
*  the dynamic default if there is a dynamic default, and if NULL is
*  TRUE.
         IF ( STATUS .EQ. PAR__NULL .AND. GOOD .AND. NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            POS = DPOS
         END IF

*  Get the indices of the first and last non-blank characters.
         CALL CHR_FANDL( POS, F, L )

*  If the string is blank, report an error.
         IF ( F .GT. L .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'PAR', PARAM )
            CALL ERR_REP( 'KPG1_GTPLR_1', 'Blank value supplied for '//
     :                    'parameter %^PAR.', STATUS )

*  Otherwise, if the supplied string is just a colon, display a
*  description of the Current Frame, and the default format.
         ELSE IF ( POS( F : L ) .EQ. ':' .AND.
     :             STATUS .EQ. SAI__OK ) THEN
            CALL KPG1_DSFRM( COPWCS, 'A position is required in the '//
     :                       'following co-ordinate frame:', AST__BAD,
     :                       AST__BAD, .TRUE., STATUS )

            CALL MSG_SETC( 'FMT', FMT )
            CALL MSG_OUT( 'KPG1_GTPLR_M3', '      Suggested format: '//
     :                    '^FMT', STATUS )
            CALL MSG_BLANK( STATUS )

*  If the supplied string is identical to the default string, use the
*  default axis values directly rather than unformatting the default
*  string, since the current Frame Format and Digits values may not
*  give sufficient accuracy to unformat the strings accurately.
         ELSE IF ( DPOS( F : L ) .EQ. POS( F : L ) .AND.
     :             STATUS .EQ. SAI__OK ) THEN

*  Assume that the supplied value was acceptable.
            LOOP = .FALSE.
            GOOD = .TRUE.

*  If we have a FrameSet, get the base Frame position corresponding to
*  the default position.
            IF ( GOTFS ) THEN
               CALL AST_TRANN( MAP, 1, NCAXES, 1, CC, .FALSE., NBAXES,
     :                         1, BC, STATUS )

*  See if this gave a good Base Frame position.
               DO I = 1, NBAXES
                  IF ( BC( I ) .EQ. AST__BAD ) GOOD = .FALSE.
               END DO

*  If not, report an error.
               IF ( .NOT. GOOD .AND. STATUS .EQ. SAI__OK ) THEN
                  LOOP = .TRUE.
                  STATUS = SAI__ERROR

                  CALL MSG_SETC( 'PAR', PARAM )
                  CALL MSG_SETC( 'POS', POS( F: L ) )
                  CALL ERR_REP( 'KPG1_GTPLR_7A', 'The default '//
     :                          'position was accepted for parameter '//
     :                          '%^PAR (''^POS'') but cannot be used!',
     :                          STATUS )
               END IF

            END IF

*  Otherwise, attempt to read a position from the supplied string.
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Replace all commas with spaces.
            DO I = F, L
               IF ( POS( I : I ) .EQ. ',' ) POS( I : I ) = ' '
            END DO

*  Assume that the supplied value was acceptable.
            LOOP = .FALSE.

*  Loop round, attempting to read valued for each axis, until all axes
*  are done, or an error occurs.
            IPOS = F
            I = 0
            DO WHILE( I .LT. NCAXES .AND. STATUS .EQ. SAI__OK )

*  Find the start of the next but one axis value (if there is more than
*  one axis value remaining in the string).
               IEND = IPOS
               CALL CHR_TOCHR( ' ', POS, .TRUE., IEND )
               IF ( IEND .LT. L ) CALL CHR_SKCHR( ' ', POS, .TRUE.,
     :                                            IEND )

*  Set the index of the last character to be read by AST_UNFORMAT.  This
*  is done since AST_UNFORMAT may allow spaces within axis values, but
*  we require spaces to be used as axis delimiters.
               IEND = IEND - 1

*  Read the value for the next axis.  NC is the number of characters
*  read by AST_UNFORMAT including trailing spaces.
               I = I + 1
               NC = AST_UNFORMAT( CURFRM, I, POS( IPOS:IEND ),
     :                            CC( I ), STATUS )

*  Get the last character read by AST_UNFORMAT.  If there are no
*  characters left, pretend the last character read was a space (i.e.
*  an axis delimiter).
               IPOS = IPOS + NC - 1
               IF ( IPOS .LT. L ) THEN
                  NEXT = POS( IPOS : IPOS )
               ELSE
                  NEXT = ' '
               END IF

*  Increment IPOS to skip the space.  It now points to the first
*  character in the next axis value, or to the first character following
*  the last non-blank character (L) if no axis values are left in the
*  string.
               IPOS = IPOS + 1

*  If the supplied string was invalid, report an error.  This is the
*  case if no characters were read form the string, or if the next
*  character is not a space.
               IF ( ( NC .EQ. 0 .OR. NEXT .NE. ' ' ) .AND.
     :              STATUS .EQ. SAI__OK ) THEN
                  LOOP = .TRUE.
                  STATUS = SAI__ERROR

                  CALL MSG_SETI( 'I', I )
                  CALL MSG_SETC( 'PAR', PARAM )
                  CALL MSG_SETC( 'LAB', LAB( I ) )
                  CALL MSG_SETC( 'POS', POS( F: L ) )

                  IF ( LAB( I )( : 5 ) .NE. 'axis ' ) THEN
                     CALL ERR_REP( 'KPG1_GTPLR_2', 'Failed to get a '//
     :                             'valid ^LAB value for axis ^I '//
     :                             'using parameter %^PAR - ''^POS''.',
     :                             STATUS )
                  ELSE
                     CALL ERR_REP( 'KPG1_GTPLR_3', 'Failed to get a '//
     :                             'valid ^LAB value using parameter '//
     :                             '%^PAR - ''^POS''.', STATUS )
                  END IF

*  If the string has been exhausted before all axis values have been
*  obtained, report an error.
               ELSE IF ( IPOS .GT. L .AND. I .LT. NCAXES .AND.
     :                   STATUS .EQ. SAI__OK ) THEN
                  LOOP = .TRUE.
                  STATUS = SAI__ERROR

                  CALL MSG_SETI( 'I', I + 1 )
                  CALL MSG_SETC( 'PAR', PARAM )
                  CALL MSG_SETC( 'LAB', LAB( I + 1 ) )
                  CALL MSG_SETC( 'POS', POS( F: L ) )

                  IF ( LAB( I + 1 )( : 5 ) .NE. 'axis ' ) THEN
                     CALL ERR_REP( 'KPG1_GTPLR_4', 'No ^LAB value '//
     :                             '(axis ^I) supplied using '//
     :                             'parameter %^PAR - ''^POS''.',
     :                             STATUS )
                  ELSE
                     CALL ERR_REP( 'KPG1_GTPLR_5', 'No ^LAB value '//
     :                             'supplied using parameter %^PAR '//
     :                             '- ''^POS''.', STATUS )
                  END IF

               END IF

            END DO

*  If there were any unused characters at the end of the supplied
*  string, report an error.
            IF ( IPOS .LE. L .AND. STATUS .EQ. SAI__OK ) THEN
               LOOP = .TRUE.
               STATUS = SAI__ERROR

               CALL MSG_SETC( 'PAR', PARAM )
               CALL MSG_SETC( 'TXT', POS( IPOS - 1 : ) )
               CALL MSG_SETC( 'LAB', LAB( NCAXES ) )
               CALL MSG_SETC( 'POS', POS( F: L ) )

               IF ( L + 2 - IPOS .EQ. 1 ) THEN
                  CALL MSG_SETC( 'WORD', 'character' )
               ELSE
                  CALL MSG_SETC( 'WORD', 'characters' )
               END IF

               CALL ERR_REP( 'KPG1_GTPLR_6', 'Extra ^WORD ''^TXT'' '//
     :                       'found at end of ^LAB value supplied for'//
     :                       ' parameter %^PAR - ''^POS''.', STATUS )

            END IF

*  If we have a valid set of Current Frame axis values, check that it
*  corresponds to a valid position in the Base Frame of the supplied
*  FrameSet.  This is not done if a Frame was supplied.
            IF ( GOTFS ) THEN

*  Convert the supplied position into the Base Frame
               CALL AST_TRANN( BPMAP, 1, NCAXES, 1, CC, .FALSE.,
     :                         NBAXES, 1, BC, STATUS )

*  See if this gave a good Base Frame position.
               GOOD = .TRUE.
               DO I = 1, NBAXES
                  IF ( BC( I ) .EQ. AST__BAD ) GOOD = .FALSE.
               END DO

*  If not, report an error.
               IF ( .NOT. GOOD .AND. STATUS .EQ. SAI__OK ) THEN
                  LOOP = .TRUE.
                  STATUS = SAI__ERROR

                  CALL MSG_SETC( 'PAR', PARAM )
                  CALL MSG_SETC( 'POS', POS( F: L ) )
                  CALL ERR_REP( 'KPG1_GTPLR_7', 'The position '//
     :                          'supplied for parameter %^PAR '//
     :                          '(''^POS'') cannot be used.', STATUS )
               END IF

            END IF

         END IF

*  Do not loop if an error occurred within an infrastructure library
*  (except for errors reported by AST_UNFORMAT indicating bad text
*  supplied by the user).
         IF ( STATUS .NE. SAI__OK .AND.
     :        STATUS .NE. SAI__ERROR .AND.
     :        STATUS .NE. AST__UNFER ) LOOP = .FALSE.

*  If a new parameter value is required...
         IF ( LOOP ) THEN

*  Flush any error.
            IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Indicate what sort of position is required.
            CALL MSG_SETC( 'DOM', DOM )
            CALL MSG_SETC( 'PAR', PARAM )
            CALL MSG_OUT( 'KPG1_GTPLR_M4', 'Please supply a new ^DOM '//
     :                    'Domain position for parameter %^PAR.',
     :                    STATUS )

*  Cancel the parameter value.
            CALL PAR_CANCL( PARAM, STATUS )

         END IF

      END DO

*  Convert to polar to original current Frame's co-ordinates.
*  ==========================================================

*  Convert polar co-ordinates to along each WCS axis of the original
*  current Frame.
      IF ( GOTFS .AND. GOODPO ) THEN
         IF ( ISSKY ) THEN

*  For a SkyFrame convert the Base Frame co-ordinates to axis
*  co-ordinates.
            CALL AST_TRANN( MAP, 1, NCAXES, 1, BC, .TRUE., NCAXES,
     :                      1, CC, STATUS )

*  The Cartesian-polar mapping has all we need.
         ELSE
            CALL AST_TRANN( CPMAP, 1, NCAXES, 1, CC, .FALSE., NCAXES,
     :                      1, PC, STATUS )

            DO I = 1, NCAXES
               CC( I ) = PC( I )
            END DO

         END IF

      END IF

*  Annul the pointer to the copied FramSet.
      IF ( GOTFS ) CALL AST_ANNUL( COPWCS, STATUS )

*  If an error has occurred, return AST__BAD values.
      IF ( STATUS .NE. SAI__OK ) THEN

         DO I = 1, NCAXES
            CC( I ) = AST__BAD
         END DO

         IF ( GOTFS ) THEN
            DO I =1, NBAXES
               BC( I ) = AST__BAD
            END DO
         END IF

      END IF

*  Error reports
*  =============
  999 CONTINUE

*  If a parameter abort value was supplied, re-report the error
*  with a more-appropriate message.
      IF ( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTPLR_8', 'Aborted attempt to obtain a '//
     :                 'position using parameter %^PARAM.', STATUS )

*  If a parameter null value was supplied, re-report the error
*  with a more-appropriate message.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__NULL
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTPLR_9', 'Aborted attempt to obtain a '//
     :                 'position using parameter %^PARAM.', STATUS )

*  Add a context message to any other error.
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTPLR_10', 'Failed to obtain a '//
     :                 'position using parameter %^PARAM.', STATUS )
      END IF

      END
