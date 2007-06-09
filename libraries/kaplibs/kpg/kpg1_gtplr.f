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
*     This routine obtains a spatial position from the environment, 
*     using a specified parameter.  The user supplies the position in 
*     polar co-ordinates about a suppled centre within the co-ordinate 
*     system of the Current Frame in the supplied WCS FrameSet.  This
*     Frame must have two axes, otherwise co-ordinates will be
*     interpreted as Cartesian.

*     To be acceptable, the supplied position must correspond to a valid
*     position (on all axes) in the Base Frame of the supplied FrameSet.
*     If a Frame is supplied instead of a FrameSet this restriction is 
*     not imposed.  The current Frame must have two axes.
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
*        anothmay be 90 for 
*     POLE( 2 )  = DOUBLE PRECISION (Given)
*        The position of the pole of the polar co-ordinates measured
*        in the current co-ordinate Frame along each axis.  If any of 
*        the co-ordinates are bad, the routine will issue a warning that
*        it is unable to handle polar co-ordinates and will expect 
*        spatial positions like routine KPG1_GTPOS.  This can be aborted
*        (!!) at the parameter prompt.
*     CC( 2 ) = DOUBLE PRECISION (Given and Returned)
*        On entry, holds the position to use as the dynamic default for 
*        the parameter, in the Current Frame of the supplied FrameSet 
*        (or Frame).  On exit, it holds the supplied position in the 
*        Current Frame.  There should be one element for each axis in 
*        the Current Frame.  The first element is the radius, and
*        subsequent  elements are angles.
*     BC( 2 ) = DOUBLE PRECISION (Returned)
*        Returned holding the Base Frame position corresponding to the
*        supplied Current Frame position.  If a Frame is supplied for 
*        IWCS instead of a FrameSet, then BC will not be accessed.  The 
*        returned values will be good on all axes.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  CC and BC are left unchanged if an error has already occurred.
*     -  Values of AST__BAD are returned in CC (and optionally BC) if an
*     error occurs during this routine.

*  Copyright:
*     Copyright (C) 1998, 1999, 2000 Central Laboratory of the Research 
*     Councils
*     Copyright (c) 2007 Science and Technology Facilities Council
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 June 1 (MJC):
*        Original version based upon DSB's KPG1_GTPOS.
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
      
*  Local Variables:
      CHARACTER*10 ATT           ! AST attribute name
      INTEGER BASFRM             ! Pointer to the Base Frame
      INTEGER BPMAP              ! Base-Polar mapping
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
      DOUBLE PRECISION INPOL( 2 ) ! Pole co-ordinates
      CHARACTER*125 INVEXP( NAX ) ! Inverse expressions for
                                 ! MathMap
      INTEGER IPOS               ! Index of next character to be read
      LOGICAL ISSKY              ! Is the current Frame a SkyFrame?
      INTEGER L                  ! Index of last non-blank character
      CHARACTER*30 LAB( NAX )    ! Axis labels
      LOGICAL LOOP               ! Get a new parameter value?
      INTEGER MAP                ! Simplified FrameSet Mapping
      INTEGER NBAXES             ! No. of axes in base Frame
      INTEGER NC                 ! No. of characters read from string
      INTEGER NCAXES             ! No. of axes in current Frame
      CHARACTER*1 NEXT           ! Next character to be read
      DOUBLE PRECISION PC( 2 )   ! Polar co-ordinates
      INTEGER POLFRM             ! Pointer to new polar Frame
      CHARACTER*255 POS          ! Position string
      CHARACTER*10 SYM( NAX )    ! Axis symbols
      CHARACTER*50 UNITS         ! Axis units
      

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if a FrameSet has been supplied.
      GOTFS = AST_ISAFRAMESET( IWCS, STATUS )

*  If so, get a pointer to the Current and Base Frames, and get the 
*  number of axes in the Base Frame.  Also get a simplified Mapping for 
*  the FrameSet.
      IF ( GOTFS ) THEN
         CURFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )         
         BASFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )         
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

*  Get the number of axes in the Current Frame and see if it is a
*  SkyFrame.
      NCAXES = AST_GETI( CURFRM, 'NAXES', STATUS )

      IF ( NCAXES .NE. NAX ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL MSG_OUT( 'KPG1_GTPLR_M2', 'Current Frame does not '//
     :                 'have two axes.  Parameter %^PARAM will '//
     :                 'interpret co-ordinates as regular axis '//
     :                 'values.  (Programming error)', STATUS )
         GOODPO = .FALSE.
      ELSE
         GOODPO = .TRUE.
      END IF

      ISSKY =  AST_ISASKYFRAME( CURFRM, STATUS )

*  See if a good position has been supplied in POLE.  At the same time,
*  set the pole position for a SkyFrame.
      IF ( GOTFS .AND. GOODPO ) THEN
         GOODPO = .TRUE.
         DO I = 1, NCAXES
            IF ( POLE( I ) .EQ. AST__BAD .OR.
     :           POLE( I ) .EQ. VAL__BADD ) GOODPO = .FALSE.
            IF ( ISSKY .AND. GOODPO ) THEN
               ATT = 'SkyRef('
               IAT = 7
               CALL CHR_PUTI( I, ATT, IAT )
               CALL CHR_APPND( ')', ATT, IAT )
               CALL AST_SETD( CURFRM, ATT( : IAT ), POLE( I ), STATUS )
            END IF
         END DO

         IF ( .NOT. GOODPO ) THEN
            CALL MSG_SETC( 'PARAM', PARAM )
            CALL ERR_REP( 'KPG1_GTPLR_M3', 'A co-ordinate of the '//
     :                    'pole is undefined.  Parameter %^PARAM '//
     :                    'wil interpret co-ordinates as regular '//
     :                    'axis values.  (Programming error)', STATUS )

*  Switch to polar co-ordinates in the SkyFrame.
         ELSE IF ( ISSKY ) THEN
            CALL AST_SETC( CURFRM, 'SkyRefIs' , 'Pole', STATUS )

         ELSE

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
            CALL CHR_APPND( 'THETA = MOD( ATAN2D( Y - (', FOREXP( 2 ), 
     :                      IAT )
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
     :                      IAT )
            CALL CHR_PUTD( PAORIG, INVEXP( 1 ), IAT )
            CALL CHR_APPND( ') ) + (', INVEXP( 1 ), IAT )
            CALL CHR_PUTD( POLE( 1 ), INVEXP( 1 ), IAT )
            CALL CHR_APPND( ')', INVEXP( 1 ), IAT )

            IAT = 0
            INVEXP( 2 ) = ' '
            CALL CHR_APPND( 'Y = R * SIND( THETA + (', INVEXP( 2 ),
     :                      IAT )
            CALL CHR_PUTD( PAORIG, INVEXP( 2 ), IAT )
            CALL CHR_APPND( ') ) + (', INVEXP( 2 ), IAT )
            CALL CHR_PUTD( POLE( 2 ), INVEXP( 2 ), IAT )
            CALL CHR_APPND( ')', INVEXP( 2 ), IAT )

            CPMAP = AST_MATHMAP( NAX, NAX, NAX, FOREXP, NAX, INVEXP,
     :                           'SimpFI=1,SimpIF=1', STATUS )

*  Simplify the Mapping.
            CPMAP = AST_SIMPLIFY( CPMAP, STATUS )

            IBASIS = AST_GETI( IWCS, 'CURRENT', STATUS )
            POLFRM = AST_COPY( CURFRM, STATUS )      
            CALL AST_ADDFRAME( IWCS, IBASIS, CPMAP, POLFRM, STATUS )

*  Describe the new Frame in case the user enters ":" at the prompt.
            CALL AST_SETC( POLFRM, 'TITLE', 'Polar co-ordinates',
     :                     STATUS )
            CALL AST_SETC( POLFRM, 'LABEL(1)', 'Radius offset', STATUS )
            CALL AST_SETC( POLFRM, 'LABEL(2)', 
     :                     'Position angle (from Y anticlockwise)',
     :                     STATUS )
            
            UNITS = AST_GETC( POLFRM, 'UNIT(1)', STATUS )
            CALL AST_SETC( POLFRM, 'UNIT(1)', UNITS, STATUS )
            CALL AST_SETC( POLFRM, 'UNIT(2)', 'degrees', STATUS )

*  Now we're make our new Frame current.
            CURFRM = POLFRM

*  Form a compound mapping between the Base Frame and the newly created
*  polar Frame.
            BPMAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT,
     :                              STATUS )
            BPMAP = AST_SIMPLIFY( BPMAP, STATUS )

*  Otherwise, create a default Frame of POLAR class.
         END IF
      END IF

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

*  Note the Domain of the Current Frame for use in messages.
      DOM = AST_GETC( CURFRM, 'DOMAIN', STATUS )

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
            CALL KPG1_DSFRM( IWCS, 'A position is required in the '//
     :                       'following co-ordinate frame:', .TRUE.,
     :                       STATUS )

            CALL MSG_SETC( 'FMT', FMT )
            CALL MSG_OUT( 'KPG1_GTPLR_M4', '      Suggested format: '//
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
               NC = AST_UNFORMAT( CURFRM, I, POS( IPOS:IEND ), CC( I ), 
     :                            STATUS ) 

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

               IF ( GOODPO .AND. .NOT. ISSKY ) THEN

*  Convert from polar into Current Frame axis values.
                  CALL AST_TRANN( BPMAP, 1, NCAXES, 1, CC, .FALSE., 
     :                            NBAXES, 1, BC, STATUS ) 
               ELSE

*  Transform the supplied `Cartesian' position into the Base Frame.
                  CALL AST_TRANN( MAP, 1, NCAXES, 1, CC, .FALSE.,
     :                            NBAXES, 1, BC, STATUS )
               END IF

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
            CALL MSG_OUT( 'KPG1_GTPLR_M5', 'Please supply a new ^DOM '//
     :                    'Domain position for parameter %^PAR.', 
     :                    STATUS )

*  Cancel the parameter value.
            CALL PAR_CANCL( PARAM, STATUS )

         END IF

      END DO

*  Convert polar co-ordinates to along each WCS axis of the original
*  current Frame.
      IF ( GOTFS .AND. GOODPO ) THEN
         CALL AST_TRANN( CPMAP, 1, NCAXES, 1, CC, .FALSE., NCAXES,
     :                   1, PC, STATUS ) 

         DO I = 1, NCAXES
            CC( I ) = PC( I )
         END DO

*  Restore the original current Frame.
         CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )
      END IF

*  Annul the pointer to the current Frame.
      CALL AST_ANNUL( CURFRM, STATUS )

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
