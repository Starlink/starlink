      SUBROUTINE KPG1_GTPOS( PARAM, IWCS, NULL, CC, BC, STATUS )
*+
*  Name:
*     KPG1_GTPOS

*  Purpose:
*     Get a spatial position from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GTPOS( PARAM, IWCS, NULL, CC, BC, STATUS )

*  Description:
*     This routine obtains a spatial position from the environment, using
*     a specified parameter. The user supplies the position in the
*     co-ordinate system of the Current Frame in the supplied WCS FrameSet.
*     To be acceptable, the supplied position must correspond to a valid
*     position (on all axes) in the Base Frame of the supplied FrameSet. If
*     a Frame is supplied instead of a FrameSet this restriction is not 
*     imposed.
*
*     If the position supplied in argument CC on entry is valid (i.e. does
*     not contain any AST__BAD values) then it is used as a dynamic
*     default for the parameter. Otherwise, no dynamic default is used.
*
*     The parameter is accessed as a single literal string containing a
*     space or comma separated list of axis values. The allowed formats for 
*     the axis values depends on the class of the Current Frame in the 
*     supplied FrameSet, and are described in SUN/210.
*
*     If the string supplied for the parameter consists of a single colon,
*     then a description of the Current co-ordinate Frame is displayed, 
*     together with an indication of the format required for each axis
*     value, and a new parameter value is then obtained.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     IWCS = INTEGER (Given)
*        A pointer to an AST Frame or FrameSet.
*     NULL = LOGICAL (Given)
*        If TRUE, a null (!) parameter value will result in the dynamic
*        default value being used. If FALSE (or if there is no dynamic
*        default), a null parameter value will result in a PAR__NULL error
*        status.
*     CC( * ) = DOUBLE PRECISION (Given and Returned)
*        On entry, holds the position to use as the dynamic default for the
*        parameter, in the Current Frame of the supplied FrameSet (or
*        Frame). On exit, holds the supplied position in the Current Frame.
*        There should be one element for each axis in the Current Frame.
*     BC( * ) = DOUBLE PRECISION (Returned)
*        Returned holding the Base Frame position corresponding to the
*        supplied Current Frame position. If a Frame is supplied for IWCS
*        instead of a FrameSet, then BC will not be accessed. The returned
*        values will be good on all axes.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  CC and BC are left unchanged if an error has already occurred.
*     -  Values of AST__BAD are returned in CC (and optionally BC) if an 
*     error occurs during this routine.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1998 (DSB):
*        Original version.
*     25-JUN-1999 (DSB):
*        Increase the accuracy of the formatted default value so that
*        it accurately represents the supplied default values. Normalize
*        the supplied default position. 
*     30-AUG-1999 (DSB):
*        If a null parameter is given use the dynamic default value.
*     3-SEP-1999 (DSB):
*        Added NULL argument.
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
      CHARACTER PARAM*(*)
      INTEGER IWCS
      LOGICAL NULL

*  Arguments Given and Returned:
      DOUBLE PRECISION CC( * )

*  Arguments Returned:
      DOUBLE PRECISION BC( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL CHR_SIMLR          ! Strings equal apart from case?

*  Local Variables:
      CHARACTER ATT*10           ! AST attribute name
      CHARACTER DOM*30           ! Domain of current frame
      CHARACTER DPOS*255         ! Default position string
      CHARACTER FMT*100          ! List of axis format strings
      CHARACTER LAB( NDF__MXDIM )*30 ! Axis labels
      CHARACTER NEXT*1           ! Next character to be read
      CHARACTER OLDFMT( NDF__MXDIM )*100 ! Original Format strings
      CHARACTER POS*255          ! Position string
      CHARACTER SYM( NDF__MXDIM )*10 ! Axis symbols
      DOUBLE PRECISION TEST      ! Result of unformatting default string
      INTEGER BASFRM             ! Pointer to the Base Frame
      INTEGER CURFRM             ! Pointer to the Current Frame
      INTEGER DIGS               ! Frame Digits value to be used
      INTEGER F                  ! Index of first non-blank character
      INTEGER FIAT               ! No. of characters in string FMT
      INTEGER I                  ! Axis index
      INTEGER IAT                ! No. of characters in a string
      INTEGER IEND               ! End of field passed to AST_UNFORMAT
      INTEGER IPOS               ! Index of next character to be read
      INTEGER L                  ! Index of last non-blank character
      INTEGER MAP                ! Simplified FrameSet Mapping
      INTEGER NBAXES             ! No. of axes in base Frame
      INTEGER NC                 ! No. of characters read from string
      INTEGER NCAXES             ! No. of axes in current Frame
      INTEGER OLDDIG             ! Original Frame Digits value
      LOGICAL GOOD               ! Is position good?
      LOGICAL GOTFS              ! Was a FrameSet supplied?
      LOGICAL LOOP               ! Get a new parameter value?
      LOGICAL SYMOK              ! Are all axis symbols non-blank?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if a FrameSet has been supplied.
      GOTFS = AST_ISAFRAMESET( IWCS, STATUS )

*  If so, get a pointer to the Current and Base Frames, and get the 
*  number of axes in the Base Frame. Also get a simplified Mapping for 
*  the FrameSet.
      IF( GOTFS ) THEN
         CURFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )         
         BASFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )         
         NBAXES = AST_GETI( BASFRM, 'NAXES', STATUS )
         CALL AST_ANNUL( BASFRM, STATUS )

         MAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, STATUS )
         MAP = AST_SIMPLIFY( MAP, STATUS )

*  If a Frame was supplied, use a clone of the Frame as the "Current
*  Frame".
      ELSE
         CURFRM = AST_CLONE( IWCS, STATUS )
      END IF

*  Get the number of axes in the Current Frame.
      NCAXES = AST_GETI( CURFRM, 'NAXES', STATUS )

*  See if a good position has been supplied in CC. At the same time,
*  save the axis labels and symbols for use in messages. Also, form a comma
*  separated list of the format strings for each axis.
      GOOD = .TRUE.
      SYMOK = .TRUE.

      FMT = ' '
      FIAT = 0

      DO I = 1, NCAXES
         IF( CC( I ) .EQ. AST__BAD ) GOOD = .FALSE.

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
         IF( SYM( I ) .EQ. ' ' ) SYMOK = .FALSE.

         IF( I .GT. 1 ) CALL CHR_APPND( ',', FMT, FIAT )

         ATT = 'FORMAT('
         IAT = 7
         CALL CHR_PUTI( I, ATT, IAT )
         CALL CHR_APPND( ')', ATT, IAT )
         CALL CHR_APPND( AST_GETC( CURFRM, ATT( : IAT ), STATUS ), FMT, 
     :                   FIAT )

      END DO

*  If a good position has been supplied in CC we use it as a dynamic
*  default for the parameter.
      IF( GOOD ) THEN

*  Normalize it.
         CALL AST_NORM( CURFRM, CC, STATUS )

*  Temporarily clear any Digits attributes for the current Frame. Save 
*  any set value so that it can be re-instated later.
         IF( AST_TEST( CURFRM, 'DIGITS', STATUS ) ) THEN
            OLDDIG = AST_GETI( CURFRM, 'DIGITS', STATUS )
            CALL AST_CLEAR( CURFRM, 'DIGITS', STATUS )
         ELSE
            OLDDIG = -1
         END IF

*  Temporarily clear any Format attributes for the Axes within the 
*  current Frame. Save any set values so that they can be re-instated 
*  later.
         DO I = 1, NCAXES

            ATT = 'FORMAT('
            IAT = 7
            CALL CHR_PUTI( I, ATT, IAT )
            CALL CHR_APPND( ')', ATT, IAT )

            IF( AST_TEST( CURFRM, ATT, STATUS ) ) THEN
               OLDFMT( I ) = AST_GETC( CURFRM, ATT, STATUS )
               CALL AST_CLEAR( CURFRM, ATT, STATUS )
            ELSE
               OLDFMT( I ) = ' '
            END IF

         END DO

*  Loop round increasing Digits until the supplied default values can
*  be formatted and unformatted with no significant change in value.
         DIGS = 4
         LOOP = .TRUE.
         DO WHILE( LOOP .AND. STATUS .EQ. SAI__OK )

*  Set the current Digits value.
            CALL AST_SETI( CURFRM, 'DIGITS', DIGS, STATUS )

*  Assume the current Digits value is OK.
            LOOP = .FALSE.

*  Check each axis.
            DO I = 1, NCAXES

*  Format this axis value.
               POS = AST_FORMAT( CURFRM, I, CC( I ), STATUS )

*  Unformat it.
               NC = AST_UNFORMAT( CURFRM, I, POS, TEST, STATUS ) 

*  If the recovered value is significantly different to the supplied value,
*  increase Digits and break out of the loop to try again.
               IF( ABS( CC( I ) - TEST ) .GT. 
     :             ABS( 10000.0*VAL__EPSD*CC( I ) ) ) THEN
                  DIGS = DIGS + 1
                  LOOP = .TRUE.             
                  GO TO 10                                 
               END IF

            END DO

 10         CONTINUE

         END DO

*  Construct a string holding the default position (a comma separated
*  list of formatted axis values) using the Digits value set above.
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

*  Re-instate any previous Digits value.
         IF( OLDDIG .NE. -1 ) CALL AST_SETI( CURFRM, 'DIGITS', OLDDIG, 
     :                                       STATUS )

*  Re-instate any previous Format values. Do each axis in turn.
         DO I = 1, NCAXES
            IF( OLDFMT( I ) .NE. ' ' ) THEN

               ATT = 'FORMAT('
               IAT = 7
               CALL CHR_PUTI( I, ATT, IAT )
               CALL CHR_APPND( ')', ATT, IAT )

               CALL AST_SETC( CURFRM, ATT, 
     :                        OLDFMT( I )( : CHR_LEN( OLDFMT( I ) ) ), 
     :                        STATUS )
            END IF
         END DO

*  Otherwise, use PAR_UNSET to ensure any previous dynamic default is 
*  cancelled.
      ELSE
         CALL PAR_UNSET( PARAM, 'DEFAULT', STATUS )
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
*  the dynamic default if there is a dynamic default, and if NULL is TRUE.
         IF( STATUS .EQ. PAR__NULL .AND. GOOD .AND. NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            POS = DPOS
         END IF            

*  Get the indices of the first and last non-blank characters.
         CALL CHR_FANDL( POS, F, L )

*  If the string is blank, report an error.
         IF( F .GT. L .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'PAR', PARAM )
            CALL ERR_REP( 'KPG1_GTPOS_1', 'Blank value supplied for '//
     :                    'parameter %^PAR.', STATUS )

*  Otherwise, if the supplied string is just a colon, display a description 
*  of the Current Frame, and the default format.
         ELSE IF( POS( F : L ) .EQ. ':' ) THEN
            CALL KPG1_DSFRM( CURFRM, 'A position is required in the '//
     :                       'following co-ordinate frame:', STATUS )

            CALL MSG_SETC( 'FMT', FMT )
            CALL MSG_OUT( 'KPG1_GTPOS_M1', '      Suggested format: '//
     :                    '^FMT', STATUS )
            CALL MSG_BLANK( STATUS )

*  Otherwise, attempt to read a position from the supplied string.
         ELSE

*  Replace all commas with spaces.
            DO I = F, L
               IF( POS( I : I ) .EQ. ',' ) POS( I : I ) = ' '
            END DO

*  Assume that the supplied value was acceptable.
            LOOP = .FALSE.

*  Loop round , attempting to read valued for each axis, until all
*  axes are done, or an error occurs.
            IPOS = F
            I = 0
            DO WHILE( I .LT. NCAXES .AND. STATUS .EQ. SAI__OK )

*  Find the start of the next but one axis value (if there is more than 1 axis
*  value remaining in the string).
               IEND = IPOS
               CALL CHR_TOCHR( ' ', POS, .TRUE., IEND ) 
               IF( IEND .LT. L ) CALL CHR_SKCHR( ' ', POS, .TRUE., 
     :                                           IEND ) 

*  Set the index of the last character to be read by AST_UNFORMAT. This
*  is done since AST_UNFORMAT may allow spaces within axis values, but
*  we require spaces to be used ax axis delimiters.
               IEND = IEND - 1

*  Read the value for the next axis. NC is the number of characters#
*  read by AST_UNFORMAT including trailing spaces.
               I = I + 1
               NC = AST_UNFORMAT( CURFRM, I, POS( IPOS:IEND ), CC( I ), 
     :                            STATUS ) 

*  Get the last character read by AST_UNFORMAT. If there are no characters 
*  left pretend the last character read was a space (i.e. an axis delimiter).
               IPOS = IPOS + NC - 1
               IF( IPOS .LT. L ) THEN
                  NEXT = POS( IPOS : IPOS )
               ELSE
                  NEXT = ' '
               END IF

*  Increment IPOS to skip the space. It now points to the first character
*  in the next axis value, or to the first character following the last
*  non-blank character (L) if no axis values are left in the string.
               IPOS = IPOS + 1

*  If the supplied string was invalid, report an error. This is the
*  case if no characters were read form the string, or if the next
*  character is not a space.
               IF( ( NC .EQ. 0 .OR. NEXT .NE. ' ' ) .AND. 
     :             STATUS .EQ. SAI__OK ) THEN
                  LOOP = .TRUE.
                  STATUS = SAI__ERROR

                  CALL MSG_SETI( 'I', I )
                  CALL MSG_SETC( 'PAR', PARAM )
                  CALL MSG_SETC( 'LAB', LAB( I ) )
                  CALL MSG_SETC( 'POS', POS( F: L ) )

                  IF( LAB( I )( : 5 ) .NE. 'axis ' ) THEN
                     CALL ERR_REP( 'KPG1_GTPOS_2', 'Failed to get a '//
     :                             'valid ^LAB value for axis ^I '//
     :                             'using parameter %^PAR - ''^POS''.',
     :                             STATUS )
                  ELSE
                     CALL ERR_REP( 'KPG1_GTPOS_3', 'Failed to get a '//
     :                             'valid ^LAB value using parameter '//
     :                             '%^PAR - ''^POS''.', STATUS )
                  END IF

*  If the string has been exhausted before all axis values have been
*  obtained, report an error.
               ELSE IF( IPOS .GT. L .AND. I .LT. NCAXES .AND.
     :                  STATUS .EQ. SAI__OK ) THEN
                  LOOP = .TRUE.
                  STATUS = SAI__ERROR

                  CALL MSG_SETI( 'I', I + 1 )
                  CALL MSG_SETC( 'PAR', PARAM )
                  CALL MSG_SETC( 'LAB', LAB( I + 1 ) )
                  CALL MSG_SETC( 'POS', POS( F: L ) )

                  IF( LAB( I + 1 )( : 5 ) .NE. 'axis ' ) THEN
                     CALL ERR_REP( 'KPG1_GTPOS_4', 'No ^LAB value '//
     :                             '(axis ^I) supplied using '//
     :                             'parameter %^PAR - ''^POS''.', 
     :                             STATUS )
                  ELSE
                     CALL ERR_REP( 'KPG1_GTPOS_5', 'No ^LAB value '//
     :                             'supplied using parameter %^PAR '//
     :                             '- ''^POS''.', STATUS )
                  END IF

               END IF

            END DO

*  If there were any unused characters at the end of the supplied string,
*  report an error.
            IF( IPOS .LE. L .AND. STATUS .EQ. SAI__OK ) THEN
               LOOP = .TRUE.
               STATUS = SAI__ERROR

               CALL MSG_SETC( 'PAR', PARAM )
               CALL MSG_SETC( 'TXT', POS( IPOS - 1 : ) )
               CALL MSG_SETC( 'LAB', LAB( NCAXES ) )
               CALL MSG_SETC( 'POS', POS( F: L ) )

               IF( L + 2 - IPOS .EQ. 1 ) THEN
                  CALL MSG_SETC( 'WORD', 'character' )
               ELSE
                  CALL MSG_SETC( 'WORD', 'characters' )
               END IF

               CALL ERR_REP( 'KPG1_GTPOS_6', 'Extra ^WORD ''^TXT'' '//
     :                       'found at end of ^LAB value supplied for'//
     :                       ' parameter %^PAR - ''^POS''.', STATUS )

            END IF

*  If we have a valid set of Current Frame axis values, check that
*  they correspond to a valid position in the Base Frame of the supplied
*  FrameSet. This is not done if a Frame was supplied.
            IF( GOTFS ) THEN

*  Transform the supplied position into the Base Frame.
               CALL AST_TRANN( MAP, 1, NCAXES, 1, CC, .FALSE., NBAXES,
     :                         1, BC, STATUS ) 

*  See if this gave a good Base Frame position.
               GOOD = .TRUE.
               DO I = 1, NBAXES
                  IF( BC( I ) .EQ. AST__BAD ) GOOD = .FALSE.
               END DO

*  If not, report an error.
               IF( .NOT. GOOD .AND. STATUS .EQ. SAI__OK ) THEN
                  LOOP = .TRUE.
                  STATUS = SAI__ERROR

                  CALL MSG_SETC( 'PAR', PARAM )
                  CALL MSG_SETC( 'POS', POS( F: L ) )
                  CALL ERR_REP( 'KPG1_GTPOS_7', 'The position '//
     :                          'supplied for parameter %^PAR '//
     :                          '(''^POS'') cannot be used.', STATUS )
               END IF

            END IF

         END IF

*  Do not loop if an error occurred within an infrastructure library
*  (except for errors reported by AST_UNFORMAT indicating bad text supplied
*  by the user).
         IF( STATUS .NE. SAI__OK .AND.
     :       STATUS .NE. SAI__ERROR .AND.
     :       STATUS .NE. AST__UNFER ) LOOP = .FALSE.

*  If a new parameter value is required...
         IF( LOOP ) THEN

*  Flush any error.
            IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Indicate what sort of position is required.
            CALL MSG_SETC( 'DOM', DOM )
            CALL MSG_SETC( 'PAR', PARAM )
            CALL MSG_OUT( 'KPG1_GTPOS_M1', 'Please supply a new ^DOM '//
     :                    'Domain position for parameter %^PAR.', 
     :                    STATUS )

*  Cancel the parameter value.
            CALL PAR_CANCL( PARAM, STATUS )

         END IF

      END DO

*  Annul the pointer to the current Frame.
      CALL AST_ANNUL( CURFRM, STATUS )

*  If an error has occurred, return AST__BAD values.
      IF( STATUS .NE. SAI__OK ) THEN

         DO I =1, NCAXES
            CC( I ) = AST__BAD
         END DO

         IF( GOTFS ) THEN    
            DO I =1, NBAXES
               BC( I ) = AST__BAD
            END DO
         END IF

      END IF

*  If a parameter abort value was supplied, re-report the error
*  with a more appropriate message.
      IF( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTPOS_8', 'Aborted attempt to obtain a '//
     :                 'position using parameter %^PARAM.', STATUS )

*  If a parameter null value was supplied, re-report the error
*  with a more appropriate message.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__NULL
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTPOS_9', 'Aborted attempt to obtain a '//
     :                 'position using parameter %^PARAM.', STATUS )

*  Add a context message to any other error.
      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTPOS_10', 'Failed to obtain a '//
     :                 'position using parameter %^PARAM.', STATUS )
      END IF

      END
