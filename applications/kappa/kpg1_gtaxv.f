      SUBROUTINE KPG1_GTAXV( PARAM, FRAME, IAXIS, AXVAL, STATUS )
*+
*  Name:
*     KPG1_GTAXV

*  Purpose:
*     Get a formatted axis value from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GTAXV( PARAM, FRAME, IAXIS, AXVAL, STATUS )

*  Description:
*     This routine obtains a formatted axis value from the environment,
*     using a specified parameter. 
*
*     If the string supplied for the parameter consists of a single colon,
*     then a description of the Current co-ordinate Frame is displayed, 
*     together with an indication of the format required for each axis
*     value, and a new parameter value is then obtained.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     FRAME = INTEGER (Given)
*        A pointer to an AST Frame in which the axis lives.
*     IAXIS = INTEGER (Given)
*        The index of the axis within the Frame for which a value is required.
*     AXVAL = DOUBLE PRECISION (Given and Returned)
*        On entry, holds the axis value to use as the dynamic default for the
*        parameter. On exit, holds the supplied axis value. No dynamic
*        default is used if the supplied value is AST__BAD.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  AXVAL is left unchanged if an error has already occurred.
*     -  AST__BAD is returned in AXVAL if an error occurs during this routine.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-SEP-1998 (DSB):
*        Original version.
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

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER FRAME
      INTEGER IAXIS

*  Arguments Given and Returned:
      DOUBLE PRECISION AXVAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATT*10           ! AST attribute name
      CHARACTER FMT*30           ! Axis format
      CHARACTER LAB*30           ! Axis label
      CHARACTER TEXT*255         ! Text string
      INTEGER F                  ! Index of first non-blank character
      INTEGER I                  ! Axis index
      INTEGER IAT                ! No. of characters in a string
      INTEGER L                  ! Index of last non-blank character
      INTEGER NC                 ! No. of characters read from string
      LOGICAL LOOP               ! Get a new parameter value?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If a good position has been supplied in AXVAL, format it and
*  use it as the dynamic default for the parameter. Otherwise, use
*  PAR_UNSET to ensure any previous dynamic default is cancelled.
      IF( AXVAL .NE. AST__BAD ) THEN
         CALL PAR_DEF0C( PARAM, AST_FORMAT( FRAME, IAXIS, AXVAL, 
     :                                      STATUS ), STATUS )
      ELSE
         CALL PAR_UNSET( PARAM, 'DEFAULT', STATUS )
      END IF

*  Get a lower case version of the axis label.
      ATT = 'LABEL('
      IAT = 6
      CALL CHR_PUTI( IAXIS, ATT, IAT )
      CALL CHR_APPND( ')', ATT, IAT )
      LAB = AST_GETC( FRAME, ATT( : IAT ), STATUS )
      CALL CHR_LCASE( LAB )

*  Get the axis format.
      ATT = 'FORMAT('
      IAT = 7
      CALL CHR_PUTI( IAXIS, ATT, IAT )
      CALL CHR_APPND( ')', ATT, IAT )
      FMT = AST_GETC( FRAME, ATT( : IAT ), STATUS )

*  Loop until a valid axis value has been obtained from the user, or an 
*  error occurs.
      LOOP = .TRUE.
      DO WHILE( LOOP .AND. STATUS .EQ. SAI__OK )       

*  Get a value for the parameter.
         CALL PAR_GET0C( PARAM, TEXT, STATUS )

*  Get the indices of the first and last non-blank characters.
         CALL CHR_FANDL( TEXT, F, L )

*  If the string is blank, report an error.
         IF( F .GT. L .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'PAR', PARAM )
            CALL ERR_REP( 'KPG1_GTAXV_1', 'Blank value supplied for '//
     :                    'parameter %^PAR.', STATUS )

*  Otherwise, if the supplied string is just a colon, display a description 
*  of the Frame, and the default format.
         ELSE IF( TEXT( F : L ) .EQ. ':' ) THEN

            TEXT = ' '
            IAT = 0
            CALL CHR_APPND( 'A value is required for axis', TEXT, IAT )
            IAT = IAT + 1
            CALL CHR_PUTI( IAXIS, TEXT, IAT )
            CALL CHR_APPND( ' in the following co-ordinate frame:',
     :                      TEXT, IAT )

            CALL KPG1_DSFRM( FRAME, TEXT( : IAT ), STATUS )

            CALL MSG_SETC( 'FMT', FMT )
            CALL MSG_OUT( 'KPG1_GTAXV_M1', '      Suggested format: '//
     :                    '^FMT', STATUS )
            CALL MSG_BLANK( STATUS )

*  Otherwise, attempt to read an axis value from the supplied string.
         ELSE

*  Assume that the supplied value was acceptable.
            LOOP = .FALSE.

*  Read the value for the axis. NC is the number of characters
*  read by AST_UNFORMAT including trailing spaces.
            NC = AST_UNFORMAT( FRAME, IAXIS, TEXT( F : L ), AXVAL, 
     :                         STATUS ) 

*  If the supplied string was invalid, report an error. This is the
*  case if there are non-blank characters left in the string after the
*  read.
            IF( F + NC .LE. L .AND. TEXT( F + NC : L ) .NE. ' ' .AND. 
     :          STATUS .EQ. SAI__OK ) THEN
               LOOP = .TRUE.
               STATUS = SAI__ERROR

               CALL MSG_SETC( 'PAR', PARAM )
               CALL MSG_SETC( 'LAB', LAB )
               CALL MSG_SETC( 'TEXT', TEXT( F: L ) )

               CALL ERR_REP( 'KPG1_GTAXV_2', 'Not a valid ^LAB '//
     :                       'value - ''^TEXT''.', STATUS )
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
            CALL MSG_SETC( 'LAB', LAB )
            CALL MSG_SETC( 'PAR', PARAM )
            CALL MSG_OUT( 'KPG1_GTAXV_M1', 'Please supply a new ^LAB '//
     :                    'value for parameter %^PAR.', STATUS )

*  Cancel the parameter value.
            CALL PAR_CANCL( PARAM, STATUS )

         END IF

      END DO

*  If an error has occurred, return AST__BAD values.
      IF( STATUS .NE. SAI__OK ) AXVAL = AST__BAD

*  If a parameter abort value was supplied, re-report the error
*  with a more appropriate message.
      IF( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTAXV_3', 'Aborted attempt to obtain an '//
     :                 'axis value using parameter %^PARAM.', STATUS )

*  If a parameter null value was supplied, re-report the error
*  with a more appropriate message.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__NULL
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTAXV_4', 'Aborted attempt to obtain an '//
     :                 'axis value using parameter %^PARAM.', STATUS )

*  Add a context message to any other error.
      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTAXV_5', 'Failed to obtain an axis '//
     :                 'value using parameter %^PARAM.', STATUS )
      END IF

      END
