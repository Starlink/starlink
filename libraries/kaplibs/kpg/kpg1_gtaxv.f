      SUBROUTINE KPG1_GTAXV( PARAM, MXVAL, EXACT, FRAME, IAXIS, AXVAL,
     :                       NVAL, STATUS )
*+
*  Name:
*     KPG1_GTAXV

*  Purpose:
*     Gets one or more formatted axis values from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GTAXV( PARAM, MXVAL, EXACT, FRAME, IAXIS, AXVAL, NVAL, STATUS )

*  Description:
*     This routine obtains one or more formatted values for a specified axis
*     from the environment, using a specified parameter.
*
*     If the string supplied for the parameter consists of a single colon,
*     then a description of the Current co-ordinate Frame is displayed,
*     together with an indication of the format required for each axis
*     value, and a new parameter value is then obtained.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     MXVAL = INTEGER (Given)
*        The maximum number of values which can be supplied. It is not an
*        error for less than MXVAL to be supplied. Must be no more than 20.
*     EXACT = LOGICAL( Given)
*        If .TRUE., then the user must supply exactly MXVAL values, and
*        he is reprompted if less than MXVAL are given. If .FALSE. then
*        the user can give between 1 and MXVAL values.
*     FRAME = INTEGER (Given)
*        A pointer to an AST Frame in which the axis lives.
*     IAXIS = INTEGER (Given)
*        The index of the axis within the Frame for which a value is required.
*     AXVAL( MXVAL ) = DOUBLE PRECISION (Given and Returned)
*        On entry, holds the axis values to use as the dynamic default for the
*        parameter. On exit, holds the supplied axis value. No dynamic
*        default is used if any of the supplied values is AST__BAD.
*     NVAL = INTEGER (Returned)
*        The number of values obtained using the parameter and returned in
*        AXVAL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If a null (!) parameter value is supplied, the supplied value of
*     AXVAL is returned, and the error is annulled if the AXVAL value is
*     not equal to AST__BAD.
*     -  AXVAL is left unchanged if an error has already occurred.
*     -  AST__BAD is returned in AXVAL if an error occurs during this routine.

*  Copyright:
*     Copyright (C) 1998, 2000 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     23-SEP-1998 (DSB):
*        Original version.
*     20-JAN-2000 (DSB):
*        Added option for obtaining more than 1 axis value.
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
      INTEGER MXVAL
      LOGICAL EXACT
      INTEGER FRAME
      INTEGER IAXIS

*  Arguments Given and Returned:
      DOUBLE PRECISION AXVAL( MXVAL )
      INTEGER NVAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER WSIZE              ! Max no. of axis values which can be got
      PARAMETER ( WSIZE = 20 )

*  Local Variables:
      CHARACTER ATT*10           ! AST attribute name
      CHARACTER FMT*30           ! Axis format
      CHARACTER LAB*30           ! Axis label
      CHARACTER TEXT*255         ! Text string
      DOUBLE PRECISION AXV       ! An axis value
      DOUBLE PRECISION AXVAL0( WSIZE )! Supplied AXVAL value
      INTEGER F                  ! Index of first non-blank character
      INTEGER I                  ! Axis index
      INTEGER IAT                ! No. of characters in a string
      INTEGER L                  ! Index of last non-blank character
      INTEGER NC                 ! No. of characters read from string
      INTEGER NGOOD              ! No. of default axis values to use
      LOGICAL GOOD               ! Have all values been good so far?
      LOGICAL LOOP               ! Get a new parameter value?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the MXVAL value is OK.
      IF( MXVAL .GT. WSIZE ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'M', MXVAL )
         CALL MSG_SETI( 'W', WSIZE )
         CALL ERR_REP( 'KPG1_GTAXV_ERR1', 'KPG1_GTAXV: Supplied MXVAL'//
     :                 ' value (^M) is too large. It must be no more '//
     :                 'than ^W (programming error).', STATUS )
         GO TO 999
      END IF

*  Save the supplied AXVAL values, and count the number of good values at
*  the begging of the array.
      GOOD = .TRUE.
      NGOOD = 0

      DO I = 1, MXVAL
         AXVAL0( I ) = AXVAL( I )
         IF( AXVAL( I ) .EQ. AST__BAD ) THEN
            GOOD = .FALSE.
         ELSE IF( GOOD ) THEN
            NGOOD = NGOOD + 1
         END IF
      END DO

*  If EXACT is .TRUE., then we must have exactly MXVAL good values to be
*  able to use them as a dynamic default.
      IF( EXACT .AND. NGOOD .LT. MXVAL ) NGOOD = 0

*  If good values has been supplied in AXVAL, format them and use them
*  as the dynamic default for the parameter. Otherwise, use PAR_UNSET
*  to ensure any previous dynamic default is cancelled.
      IF( NGOOD .GT. 0 ) THEN
         TEXT = ' '
         IAT = 0
         DO I = 1, NGOOD
            CALL CHR_APPND( AST_FORMAT( FRAME, IAXIS, AXVAL( I ),
     :                                  STATUS ), TEXT, IAT )
            IAT = IAT + 1
         END DO

         CALL PAR_DEF0C( PARAM, TEXT( : IAT ), STATUS )

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

*  Loop until a set of valid axis values has been obtained from the user, or
*  an error occurs.
      LOOP = .TRUE.
      DO WHILE( LOOP .AND. STATUS .EQ. SAI__OK )

*  Get a value for the parameter.
         CALL PAR_GET0C( PARAM, TEXT, STATUS )

*  If a parameter null value was supplied, Return the original AXVAL
*  values, and annul the error unless adynamic default was supplied.
         IF( STATUS .EQ. PAR__NULL ) THEN
            DO I = 1, MXVAL
               AXVAL( I ) = AXVAL0( I )
            END DO
            IF( NGOOD .GT. 0 ) THEN
               CALL ERR_ANNUL( STATUS )
               NVAL = NGOOD
            END IF
            GO TO 999
         END IF

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
            IF( MXVAL .EQ. 1 ) THEN
               CALL CHR_APPND( 'A value is required for axis', TEXT,
     :                         IAT )
            ELSE IF( EXACT ) THEN
               CALL CHR_PUTI( MXVAL, TEXT, IAT )
               IAT = IAT + 1
               CALL CHR_APPND( 'values are required for axis', TEXT,
     :                         IAT )
            ELSE
               CALL CHR_APPND( 'Up to', TEXT, IAT )
               IAT = IAT + 1
               CALL CHR_PUTI( MXVAL, TEXT, IAT )
               IAT = IAT + 1
               CALL CHR_APPND( 'values are required for axis', TEXT,
     :                         IAT )
            END IF
            IAT = IAT + 1
            CALL CHR_PUTI( IAXIS, TEXT, IAT )
            CALL CHR_APPND( ' in the following co-ordinate frame:',
     :                      TEXT, IAT )

            CALL KPG1_DSFRM( FRAME, TEXT( : IAT ), AST__BAD, AST__BAD,
     :                       .TRUE., STATUS )

            CALL MSG_SETC( 'FMT', FMT )
            CALL MSG_OUT( 'KPG1_GTAXV_M1', '      Suggested format: '//
     :                    '^FMT', STATUS )
            CALL MSG_BLANK( STATUS )

*  Otherwise, attempt to read axis values from the supplied string.
         ELSE

*  Replace any commas by spaces.
            DO I = F, L
               IF( TEXT( I : I ) .EQ. ',' ) THEN
                  TEXT( I : I ) = ' '
               END IF
            END DO

*  Assume that the supplied parameter value was acceptable.
            LOOP = .FALSE.

*  Read values for the axis, counting the number obtained. NC is the number
*  of characters read by AST_UNFORMAT including trailing spaces.
            NVAL = 0
            DO WHILE( F .LE. L .AND. STATUS .EQ. SAI__OK )
               NC = AST_UNFORMAT( FRAME, IAXIS, TEXT( F : L ), AXV,
     :                            STATUS )

*  If the supplied string was invalid, report an error.
               IF( NC .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
                  LOOP = .TRUE.
                  STATUS = SAI__ERROR

                  CALL MSG_SETC( 'LAB', LAB )
                  CALL MSG_SETC( 'TEXT', TEXT )

                  CALL ERR_REP( 'KPG1_GTAXV_2', 'Invalid ^LAB '//
     :                          'value supplied in ''^TEXT''.',
     :                          STATUS )

*  Otherwise increment the number of axis values obtained, and store the
*  new starting character. Report an error and loop if too many axis
*  values have been supplied.
               ELSE
                  IF( NVAL .EQ. MXVAL .AND. STATUS .EQ. SAI__OK ) THEN
                     LOOP = .TRUE.
                     STATUS = SAI__ERROR

                     CALL MSG_SETI( 'M', MXVAL )
                     CALL MSG_SETC( 'LAB', LAB )
                     CALL MSG_SETC( 'TEXT', TEXT )

                     CALL ERR_REP( 'KPG1_GTAXV_2', 'Too many (i.e. '//
     :                             'more than ^M) ^LAB values '//
     :                             'supplied in ''^TEXT''.', STATUS )
                  ELSE
                     NVAL = NVAL + 1
                     AXVAL( NVAL ) = AXV
                     F = F + NC
                  END IF
               END IF
            END DO

*  Report an error if too few values were obtained
            IF( NVAL .LT. MXVAL .AND. EXACT .AND.
     :          STATUS .EQ. SAI__OK ) THEN
               LOOP = .TRUE.
               STATUS = SAI__ERROR

               CALL MSG_SETI( 'M', MXVAL )
               CALL MSG_SETC( 'LAB', LAB )
               CALL MSG_SETC( 'TEXT', TEXT )

               CALL ERR_REP( 'KPG1_GTAXV_2', 'Too few (i.e. less than'//
     :                       ' ^M) ^LAB values supplied in ''^TEXT''.',
     :                       STATUS )
            END IF

         END IF

*  Loop if an error has occurred, unless it occurred within an infrastructure
*  library (except for errors reported by AST_UNFORMAT indicating bad text
*  supplied by the user).
         IF( STATUS .EQ. SAI__ERROR .OR.
     :       STATUS .EQ. AST__UNFER ) LOOP = .TRUE.

*  If a new parameter value is required...
         IF( LOOP ) THEN

*  Flush any error.
            IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Indicate what sort of position is required.
            CALL MSG_SETC( 'LAB', LAB )
            CALL MSG_SETC( 'PAR', PARAM )
            IF( MXVAL .EQ. 1 ) THEN
               CALL MSG_OUT( 'KPG1_GTAXV_M1', 'Please supply a new '//
     :                       '^LAB value for parameter %^PAR.', STATUS )
            ELSE IF( EXACT ) THEN
               CALL MSG_SETI( 'M', MXVAL )
               CALL MSG_OUT( 'KPG1_GTAXV_M1', 'Please supply ^M ^LAB '//
     :                       'values for parameter %^PAR.', STATUS )
            ELSE
               CALL MSG_SETI( 'M', MXVAL )
               CALL MSG_OUT( 'KPG1_GTAXV_M1', 'Please supply up to ^M'//
     :                       ' ^LAB values for parameter %^PAR.',
     :                       STATUS )
            END IF

*  Cancel the parameter value.
            CALL PAR_CANCL( PARAM, STATUS )

         END IF

      END DO

*  Tidy up.
 999  CONTINUE

*  If an error has occurred, return AST__BAD values.
      IF( STATUS .NE. SAI__OK ) THEN
         DO I = 1, MXVAL
            AXVAL( I ) = AST__BAD
         END DO
         NVAL = 0
      END IF

*  If a parameter abort value was supplied, re-report the error
*  with a more appropriate message.
      IF( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTAXV_3', 'Aborted attempt to obtain '//
     :                 'axis values using parameter %^PARAM.', STATUS )

*  If a parameter null value was supplied, re-report the error
*  with a more appropriate message.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__NULL
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTAXV_4', 'Aborted attempt to obtain '//
     :                 'axis values using parameter %^PARAM.', STATUS )

*  Add a context message to any other error.
      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_GTAXV_5', 'Failed to obtain axis '//
     :                 'values using parameter %^PARAM.', STATUS )
      END IF

      END
