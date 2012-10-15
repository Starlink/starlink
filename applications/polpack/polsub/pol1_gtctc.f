      SUBROUTINE POL1_GTCTC( PARAM, CI, TYPE, DEF, GI, STATUS )
*+
*  Name:
*     POL1_GTCTC

*  Purpose:
*     Obtain a CAT identifier for a catalogue component named by the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_GTCTC( PARAM, CI, TYPE, DEF, GI, STATUS )

*  Description:
*     This routine uses the specified environment parameter to obtain the
*     name of a catalogue component, selected from those available in the
*     supplied catalogue. A CAT identifier (see SUN/181) for the selected
*     component is returned.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     CI = INTEGER (Given)
*        A CAT identifier for the supplied catalogue.
*     TYPE = INTEGER (Given)
*        A symbolic constant specifying the type of component to be
*        obtained. Choose from CAT__FITYP (column), CAT__QITYP (parameter,
*        CAT__EITYP (expression), CAT__IITYPE (index), CAT__SITYP
*        (selection), CAT__JITYP (join), CAT__FETYP (vector column element).
*        These constants are defined in include file CAT_PAR.
*     DEF = CHARACTER * ( * ) (Given)
*        The name to use as the dynamic default for the parameter. No
*        dynamic default is set if this is blank.
*     GI = INTEGER (Returned)
*        The CAT identifier for the selected component. Returned equal to
*        CAT__NOID if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1998 (DSB):
*        Original version.
*     6-AUG-1998 (DSB):
*        Corrected reporting of available column names.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! CAT constants
      INCLUDE 'CAT_ERR'          ! CAT error constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER CI
      INTEGER TYPE
      CHARACTER DEF*(*)

*  Arguments Returned:
      INTEGER GI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CNAME*(CAT__SZCNM)! Catalogue name
      CHARACTER DESC*25          ! Textual description of component type
      CHARACTER NAME*(CAT__SZEXP)! String supplied by user
      CHARACTER TBUF*255         ! Buffer for error text
      INTEGER IAT                ! No. of characters in TBUF
      INTEGER IDTYP              ! Type of specified component
      INTEGER N                  ! Component index
      LOGICAL GOOD               ! Has a good component name been given?
*.

*  Initialise.
      GI = CAT__NOID

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store a textual description of the type of component required.
      IF( TYPE .EQ. CAT__CITYP ) THEN
         DESC = ' '
      ELSE IF( TYPE .EQ. CAT__FITYP ) THEN
         DESC = 'column'
      ELSE IF( TYPE .EQ. CAT__FETYP ) THEN
         DESC = 'vector column element'
      ELSE IF( TYPE .EQ. CAT__QITYP ) THEN
         DESC = 'parameter'
      ELSE IF( TYPE .EQ. CAT__EITYP ) THEN
         DESC = 'expression'
      ELSE IF( TYPE .EQ. CAT__SITYP ) THEN
         DESC = 'selection'
      ELSE IF( TYPE .EQ. CAT__JITYP ) THEN
         DESC = 'join'
      ELSE IF( TYPE .EQ. CAT__IITYP ) THEN
         DESC = 'index'
      ELSE
         DESC = 'component'
      END IF

*  Get the catalogue name for use in error messages.
      CALL CAT_TIQAC( CI, 'NAME', CNAME, STATUS )

*  If a default component name was supplied, set it.
      IF( DEF .NE. ' ' ) CALL PAR_DEF0C( PARAM, DEF, STATUS )

*  Loop until a good component name has been obtained, or an error occurs.
      GOOD = .FALSE.
      DO WHILE ( .NOT. GOOD .AND. STATUS .EQ. SAI__OK )

*  Get a value for the parameter.
         CALL PAR_GET0C( PARAM, NAME, STATUS )

*  Abort if an abort request or null value or "no user" error was obtained.
         IF( STATUS .EQ. PAR__ABORT .OR.
     :       STATUS .EQ. PAR__NULL .OR.
     :       STATUS .EQ. PAR__NOUSR ) GO TO 999

*  Attempt to get an identifier for the named item.
         CALL CAT_TIDNT( CI, NAME, GI, STATUS )

*  If it exists, check that the component is of the correct type.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL CAT_TIDTP( GI, IDTYP, STATUS )

*  If the component is not of the correct type, report an error.
            IF( IDTYP .NE. TYPE .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'DESC', DESC )
               CALL MSG_SETC( 'NAME', NAME )
               CALL ERR_REP( 'POL1_GTCTC_1', '^NAME is not a '//
     :                       'catalogue ^DESC.', STATUS )
            END IF

*  If the component was not found, annul the error.
         ELSE IF( STATUS .EQ. CAT__NOCMP ) THEN
            CALL ERR_ANNUL( STATUS )

*  If a column name was requested, we also allow expressions to be
*  supplied. Attempt to parse the suupleid string as an expression.
            IF( TYPE .EQ. CAT__FITYP ) THEN
               CALL CAT_EIDNT( CI, NAME, GI, STATUS )

*  If this also failed, annul the error and report a concise error message.
               IF( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  STATUS = CAT__NOCMP
                  CALL MSG_SETC( 'NAME', NAME )
                  CALL MSG_SETC( 'CNAME', CNAME )
                  CALL ERR_REP( 'POL1_GTCTC_2', 'Cannot use the '//
     :                          'column name or expression ''^NAME'' '//
     :                          'with catalogue ''^CNAME''.', STATUS )
               END IF

*  If the component is not a column, report a concise message.
            ELSE
               STATUS = CAT__NOCMP
               CALL MSG_SETC( 'DESC', DESC )
               CALL MSG_SETC( 'NAME', NAME )
               CALL MSG_SETC( 'CNAME', CNAME )
               CALL ERR_REP( 'POL1_GTCTC_3', 'No ^DESC named '//
     :                       '''^NAME'' was found in catalogue '//
     :                       '''^CNAME''.', STATUS )
            END IF

         END IF

*  If no error has occurred, we can leave the loop.
         IF( STATUS .EQ. SAI__OK ) THEN
            GOOD = .TRUE.

*  Otherwise, flush the error, release the component identifier, cancel
*  the parameter, display a list of valid component names, and go round
*  for a new value.
         ELSE
            CALL CAT_TRLSE( GI, STATUS )
            CALL ERR_FLUSH( STATUS )
            CALL PAR_CANCL( PARAM, STATUS )

            CALL CAT_TNDNT( CI, TYPE, 1, GI, STATUS )
            N = 1
            TBUF = ' '
            IAT = 0
            DO WHILE( GI .NE. CAT__NOID )
               CALL CAT_TIQAC( GI, 'NAME', NAME, STATUS )
               CALL CAT_TRLSE( GI, STATUS )
               CALL CHR_APPND( ' ', TBUF, IAT )
               CALL CHR_APPND( NAME, TBUF, IAT )
               CALL CHR_APPND( ',', TBUF, IAT )
               N = N + 1
               CALL CAT_TNDNT( CI, TYPE, N, GI, STATUS )
            END DO

            CALL MSG_SETC( 'DESCS', DESC )
            CALL MSG_SETC( 'DESCS', 's' )
            CALL MSG_SETC( 'NAMES', TBUF( : IAT ) )
            CALL MSG_OUT( 'POL1_GTCTC', 'The following catalogue '//
     :                    '^DESCS are available: ^NAMES', STATUS )

         END IF

      END DO

*  Arrive here if an error occurs.
 999  CONTINUE

*  Release the returned identifier if an error has occurred.
      IF( STATUS .NE. SAI__OK ) CALL CAT_TRLSE( GI, STATUS )

*  If a parameter abort or null value was supplied, re-report the error with
*  a more appropriate message.
      IF( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL MSG_SETC( 'DESC', DESC )
         CALL ERR_REP( 'POL1_GTCTC_4', 'Aborted attempt to obtain '//
     :                 'the name of a catalogue ^DESC using parameter'//
     :                 ' %^PARAM.', STATUS )

      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__NULL
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL MSG_SETC( 'DESC', DESC )
         CALL ERR_REP( 'POL1_GTCTC_5', 'Null value obtained for  '//
     :                 'the name of a catalogue ^DESC using parameter'//
     :                 ' %^PARAM.', STATUS )

*  Add a context message to any other error.
      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL MSG_SETC( 'DESC', DESC )
         CALL ERR_REP( 'POL1_GTCTC_6', 'Failed to obtain the name of '//
     :                 'a catalogue ^DESC using parameter %^PARAM.',
     :                 STATUS )
      END IF

      END
