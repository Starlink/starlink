      SUBROUTINE POL1_NWCTC( PARAM, CI, TYPE, DEF, DTYPE, GI, STATUS )
*+
*  Name:
*     POL1_NWCTC

*  Purpose:
*     Obtain a CAT identifier for a new catalogue component named by the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_NWCTC( PARAM, CI, TYPE, DEF, DTYPE, GI, STATUS )

*  Description:
*     This routine uses the specified environment parameter to obtain the
*     name of a new scalar parameter or column to create within the supplied
*     catalogue. A CAT identifier (see SUN/181) for the created component
*     is returned.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     CI = INTEGER (Given)
*        A CAT identifier for the supplied catalogue.
*     TYPE = INTEGER (Given)
*        A symbolic constant specifying the type of component to be
*        obtained. Choose from CAT__FITYP (column) or CAT__QITYP
*        (parameter). These constants are defined in include file CAT_PAR.
*     DEF = CHARACTER * ( * ) (Given)
*        The name to use as the dynamic default for the parameter. No
*        dynamic default is set if this is blank.
*     DTYPE = INTEGER (Given)
*        A symbolic constant specifying the data type of the component to
*        be created.
*     GI = INTEGER (Returned)
*        The CAT identifier for the created component. Returned equal to
*        CAT__NOID if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory.

*  Authors:
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     14-APR-2020 (DSB):
*        Original version.
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
      INTEGER DTYPE

*  Arguments Returned:
      INTEGER GI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CNAME*(CAT__SZCNM)! Catalogue name
      CHARACTER DESC*25          ! Textual description of component type
      CHARACTER NAME*(CAT__SZEXP)! String supplied by user
      LOGICAL GOOD               ! Has a good component name been given?
*.

*  Initialise.
      GI = CAT__NOID

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store a textual description of the type of component to be created,
*  and report an error for inappropriate types.
      IF( TYPE .EQ. CAT__FITYP ) THEN
         DESC = 'column'
      ELSE IF( TYPE .EQ. CAT__QITYP ) THEN
         DESC = 'parameter'
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'T', TYPE )
         CALL ERR_REP( ' ', 'POL1_NWCTC: Illegal type ^T supplied.',
     :                 STATUS )
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

*  If no error has occurred, attempt ot get an identifier for the named
*  component. This should fail since we assume the component does not
*  already exist.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL CAT_TIDNT( CI, NAME, GI, STATUS )

*  If the component was not there, annul the error.
            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )

*  If the component was there, report an error.
            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'N', NAME )
               CALL MSG_SETC( 'D', DESC )
               CALL ERR_REP( ' ', 'A ^D named ^N already exists.',
     :                       STATUS )
            END IF
         END IF

*  Attempt to create the named item, getting an identifier for it.
         CALL CAT_PNEW0( CI, TYPE, NAME, DTYPE, GI, STATUS )

*  If no error has occurred, we can leave the loop.
         IF( STATUS .EQ. SAI__OK ) THEN
            GOOD = .TRUE.

*  Otherwise, release the component identifier, flush the error, cancel
*  the parameter and go round for a new value.
         ELSE
            CALL CAT_TRLSE( GI, STATUS )
            CALL MSG_SETC( 'N', CNAME )
            CALL MSG_SETC( 'D', DESC )
            CALL ERR_REP( ' ', 'Failed to create a new ^D in ^N.',
     :                    STATUS )
            CALL ERR_FLUSH( STATUS )
            CALL PAR_CANCL( PARAM, STATUS )
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
         CALL ERR_REP( 'POL1_NWCTC_4', 'Aborted attempt to obtain '//
     :                 'the name of a new catalogue ^DESC using '//
     :                 'parameter %^PARAM.', STATUS )

      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__NULL
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL MSG_SETC( 'DESC', DESC )
         CALL ERR_REP( 'POL1_NWCTC_5', 'Null value obtained for the '//
     :                 'name of a new catalogue ^DESC using parameter'//
     :                 ' %^PARAM.', STATUS )

*  Add a context message to any other error.
      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL MSG_SETC( 'DESC', DESC )
         CALL ERR_REP( 'POL1_NWCTC_6', 'Failed to obtain the name of '//
     :                 'a new catalogue ^DESC using parameter %^PARAM.',
     :                 STATUS )
      END IF

      END
