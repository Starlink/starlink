      SUBROUTINE KPG1_CTCRE( PARAM, CI, STATUS )
*+
*  Name:
*     KPG1_CTCRE

*  Purpose:
*     Create a new CAT catalogue, obtaining the catalogue name through
*     the environment.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL KPG1_CTCRE( PARAM, CI, STATUS)

*  Description:
*     Create a new CAT catalogue, obtaining the catalogue name through
*     the environment. This is like CAT_CREAT except that error reporting
*     is more succinct.

*  Arguments:
*     PARAM  =  CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter from which the catalogue name will
*        be obtained.
*     CI  =  INTEGER (Returned)
*        Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)

*  History:
*     29-OCT-1998 (DSB):
*        Original version.

*-
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'CAT_PAR'
      INCLUDE 'PAR_ERR'

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      INTEGER CI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER PARIND             ! Parameter table index
      LOGICAL MORE               ! Continue looping for catalogue?
      CHARACTER CNAME*(CAT__SZCNF) ! Catalogue name (inc. directory spec.)
*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Attempt to find the index for the parameter in the parameter tables.
      CALL SUBPAR_FINDPAR( PARAM, PARIND, STATUS )

*  Loop until a catalogue has been opened successfully or a non-recoverable 
*  error occurs.
      MORE = .TRUE.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Attempt to obtain a name for the catalogue from the parameter system and 
*  only proceed if ok.
         CALL SUBPAR_GETNAME( PARIND, CNAME, STATUS )
         IF( STATUS .EQ. SAI__OK ) THEN

*  Attempt to open the catalogue.
            CALL CAT_TOPEN( CNAME, 'NEW', 'WRITE', CI, STATUS )

*  If the status is ok then the catalogue opened successfully and the 
*  termination flag can be set. Otherwise an error must be reported and 
*  looping continues.
            IF( STATUS .EQ. SAI__OK ) THEN
               MORE = .FALSE.

            ELSE

*  The given catalogue could not be opened and the user must be reprompted.  
*  The procedure for handling the error is:
*     - annul the extremely verbose CAT error messages.
*     - report a more succinct error message.
*     - cancel the parameter association 
               CALL ERR_ANNUL( STATUS )
               CALL MSG_SETC( 'PAR', PARAM )
               CALL MSG_SETC( 'NAME', CNAME )

               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPG1_CTCRE_ERR1', 'Unable to create '//
     :                       'catalogue ''^NAME'' using parameter '//
     :                       '%^PAR (does the catalogue already '//
     :                       'exist?).', STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL SUBPAR_CANCL( PARIND, STATUS )
            END IF

         END IF

      END DO

*  If an error occurred...
      IF( STATUS .NE. SAI__OK ) THEN

*  Return a null identifier.
         CI = CAT__NOID

*  If an abort was requested, then annul any error messages and substitute a 
*  more appropriate one.
         IF( STATUS .EQ. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )

            STATUS = PAR__ABORT
            CALL MSG_SETC( 'PAR', PARAM )
            CALL ERR_REP( 'KPG1_CTCRE_ERR2', 'Aborted attempt to '//
     :                    'create a new catalogue using parameter '//
     :                    '%^PAR.', STATUS )

*   Do the same for null parameter values.
         ELSE IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

            STATUS = PAR__NULL
            CALL MSG_SETC( 'PAR', PARAM )
            CALL ERR_REP( 'KPG1_CTCRE_ERR3', 'Aborted attempt to '//
     :                    'create a new catalogue using parameter '//
     :                    '%^PAR.', STATUS )

*  Add a context message to any other error.
         ELSE 
            CALL MSG_SETC( 'PAR', PARAM )
            CALL ERR_REP( 'KPG1_CTCRE_ERR4', 'Failed to create a new '//
     :                    'catalogue using parameter %^PAR.', STATUS )
         END IF

      END IF

      END
