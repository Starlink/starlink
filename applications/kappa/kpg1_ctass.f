      SUBROUTINE KPG1_CTASS( PARAM, MODE, CI, STATUS )
*+
*  Name:
*     KPG1_CTASS 

*  Purpose:
*     Open an existing catalogue, obtaining the name of the catalogue
*     through the environment.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL KPG1_CTASS( PARAM, MODE, CI, STATUS)

*  Description:
*     Open an existing catalogue, obtaining the name of the catalogue
*     through the environment. If an existing catalogue is opened with
*     MODE = 'WRITE' then it is overwritten.
*
*     This is like CAT_ASSOC except that the verbose CAT error messages are
*     replaced with a more succinct error message.

*  Arguments:
*     PARAM  =  CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter from which the catalogue name will
*        be obtained.
*     MODE  =  CHARACTER * ( * ) (Given)
*        Mode in which the catalogue will be accessed.  One of:
*        READ   -  the catalogue may only be read from,
*        WRITE  -  a new catalogue is to be written.
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
      CHARACTER MODE*(*)

*  Arguments Returned:
      INTEGER CI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER PARIND             ! Parameter table inde
      INTEGER LSTAT              ! Local copy of ADAM status
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
*  proceed only if ok.
         CALL SUBPAR_GETNAME( PARIND, CNAME, STATUS )
         IF( STATUS .EQ. SAI__OK ) THEN

*  Attempt to open the catalogue.
            CALL CAT_TOPEN( CNAME, 'OLD', MODE, CI, STATUS )

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
               CALL ERR_REP( 'KPG1_CTCRE_ERR1', 'Unable to open an '//
     :                       'existing catalogue called ''^NAME'' '//
     :                       'using parameter %^PAR.', STATUS )
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
            CALL ERR_REP( 'KPG1_CTCRE_ERR2', 'Aborted attempt to open'//
     :                    ' an existing catalogue using parameter '//
     :                    '%^PAR.', STATUS )

*   Do the same for null parameter values.
         ELSE IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

            STATUS = PAR__NULL
            CALL MSG_SETC( 'PAR', PARAM )
            CALL ERR_REP( 'KPG1_CTCRE_ERR3', 'Aborted attempt to open'//
     :                    ' an existing catalogue using parameter '//
     :                    '%^PAR.', STATUS )

*  Add a context message to any other error.
         ELSE 
            CALL MSG_SETC( 'PAR', PARAM )
            CALL ERR_REP( 'KPG1_CTCRE_ERR4', 'Failed to open an '//
     :                    'existing catalogue using parameter %^PAR.', 
     :                    STATUS )
         END IF

      END IF

      END
