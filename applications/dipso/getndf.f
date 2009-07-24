      SUBROUTINE GETNDF( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, MODE,
     :                   SUFFIX, INDF, STATUS )
*+
* Name:
*     GETNDF

*  Purpose:
*     Obtain an identifier for an existing NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GETNDF( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, MODE, SUFFIX,
*                  INDF, STATUS )

*  Description:
*     The root name of an NDF is obtained either from the supplied list
*     of parameter values or from the user. The complete name is formed
*     by appending the supplied suffix (if any) to the root name. This
*     is only done if the root name does not already end with the
*     suffix. An attempt is made to open the NDF with the specified
*     access mode. If succesful, the NDF title is displayed and the
*     identifier is returned, otherwise the user is re-prompted.

*  Arguments:
*     PARAMS = CHARACTER * ( * ) (Given)
*        A string containing the supplied command parameters.
*     POS = INTEGER (Given)
*        The index of the required parameter within the list of all
*        possible parameters. 
*     OPT = LOGICAL (Given)
*        Is the parameter an optional parameter? If so, then the
*        supplied default value will be used if no value has
*        been supplied. Otherwise, the user is prompted if no value
*        has been supplied.
*     COMM = CHARACTER * ( * ) (Given)
*        The command name.
*     PROMPT = CHARACTER * ( * ) (Given)
*        The prompt string.
*     DEFVAL = CHARACTER * ( * ) (Given)
*        The default root NDF name (i.e. without the suffix).
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode required READ, WRITE or UPDATE.
*     SUFFIX = CHARACTER * ( * ) (Given)
*        A suffix for the NDF name. 
*     INDF = INTEGER (Returned)
*        The NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If an error occurs, a null value is returned for INDF.
      
*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     {original_version_entry}
*     13-DEC-1995 (DSB):
*        Use NDF_OPEN instead of HDS_OPEN, and remove LOC argument.
*     23-JUL-2009 (TIMJ):
*        Use MSG_FLEVOK rather than MSG_IFLEV

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      
*  Arguments Given:
      CHARACTER * ( * ) PARAMS
      INTEGER POS
      LOGICAL OPT
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) PROMPT
      CHARACTER * ( * ) DEFVAL
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) SUFFIX

*  Arguments Returned:
      INTEGER INDF
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        NDFNM*255,         ! NDF name
     :        ROOT*255,          ! NDF root name
     :        TITLE*80           ! NDF title
      
      INTEGER
     :        PLACE              ! A dummy NDF place holder (NDF__NOPL)      

      LOGICAL
     :        ISVERB             ! Is VERBOSE or DEBUG
*.

*  Ensure a null NDF identifier get returned if an error has 
*  already occurred.
      INDF = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Defer the reporting of errors.
      CALL ERR_MARK

*  Check the MSG message filtering level.
      ISVERB = MSG_FLEVOK( MSG__VERB, STATUS )

*  Establish the shell used to expand shell meta-characters within HDS. 
*  This will be the first available in the list: tcsh,csh,sh.
      CALL HDS_TUNE( 'SHELL', 2, STATUS )

*  Get the root NDF name.
      CALL GET0C( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, ROOT,
     :            STATUS )

*  Ensure the NDF name ends with the supplied suffix.
      CALL NDFNAM( ROOT, SUFFIX, NDFNM, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999      

*  Jump to here if a new NDF name has been obtained.
 10   CONTINUE
      
*  Open the NDF.
      CALL NDF_OPEN( DAT__ROOT, NDFNM, MODE, 'OLD', INDF, PLACE, 
     :               STATUS )

*  If an error has occured, 
      IF( STATUS .NE. SAI__OK ) THEN 

*  If the MSG message filtering level is verbose flush all the error
*  messages. Otherwise, annul them.
         IF( ISVERB ) THEN
            CALL ERR_FLUSH( STATUS )
         ELSE
            CALL ERR_ANNUL( STATUS )
         END IF

*  Add a context message and flush it.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GETNDF_ERR1', 'Failed to get access to an '//
     :                 'existing NDF.', STATUS )
         CALL ERR_FLUSH( STATUS )

*  Get a new root NDF name from the user.
         CALL RDSTR( COMM, PROMPT, DEFVAL, ROOT, STATUS )

*  Ensure the NDF name ends with the supplied suffix.
         CALL NDFNAM( ROOT, SUFFIX, NDFNM, STATUS )

*  If a new name was supplied, go round to try to open the NDF.
         IF( STATUS .EQ. SAI__OK ) GO TO 10         

*  If an NDF has been obtained succesfully, display its title (unless
*  it is blank or undefined).
      ELSE
         TITLE = ' '
         CALL NDF_CGET( INDF, 'TITLE', TITLE, STATUS )

         IF( TITLE .NE. ' ' ) THEN
            CALL MSG_SETC( 'TITLE', TITLE )         
            CALL MSGOUT( COMM, 'NDF title - ''^TITLE''', .FALSE.,
     :                   STATUS )
         END IF

      END IF

*  Re-report null and abort messages with more friendly messages.
 999  CONTINUE

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__NULL
         CALL ERR_REP( 'GETNDF_ERR', 'Null NDF structure specified.',
     :                 STATUS )

      ELSE IF( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL ERR_REP( 'GETNDF_ERR', 'Aborted attempt to open an ' //
     :                 'existing NDF structure.', STATUS )

      END IF

*  If an error has been reported, attempt to annul the NDF identifier.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_ANNUL( INDF, STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE      

      END
