      SUBROUTINE CCD1_NDFPR( NAME, NDFIN, CLIST, NDFOUT, STATUS )
*+
*  Name:
*     CCD1_NDFPR

*  Purpose:
*     To propagate an NDF using the IRG system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NDFPR( NAME, NDFIN, CLIST, NDFOUT, STATUS )

*  Description:
*     The routine creates a new NDF propagating the components as
*     specified in clist (see SUN/33 for description of clist). The
*     output NDF name is obtained through the IRG system. This is done
*     to keep a uniformity of interface, between all NDF commands specs
*     and prompts (i.e. can be enclosed in quotes, read from a file
*     etc.). The propagation actually occurs using the IRG_NDFPR
*     routine.

*  Notes:
*     - the routine uses the CCDPACK IRH continuation character '-'
*     to force reprompting. Although in this case this will not be of
*     any immediate use.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The ADAM parameter name.
*     NDFIN = INTEGER (Given)
*        The NDF identifier of the NDF to be propagated.
*     CLIST = CHARACTER * ( * ) (Given)
*        The component propagation list (see SUN/33).
*     NDFOUT = INTEGER (Given)
*        The identifier of the output NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUL-1991 (PDRAPER):
*        Original Version.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator code (no longer used as HDS closes
*        files correctly).
*     {enter_further_changes_here}

*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRG_FAC'          ! IRH and IRG system parameters
                                 ! (IRH__NOID) IRG error codes etc.
      INCLUDE 'PAR_ERR'          ! Parameter system error codes
      INCLUDE 'FIO_ERR'          ! FIO system error codes
      INCLUDE 'CCD1_PAR'         ! CCDPACK system constants

*  Arguments Given:
      CHARACTER NAME * ( * )
      CHARACTER CLIST * ( * )
      INTEGER NDFIN

*  Arguments Returned:
      INTEGER NDFOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER GID                ! IRG group identifier
      INTEGER ADDED              ! Number of NDFs added this loop.
      LOGICAL TERM               ! Returned true if a termination
                                 ! character is issued.
      LOGICAL AGAIN              ! Controls looping for NDf names
      INTEGER NTRY               ! Number of attempts to get prompt
                                 ! right quit after 10, probably batch
                                 ! job in error.
      INTEGER NNDF               ! Number of NDFs returned.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the NDF name through IRG. Set GID to no previous entries.
*  Set the termination character to '-' if this is added to any lines
*  of data it will be stripped and ignored.
      GID = IRH__NOID
      NTRY = 0
 1    CONTINUE                   ! start of repeat until loop
         TERM = .FALSE.
         ADDED = -1
         CALL IRG_CREAT( NAME, IRH__NOID, '-', GID, NNDF, ADDED, TERM,
     :                   STATUS )

*  Get out if a given a par_abort. Also quit after an unreasonble
*  number of attempts.
         IF ( STATUS .EQ. PAR__ABORT )THEN
            GO TO 99
         ELSE IF ( NTRY .GT. 10 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'CCD1_NDFPR',
     :      '  Unable to obtain valid list of NDF names using'//
     :      ' parameter %^NAME' , STATUS )
            GO TO 99
         END IF

*  Check that the number of NDFs returned is in the permitted range. If
*  the continuation character has been used ignore it IRG has stripped
*  it off.
         AGAIN = .FALSE.

*  One NDF only is allowed.
         IF ( NNDF .GT. 1 ) THEN
             CALL MSG_SETI( 'MAXVAL', 1 )
             CALL MSG_OUT( ' ', 'You cannot supply more than ^MAXVAL '//
     :                     'NDF names - try again', STATUS )

*  Reset everything and try again.
             CALL IRH_ANNUL( GID, STATUS )
             GID = IRH__NOID
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )
         ELSE IF ( TERM ) THEN

*  Continuation character has been issued ignore it.
            CALL PAR_CANCL( NAME, STATUS )
            AGAIN = .TRUE.

*  Status may have been set by IRG for a good reason. Check for this
*  and reprompt. (Note FIO system filename and file not found errors
*  these are not captured by IRG).
         ELSE IF ( STATUS .EQ. IRG__BADFN .OR. STATUS .EQ. IRG__NOFIL
     :        .OR. STATUS .EQ. FIO__NAMER .OR. STATUS .EQ. FIO__FILNF )
     :   THEN

*  Issue the error.
             CALL ERR_FLUSH( STATUS )

*  Reset everything and try again.
             CALL IRH_ANNUL( GID, STATUS )
             GID = IRH__NOID
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )

*  Try to trap a special case of ' ' string return. This should leave
*  added unmodified. This will be taken as a request to exit. The normal
*  stop entry request from an blank line will be `!' .
         ELSE IF ( ( ADDED .EQ. -1 ) .AND. ( .NOT. TERM ) ) THEN
            AGAIN = .FALSE.

*  only tested if the checks for number of NDFs etc. have been passed.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            AGAIN = .FALSE.
         END IF
      IF ( AGAIN ) GO TO 1

*  If we've got a valid list then get the NDF identifier.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL IRG_NDFPR( GID, 1, NDFIN, CLIST, NDFOUT, STATUS )
      END IF

 99   END
* $Id$
