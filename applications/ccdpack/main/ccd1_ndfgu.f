      SUBROUTINE CCD1_NDFGU( GID, NNDF, NAME, MINNDF, MAXNDF, STATUS )
*+
*  Name:
*     CCD1_NDFGU

*  Purpose:
*     To access a group of NDFs for UPDATE using IRG.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NDFGL( GID, NNDF, NAME, MAXNDF, MINNDF, STATUS )

*  Description:
*     The routine accesses a group of NDFs whose ADAM parameter is
*     given by 'NAME'. The NDF group identifier is returned in
*     GID. A minimum of MINNDF and up to MAXNDF entries are made to
*     the group. The actual number of NDFs is returned is NNDF.
*     The NDFs are set access for UPDATE.

*  Notes:
*     - the IRH system should be closed (by calling IRH_CLOSE ) after
*     use of the GID group has finished.

*  Arguments:
*     GID = INTEGER (Returned)
*        IRG identifier for the group of NDF names.
*     NNDF = INTEGER (Returned)
*        The number of NDF identifiers returned from user (less than
*        MAXNDF)
*     NAME = CHARACTER * ( * ) (Given)
*        The ADAM parameter name.
*     MINNDF = INTEGER (Given)
*        The minimum number of NDFs allowed.
*     MAXNDF = INTEGER (Given)
*        The maximum number of NDFs allowed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JUL-1992 (PDRAPER):
*        Changed to have a lower limit and access the group for
*        update. Based on CCD1_NDFGL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRG_FAC'          ! IRH and IRG system parameters
                                 ! (IRH__NOID) IRG error codes etc.
      INCLUDE 'PAR_ERR'          ! Parameter system error codes

*  Arguments Given:
      INTEGER MINNDF
      INTEGER MAXNDF
      CHARACTER NAME * ( * )

*  Arguments Returned:
      INTEGER GID
      INTEGER NNDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ADDED              ! Number of NDFs added this loop.
      LOGICAL TERM               ! Returned true if a termination
                                 ! character is issued.
      LOGICAL AGAIN              ! Controls looping for NDf names
      INTEGER NTRY               ! Number of attempts to get prompt
                                 ! right quit after 10, probably batch
                                 ! job in error.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the NDF names through IRG. Set GID to no previous entries.
*  Set the termination character to '-' if this is added to any lines
*  of data then a continuation line is used.
      GID = IRH__NOID
      NTRY = 0
 1    CONTINUE                   ! start of repeat until loop
         CALL IRG_GROUP( NAME, '-', 'UPDATE', GID, NNDF, ADDED, TERM,
     :                   STATUS )

*  Get out if a null return has been given or a par_abort. Also quit
*  after an unreasonble number of attempts.
         IF ( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT )THEN
            GO TO 99
         ELSE IF ( NTRY .GT. 10 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'CCD1_NDFGL',
     :      '  Unable to obtain valid list of NDF names using'//
     :      ' parameter %^NAME' , STATUS )
            GO TO 99
         END IF

*  Check that the number of NDFs returned it has to be greater than zero
*  and less than MAXNDF + 1. If the continuation character has been used
*  then reprompt.
         AGAIN = .FALSE.

*  Check that maximum number has not been exceeded, if so reset the
*  group and reprompt.
         IF ( NNDF .GT. MAXNDF ) THEN
             CALL MSG_SETI( 'MAXNDF', MAXNDF )
             CALL MSG_OUT( ' ', 'You cannot supply more than ^MAXNDF '//
     :                     'NDF names - try again', STATUS )

*  Reset everything and try again.
             CALL IRH_ANNUL( GID, STATUS )
             GID = IRH__NOID
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )
         ELSE IF ( NNDF .LT. MINNDF ) THEN
             CALL MSG_SETI( 'MINNDF', MINNDF )
             CALL MSG_OUT( ' ', 'You must supply at least ^MINNDF '//
     :                     'NDF names - try again', STATUS )

*  Reset everything and try again.
             CALL IRH_ANNUL( GID, STATUS )
             GID = IRH__NOID
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )
         ELSE IF ( TERM ) THEN

*  Continuation character has been issued reprompt, appending to the
*  present group.
            CALL PAR_CANCL( NAME, STATUS )
            AGAIN = .TRUE.
         ELSE IF ( NNDF .LT. 1 ) THEN

*  No continuation character and less than one NDF, without get out
*  request. Issue warning and try again.
             CALL MSG_OUT( ' ', 'You must supply at least one NDF',
     :                     STATUS )
             AGAIN = .TRUE.

*  Status may have been set by IRG for a good reason.. check for this
*  and reprompt.
         ELSE IF ( STATUS .EQ. IRG__BADFN ) THEN
            CALL PAR_CANCL( NAME, STATUS )
            AGAIN =.TRUE.

*  Reset everything and try again.
            CALL IRH_ANNUL( GID, STATUS )
            GID = IRH__NOID
            NTRY = NTRY + 1
            CALL PAR_CANCL( NAME, STATUS )
         END IF
      IF ( AGAIN ) GO TO 1

*  Write out how many NDFs we've got.
      CALL MSG_SETI( 'NDFAC_NNDF', NNDF )
      CALL MSG_SETC( 'PARNAME', NAME )
      IF ( NNDF .GT. 1 ) THEN
         CALL MSG_OUT( ' ',
     :   '  ^NDFAC_NNDF input NDFs accessed using parameter %^PARNAME',
     :    STATUS )
      ELSE
         CALL MSG_OUT( ' ',
     :   '  ^NDFAC_NNDF NDF accessed using parameter %^PARNAME',
     :   STATUS )
      END IF

 99   END
* $Id$
