      SUBROUTINE CCD1_NDFPG( NAME, GIDIN, NVALS, GIDOUT, STATUS )
*+
*  Name:
*     CCD1_NDFPG

*  Purpose:
*     To create an output group of NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NDFPG( NAME, GIDIN, NVALS, GIDOUT, STATUS )

*  Description:
*     The routine calls the IRG package routine IRG_CREAT to create
*     a group of output NDF names. The group GIDIN is used as a
*     modification group on which to base the output names. I.e.
*     *_tmp means that all the output NDFs are to have the same names as
*     the input group except that there will be a trailing _TMP on
*     every name. The number of output NDF names is restricted to be
*     exactly those of the input group.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter to be used for accessing the
*        input data string.
*     GIDIN = INTEGER (Given)
*        The IRG group identifier of the NDFs to be used as modification
*        elements for the output group.
*     NVALS = INTEGER (Given)
*        The number of NDF names required for the output group (same as
*        the number in the input group).
*     GIDOUT = INTEGER (Returned)
*        Group identifier for the output list of NDF names.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-JUL-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRG_FAC'          ! IRG and IRH system status constants
      INCLUDE 'PAR_ERR'          ! Parmeter system error codes
      INCLUDE 'FIO_ERR'          ! FIO system status values

*  Arguments Given:
      CHARACTER * ( * ) NAME
      INTEGER GIDIN
      INTEGER NVALS

*  Arguments Returned:
      INTEGER GIDOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NNDF               ! Number of output NDFs
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

*  Get the names of the output NDFs.
      GIDOUT = IRH__NOID
      NTRY = 0
 1    CONTINUE
         TERM = .FALSE.
         ADDED = -1
         CALL IRG_CREAT( NAME, GIDIN, '-', GIDOUT, NNDF, ADDED, TERM,
     :                   STATUS )

*  Get out if a given a par_abort. Also quit after an unreasonble
*  number of attempts.
         IF ( STATUS .EQ. PAR__ABORT )THEN
            GO TO 99
         ELSE IF ( NTRY .GT. 10 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'CCD1_NDFPG',
     :      '  Unable to obtain valid list of NDF names using'//
     :      ' parameter %^NAME' , STATUS )
            GO TO 99
         END IF

*  Check that the number of NDFs returned is equal to the number
*  required. If the continuation character has been used then reprompt.
         AGAIN = .FALSE.

*  Check that maximum number has not been exceeded, if so reset the
*  group and reprompt.
         IF ( NNDF .GT. NVALS ) THEN
             CALL MSG_SETI( 'MAXVAL', NVALS )
             CALL MSG_OUT( ' ', 'You cannot supply more than ^MAXVAL '//
     :                     'NDF names - try again', STATUS )

*  Reset everything and try again.
             CALL IRH_ANNUL( GIDOUT, STATUS )
             GIDOUT = IRH__NOID
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )

*  If not enough NDFs have been given then ask for some more.
         ELSE IF ( NNDF .LT. NVALS ) THEN
             CALL MSG_SETI( 'MAXVAL', NVALS )
             CALL MSG_OUT( ' ', 'You must supply ^MAXVAL '//
     :                     'NDF names - try again', STATUS )

*  and try again.
             CALL IRH_ANNUL( GIDOUT, STATUS )
             GIDOUT = IRH__NOID
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )
         ELSE IF ( TERM ) THEN

*  Continuation character has been issued reprompt, appending to the
*  present group.
            CALL PAR_CANCL( NAME, STATUS )
            AGAIN = .TRUE.

*  Status may have been set by IRG for a good reason.. check for this
*  and reprompt. (Note FIO system filename and file not found errors
*  these are not captured by IRG).
         ELSE IF ( STATUS .EQ. IRG__BADFN .OR. STATUS .EQ. IRG__NOFIL
     :        .OR. STATUS .EQ. FIO__NAMER .OR. STATUS .EQ. FIO__FILNF )
     :   THEN

*  Issue the error.
            CALL ERR_FLUSH( STATUS )

*  Reset everything and try again.
            CALL IRH_ANNUL( GIDOUT, STATUS )
            GIDOUT = IRH__NOID
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( NAME, STATUS )

*  Try to trap a special case of ' ' string return. This should leave
*  added unmodified. This will be taken as a request to exit. The normal
*  stop entry request from an blank line will be `!' .
         ELSE IF ( ( ADDED .EQ. -1 ) .AND. ( .NOT. TERM ) ) THEN
            AGAIN = .FALSE.

*  If status is set to par__null then reset status, note that this is
*  only tested if the checks for number of NDFs etc. have been passed.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            AGAIN = .FALSE.
         END IF
      IF ( AGAIN ) GO TO 1

99    END
* $Id$
