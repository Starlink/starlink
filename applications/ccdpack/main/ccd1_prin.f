      SUBROUTINE CCD1_PRIN( PROMPT, NDFS, NNDF, FTYPES, FILT, DARK, 
     :                      FLASH, STATUS )
*+
*  Name:
*     CCD1_PRIN

*  Purpose:
*     Gets a list of NDF names their frame types and filter type
*     or exposure factors.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_PRIN( PROMPT, NDFS, NNDF, FTYPES, FILT, DARK, FLASH, 
*                     STATUS)

*  Description:
*     This routine intercepts an IRH group from the user and interprets
*     its contents as a list of NDF names together with its frame type,
*     filter type, dark time and pre-flash exposure. Filters are
*     relevant to target and flatfields, exposures to darks or
*     flashes. A null (!)  character for any of the final three fields
*     is accepted and will mean that they are ignored. The input from
*     the user is an IRH group with all these five fields present for
*     each NDF (hence this is really intended for use from a procedure
*     were constructing such a list is not tedious). The outputs are an
*     IRG group of NDFs, an IRH group of their frame types and a group
*     containing the filters a group containing the dark times and a
*     group containing the flash times.

*  Arguments:
*     PROMPT = CHARACTER * ( * ) (Given)
*        The prompt to use when getting the IRH group from the user.
*     NDFS = INTEGER (Returned)
*        An IRG group identifier for the NDFs.
*     NNDF = INTEGER (Returned)
*        The number of NDFs given.
*     FTYPES = INTEGER (Returned)
*        An IRH identifier with a list of the NDF frame types.
*     FILT = INTEGER (Returned)
*        Group of the NDF filter types. ! indicates none.
*     DARK = INTEGER (Returned)
*        Group of the NDF dark exposure times. ! indicates none.
*     FLASH = INTEGER (Returned)
*        Group of the NDF pre-flash exposure time. ! indicates none.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1994 (PDRAPER):
*        Original version.
*     24-MAY-1994 (PDRAPER):
*        Added dark and pre-flash arguments.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters
      INCLUDE 'FIO_ERR'          ! FIO system error codes.
      INCLUDE 'IRG_FAC'          ! IRG and IRH facility errors and parameters
      INCLUDE 'PAR_ERR'          ! Parameter system error codes

*  Arguments Given:
      CHARACTER * ( * ) PROMPT

*  Arguments Returned:
      INTEGER NDFS
      INTEGER NNDF
      INTEGER FTYPES
      INTEGER FILT
      INTEGER DARK
      INTEGER FLASH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDH1               ! Dummy IRH group
      INTEGER IDH2               ! Group of given names
      INTEGER NTRY               ! Number of tries
      INTEGER INDEX              ! Loop index
      LOGICAL RENEW              ! Whether to renew input group
      LOGICAL TERM               ! Input has terminator flagged 
      LOGICAL AGAIN              ! Loop again or not.
      INTEGER ADDED              ! How many values have been added
      INTEGER NRET               ! Number of values returned
      CHARACTER * ( IRH__SZNAM ) CURSTR ! Item read from group
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access a number of character strings using IRH. These should
*  represent the NDF the frame type and the associated factor.
      NTRY = 0
      IDH1 = IRH__NOID
      CALL IRH_NEW( 'CCD1_PRINT:INPUT', IDH2, STATUS )
      RENEW = .FALSE.
 3    CONTINUE                   ! Start of DO UNTIL

*  Create a new group to associate names with if this isn't the first
*  time and a problem has occurred.
         IF ( RENEW ) THEN
            CALL IRH_ANNUL( IDH2, STATUS )
            CALL IRH_NEW( 'CCD1_PRINT:INPUT', IDH2, STATUS )
            RENEW = .FALSE.
         END IF

*  Get the user return.
         TERM = .FALSE.
         ADDED = -1
         CALL IRH_GROUP( PROMPT, IDH1, IDH2, '-', NRET, ADDED, TERM,
     :                   STATUS )

*  Get out if a null return has been given or a PAR__ABORT. Also quit
*  after an unreasonble number of attempts.
         IF ( STATUS .EQ. PAR__ABORT )THEN
            GO TO 99
         ELSE IF ( NTRY .GT. 10 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', PROMPT )
            CALL ERR_REP( 'CCD1_PRIN1',
     :      '  Unable to obtain valid list of values using parameter'//
     :      ' %^NAME ' , STATUS )
            GO TO 99
         END IF

*  Check that the number of values returned. This has to be between the
*  the limits 5 and 5*CCD1__MXNDF. If the continuation character has
*  been used then reprompt if appropriate. etc.
         AGAIN = .FALSE.
         IF ( NRET .GT. 5 * CCD1__MXNDF ) THEN

*  Comment on this and try again.
            CALL MSG_SETI( 'MAXVAL', 5 * CCD1__MXNDF  )
            CALL MSG_OUT( ' ',
     :'Too many values given only ^MAXVAL allowed - try again', STATUS )

*  Reset everything ready for next attempt.
            RENEW = .TRUE.
            IDH1 = IRH__NOID
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( PROMPT, STATUS )
         ELSE IF ( NRET .LT. 5 ) THEN

*  Comment on this futile exercise.
            CALL MSG_OUT( ' ',
     :'Too few values given need multiple of 5 - try again', STATUS )

*  Reset everything ready for next attempt.
            RENEW = .TRUE.
            IDH1 = IRH__NOID
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( PROMPT, STATUS )
         ELSE IF ( TERM ) THEN

*  Continuation character has been issued reprompt, appending to the
*  present group.
            AGAIN = .TRUE.
            CALL PAR_CANCL( PROMPT, STATUS )

*  Status may have been set by IRH for a good reason. Check for this
*  and reprompt. (Note FIO system filename and file not found errors
*  these are not captured by IRG).
         ELSE IF ( STATUS .EQ. IRG__BADFN .OR. STATUS .EQ. IRG__NOFIL
     :        .OR. STATUS .EQ. FIO__NAMER .OR. STATUS .EQ. FIO__FILNF )
     :   THEN

*  Issue the error.
            CALL ERR_FLUSH( STATUS )

*  Reset everything and try again.
            RENEW = .TRUE.
            IDH1 = IRH__NOID
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( PROMPT, STATUS )

*  Try to trap a special case of ' ' string return. This should leave
*  added unmodified. This will be taken as a request to exit. The normal
*  stop entry request from an blank line will be `!' .
         ELSE IF ( ADDED .EQ. -1 .AND. .NOT. TERM ) THEN
            AGAIN = .FALSE.

*  If status is set to PAR__NULL then reset status, note that this is
*  only tested if the checks for number of values etc. have been passed.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            AGAIN = .FALSE.

         ELSE IF ( MOD ( NRET, 5 ) .NE. 0 ) THEN

*  Return must be a multiple of 5. Check this.
            CALL MSG_SETI( 'NRET', NRET )
            CALL MSG_OUT( ' ',
     :'Can only give a multiple of 5 fields (^NRET given) - try again',
     :      STATUS )

*  Reset everything ready for next attempt.
            RENEW = .TRUE.
            IDH1 = IRH__NOID
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( PROMPT, STATUS )
         END IF

*  End of DO UNTIL.
      IF ( AGAIN ) GO TO 3
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Extract the fields from the input and try to access the NDFs. The 
*  NDF types are entered into their own group as is the final field.

*  Create a group consisting of only the NDF names.
      CALL IRH_NEW( 'CCD1_PRINT:NDFS', NDFS, STATUS )
      NNDF = 0 
      DO 4 INDEX = 1, NRET, 5

*  Get the NDF name string.
         CALL IRH_GET( IDH2, INDEX, 1, CURSTR, STATUS )

*  Enter this into the group.
         CALL IRH_PUT( NDFS, 1, CURSTR, 0, STATUS )
         NNDF = NNDF + 1
 4    CONTINUE

*  Now try to import this as an NDF group.
      IF ( STATUS .EQ. SAI__OK ) THEN 
         CALL IRG_GIN( NDFS, .FALSE., 'UPDATE', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Error importing the NDFs, add a further error message.
            CALL ERR_REP( 'CCD_PRIN2', 'NDF does not exist', STATUS )
         END IF
      END IF

*  Now create a group for the NDF frame types.
      CALL IRH_NEW( 'CCD1_PRIN:FTYPES', FTYPES, STATUS )

*  And extract the frame types.
      DO 5 INDEX = 2, NRET, 5
         CALL IRH_GET( IDH2, INDEX, 1, CURSTR, STATUS )
         CALL CHR_UCASE( CURSTR )
         CALL CHR_LDBLK( CURSTR )
         CALL IRH_PUT( FTYPES, 1, CURSTR, 0, STATUS )
 5    CONTINUE

*  Extract the filter types.
      CALL IRH_NEW( 'CCD1_PRIN:FILT', FILT, STATUS )
      DO 6 INDEX = 3, NRET, 5
         CALL IRH_GET( IDH2, INDEX, 1, CURSTR, STATUS )
         CALL CHR_LDBLK( CURSTR )
         CALL IRH_PUT( FILT, 1, CURSTR, 0, STATUS )
 6    CONTINUE

*  Extract the dark times.
      CALL IRH_NEW( 'CCD1_PRIN:DARK', DARK, STATUS )
      DO 7 INDEX = 4, NRET, 5
         CALL IRH_GET( IDH2, INDEX, 1, CURSTR, STATUS )
         CALL IRH_PUT( DARK, 1, CURSTR, 0, STATUS )
 7    CONTINUE

*  Extract the pre-flash times.
      CALL IRH_NEW( 'CCD1_PRIN:FLASH', FLASH, STATUS )
      DO 8 INDEX = 5, NRET, 5
         CALL IRH_GET( IDH2, INDEX, 1, CURSTR, STATUS )
         CALL IRH_PUT( FLASH, 1, CURSTR, 0, STATUS )
 8    CONTINUE

*  Exit with error label.
 99   CONTINUE
      END
