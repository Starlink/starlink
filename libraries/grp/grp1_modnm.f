      SUBROUTINE GRP1_MODNM( SLOT2, ELEM, DEPTH, IFILE, STATUS )
*+
*  Name:
*     GRP1_MODNM

*  Purpose:
*     Perform specified editing on a single specified name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_MODNM( SLOT2, ELEM, DEPTH, IFILE, STATUS )

*  Description:
*     Modification elements may contain a literal string instead of
*     a NAME_TOKEN character. In this case the editing specified by
*     the strings between the SEPARATOR characters is applied to the
*     literal string preceeding the first separator (i.e. all
*     occurrences of the string between the first pair of separators is
*     replaced by the string between the second pair of separators),
*     and the resulting single name is added to the end of the group.
*     See routine GRP1_MODIF for more information about modification
*     elements. This routine assumes that there is at least one
*     separator character in the supplied element, and that the
*     SEPARATOR control character is not currently set to the NULL
*     character.
*
*  Arguments:
*     SLOT2 = INTEGER (Given)
*        A GRP slot number for the output group. The control characters
*        associated with this group are used to define the syntax of the
*        modification element.
*     ELEM = CHARACTER * ( * ) (Given)
*        The text of the modification element.
*     DEPTH = INTEGER (Given)
*        The indirection depth at which the modification element was
*        given.
*     IFILE = INTEGER (Given)
*        The index within the FILES array (see routine GRP1_PTIND) at
*        which the indirection file in which the modification element
*        was given is stored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.
      INCLUDE 'GRP_ERR'          ! GRP error values.

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_UPPER( GRP__MAXG ) = LOGICAL (Read)
*           If true, then all names in the group should be converted
*           to upper case before being used. Otherwise, they are left
*           as they are.

*  Arguments Given:
      INTEGER SLOT2
      CHARACTER ELEM*(*)
      INTEGER DEPTH
      INTEGER IFILE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a string.


*  Local Variables:
      CHARACTER MSPC*1           ! Character used to separate
                                 ! substitution strings within the
                                 ! modification element.
      LOGICAL   MSPOK            ! .TRUE. if MSPC can be used.
      CHARACTER NEW*(GRP__SZNAM) ! The text to be substituted for the
                                 ! text held in OLD.
      INTEGER   NEWLEN           ! Used length of NEW.
      CHARACTER NEWNAM*(GRP__SZNAM)! Name after substitution of NEW for
                                 ! OLD.
      LOGICAL   NEWNUL           ! .TRUE. if the replacement string is
                                 ! null.
      INTEGER   NLEN             ! Used length of NEWNAM.
      INTEGER   NSUB             ! No. of substitutions made.
      CHARACTER OLD*(GRP__SZNAM) ! The string to be replaced by NEW.
      INTEGER   OLDLEN           ! Used length of OLD.
      INTEGER   SEP1             ! The position of the first separator
                                 ! character within the given element.
      INTEGER   SEP2             ! The position of the second separator
                                 ! character within the given element.
      INTEGER   SEP3             ! The position of the third separator
                                 ! character within the given element.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the groups current SEPARATOR control character.
      CALL GRP1_CONC( SLOT2, GRP__PMSPC, MSPC, MSPOK, STATUS )

*  Get the position of the first separator character. If it is the last
*  or first character in the string then the element is not a vlaid
*  modification element.
      SEP1 = INDEX( ELEM, MSPC )
      IF( SEP1 .EQ. 1 .OR. SEP1 .EQ. LEN( ELEM ) ) THEN
         STATUS = GRP__BADME
         GO TO 999
      END IF

*  Get the position of the second separator character. If the string
*  between the first and second separators is null, or if the second
*  separator is the last character in the string then the element is
*  not a vlaid modification element.
      SEP2 = INDEX( ELEM( SEP1 + 1 : ), MSPC ) + SEP1
      IF( SEP2 .LE. SEP1 + 1 .OR. SEP2 .EQ. LEN( ELEM ) ) THEN
         STATUS = GRP__BADME
         GO TO 999
      END IF

*  Get the position of the third separator character. If it is not
*  found, or if there are any non-blank characters remaining after the
*  third separator then the element is not a vlaid modification
*  element.
      SEP3 = INDEX( ELEM( SEP2 + 1 : ), MSPC ) + SEP2
      IF( SEP3 .LE. SEP2 .OR. SEP3 .NE. CHR_LEN( ELEM ) ) THEN
         STATUS = GRP__BADME
         GO TO 999
      END IF

*  Save the string to be replaced, and its length.
      OLD = ELEM( SEP1 + 1 : SEP2 - 1 )
      OLDLEN = SEP2 - SEP1 - 1

*  If the new string is null, set a flag to indicate this, and store a
*  safe value for NEWLEN (to avoid access violations when calling
*  GRP1_SUBST).
      IF( SEP3 .LE. SEP2 + 1 ) THEN
         NEWNUL = .TRUE.
         NEWLEN = 1

*  Otherwise, save the new string and its length.
      ELSE
         NEWNUL = .FALSE.
         NEW = ELEM( SEP2 + 1 : SEP3 - 1 )
         NEWLEN = SEP3 - SEP2 - 1
      END IF

*  Make the substitution.
      CALL GRP1_SUBST( ELEM( : SEP1 - 1 ), OLD( : OLDLEN ),
     :                 NEW( : NEWLEN ), .TRUE.,
     :                 .NOT. CMN_UPPER( SLOT2 ), NEWNUL, NEWNAM, NSUB,
     :                 STATUS )

*  Get the used length of the new name.
      NLEN = CHR_LEN( NEWNAM )

*  Give a warning error message if the expanded name may potentially be
*  truncated. Note, status is set before reporting the error and then
*  the error is flushed. This avoids using MSG_OUT.
      IF( NLEN .EQ. GRP__SZNAM ) THEN
         CALL ERR_MARK

         STATUS = GRP__SHORT
         CALL ERR_REP( 'GRP1_MODNM_ERR1', ' ', STATUS )
         CALL MSG_SETC( 'ELEM', ELEM )
         CALL MSG_SETC( 'NAME', NEWNAM )
         CALL ERR_REP( 'GRP1_MODNM_ERR2',
     : 'The name ^NAME (specified by the modification element '//
     : '"^ELEM") may have suffered truncation:', STATUS )
         CALL ERR_FLUSH( STATUS )

         CALL ERR_RLSE
      END IF

*  Append the new name to the end of the output group.
      CALL GRP1_PTELM( SLOT2, 0, NEWNAM( : NLEN ), DEPTH, IFILE,
     :                 GRP__NOID, 0, STATUS )

*  If an error occurred, give a report.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'ELEM', ELEM )
         CALL ERR_REP( 'GRP1_MODNM_ERR3',
     :        'GRP1_MODNM: Unable to expand modification element ^ELEM',
     :        STATUS )
      END IF

      END
