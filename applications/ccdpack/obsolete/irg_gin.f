      SUBROUTINE IRG_GIN( GID, OUT, AMODE, STATUS )
*+
*  Name:
*     IRG_GIN

*  Purpose:
*     Modify a group created by IRH so that it can be used in IRG.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG_GIN( GID, OUT, AMODE, STATUS )

*  Description:
*     Groups created within IRG can be directly used by any IRH
*     routine, but the converse is not true; groups created by IRH
*     cannot be used directly within IRG routines. This routine
*     modifies such groups so that they can be used within IRG. An
*     error is reported if the group is already in IRG format, and the
*     status IRG__NOTHG is returned.
*     An error is reported if any of the names contained in the
*     group on entry do not correspond to accessable NDFs.

*  Arguments:
*     GID = INTEGER (Given)
*        The identifier for the group. An error is reported if the group
*        is already suitable for use with IRG. An error is also reported
*        if the group identifier is invalid.
*     OUT = LOGICAL (Given)
*        If the group contains a list of existing NDFs then OUT should be
*        given as .FALSE., if it contains a list of NDFs to be created
*        then OUT should be given as .TRUE.
*     AMODE = CHARACTER * ( * ) (Given)
*        The access mode required for the NDFs in the group. This can be
*        'READ', 'WRITE' or 'UPDATE', or any abbreviation. If OUT is
*        TRUE this argument is ignored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1991 (DSB):
*        Original version.
*     31-JAN-1992 (DSB):
*        Modified to take account of new behaviour in IRG1_NDFCH.
*     27-FEB-1992 (PDRAPER):
*        Removed I90_PAR and string concatenation in argument list.
*     27-FEB-1997 (PDRAPER):
*        Removed error report when data files do not have type
*        ".sdf". IRG may now deal with foreign data types. Removed 
*        expansion of names with ".sdf" data type. This is now done in
*        the wildcard expansion script. Also name is either a fully
*        qualified foreign one, or an NDF (without file type), so
*        output names are no longer stripped.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRG_PAR'          ! IRG constants.
      INCLUDE 'IRG_ERR'          ! IRG error values.

*  Global Variables:
      INCLUDE 'IRG_COM'          ! IRG common blocks.
*        GCM_OUT( IRH__MAXG ) = LOGICAL (Read and Write)
*           If true, then the group is an output group. Otherwise it is
*           an input group.
*        GCM_AMODE( IRH__MAXG ) = CHARACTER (Read and Write)
*           The access mode for existsing NDFs.

*  Arguments Given:
      INTEGER GID
      LOGICAL OUT
      CHARACTER AMODE * ( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string.
*  Local Variables:
      CHARACTER CVAL * ( 1 )    ! Dummy character argument.
      CHARACTER FSPEC * ( IRH__SZNAM ) ! Current full file spec.
      CHARACTER NAME * ( IRH__SZNAM ) ! Current name.
      CHARACTER TITLE * ( 80 )  ! The title from the group.
      CHARACTER TYPE * ( IRH__SZNAM ) ! File type (eg ".SDF").
      INTEGER I                 ! Loop count.
      INTEGER IVAL              ! Dummy integer argument.
      INTEGER SIZE              ! Size of input group.
      INTEGER TLEN              ! The used length of the title.
      LOGICAL FNF               ! True if any NDFs specified in 
                                ! the input group could not be accessed.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the group is already an IRG group.
      CALL IRG1_CHECK( GID, .FALSE., STATUS )

*  If the title and access mode indicate that the group is already
*  acceptable to IRG, report an error.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL IRH_GTTL( GID, TITLE, STATUS )
         CALL MSG_SETC( 'TTL', TITLE )
         STATUS = IRG__NOTHG
         CALL ERR_REP( 'IRG_GIN_ERR1',
     :                 'IRG_GIN: Group "^TTL" is already in IRG format',
     :                  STATUS )

*  If the title or access mode was wrong, annul the error.
      ELSE IF( STATUS .EQ. IRG__NOIRG .OR. STATUS .EQ. IRG__AMODE ) THEN
         CALL ERR_ANNUL( STATUS )

*  If the group is an output group...
         IF ( OUT ) THEN

*  Go through each name in the group copying it to the new group,
*  without the slice information.
            CALL IRH_GRPSZ( GID, SIZE, STATUS )
            DO I = 1, SIZE
               CALL IRH_GET( GID, I, 1, NAME, STATUS )
               CALL IRG1_SLICE( NAME, CVAL, IVAL, STATUS )
               CALL IRH_PUT( GID, 1, NAME, I, STATUS )
            END DO

*  Store an access mode of WRITE.
            GCM_AMODE( GID ) = 'WRITE'

*  If the group is an input group...
         ELSE

*  Expand wild cards, and check that all names in the group correspond
*  to existing image files. IRG1_NDFCH gives warning messages if any do
*  not exist. The final group consists of a set of full file
*  specifications (minus file type and version number).
            CALL IRG1_NDFCH( 1, GID, FNF, STATUS )

*  If any of the files could not be found, report an error.
            IF( FNF .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = IRG__NOFIL
               CALL MSG_SETC( 'TTL', TITLE )
               CALL ERR_REP( 'IRG_GIN_ERR2',
     :         'IRG_GIN: Group "^TTL" contains inaccessable files',
     :              STATUS )
            END IF

*  ...verify and store the access mode.
            CALL IRG1_VMODE( AMODE, GCM_AMODE( GID ), STATUS )
         END IF

*  Get the title from the input group.
         CALL IRH_GTTL( GID, TITLE, STATUS )

*  Put the string given by the symbolic constant IRG__PREFX at the
*  start of the title.
         NAME = ' '
         TLEN = MAX( 1, CHR_LEN( TITLE ) )
         NAME = IRG__PREFX//TITLE( :TLEN )
         TLEN = MAX( 1, CHR_LEN( NAME ) )
         CALL IRH_PTTL( GID, NAME( : TLEN ), STATUS )

*  Store the value of OUT.
         GCM_OUT( GID ) = OUT
      END IF

*  If an error occured give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRG_GIN_ERR3',
     : 'IRG_GIN: Error importing a group into IRG', STATUS )
      END IF

      END
* $Id$
