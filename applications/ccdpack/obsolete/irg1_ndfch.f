      SUBROUTINE IRG1_NDFCH( START, GID, FNF, STATUS )
*+
*  Name:
*     IRG1_NDFCH

*  Purpose:
*     Expand wild card templates and check all files exist.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG1_NDFCH( START, GID, FNF, STATUS )

*  Description:
*     Names stored in the specified group with indices greater than or
*     equal to START are expanded into a list of explicit file names
*     using the routine IRG1_WILD. IRG1_WILD expands files with all
*     possible types (which are defined as ".sdf" and any in the
*     environmental variable NDF_FORMATS_IN). Files with types of ".sdf"
*     are also checked to see if they are container files for top-level
*     NDFs, others are returned unchecked.

*  Arguments:
*     START = INTEGER (Given)
*        The index of the first name to be expanded into a list of .SDF
*        files.
*     GID = INTEGER (Given and Returned)
*        The group identifier, created by a call to IRH_GROUP. If the
*        supplied group contains any files which cannot be accessed for
*        any reason, then the supplied group identifier is replaced by
*        a new identifier for a group from which the inaccesable files
*        have been removed. In this case, the supplied group identifier
*        is annulled.
*     FNF = LOGICAL (Returned)
*        True if any specified files could not be found. False
*        otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1992 (PDRAPER):
*        Original Unixified version - based on VMS version
*     18-MAR-1992 (PDRAPER):
*        Changed to locate DATA_ARRAY object instead of NDF.
*     31-MAR-1992 (PDRAPER):
*        Added MSG_OUTIFs  to allow suppression of messages.
*     16-MAR-1995 (PDRAPER):
*        Changed IRG1_FSPEC call to not use NAME variable twice.
*     26-FEB-1997 (PDRAPER):
*        Rewrite to use IRG1_WILD to deal with possible foreign formats.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRG_PAR'          ! IRG  constants
      INCLUDE 'IRG_ERR'
      INCLUDE 'MSG_PAR'          ! EMS constants

*  Arguments Given:
      INTEGER START

*  Arguments Given and Returned:
      INTEGER GID

*  Arguments Returned:
      LOGICAL FNF

*  Status:
      INTEGER STATUS            ! Global status

*  Local References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN          ! Length of string excluding blanks
      INTEGER IRG1_WILD
      EXTERNAL IRG1_WILD        ! Lists files (inc wildcards)
      INTEGER IRG1_EWILD
      EXTERNAL IRG1_EWILD       ! Ends file listing

*  Local Variables:
      CHARACTER FILE * ( IRH__SZNAM ) ! File spec. which matches NAME.
      CHARACTER FTYPE * ( 10 )  ! File type of matching file.
      CHARACTER NAME * ( IRH__SZNAM ) ! Current group name.
      CHARACTER SLICE * ( IRH__SZNAM ) ! Any slice specification which was included in the original name.
      INTEGER I                 ! Name index.
      INTEGER ICONTX            ! Wild card search context.
      INTEGER ID                ! NDF identifier
      INTEGER ISTAT             ! VMS status value.
      INTEGER LAST              ! Index of last name to be checked.
      INTEGER NAMEND            ! Position of first character beyond
      INTEGER NEWGID            ! Identifier for group from which bad NDF has been excluded.
      INTEGER PLACE             ! NDF placeholder
      LOGICAL FOUND             ! True if a matching .SDF file found
      LOGICAL VALID             ! True if filename is valid
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the initial group size.
      CALL IRH_GRPSZ( GID, LAST, STATUS )

*  Initialise FNF to indicate that all files have been found.
      FNF = .FALSE.

*  Loop round all the names stored in the group on entry, starting with
*  the name specified by argument START.
      I = MAX( 1, START )
 10   CONTINUE
      IF( I .LE. LAST ) THEN

*  Get the name with the current index.
         CALL IRH_GET( GID, I, 1, NAME, STATUS )

*  If all has gone OK...
         IF( STATUS .EQ. SAI__OK ) THEN

*  Extract and remove any slice specification from the name (i.e.
*  any string occuring between opening and closing parenthesise at the
*  end of the name).
            CALL IRG1_SLICE( NAME, SLICE, NAMEND, STATUS )

*  ...set a flag to indicate that this name has not yet been matched by
*  any existing file.
            FOUND = .FALSE.

*  Initialise the wild card search context.
            ICONTX = 0

*  Loop round finding all the files which match the current name.
 20         CONTINUE

*  Find the next file which satisfies the given name.
            FILE = ' '
            VALID = .FALSE.
            ISTAT = IRG1_WILD( NAME, FILE, ICONTX )

*  If another file was found which matches the name ...
            IF( ISTAT .EQ. SAI__OK ) THEN

*  If the file type is ".sdf" then test it to see if it really is an NDF.
               CALL IRG1_FSPEC( FILE, ' ', 'TYPE', FTYPE, STATUS )
               IF( FTYPE .EQ. IRG__NDFTP .AND. STATUS .EQ. SAI__OK ) 
     :              THEN

*  Remove the file type (not used for HDS).
                  FILE( INDEX( FILE, IRG__NDFTP ): ) = ' '

*  Check that it is an NDF.
                  CALL ERR_MARK
                  CALL NDF_OPEN( DAT__ROOT, FILE, 'READ', 'OLD',
     :                           ID, PLACE, STATUS )

*  If not an NDF then annul the error and wait for next pass.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                  ELSE 
                     VALID = .TRUE.
                     CALL NDF_ANNUL( ID, STATUS )
                  END IF
                  CALL ERR_RLSE
               ELSE 

*  Assume file is a foreign format.
                  VALID = .TRUE.
               END IF
               IF ( VALID ) THEN 

*  Append any slice information to the name.
                  NAMEND = CHR_LEN( FILE ) + 1
                  FILE( NAMEND: ) = SLICE

*  If this is the first file found which matches the current name, store
*  the full file name in the group at the same index as the original
*  name. Otherwise, add the file name to the end of the group.
                  IF( .NOT. FOUND ) THEN
                     CALL IRH_PUT( GID, 1, FILE, I, STATUS )
                  ELSE
                     CALL IRH_PUT( GID, 1, FILE, 0, STATUS )
                  END IF
                  FOUND = .TRUE.
               END IF

*  Go round for the next matching file.
               GO TO 20
               
            ELSE IF( ISTAT .EQ. IRG__WNMF ) THEN
*  If no further files can be found which match the current name.
*  If no matching file was found, give a warning message and set FNF
*  to indicate this.
               IF( .NOT. FOUND ) THEN
                  IF( .NOT. FNF ) THEN
                     CALL MSG_OUTIF( MSG__NORM, ' ', ' ', STATUS )
                     FNF = .TRUE.
                  END IF
                  CALL MSG_SETC( 'NAME', NAME )
                  CALL MSG_OUTIF( MSG__NORM, 'IRG1_NDFCH_MSG2',
     :            '   No images found which match name "^NAME"', 
     :                 STATUS )

*  Create a new group in which the bad name is not included.
                  CALL IRH_SECT( GID, I, I, .TRUE., NEWGID, STATUS )

*  Annul the old group and use the new group instead.
                  CALL IRH_ANNUL( GID, STATUS )
                  GID = NEWGID

*  Modify the index of the last name to be checked, and the current
*  name index.
                  LAST = LAST - 1
                  I = I - 1
               END IF

*  If an error was detected report it appropriately.
            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'NAME', NAME )
               CALL ERR_REP( 'IRG1_NDFCH_ERR1',
     :'IRG1_NDFCH: error while searching for names "^NAME": ^ERR',
     :                        STATUS )
               IF ( ISTAT .EQ. IRG__WPER ) THEN
                  CALL ERR_REP( 'IRG1_NDFCH_ERR2',
     :'IRG1_NDFCH: Error getting pipe from forked process', STATUS )
               ELSE IF ( ISTAT .EQ. IRG__WMER ) THEN
                  CALL ERR_REP( 'IRG1_NDFCH_ERR4',
     :'IRG1_NDFCH: Insufficient memory', STATUS )
               ELSE IF ( ISTAT .EQ. IRG__WSER ) THEN
                  CALL ERR_REP( 'IRG1_NDFCH_ERR5',
     :'IRG1_NDFCH: Failed to locate irg_wild script', STATUS )
               END IF
            END IF
         END IF

*  End the file search.
         ISTAT = IRG1_EWILD( ICONTX )

*  Go round for the next name.
         I = I + 1
         GO TO 10
      END IF
      END
* $Id$
