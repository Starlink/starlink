      SUBROUTINE IRG1_NDFCH( START, GID, FNF, STATUS )
*+
*  Name:
*     IRG1_NDFCH

*  Purpose:
*     Expand wild card templates and check all NDFs exist.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG1_NDFCH( START, GID, FNF, STATUS )

*  Description:
*     This routine is VMS specific.
*     
*     Names stored in the specified group with indices greater than or
*     equal to START are expanded into a list of explicit file names
*     using the VMS Run Time Library routine LIB$FIND_FILE. All files
*     with file type ".SDF" are checked. Any which contain an object
*     "DATA_ARRAY" are stored in the group. Others are ignored.  If any
*     explicitly named files do not exist (or do not contain an NDF
*     structure), a warning mesage is given and argument FNF is
*     returned true. On exit, the group contains no names containing
*     wild cards. The file type and version number are stripped from
*     the output names. If an input name contained an NDF slice
*     specification, it is copied to all related output file names.

*  Arguments:
*     START = INTEGER (Given)
*        The index of the first name to be expanded into a list of .SDF
*        files.
*     GID = INTEGER (Given and Returned)
*        The group identifier, created by a call to IRH_GROUP. If the 
*        supplied group contains any NDFs which cannot be accessed for 
*        any reason, then the supplied group identifier is replaced by 
*        a new identifier for a group from which the inaccesable NDFs 
*        have been removed. In this case, the supplied group identifier
*        is annulled.
*     FNF = LOGICAL (Returned)
*        True if any specified files could not be found. False 
*        otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is VAX specific in that it uses two VAX RTL
*        routines.   
      
*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JUN-1991 (DSB):
*        Original version.
*     30-JAN-1992 (DSB):
*        Argument FNF added. Previously, an error was reported if a 
*        file could not be found. Now, a message is displayed, but no 
*        error is reported. GID made given and returned, instead of
*        just given.
*     27-FEB-1992 (PDRAPER):
*        Removed I90_PAR and added DAT_PAR.
*     18-MAR-1992 (PDRAPER):
*        Changed to check for "DATA_ARRAY" object rather than
*        an object of type "NDF"
*     31-MAR-1992 (PDRAPER):
*        Added MSG_OUTIFs to allow users to stop unwanted messages.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'PRM_PAR'          ! VAL__ data constants.
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRG_ERR'          ! IRG error constants.
      INCLUDE 'IRG_PAR'          ! IRG constants
      INCLUDE 'MSG_PAR'          ! EMS constants
      INCLUDE '($RMSDEF)'        ! VMS RMS definitions.

*  Arguments Given:
      INTEGER START

*  Arguments Given and Returned:
      INTEGER GID

*  Arguments Returned:
      LOGICAL FNF

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER LIB$FIND_FILE      ! Wild card search routine.
      EXTERNAL LIB$FIND_FILE
      INTEGER LIB$FIND_FILE_END  ! End wild card file search.
      EXTERNAL LIB$FIND_FILE_END      

*  Local Variables:
      CHARACTER FILE*(IRH__SZNAM)! File spec. which matches NAME.
      LOGICAL FOUND              ! True if a matching .SDF file found.
      INTEGER FSTAT              ! "File found/not found" status value.
      CHARACTER FTYPE*10         ! File type of matching file.
      INTEGER I                  ! Name index.
      INTEGER ICONTX             ! Wild card search context.
      INTEGER ISTAT              ! VMS status value.
      INTEGER LAST               ! Index of last name to be checked.
      CHARACTER LOC*(DAT__SZLOC) ! HDS locator to top level object.
      CHARACTER NAME*(IRH__SZNAM)! Current group name.
      INTEGER NAMEND             ! Position of first character beyond
                                 ! the end of the file name.
      INTEGER NEWGID             ! Identifier for group from which bad 
                                 ! NDF has been excluded.
      CHARACTER SLICE*(IRH__SZNAM)! Any NDF slice specification which
                                 ! was inclueded in the original name.
      LOGICAL THERE              ! True when DATA_ARRAY object is there

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

*  Extract and remove any NDF slice specification from the name (i.e.
*  any string occuring between opening and closing parenthesise at the
*  end of the name).
            CALL IRG1_SLICE( NAME, SLICE, NAMEND, STATUS )
      
*  ...set a flag to indicate that this name has not yet been matched by
*  any existing .SDF file.
            FOUND = .FALSE.            

*  Initialise the wild card search context.
            ICONTX = 0

*  Loop round finding all the files which match the current name.
 20         CONTINUE

*  Find the next file which satisfies the given name.
            FTYPE = IRG__NDFTP//';0'
            ISTAT = LIB$FIND_FILE( NAME, FILE, ICONTX, FTYPE )

*  If another file was found which matches the name ...
            IF( ISTAT ) THEN

*  ... get the file type.
               CALL IRG1_FSPEC( FILE, ' ', 'TYPE', FTYPE, STATUS )

*  If the file is a ".SDF" file...
               IF( FTYPE .EQ. IRG__NDFTP ) THEN

*  Check the file contains a next level object "DATA_ARRAY"
                  CALL ERR_MARK
                  CALL HDS_OPEN( FILE, 'READ', LOC, STATUS )

*  Trap silly status values for invalid container file etc.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CALL ERR_RLSE
                     CALL DAT_THERE( LOC, 'DATA_ARRAY', THERE, STATUS )
                     CALL HDS_CLOSE( LOC, STATUS )
                     IF( THERE ) THEN

*  Replace the file type and version number with any NDF slice
*  specification which was included in the original name. If the
*  original name did not contain a slice specification, the file
*  type and version number will be set blank.
                        NAMEND = INDEX( FILE, IRG__NDFTP )
                        FILE( NAMEND: ) = SLICE      

*  If this is the first file found which matches the current name, store
*  the full file name in the group at the same index as the original
*  name. Otherwise, add the file name to the end of the group. 
                        IF( .NOT.FOUND ) THEN
                           CALL IRH_PUT( GID, 1, FILE, I, STATUS )
                        ELSE
                           CALL IRH_PUT( GID, 1, FILE, 0, STATUS )
                        END IF
                        FOUND = .TRUE.
                     END IF
                  ELSE

*  Annul error and try for next name match.
                     CALL ERR_ANNUL( STATUS )
                     CALL ERR_RLSE
                  END IF
               END IF

*  Go round for the next matching file.
               GO TO 20

*  If no further files can be found which match the current name...
            ELSE IF( ISTAT .EQ. RMS$_NMF .OR. ISTAT .EQ. RMS$_FNF ) THEN

*  If no matching file was found, give a warning message and set FNF
*  to indicate this.
               IF( .NOT. FOUND ) THEN

                  IF( .NOT. FNF ) THEN
                     CALL MSG_OUTIF( MSG__NORM, ' ', ' ', STATUS )
                     FNF = .TRUE.
                  END IF

                  CALL IRG1_FSPEC( NAME, IRG__NDFTP, ' ', NAME, STATUS )
                  CALL MSG_SETC( 'NAME', NAME )
                  
                  IF( INDEX( NAME, '*') .EQ. 0 .AND.
     :                INDEX( NAME, '%') .EQ. 0 ) THEN

                     CALL MSG_OUTIF( MSG__NORM, 'IRG1_NDFCH_MSG1',
     :                             '   NDF "^NAME" not found', STATUS )

                  ELSE

                     CALL MSG_OUTIF( MSG__NORM, 'IRG1_NDFCH_MSG2',
     :               '   No NDFs found which match "^NAME"', STATUS )

                  END IF

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

*  If a VMS error was detected, report it.
            ELSE
               STATUS = IRG__VMSER
               CALL MSG_SETC( 'NAME', NAME )
               CALL ERR_SYSER( 'ERR', ISTAT )
               CALL ERR_REP( 'IRG1_NDFCH_ERR1',
     :'IRG1_NDFCH: VMS error while searching for NDF "^NAME": ^ERR',
     :                        STATUS )

            END IF

         END IF

*  End the file search.
         ISTAT = LIB$FIND_FILE_END( ICONTX )

*  Go round for the next name.
         I = I + 1
         GO TO 10
      END IF

      END
* $Id$
