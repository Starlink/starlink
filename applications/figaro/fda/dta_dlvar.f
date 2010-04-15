      SUBROUTINE DTA_DLVAR( PATH, STATUS )
*+
*  Name:
*     DTA_DLVAR

*  Purpose:
*     Delete an object specified by its DTA object name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTA_DLVAR( PATH, DTA_STATUS )

*  Description:
*     This routine deletes a named structure. The object should not be a
*     top level name, but it can be a structure - in which case all the
*     objects it contains will also be deleted.

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The DTA structure name. This should not contain any dimensional
*        specifications (DLVAR cannot delete a single array element).
*        Case is ignored.
*     DTA_STATUS = INTEGER (Returned)
*        The DTA status.

*  Authors:
*     ks: Keith Shortridge (CIT, AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     25 May 1984 (ks):
*        Original version.
*     20 Mar 1986 (ks):
*        Re-written to use HDS routines.
*     10 Jan 1992 (ks):
*        Syntax of include statements changed to remove VMS logical
*        names and to use lower case, to enable compilation on a SUN.
*     24 Jan 1992 (ks):
*        Calls to EMS added to control error reporting.
*     14 Oct 1992 (hme):
*        Locating one level up is not as easy as calling DTA_LOCATE with
*        LEVELS-1. One must make sure that LASTC(LEVELS-1) points in
*        front of any array index.
*     12 Mar 1993 (hme):
*        Changed CHARACTER*15 to *(DAT__SZLOC).
*     29 Feb 1996 (hme):
*        FDA library.
*     08 Mar 1996 (hme):
*        Use DTA_SPLIT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Arguments Given:
      CHARACTER * ( * ) PATH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDOT1, IDOT2, IBRA ! Pointers into PATH
      INTEGER NDIM               ! Ignored
      INTEGER DIMS( DAT__MXDIM ) ! Ignored
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator

*.

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Split the path into parent and component.
*  Also gives us the dimension specification at the end.
      CALL DTA1_SPLIT( PATH, DAT__MXDIM,
     :   IDOT1, IDOT2, IBRA, NDIM, DIMS, STATUS )
      IF ( IDOT2 .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T016', PATH )
         CALL ERR_REP( 'FDA_E084', 'DTA1_DLVAR: Error deleting the ' //
     :      'object ^FDA_T016. The DTA object name cannot be ' //
     :      'split into parent and new component.', STATUS )
         GO TO 500
      END IF

*  Locate the parent.
      CALL DTA1_LOC( PATH(:IDOT2-1), LOC, STATUS )

*  Erase the component.
*  If there is a cell spec in square brackets, this will return moaning
*  about an invalid component name. That's just fine.
      CALL DAT_ERASE( LOC, PATH(IDOT2+1:), STATUS )

*  Tidy up.
 500  CONTINUE
      CALL DAT_ANNUL( LOC, STATUS )

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
