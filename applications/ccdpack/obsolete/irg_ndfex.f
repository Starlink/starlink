      SUBROUTINE IRG_NDFEX( GID, INDX, INDF, STATUS )
*+
*  Name:
*     IRG_NDFEX

*  Purpose:
*     Obtain an NDF identifier for an existing NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG_NDFEX( GID, INDX, INDF, STATUS )

*  Description:
*     The routine obtains an NDF identifier for a member of a group
*     created by IRG_GROUP. The returned identifier can be used by all
*     the normal NDF_ routines (see SUN/33). NDF identifiers for groups
*     created by IRG_CREAT cannot be obtained using this routine
*     (routine IRG_NDFCR or IRG_NDFPR should be used instead).

*  Arguments:
*     GID = INTEGER (Given)
*        A group identifier, created by IRG
*     INDX = INTEGER (Given)
*        The index within the group of the NDF name.
*     INDF = INTEGER (Returned)
*        NDF identifier. This will be an identifier to an NDF section if
*        the user specified a slice on the NDF name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-JUN-1991 (DSB):
*        Original version.
*     31-JAN-1992 (DSB):
*        Modified to cope with NDF slices.
*     27-FEB-1992 (PDRAPER):
*        Removed I90_PAR.
*     26-FEB-1997 (PDRAPER):
*        Removed LOC argument. HDS has made HDS_CLOSE obsolete so this
*        can no longer be used. Modified to allow foreign data access.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRG_ERR'          ! IRG error values.
      INCLUDE 'IRG_PAR'          ! IRG parameters.

*  Global Variables:
      INCLUDE 'IRG_COM'          ! IRG common blocks.
*        GCM_AMODE( IRH__MAXG ) = CHARACTER (Read)
*           Access mode (READ, WRITE or UPDATE) for each group. 
*           the corresponding GROUP strcuture.
*        GCM_OUT( IRH__MAXG ) = LOGICAL (Read)
*           If true, then the group is an output group. Otherwise it is
*           an input group.

*  Arguments Given:
      INTEGER GID
      INTEGER INDX

*  Arguments Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER BNDF              ! Identifier to base NDF.
      INTEGER ITYPE             ! Index of file type
      INTEGER PLACE             ! Dummy placeholder
      CHARACTER NAME * ( IRH__SZNAM ) ! NDF file name (without file type).
      CHARACTER SLICE * ( 50 )  ! NDF slice specifier.
      INTEGER START             ! Position of start of slice specifier.
      CHARACTER TITLE * ( 50 )  ! Group title.
*.

*  Set an initial value for the INDF argument.
      INDF = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that any group identified by GID is a group created by IRG.
*  This is assumed to be true if the group title starts with the string
*  given by symbolic constant IRG__PREFX, and if the access mode is
*  legal.
      CALL IRG1_CHECK( GID, .TRUE., STATUS )

*  If the group is designated as an output group, report an error.
      IF( GCM_OUT( GID ) .AND. STATUS .EQ. SAI__OK ) THEN
         CALL IRH_GTTL( GID, TITLE, STATUS )
         STATUS = IRG__OUT
         CALL MSG_SETC( 'TTL', TITLE )
         CALL ERR_REP( 'IRG_NDFEX_ERR1',
     :                 'IRG_NDFEX: Group "^TTL" is an output group.',
     :                 STATUS )
      END IF

*  Get the required name.
      CALL IRH_GET( GID, INDX, 1, NAME, STATUS )

*  Remove any slice specifier from the name.
      CALL IRG1_SLICE( NAME, SLICE, START, STATUS )

*  And the HDS file type (if being used).
      ITYPE = INDEX( NAME, IRG__NDFTP )
      IF ( ITYPE .NE. 0 ) THEN 
         NAME( ITYPE: ) = ' '
      END IF

*  Open the NDF.
      CALL NDF_OPEN( DAT__ROOT, NAME, GCM_AMODE( GID ), 'OLD', 
     :               BNDF, PLACE, STATUS )

*  Obtain an identifier for the specified section of the NDF.
      CALL IRG1_NCUT( BNDF, SLICE, INDF, STATUS )

*  Annul the base identifier.
      CALL NDF_ANNUL( BNDF, STATUS )      

*  If an error occured, annul the NDF identifier and add context
*  information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF_ANNUL( INDF, STATUS )
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'IRG_NDFEX_ERR2',
     :         'IRG_NDFEX: Unable to open file "^NAME"',
     :          STATUS )
      
      END IF

      END
* $Id$
