      SUBROUTINE IRG_NDFPR( GID, INDX, INDF1, CLIST, INDF2, STATUS )
*+
*  Name:
*     IRG_NDFPR

*  Purpose:
*     Create an NDF by propagation and get an NDF identifier for it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG_NDFPR( GID, INDX, INDF1, CLIST, INDF2, STATUS )

*  Description:
*     The routine creates an NDF with name specified by INDX and GID
*     by propagation from the NDF identified by INDF1. An NDF
*     identifier for the created NDF is returned which can then be used
*     by all the normal NDF_ routines (see SUN/33). 

*  Arguments:
*     GID = INTEGER (Given)
*        An identifier for the group containing the name of the NDF to
*        be created.
*     INDX = INTEGER (Given)
*        The index (within the group identified by GID) of the name of
*        the NDF to be created.
*     INDF1 = INTEGER (Given)
*        An NDF identifier for the NDF on which the created NDF is to
*        be based.
*     CLIST = CHARACTER (Given)
*        A list of components to be propagated, in the same format as
*        the CLIST argument for thr NDF_PROP routine (see SUN/33).
*     INDF2 = INTEGER (Returned)
*        An NDF identifier for the created NDF.
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
*        Modified to ignore NDF slice specifiers.
*     27-FEB-1992 (PDRAPER):
*        Removed I90_PAR and added DAT_PAR.
*     16-MAR-1992 (PDRAPER):
*        Changed calls to NDF internals to use NDF1_, port to Unix.
*     26-FEB-1997 (PDRAPER):
*        Finally removed NDF internal calls and replaced with NDF_SCOPY.
*        New NDFs can also be created by name now, so these use
*        NDF_OPEN. LOC removed from argument list, HDS no longer 
*        supports HDS_CLOSE. All this is to allow foreign data access.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRG_ERR'          ! IRG error values.
      INCLUDE 'IRG_PAR'          ! IRG parameters.

*  Global Variables:
      INCLUDE 'IRG_COM'          ! IRG common blocks.
*        GCM_OUT( IRH__MAXG ) = LOGICAL (Read)
*           If true, then the group is an output group. Otherwise it is
*           an input group.

*  Arguments Given:
      INTEGER GID
      INTEGER INDX
      INTEGER INDF1
      CHARACTER CLIST * ( * )

*  Arguments Returned:
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                 ! Loop count.
      INTEGER IDUM              ! Dummy identifier
      INTEGER ITYPE             ! Index of file type 
      CHARACTER NAME * ( IRH__SZNAM ) ! NDF file name (without file type).
      INTEGER PLACE             ! NDF placeholder.
      CHARACTER SLICE * ( 50 )  ! NDF slice specifier.
      INTEGER START             ! Starting position of NDF slice specifier.
      CHARACTER TITLE * ( 50 )  ! Group title.
*.

*  Set an initial value for the INDF argument.
      INDF2 = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the group identified by GID is a group created by IRG.
*  This is assumed to be true if the group title starts with the string
*  given by symbolic constant IRG__PREFX and the access mode is legal.
      CALL IRG1_CHECK( GID, .TRUE., STATUS )

*  If the group is designated as an input group, report an error.
      IF( .NOT. GCM_OUT( GID ) .AND. STATUS .EQ. SAI__OK ) THEN
         CALL IRH_GTTL( GID, TITLE, STATUS )
         STATUS = IRG__IN
         CALL MSG_SETC( 'TTL', TITLE )
         CALL ERR_REP( 'IRG_NDFPR_ERR1',
     :                 'IRG_NDFPR: Group "^TTL" is an input group.',
     :                 STATUS )
      END IF

*  Get the required name.
      CALL IRH_GET( GID, INDX, 1, NAME, STATUS )

*  Remove any NDF slice specifier.
      CALL IRG1_SLICE( NAME, SLICE, START, STATUS )

*  And the HDS file type (if being used).
      ITYPE = INDEX( NAME, IRG__NDFTP )
      IF ( ITYPE .NE. 0 ) THEN 
         NAME( ITYPE: ) = ' '
      END IF

*  Create the new NDF.
      CALL NDF_OPEN( DAT__ROOT, NAME, GCM_AMODE( GID ), 'NEW', IDUM, 
     :               PLACE, STATUS )

*  Copy the required components into new NDF.
      CALL NDF_SCOPY( INDF1, CLIST, PLACE, INDF2, STATUS )

*  If an error occured, annul the NDF identifier and add context
*  information.
 999  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF_ANNUL( INDF2, STATUS )
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'IRG_NDFPR_ERR2',
     :         'IRG_NDFPR: Unable to create file "^NAME"',
     :          STATUS )

      END IF

      END
* $Id$
