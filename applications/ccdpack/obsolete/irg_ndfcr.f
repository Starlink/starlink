      SUBROUTINE IRG_NDFCR( GID, INDX, FTYPE, NDIM, LBND, UBND, INDF,
     :                      STATUS )
*+
*  Name:
*     IRG_NDFCR

*  Purpose:
*     Create an NDF and obtain an NDF identifier for it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG_NDFCR( GID, INDX, FTYPE, NDIM, LBND, UBND, INDF,
*                     STATUS )

*  Description:
*     The routine creates a "simple" NDF with the name stored in the
*     given group with the given index. It is created with the shape
*     and data type specified by arguments FTYPE, NDIM, LBND, UBND. An
*     NDF identifier is returned for it which can be used by all the
*     normal NDF_ routines (see SUN/33).  

*  Arguments:
*     GID = INTEGER (Given)
*        A group identifier.
*     INDX = INTEGER (Given)
*        The index within the group of the NDF name.
*     FTYPE = CHARACTER (Given)
*        The full data type required for the DATA component.
*     NDIM = INTEGER (Given)
*        The required number of NDF dimensions. 
*     LBND( NDIM ) = INTEGER (Given)
*        The lower bounds required for the NDF. 
*     UBND( NDIM ) = INTEGER (Given)
*        The upper bounds required for the NDF. 
*     INDF = INTEGER (Returned)
*        NDF identifier.
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
*        Modified to ignore slice specifiers.
*     27-FEB-1992 (PDRAPER):
*        Removed I90_PAR.
*     26-FEB-1997 (PDRAPER):
*        Removed LOC argument. Changed to use pure NDF calls for 
*        foreign data access.
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
      INCLUDE 'IRG_PAR'          ! IRG constants.

*  Global Variables:
      INCLUDE 'IRG_COM'          ! IRG common blocks.
*        GCM_OUT( IRH__MAXG ) = LOGICAL (Read)
*           If true, then the group is an output group. Otherwise it is
*           an input group.

*  Arguments Given:
      INTEGER GID
      INTEGER INDX
      CHARACTER FTYPE * ( * )
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Arguments Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAME * ( IRH__SZNAM ) ! NDF file name (without file type).
      CHARACTER SLICE * ( 50 )  ! NDF slice specifier.
      CHARACTER TITLE * ( 50 )  ! Group title.
      INTEGER IDUM              ! Dummy identifier
      INTEGER IEND              ! End of name string
      INTEGER ITYPE             ! Index of file type
      INTEGER PLACE             ! NDF placeholder.
      INTEGER START             ! Position of start of NDF slice specifier.
*.

*  Set an initial value for the INDF argument.
      INDF = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the group identified by GID is a group created by IRG.
*  This is assumed to be true if the group title starts with the string
*  given by the symbolic constant IRG__PREFX, and the access mode is
*  legal.
      CALL IRG1_CHECK( GID, .TRUE., STATUS )

*  If the group is designated as an input group, report an error.
      IF( .NOT. GCM_OUT( GID ) .AND. STATUS .EQ. SAI__OK ) THEN
         CALL IRH_GTTL( GID, TITLE, STATUS )
         STATUS = IRG__IN
         CALL MSG_SETC( 'TTL', TITLE )
         CALL ERR_REP( 'IRG_NDFCR_ERR1',
     :                 'IRG_NDFCR: Group "^TTL" is an input group.',
     :                 STATUS )
      END IF

*  Get the required name.
      CALL IRH_GET( GID, INDX, 1, NAME, STATUS )

*  Remove any NDF slice specifier.
      CALL IRG1_SLICE( NAME, SLICE, START, STATUS )

*  And the HDS file type (if being used). Note to check that the file
*  name ends in IRG__NDFTP and doesn't have a further extension.
      ITYPE = INDEX( NAME, IRG__NDFTP )
      IF ( ITYPE .NE. 0 ) THEN
         IEND = ITYPE + LEN( IRG__NDFTP ) - 1
         IF ( LEN( NAME ) .GT. IEND ) THEN 
            IF ( NAME( IEND + 1: ) .EQ. ' ' ) THEN 
               NAME( ITYPE: ) = ' '
            END IF
         ELSE
            NAME( ITYPE: ) = ' '
         END IF
      END IF

*  Create the container file.
      CALL NDF_OPEN( DAT__ROOT, NAME, GCM_AMODE( GID ), 'NEW', IDUM,
     :               PLACE, STATUS )

*  Now create the actual NDF.
      CALL NDF_NEW( FTYPE, NDIM, LBND, UBND, PLACE, INDF, STATUS)

*  If an error occured, annul the NDF identifier and add context
*  information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF_ANNUL( INDF, STATUS )
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'IRG_NDFCR_ERR2',
     :         'IRG_NDFCR: Unable to create the file "^NAME"',
     :          STATUS )

      END IF

      END
* $Id$
