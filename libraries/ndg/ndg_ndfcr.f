      SUBROUTINE NDG_NDFCR( IGRP, INDEX, FTYPE, NDIM, LBND, UBND, INDF,
     :                      STATUS )
*+
*  Name:
*     NDG_NDFCR

*  Purpose:
*     Obtain an NDF identifier for a new simple NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_NDFCR( IGRP, INDEX, FTYPE, NDIM, LBND, UBND, INDF,
*                     STATUS )

*  Description:
*     The routine returns an NDF identifier for a new simple NDF created 
*     with the specified attributes. The name of the new NDF is held
*     at a given index within a given group. It is equivalent to NDF_CREAT.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group holding the names of NDFs. This
*        will often be creted using NDG_CREAT, but groups created "by 
*        hand" using GRP directly can also be used.
*     INDEX = INTEGER (Given)
*        The index within the group at which the name of the NDF to be
*        created is stored.
*     FTYPE = CHARACTER * ( * ) (Given)
*        Full data type of the NDF's DATA component (e.g. '_DOUBLE' or
*        'COMPLEX_REAL').
*     NDIM = INTEGER (Given)
*        Number of NDF dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        Lower pixel-index bounds of the NDF.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper pixel-index bounds of the NDF.
*     INDF = INTEGER (Returned)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1992 (DSB):
*        Original version.
*     29-AUG-1997 (DSB):
*        Modified to use automatica NDF data conversion.
*     7-MAR-2006 (DSB):
*        Switch off interpretation of shell metacharacters by HDS.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'NDG_CONST'          ! NDG constants.
                                  
*  Arguments Given:
      INTEGER IGRP
      INTEGER INDEX
      CHARACTER FTYPE*(*)
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Arguments Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAME*(GRP__SZNAM)! NDF file name (without file type).
      CHARACTER ENAME*(GRP__SZNAM)! Expanded NDF file name
      INTEGER PLACE              ! NDF placeholder.
      INTEGER SHELL              ! Original value of HDS SHELL tuning param
*.

*  Set an initial value for the INDF argument.
      INDF = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( NDG__UCASE ) CALL GRP_SETCS( IGRP, .FALSE., STATUS )

*  Get the required name.
      CALL GRP_GET( IGRP, INDEX, 1, NAME, STATUS )

*  If the name could not be obtained, set the name blank and abort.     
      IF ( STATUS .NE. SAI__OK ) THEN
         NAME = ' '
         GO TO 999
      END IF

*  Expand any shell metacharacters in it. Having done this we can safely
*  switch off HDS metacharacter interpretation, since HDS has problems
*  with spaces in file names.
      CALL ONE_SHELL_ECHO( NAME, ENAME, STATUS )      
      CALL HDS_GTUNE( 'SHELL', SHELL, STATUS )         
      CALL HDS_TUNE( 'SHELL', -1, STATUS )         
      
*  Create the NDF place holder.
      CALL NDG1_OPEN( ENAME, PLACE, STATUS )

*  Create the NDF.
      CALL NDF_NEW( FTYPE, NDIM, LBND, UBND, PLACE, INDF, STATUS)

*  Re-instate the original HDS SHELL value.
      CALL ERR_BEGIN( STATUS )
      CALL HDS_TUNE( 'SHELL', SHELL, STATUS )         
      CALL ERR_END( STATUS )

*  If an error occured, add context information.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN

         IF( NAME .NE. ' ' ) THEN
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'NDG_NDFCR_ERR1', 'Unable to get an NDF '//
     :                    'identifier for ''^NAME''.', STATUS )

         ELSE
            CALL ERR_REP( 'NDG_NDFCR_ERR2', 'Unable to get an NDF '//
     :                    'identifier for a new data set.', STATUS )

         END IF

      END IF

      END
