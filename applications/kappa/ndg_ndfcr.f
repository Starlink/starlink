      SUBROUTINE NDG_NDFCR( IGRP, INDEX, FTYPE, NDIM, LBND, UBND, INDF,
     :                      STATUS )
*+
*  Name:
*     NDG_NDFCR

*  Purpose:
*     Create a new simple NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_NDFCR( IGRP, INDEX, FTYPE, NDIM, LBND, UBND, INDF,
*                     STATUS )

*  Description:
*     The routine creates a new simple NDF with the name stored in the
*     given group with the given index, and returns an NDF identifier
*     for it. It is equivalent to NDF_CREAT.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group holding NDF names.
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

*  Notes:
*     -  This routine creates a "simple" NDF, i.e. one whose array
*     components will be stored in "simple" form by default (see
*     SGP/38).
*     -  The full data type of the DATA component is specified via the
*     FTYPE argument and the data type of the VARIANCE component
*     defaults to the same value. These data types may be set
*     individually with the NDF_STYPE routine if required.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The NDF__NOID
*     constant is defined in the include file NDF_PAR.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1992 (DSB):
*        Original version.
*     29-AUG-1997 (DSB):
*        Modified to use automatica NDF data conversion.
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
      INTEGER PLACE              ! NDF placeholder.
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

*  Create the NDF place holder.
      CALL NDF_OPEN( DAT__ROOT, NAME, 'WRITE', 'NEW', INDF, PLACE, 
     :               STATUS )

*  Create the NDF.
      CALL NDF_NEW( FTYPE, NDIM, LBND, UBND, PLACE, INDF, STATUS)

*  If an error occured, add context information.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN

         IF( NAME .NE. ' ' ) THEN
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'NDG_NDFCR_ERR1',
     :         'NDG_NDFCR: Unable to get an NDF identifier for "^NAME"',
     :                     STATUS )

         ELSE
            CALL ERR_REP( 'NDG_NDFCR_ERR2',
     :   'NDG_NDFCR: Unable to get an NDF identifier for a created'//
     :   'data set.', STATUS )

         END IF

      END IF

      END
