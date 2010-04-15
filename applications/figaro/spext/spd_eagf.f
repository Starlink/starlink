      SUBROUTINE SPD_EAGF( NDF, XLOC, TYPE, STATUS )
*+
*  Name:
*     SPD_EAGF

*  Purpose:
*     Create covariance row sums.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAGF( NDF, XLOC, TYPE, STATUS )

*  Description:
*     This routine creates a new NDF for covariance row sums in the
*     Specdre Extension and sets the values to zeros. The Extension must
*     already exist, the COVRS structure must not exist. If the given
*     NDF is a section rather than a base NDF, then the appropriate base
*     NDF will be used to create the Extension NDF.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric type for creating the Extension NDF. This can be
*        '_REAL' or '_DOUBLE'.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set
*        -  if the structure to be created already exists,
*        -  if the requested data type is invalid.

*  Notes:
*     This routine recognises Specdre Extension v. 0.7.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01-JUL-1992 (HME):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'           ! Specdre Extension parameters

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EXIST              ! True if structure exists
      INTEGER I, J               ! Temporary integers
      INTEGER BNDF               ! Main's base NDF indentifier
      INTEGER PLACE              ! NDF placeholder
      INTEGER XNDF               ! Structure's NDF identifier
      INTEGER NDIM               ! Number of axes of NDF
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that Extension NDF does not exist.
      CALL DAT_THERE( XLOC, XCMP8, EXIST, STATUS )
      IF ( EXIST ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_EXISTS', 'SPD_EAGF: Error:' //
     :      'The Extension structure COVRS already exists.', STATUS )
         GO TO 500
      END IF

*  Check that requested type is valid.
      IF ( TYPE .NE. '_DOUBLE' .AND. TYPE .NE. '_REAL' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAGF: Error creating covariance row sums. ' //
     :      'Requested data type is invalid.', STATUS )
         GO TO 500
      END IF

*  Find the base of the main NDF and its bounds.
      CALL NDF_BASE(  NDF, BNDF, STATUS )
      CALL NDF_BOUND( BNDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Create the Extension NDF.
      CALL NDF_PLACE( XLOC, XCMP6, PLACE, STATUS )
      CALL NDF_NEW( TYPE, NDIM, LBND, UBND, PLACE, XNDF, STATUS )

*  The initial data are zeros. We do not call these the default values,
*  because the default is that COVRS does not exist.
      CALL NDF_MAP( XNDF, 'DATA', TYPE, 'WRITE/ZERO', I, J, STATUS )

*  Tidy up.
 500  CONTINUE

*  Annul the new NDF and the main's base NDF.
      CALL NDF_ANNUL( XNDF, STATUS )
      CALL NDF_ANNUL( BNDF, STATUS )

*  Return.
      END
