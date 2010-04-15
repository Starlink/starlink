      SUBROUTINE SPD_EAFJ( NDF, XLOC, TYPE, PNTR, ONDF, NELM, STATUS )
*+
*  Name:
*     SPD_EAFJ

*  Purpose:
*     Read-access non-existing spectroscopic widths from the main NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAFJ( NDF, XLOC, TYPE, PNTR, ONDF, NELM, STATUS )

*  Description:
*     This routine creates and accesses a temporary SPECWIDS NDF as it
*     would be accessed in the Specdre Extension, and sets the
*     spectroscopic widths to the default values. The default values are
*     derived from the main NDF's width array. For this routine it does
*     not matter if the Extension exists or not, or if the SPECWIDS
*     structure exists or not. However, the SPECVALS structure must not
*     exist in the Extension. The only information needed from the
*     Extension is the number of the spectrocopic axis.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF. If the Specdre Extension does not
*        exist, then this argument must be given as DAT__NOLOC.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric type for creating the temporary NDF. This can be
*        '_REAL' or '_DOUBLE'.
*     PNTR = INTEGER (Returned)
*        The pointer to which the width array is mapped.
*     ONDF = INTEGER (Returned)
*        The identifier of the temporary NDF.
*     NELM = INTEGER (Returned)
*        The number of elements in the mapped array.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set
*        -  if the SPECVALS structure exists in the same Extension,
*        -  if the requested data type is invalid.

*  Notes:
*     This routine recognises Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15 Jun 1992 (hme):
*        Original version.
*     24 Feb 1994 (hme):
*        Make it an SPD_E* routine, and use internal routine to grow.
*     2005 June 2 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER PNTR
      INTEGER ONDF
      INTEGER NELM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EXIST              ! True if SPECVALS exist
      INTEGER SPAXIS             ! The spectroscopic axis
      INTEGER INDAT              ! Pointer to grow source data
      INTEGER PLACE              ! NDF placeholder
      INTEGER NDIM               ! Number of axes of NDF
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF
      INTEGER INELM              ! Size of grow source
      INTEGER ODIM( NDF__MXDIM ) ! Dimensions of grow target

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that requested type is valid.
      IF ( TYPE .NE. '_DOUBLE' .AND. TYPE .NE. '_REAL' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAFJ: Error creating spectroscopic values. ' //
     :      'Requested data type is invalid.', STATUS )
         GO TO 500
      END IF

*  Check that corresponding SPECVALS do not exist.
      IF ( XLOC .NE. DAT__NOLOC ) THEN
         CALL DAT_THERE( XLOC, XCMP6, EXIST, STATUS )
         IF ( EXIST ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPE_EXISTS', 'SPD_EAFJ: Error:' //
     :         'The Extension structure SPECVALS exists, ' //
     :         'in which case that information should be used' //
     :         'to derive default values.', STATUS )
            GO TO 500
         END IF
      END IF

*  Find the main NDF bounds.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Create the temporary NDF.
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( TYPE, NDIM, LBND, UBND, PLACE, ONDF, STATUS )

*  Get spectroscopic axis number.
      CALL SPD_EABA( NDF, (XLOC.NE.DAT__NOLOC), SPAXIS, STATUS )

*  The default data are copied from the spectroscopic axis of the main
*  NDF. That 1-D array is grown to fill the whole N-D array.
*  Get/initialise the dimensions of the target and source data.
      CALL NDF_DIM( ONDF, NDF__MXDIM, ODIM, NDIM, STATUS )

*  Map the target data.
      CALL NDF_MAP( ONDF, 'DATA', TYPE, 'WRITE', PNTR, NELM, STATUS )

*  Map the source data. If they do not exist, NDF will provide a decent
*  default. NDF will also do the type conversion for us. Note that the
*  relevant dimension of the source data is updated here.
      CALL NDF_AMAP( NDF, 'WIDTH', SPAXIS, TYPE, 'READ', INDAT,
     :   INELM, STATUS )

*  Now grow from the source to the target, filling the target.
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL SPD_EBAAD( NDF__MXDIM, SPAXIS, ODIM, INELM, NELM,
     :      %VAL( CNF_PVAL(INDAT) ), %VAL( CNF_PVAL(PNTR) ), STATUS )
      ELSE
         CALL SPD_EBAAR( NDF__MXDIM, SPAXIS, ODIM, INELM, NELM,
     :      %VAL( CNF_PVAL(INDAT) ), %VAL( CNF_PVAL(PNTR) ), STATUS )
      END IF

*  Unmap the source data.
      CALL NDF_AUNMP( NDF, 'WIDTH', SPAXIS, STATUS )

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL NDF_ANNUL( ONDF, STATUS )

*  Return.
      END
