      SUBROUTINE SPD_EAFH( NDF, XLOC, TYPE, STATUS )
*+
*  Name:
*     SPD_EAFH

*  Purpose:
*     Create spectroscopic widths from the main NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAFH( NDF, XLOC, TYPE, STATUS )

*  Description:
*     This routine creates a new SPECWIDS NDF in the Specdre Extension
*     and sets the spectroscopic widths to the default values. The
*     default values are derived from the main NDF's width array. The
*     Extension must already exist. The SPECWIDS and the SPECVALS
*     structures must not exist. If the given NDF is a section rather
*     than a base NDF, then the appropriate base NDF will be used to
*     create the SPECWIDS NDF.

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
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'           ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EXIST              ! True if structure exists
      INTEGER I, J               ! Temporary integers
      INTEGER SPAXIS             ! The spectroscopic axis
      INTEGER INDAT              ! Pointer to grow source data
      INTEGER OUTDAT             ! Pointer to grow target data
      INTEGER BNDF               ! Main's base NDF indentifier
      INTEGER INELM              ! Size of grow source
      INTEGER ODIM( NDF__MXDIM ) ! Dimensions of grow target
      INTEGER ONELM              ! Total size of grow target
      INTEGER PLACE              ! NDF placeholder
      INTEGER XNDF               ! Structure's NDF identifier
      INTEGER NDIM               ! Number of axes of NDF
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that Extension NDF does not exist.
      CALL DAT_THERE( XLOC, XCMP7, EXIST, STATUS )
      IF ( EXIST ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_EXISTS', 'SPD_EAFH: Error:' //
     :      'The Extension structure SPECWIDS already exists.', STATUS )
         GO TO 500
      END IF

*  Check that corresponding SPECVALS do not exist.
      CALL DAT_THERE( XLOC, XCMP6, EXIST, STATUS )
      IF ( EXIST ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_EXISTS', 'SPD_EAFH: Error:' //
     :      'The Extension structure SPECVALS exists, ' //
     :      'in which case that information should be used' //
     :      'to derive default values.', STATUS )
         GO TO 500
      END IF

*  Check that requested type is valid.
      IF ( TYPE .NE. '_DOUBLE' .AND. TYPE .NE. '_REAL' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAFH: Error creating spectroscopic widths. ' //
     :      'Requested data type is invalid.', STATUS )
         GO TO 500
      END IF

*  Find the base of the main NDF and its bounds.
      CALL NDF_BASE(  NDF, BNDF, STATUS )
      CALL NDF_BOUND( BNDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Create the Extension NDF.
      CALL NDF_PLACE( XLOC, XCMP7, PLACE, STATUS )
      CALL NDF_NEW( TYPE, NDIM, LBND, UBND, PLACE, XNDF, STATUS )

*  Get spectroscopic axis number.
      CALL SPD_EABA( BNDF, .TRUE., SPAXIS, STATUS )

*  The default data are copied from the spectroscopic axis of the main
*  NDF. That 1-D array is grown to fill the whole N-D array.
*  Get/initialise the dimensions of the target and source data.
      CALL NDF_DIM( XNDF, NDF__MXDIM, ODIM, NDIM, STATUS )
      ONELM = 1
      DO 1 I = 1, NDF__MXDIM
         ONELM     = ONELM * ODIM(I)
 1    CONTINUE

*  Map the source data. If they do not exist, NDF will provide a decent
*  default. NDF will also do the type conversion for us. Note that the
*  relevant dimension of the source data is updated here.
      CALL NDF_AMAP( BNDF, 'WIDTH', SPAXIS, TYPE, 'READ', INDAT,
     :   INELM, STATUS )

*  Map the target data.
      CALL NDF_MAP( XNDF, 'DATA', TYPE, 'WRITE', OUTDAT, J, STATUS )

*  Now grow from the source to the target, filling the target.
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL SPD_EBAAD( NDF__MXDIM, SPAXIS, ODIM, INELM, ONELM,
     :      %VAL( CNF_PVAL(INDAT) ), %VAL( CNF_PVAL(OUTDAT) ), STATUS )
      ELSE
         CALL SPD_EBAAR( NDF__MXDIM, SPAXIS, ODIM, INELM, ONELM,
     :      %VAL( CNF_PVAL(INDAT) ), %VAL( CNF_PVAL(OUTDAT) ), STATUS )
      END IF

*  Unmap the source data.
      CALL NDF_AUNMP( BNDF, 'WIDTH', SPAXIS, STATUS )

*  Tidy up.
 500  CONTINUE

*  Annul the new NDF and the main's base NDF.
      CALL NDF_ANNUL( XNDF, STATUS )
      CALL NDF_ANNUL( BNDF, STATUS )

*  Return.
      END
