      SUBROUTINE SPD_EAEF( NDF, XLOC, TYPE, STATUS )
*+
*  Name:
*     SPD_EAEF

*  Purpose:
*     Create spectroscopic values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAEF( NDF, XLOC, TYPE, STATUS )

*  Description:
*     This routine creates a new SPECVALS NDF in the Specdre Extension
*     and sets the spectroscopic values to the default values. The
*     Extension must already exist, the SPECVALS structure must not
*     exist. If the given NDF is a section rather than a base NDF, then
*     the appropriate base NDF will be used to create the SPECVALS NDF.

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
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13 Mar 1992 (hme):
*        Original version (SPAAM).
*     18 May 1992 (hme):
*        Report an error if an attempt is made to create permanent
*        SPECVALS for a non-base NDF (SPAAM).
*     15 Jun 1992 (hme):
*        Adapted to SPE-routines' argument list convention. Rationalised
*        functionality.
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
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
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
      CHARACTER * ( 64 ) LABEL   ! SPECVALS label
      CHARACTER * ( 64 ) UNITS   ! SPECVALS unit

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that Extension NDF does not exist.
      CALL DAT_THERE( XLOC, XCMP6, EXIST, STATUS )
      IF ( EXIST ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_EXISTS', 'SPD_EAEF: Error:' //
     :      'The Extension structure SPECVALS already exists.', STATUS )
         GO TO 500
      END IF

*  Check that requested type is valid.
      IF ( TYPE .NE. '_DOUBLE' .AND. TYPE .NE. '_REAL' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAEF: Error creating spectroscopic values. ' //
     :      'Requested data type is invalid.', STATUS )
         GO TO 500
      END IF

*  Find the base of the main NDF and its bounds.
      CALL NDF_BASE(  NDF, BNDF, STATUS )
      CALL NDF_BOUND( BNDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Create the Extension NDF.
      CALL NDF_PLACE( XLOC, XCMP6, PLACE, STATUS )
      CALL NDF_NEW( TYPE, NDIM, LBND, UBND, PLACE, XNDF, STATUS )

*  Get spectroscopic axis number.
*  Copy the label and unit from the spectroscopic axis of the
*  base NDF.
      CALL SPD_EABA( BNDF, .TRUE., SPAXIS, STATUS )
      LABEL = 'unknown'
      UNITS = 'unknown'
      CALL NDF_ACGET( BNDF, 'LABEL', SPAXIS, LABEL, STATUS )
      CALL NDF_ACGET( BNDF, 'UNITS', SPAXIS, UNITS, STATUS )
      CALL NDF_CPUT( LABEL, XNDF, XC6L, STATUS )
      CALL NDF_CPUT( UNITS, XNDF, XC6U, STATUS )

*  The default data are copied from the spectroscopic axis of the main
*  NDF. That 1-D array is grown to fill the whole N-D array.
*  Get/initialise the dimensions of the target and source data.
      CALL NDF_DIM( XNDF, NDF__MXDIM, ODIM, NDIM, STATUS )
      ONELM = 1
      DO 1 I = 1, NDF__MXDIM
         ONELM = ONELM * ODIM(I)
 1    CONTINUE

*  Map the source data. If they do not exist, NDF will provide a decent
*  default. NDF will also do the type conversion for us. Note that the
*  relevant dimension of the source data is updated here.
      CALL NDF_AMAP( BNDF, 'CENTRE', SPAXIS, TYPE, 'READ', INDAT,
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
      CALL NDF_AUNMP( BNDF, 'CENTRE', SPAXIS, STATUS )

*  Tidy up.
 500  CONTINUE

*  Annul the new NDF and the main's base NDF.
      CALL NDF_ANNUL( XNDF, STATUS )
      CALL NDF_ANNUL( BNDF, STATUS )

*  Return.
      END
