      SUBROUTINE SPD_EAFE( NDF, XLOC, ACCESS, TYPE,
     :   PNTR, ONDF, NELM, STATUS )
*+
*  Name:
*     SPD_EAFE

*  Purpose:
*     Access existing spectroscopic widths.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAFE( NDF, XLOC, ACCESS, TYPE, PNTR, ONDF, NELM, STATUS )

*  Description:
*     This routine will access an array of spectroscopic pixel widths
*     from the SPECWIDS structure in the Specdre Extension if this is
*     known to exist. This rotuine will access a sectin with the same
*     bounds as the main NDF.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access mode required. This can be 'READ', 'WRITE', or
*        'UPDATE'.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric type for mapping the width array. This can be any
*        NDF data type.
*     PNTR = INTEGER (Returned)
*        The pointer to the mapped array.
*     ONDF = INTEGER (Returned)
*        The identifier of the SPECWIDS NDF, which is a component of the
*        Specdre Extension.
*     NELM = INTEGER (Returned)
*        The total number of elements in the mapped array.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set
*        -  if the main NDF is a base NDF and its shape does not match
*           the Extension NDF,
*        -  if the Extension NDF section to be returned is accessed for
*           read or update and contains bad values.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     18-MAR-1992 (HME):
*        Original version (SPABL).
*     15-JUN-1992 (HME):
*        Adapted to SPE-routines' argument list convention.
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
      CHARACTER * ( * ) ACCESS
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER PNTR
      INTEGER ONDF
      INTEGER NELM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL ISBAS              ! True if given NDF is base
      LOGICAL MATCH              ! True if NDF dimensions match
      LOGICAL BADEX              ! True if NDF sect. contains bad pixels
      INTEGER I                  ! Loop index
      INTEGER TNDF               ! Temporary NDF identifier
      INTEGER NDIM1               ! Number of axes of main NDF
      INTEGER LBND1( NDF__MXDIM ) ! Lower bounds of main NDF
      INTEGER UBND1( NDF__MXDIM ) ! Upper bounds of main NDF
      INTEGER NDIM2              ! Number of axes of Extension NDF
      INTEGER LBND2( NDF__MXDIM ) ! Lower bounds of Extension NDF
      INTEGER UBND2( NDF__MXDIM ) ! Upper bounds of Extension NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out if the main NDF is a base NDF or a section.
*  Find the SPECWIDS NDF.
*  Get the NDFs' bounds.
      CALL NDF_ISBAS( NDF, ISBAS, STATUS )
      CALL NDF_FIND( XLOC, XCMP7, ONDF, STATUS )
      CALL NDF_BOUND(  NDF, NDF__MXDIM, LBND1, UBND1, NDIM1, STATUS )
      CALL NDF_BOUND( ONDF, NDF__MXDIM, LBND2, UBND2, NDIM2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  If main NDF is base, check bounds.
      IF ( ISBAS ) THEN
         MATCH = .TRUE.
         DO 1 I = 1, NDF__MXDIM
            IF ( LBND2(I) .NE. LBND1(I) .OR. UBND2(I) .NE. UBND1(I) )
     :         MATCH = .FALSE.
 1       CONTINUE
         IF ( .NOT. MATCH ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPE_INCSHP', 'SPD_EAFE: Error: ' //
     :         'Main and Extension shapes are inconsistent.', STATUS )
            GO TO 500
         END IF

*  Else (if main NDF is section), take section and abandon base.
      ELSE
         CALL NDF_SECT(  ONDF, NDIM1, LBND1, UBND1, TNDF, STATUS )
         CALL NDF_ANNUL( ONDF, STATUS )
         ONDF = TNDF
      END IF

*  If the access is read or update, check that ONDF has no bad values.
      IF ( ACCESS(:5) .NE. 'WRITE' ) THEN
         BADEX = .FALSE.
         CALL NDF_BAD( ONDF, 'DATA', .TRUE., BADEX, STATUS )
      END IF
      IF ( BADEX ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVDAT', 'SPD_EAFE: Error: ' //
     :      'There are bad spectroscopic widths present.', STATUS )
         GO TO 500
      END IF

*  Map the array.
      CALL NDF_MAP( ONDF, 'DATA', TYPE, ACCESS, PNTR, NELM, STATUS )

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL NDF_ANNUL( ONDF, STATUS )

*  Return.
      END
