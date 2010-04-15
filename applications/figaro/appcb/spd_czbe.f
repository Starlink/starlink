      SUBROUTINE SPD_CZBE( NDF, AXIS, FRAME, BADVAL, IMIN, IMAX,
     :                     OMIN, OMAX, NELM, ODATA, STATUS )
*+
*  Name:
*     SPD_CZBE

*  Purpose:
*     Prepare nominated NDF slice for display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZBE( NDF, AXIS, FRAME,
*        BADVAL, IMIN, IMAX, OMIN, OMAX, NELM, ODATA, STATUS )

*  Description:
*     This routine extracts the specified two-dimensional slice from an
*     NDF on a grey/colour display. The NDF must be three-dimensional
*     (ignoring degenerate axes). The slice-counting axis must be
*     specified by number (include degenerate axes when counting). The
*     frame must be specified in terms of NDF pixel indices, i.e. should
*     be between the lower bound and the upper bound.
*
*     This routine uses NDF to create and map the slice with type _REAL.
*     It then converts the slice to INTEGER with the limits specified.
*     The _REAL slice is discarded and the INTEGER array returned.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF. It must have three axes with more
*        than 1 pixel.
*     AXIS = INTEGER (Given)
*        The number of the axis along which slices are counted.
*     FRAME = INTEGER (Given)
*        The number of the frame or slice. This should be between the
*        relevant lower and upper bound of the NDF pixel index.
*     BADVAL = INTEGER (Given)
*        The output value to replace bad values in the NDF slice.
*     IMIN = REAL (Given)
*        The lower threshold for input values.
*     IMAX = REAL (Given)
*        The upper threshold for input values. IMAX must not equal IMIN.
*     OMIN = INTEGER (Given)
*        The output value to correspond to input IMIN. OMIN must be
*        positive.
*     OMAX = INTEGER (Given)
*        The output value to correspond to input IMAX. OMAX must be
*        positive.
*     NELM = INTEGER (Given)
*        The size of the array provided by the calling routine for the
*        integer output data. This must be at least the size of the NDF
*        slice.
*     ODATA( NELM ) = INTEGER (Given)
*        The converted slice array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19 May 1994 (hme):
*        Original version.
*     2005 May 31 (MJC):
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      INTEGER AXIS
      INTEGER FRAME
      INTEGER BADVAL
      REAL    IMIN, IMAX
      INTEGER OMIN, OMAX
      INTEGER NELM

*  Arguments Returned:
      INTEGER ODATA( NELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SNDF               ! NDF section identifier
      INTEGER NDIM               ! NDF dimensionality
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      INTEGER PNTR               ! Pointer to section data
      INTEGER SNELM              ! Section size

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get NDF bounds.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Check frame number within bounds.
      IF ( FRAME .LT. LBND(AXIS) .OR. FRAME .GT. UBND(AXIS) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_CZBE_E02', 'SPD_CZBE: Error taking cube ' //
     :      'slice: Slice is outside cube.', STATUS )
         GO TO 500
      END IF

*  Replace the relevant bounds by given frame number.
      LBND(AXIS) = FRAME
      UBND(AXIS) = FRAME

*  Get the slice, map its data as _REAL.
      CALL NDF_SECT( NDF, NDIM, LBND, UBND, SNDF, STATUS )
      CALL NDF_MAP( SNDF, 'DATA', '_REAL', 'READ', PNTR, SNELM, STATUS )

*  Check size small enough to fit output array.
      IF ( SNELM .GT. NELM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_CZBE_E03', 'SPD_CZBE: Error taking cube ' //
     :      'slice: Slice is too big.', STATUS )
         GO TO 500
      END IF

*  Prepare for display.
      CALL SPD_WZBA( BADVAL, IMIN, IMAX, OMIN, OMAX, SNELM,
     :               %VAL( CNF_PVAL( PNTR ) ), ODATA, STATUS )

*  Tidy up.
 500  CONTINUE

*  Release slice.
      CALL NDF_ANNUL( SNDF, STATUS )

*  Return.
      END
