      SUBROUTINE SPD_EAFG( NDF, TYPE, BNELM, SPVALS,
     :   PNTR, ONDF, NELM, STATUS )
*+
*  Name:
*     SPD_EAFG

*  Purpose:
*     Read-access non-existing spectr. widths from given spectr. values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAFG( NDF, TYPE, BNELM, SPVALS, PNTR, ONDF, NELM, STATUS )

*  Description:
*     This routine creates and accesses a temporary SPECWIDS NDF as it
*     would be accessed in the Specdre Extension. The spectroscopic
*     values will be the default values. For this routine it does not
*     matter if the Extension exists or not, or if the SPECWIDS
*     structure exists or not. However, this routine needs to know about
*     the main NDF and its base NDF. And it needs to be given the array
*     of spectroscopic values corresponding to the base NDF. The
*     spectroscopic values must be given, since these determine the
*     widths according to the usual convention (SUN/33). While the given
*     spectroscopic values must correspond to the base NDF, the returned
*     spectroscopic widths will correspond to the main NDF (which may be
*     a section of the base NDF). This complication is because the
*     widths of pixels on the edge of the array depend on positions of
*     pixels outside the array. If these are really unavailable a
*     suitable extrapolation is applied. But if NDF is a section, then
*     its base BNDF must be consulted because it may provide the missing
*     pixels.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric type for creating the temporary NDF. This can be
*        '_REAL' or '_DOUBLE'.
*     BNELM = INTEGER (Given)
*        The size of the SPVALS array.
*     SPVALS( BNELM ) = (TYPE) (Given)
*        The array of spectroscopic values. The actual type of this
*        array must correspond to the given value for the argument TYPE.
*        The array is not referenced in this routine, but only passed on
*        to an appropriate routine. The size and shape of the array must
*        correspond to the size and shape of the base NDF (!) of the
*        given main NDF. This array must not contain bad values.
*     PNTR = INTEGER (Returned)
*        The pointer to which the requested width array is mapped. This
*        is the data array of ONDF.
*     ONDF = INTEGER (Returned)
*        The identifier of the requested temporary NDF. This is in
*        general a section corresponding to the main NDF, not to the
*        main's base NDF.
*     NELM = INTEGER (Returned)
*        The number of elements in the mapped array.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set
*        -  if BNELM is not the size of the base of NDF,
*        -  if the requested data type is invalid.

*  Notes:
*     This routine recognises Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02 Jul 1992 (hme):
*        Original version.
*     24 Feb 1994 (hme):
*        Change to SPD_E*, call SPD_E*<T> instead of SPABJ<T>.
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) TYPE
      INTEGER BNELM
      INTEGER SPVALS( 1 )        ! Actually (TYPE) SPVALS( BNELM )

*  Arguments Returned:
      INTEGER PNTR
      INTEGER ONDF
      INTEGER NELM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BNDF               ! The base of the given main NDF

      LOGICAL ISBAS              ! True if base NDF
      INTEGER SPAXIS             ! The spectroscopic axis
      INTEGER PLACE              ! NDF placeholder
      INTEGER TNELM              ! Temporary integer
      INTEGER NDIM               ! Number of axes of NDF
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensionality
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that requested type is valid.
      IF ( TYPE .NE. '_DOUBLE' .AND. TYPE .NE. '_REAL' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAFG: Error creating spectroscopic widths. ' //
     :      'Requested data type is invalid.', STATUS )
         GO TO 500
      END IF

*  Find the main's base NDF, check that its size is the same as BNELM.
      CALL NDF_BASE( NDF, BNDF, STATUS )
      CALL NDF_SIZE(  BNDF, TNELM, STATUS )
      CALL NDF_BOUND( BNDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      IF ( TNELM .NE. BNELM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_NOTBAS',
     :      'SPD_EAFG: Error creating spectroscopic widths. ' //
     :      'The given array of spectroscopic values seems not to ' //
     :      'correspond to the base of the given NDF.', STATUS )
         GO TO 500
      END IF

*  Create the temporary base NDF.
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( TYPE, NDIM, LBND, UBND, PLACE, ONDF, STATUS )

*  Get spectroscopic axis number.
*  Get the array shape.
*  Map the new base array.
      CALL SPD_EABA( BNDF, .TRUE., SPAXIS, STATUS )
      CALL NDF_DIM( BNDF, NDF__MXDIM, DIM, NDIM, STATUS )
      CALL NDF_MAP( ONDF, 'DATA', TYPE, 'WRITE', PNTR, NELM, STATUS )

*  Go and do the arithmetics.
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL SPD_EBABD( MIN(NDF__MXDIM,7), SPAXIS,
     :      DIM(1), DIM(2), DIM(3), DIM(4), DIM(5), DIM(6), DIM(7),
     :      SPVALS, %VAL( CNF_PVAL(PNTR) ), STATUS )
      ELSE
         CALL SPD_EBABR( MIN(NDF__MXDIM,7), SPAXIS,
     :      DIM(1), DIM(2), DIM(3), DIM(4), DIM(5), DIM(6), DIM(7),
     :      SPVALS, %VAL( CNF_PVAL(PNTR) ), STATUS )
      END IF

*  This would be it, if NDF were a base and the same as BNDF. If NDF is
*  a section of BNDF, we want ONDF and PNTR to be the corresponding
*  section of what they are now.
      CALL NDF_ISBAS( NDF, ISBAS, STATUS )
      IF ( .NOT. ISBAS ) THEN
         CALL NDF_UNMAP( ONDF, 'DATA', STATUS )
         CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
         CALL NDF_SBND( NDIM, LBND, UBND, ONDF, STATUS )
         CALL NDF_MAP( ONDF, 'DATA', TYPE, 'UPDATE',
     :      PNTR, NELM, STATUS )
      END IF

*  Tidy up.
 500  CONTINUE
      CALL NDF_ANNUL( BNDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL NDF_ANNUL( ONDF, STATUS )

*  Return.
      END
