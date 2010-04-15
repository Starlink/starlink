      SUBROUTINE SPD_EAFF( NDF, XLOC, TYPE, BNELM, SPVALS, STATUS )
*+
*  Name:
*     SPD_EAFF

*  Purpose:
*     Create spectroscopic widths from given spectroscopic values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAFF( NDF, XLOC, TYPE, BNELM, SPVALS, STATUS )

*  Description:
*     This routine creates a new SPECWIDS NDF in the Specdre Extension
*     and sets the spectroscopic widths to the default values. The
*     Extension must already exist, the SPECWIDS structure must not
*     exist. The given NDF must be a base NDF. The array of
*     spectroscopic values must be given, since these determine the
*     widths according to the usual convention (SUN/33).

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF. This must be a base NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric type for creating the Extension NDF. This can be
*        '_REAL' or '_DOUBLE'.
*     BNELM = INTEGER (Given)
*        The size of the SPVALS array.
*     SPVALS( BNELM ) = (TYPE) (Given)
*        The array of spectroscopic values. The actual type of this
*        array must correspond to the given value for the argument TYPE.
*        The array is not referenced in this routine, but only passed on
*        to an appropriate routine. The size and shape of the array must
*        correspond to the size and shape of the given main NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set
*        -  if the structure to be created already exists,
*        -  if the given NDF is not a base NDF,
*        -  if BNELM differs from the size of the given NDF,
*        -  if the requested data type is invalid.

*  Notes:
*     This routine recognises Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02 Jul 1992 (hme):
*        Original version, adapted from SPABG and SPABM.
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
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) TYPE
      INTEGER BNELM
      INTEGER SPVALS( 1 )        ! Actually (TYPE) SPVALS( BNELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL ISBAS              ! True if given NDF is base
      LOGICAL EXIST              ! True if structure exists
      INTEGER J                  ! Temporary integer
      INTEGER SPAXIS             ! The spectroscopic axis
      INTEGER OUTDAT             ! Pointer to grow target data
      INTEGER PLACE              ! NDF placeholder
      INTEGER XNDF               ! Structure's NDF identifier
      INTEGER NDIM               ! Number of axes of NDF
      INTEGER DIM( NDF__MXDIM )  ! Dimensions of grow target
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that Extension NDF does not exist.
      CALL DAT_THERE( XLOC, XCMP7, EXIST, STATUS )
      IF ( EXIST ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_EXISTS', 'SPD_EAFF: Error:' //
     :      'The Extension structure SPECWIDS already exists.', STATUS )
         GO TO 500
      END IF

*  Check that the given NDF (the shape of which would be used) is a
*  base NDF rather than just a section.
      CALL NDF_ISBAS( NDF, ISBAS, STATUS )
      IF ( .NOT. ISBAS ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_NOTBAS',
     :      'SPD_EAFF: Error creating spectroscopic widths. ' //
     :      'Given NDF is not a base NDF.', STATUS )
         GO TO 500
      END IF

*  Check that the size of the given NDF is the same as the size of the
*  given array of spectroscopic values.
      CALL NDF_SIZE( NDF, J, STATUS )
      IF ( J .NE. BNELM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_NOTBAS',
     :      'SPD_EAFF: Error creating spectroscopic widths. ' //
     :      'The given array of spectroscopic values seems not to ' //
     :      'correspond to the given NDF.', STATUS )
         GO TO 500
      END IF

*  Check the data type for creation.
      IF ( TYPE .NE. '_REAL' .AND. TYPE .NE. '_DOUBLE' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAFF: Error creating spectroscopic widths. ' //
     :      'Requested data type is invalid.', STATUS )
         GO TO 500
      END IF

*  Create the Extension NDF.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      CALL NDF_PLACE( XLOC, XCMP7, PLACE, STATUS )
      CALL NDF_NEW( TYPE, NDIM, LBND, UBND, PLACE, XNDF, STATUS )

*  The SPECWIDS have to be based on the SPECVALS in the same way as
*  SUN/33 prescribes: Wn(i) = 1/2 | Cn(i+1) - Cn(i-1) |. At the edge
*  this probably becomes: Wn(i) = | Cn(i+1) - Cn(i) | etc.

*  Get spectroscopic axis number.
*  Get the array shape.
*  Map the new array.
      CALL SPD_EABA( NDF, .TRUE., SPAXIS, STATUS )
      CALL NDF_DIM( NDF, NDF__MXDIM, DIM, NDIM, STATUS )
      CALL NDF_MAP( XNDF, 'DATA', TYPE, 'WRITE', OUTDAT, J, STATUS )

*  Go and do the arithmetics.
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL SPD_EBABD( MIN(NDF__MXDIM,7), SPAXIS,
     :      DIM(1), DIM(2), DIM(3), DIM(4), DIM(5), DIM(6), DIM(7),
     :      SPVALS, %VAL( CNF_PVAL(OUTDAT) ), STATUS )
      ELSE
         CALL SPD_EBABR( MIN(NDF__MXDIM,7), SPAXIS,
     :      DIM(1), DIM(2), DIM(3), DIM(4), DIM(5), DIM(6), DIM(7),
     :      SPVALS, %VAL( CNF_PVAL(OUTDAT) ), STATUS )
      END IF

*  Tidy up.
 500  CONTINUE

*  Annul the new NDF.
      CALL NDF_ANNUL( XNDF, STATUS )

*  Return.
      END
