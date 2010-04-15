      SUBROUTINE SPD_EAJG( NDF, XLOC, TYPE, LABEL, UNITS,
     :   PNTR, ONDF, NELM, STATUS )
*+
*  Name:
*     SPD_EAJG

*  Purpose:
*     Read-access non-existing coordinate values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAJG( NDF, XLOC, TYPE, LABEL, UNITS,
*        PNTR, ONDF, NELM, STATUS )

*  Description:
*     This routine creates and accesses temporary COORD1/2 NDFs as they
*     would be accessed in the Specdre Extension. The coordinate values
*     will be the default values. For this routine it does not matter
*     if the Extension exists or not, or if COORD1/2 exist or not.
*     The only information needed from the Extension is the number of
*     the spectrocopic axis.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF. If the Specdre Extension does not
*        exist, then this argument must be given as DAT__NOLOC.
*     TYPE( 2 ) = CHARACTER * ( * ) (Given)
*        The numeric types for creating the temporary NDF. These can be
*        '_REAL' or '_DOUBLE'.
*     LABEL( 2 ) = CHARACTER * ( * ) (Returned)
*        The Extension NDFs' data component labels.
*     UNITS( 2 ) = CHARACTER * ( * ) (Returned)
*        The Extension NDFs' data component units.
*     PNTR( 2 ) = INTEGER (Returned)
*        The pointers to which the arrays are mapped.
*     ONDF( 2 ) = INTEGER (Returned)
*        The identifiers of the temporary NDFs.
*     NELM( 2 ) = INTEGER (Returned)
*        The numbers of elements in the mapped arrays.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set if the requested data type is
*        invalid.

*  Notes:
*     This routine recognises Specdre Extension v. 1.1.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     04 Aug 1994 (hme):
*        Original version, adapted from SPEEG.
*     21 Sep 1994 (hme):
*        Use grow routine properly.
*     30 Jan 1995 (hme):
*        Remove unused variables STA/ENDPIX.
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) TYPE( 2 )

*  Arguments Returned:
      CHARACTER * ( * ) LABEL( 2 )
      CHARACTER * ( * ) UNITS( 2 )
      INTEGER PNTR( 2 )
      INTEGER ONDF( 2 )
      INTEGER NELM( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Temporary integer
      INTEGER SPAXIS             ! The spectroscopic axis
      INTEGER AXIS( 2 )          ! Value-providing axes
      INTEGER INDAT              ! Pointer to grow source data
      INTEGER PLACE              ! NDF placeholder
      INTEGER NDIM               ! Number of axes of NDF
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF
      INTEGER LDIM( NDF__MXDIM ) ! Dimensions of grow source
      INTEGER ODIM( NDF__MXDIM ) ! Dimensions of grow target

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that requested types are valid.
      IF ( TYPE(1) .NE. '_DOUBLE' .AND. TYPE(1) .NE. '_REAL' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAJG: Error creating coordinate values. ' //
     :      'Requested data type is invalid.', STATUS )
         GO TO 500
      END IF
      IF ( TYPE(2) .NE. '_DOUBLE' .AND. TYPE(2) .NE. '_REAL' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAJG: Error creating coordinate values. ' //
     :      'Requested data type is invalid.', STATUS )
         GO TO 500
      END IF

*  Get spectroscopic axis number. Work out which axes provide the
*  values.
      CALL SPD_EABA( NDF, (XLOC.NE.DAT__NOLOC), SPAXIS, STATUS )
      IF ( SPAXIS .EQ. 1 ) THEN
         AXIS(1) = 2
         AXIS(2) = 3
      ELSE IF ( SPAXIS .EQ. 2 ) THEN
         AXIS(1) = 1
         AXIS(2) = 3
      ELSE
         AXIS(1) = 1
         AXIS(2) = 2
      END IF

*  Find the main NDF bounds.
*  Modify the bounds for creating the Extension NDFs.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      LBND(SPAXIS) = 1
      UBND(SPAXIS) = 1

*  Create the temporary NDFs.
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( TYPE(1), NDIM, LBND, UBND, PLACE, ONDF(1), STATUS )
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( TYPE(2), NDIM, LBND, UBND, PLACE, ONDF(2), STATUS )

*  Get the labels and units from the axes of the main NDF.
*  We omit putting these into the temporary NDFs, assuming that the
*  calling routine will make no further access to label and unit via
*  the returned NDF identifiers.
      LABEL(1) = 'unknown'
      UNITS(1) = 'unknown'
      CALL NDF_ACGET( NDF, 'LABEL', AXIS(1), LABEL(1), STATUS )
      CALL NDF_ACGET( NDF, 'UNITS', AXIS(1), UNITS(1), STATUS )
      LABEL(2) = 'unknown'
      UNITS(2) = 'unknown'
      CALL NDF_ACGET( NDF, 'LABEL', AXIS(2), LABEL(2), STATUS )
      CALL NDF_ACGET( NDF, 'UNITS', AXIS(2), UNITS(2), STATUS )

*  Generate the data for POSIT1.
*  =============================

*     The default data are copied from one axis of the main NDF. That
*     That 1-D array is grown to fill the whole N-D array.
*     Get/initialise the dimensions of the target and source data.
         CALL NDF_DIM( ONDF(1), NDF__MXDIM, ODIM, NDIM, STATUS )
         DO 1 I = 1, NDF__MXDIM
            LDIM(I)   = 1
 1       CONTINUE

*     Map the target data.
         CALL NDF_MAP( ONDF(1), 'DATA', TYPE(1), 'WRITE',
     :      PNTR(1), NELM(1), STATUS )

*     Map the source data. If they do not exist, NDF will provide a
*     decent default. NDF will also do the type conversion for us.
*     Note that the relevant dimension of the source data is updated
*     here.
         CALL NDF_AMAP( NDF, 'CENTRE', AXIS(1), TYPE(1), 'READ', INDAT,
     :      LDIM(AXIS(1)), STATUS )

*     Now grow from the source to the target, filling the target.
         IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
            CALL SPD_EBAAD( NDF__MXDIM, AXIS(1), ODIM, LDIM(AXIS(1)),
     :                      NELM(1), %VAL( CNF_PVAL(INDAT) ),
     :                      %VAL( CNF_PVAL(PNTR(1) )), STATUS )
         ELSE
            CALL SPD_EBAAR( NDF__MXDIM, AXIS(1), ODIM, LDIM(AXIS(1)),
     :                      NELM(1), %VAL( CNF_PVAL(INDAT) ),
     :                      %VAL( CNF_PVAL(PNTR(1) )), STATUS )
         END IF

*     Unmap the source data.
         CALL NDF_AUNMP( NDF, 'CENTRE', AXIS(1), STATUS )

*  Generate the data for POSIT2.
*  =============================

*     See above.
         CALL NDF_DIM( ONDF(2), NDF__MXDIM, ODIM, NDIM, STATUS )
         DO 2 I = 1, NDF__MXDIM
            LDIM(I)   = 1
 2       CONTINUE

         CALL NDF_MAP( ONDF(2), 'DATA', TYPE(2), 'WRITE',
     :      PNTR(2), NELM(2), STATUS )

         CALL NDF_AMAP( NDF, 'CENTRE', AXIS(2), TYPE(2), 'READ', INDAT,
     :      LDIM(AXIS(2)), STATUS )

         IF ( TYPE(2) .EQ. '_DOUBLE' ) THEN
            CALL SPD_EBAAD( NDF__MXDIM, AXIS(2), ODIM, LDIM(AXIS(2)),
     :                      NELM(2), %VAL( CNF_PVAL(INDAT) ),
     :                      %VAL( CNF_PVAL(PNTR(2) )), STATUS )
         ELSE
            CALL SPD_EBAAR( NDF__MXDIM, AXIS(2), ODIM, LDIM(AXIS(2)),
     :                      NELM(2), %VAL( CNF_PVAL(INDAT) ),
     :                      %VAL( CNF_PVAL(PNTR(2) )), STATUS )
         END IF

*     Unmap the source data.
         CALL NDF_AUNMP( NDF, 'CENTRE', AXIS(2), STATUS )

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF_ANNUL( ONDF(1), STATUS )
         CALL NDF_ANNUL( ONDF(2), STATUS )
      END IF

*  Return.
      END
