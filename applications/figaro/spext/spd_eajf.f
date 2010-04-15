      SUBROUTINE SPD_EAJF( NDF, XLOC, TYPE, STATUS )
*+
*  Name:
*     SPD_EAJF

*  Purpose:
*     Create coordinate values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAJF( NDF, XLOC, TYPE, STATUS )

*  Description:
*     This routine creates new COORD1/2 NDFs in the Specdre Extension
*     and sets the coordinate values to the default values. The
*     Extension must already exist, the COORD1 and COORD2 structures
*     must not exist. If the given NDF is a section rather than a base
*     NDF, then the appropriate base NDF will be used to create the
*     COORD1/2 NDFs.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF.
*     TYPE( 2 ) = CHARACTER * ( * ) (Given)
*        The numeric types for creating the Extension NDFs. They can be
*        '_REAL' or '_DOUBLE'.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set
*        -  if the structure to be created already exists,
*        -  if the requested data type is invalid.

*  Notes:
*     This routine recognises Specdre Extension v. 1.1.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     04 Aug 1994 (hme):
*        Original version, adapted from SPEEF.
*     21 Sep 1994 (hme):
*        Use grow routine properly.
*     30 Jan 1995 (hme):
*        Remove unused variables STA/ENDPIX.
*     29 Nov 1995 (hme):
*        Take care of the case where there the base NDF has only one or
*        two axes. Before an error would result in finding the label,
*        units, or centres of an axis beyond the dimensionality of the
*        NDF. Now use 'unknown', 'unknown', 0.5 for COORD.
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
      CHARACTER * ( * ) TYPE( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EXIST              ! True if structure exists
      INTEGER I, J               ! Temporary integers
      INTEGER SPAXIS             ! The spectroscopic axis
      INTEGER AXIS( 2 )          ! Value-providing axes
      INTEGER INDAT              ! Pointer to grow source data
      INTEGER OUTDAT             ! Pointer to grow target data
      INTEGER BNDF               ! Main's base NDF indentifier
      INTEGER LDIM( NDF__MXDIM ) ! Dimensions of grow source
      INTEGER ODIM( NDF__MXDIM ) ! Dimensions of grow target
      INTEGER ONELM              ! Total size of grow target
      INTEGER PLACE              ! NDF placeholder
      INTEGER XNDF( 2 )          ! Structures' NDF identifiers
      INTEGER NDIM               ! Number of axes of NDF
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF
      CHARACTER * ( 64 ) LABEL   ! COORD1/2 label
      CHARACTER * ( 64 ) UNITS   ! COORD1/2 unit

*.


*  Checks.
*  =======

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that Extension NDF does not exist.
      CALL DAT_THERE( XLOC, XCMP10, EXIST, STATUS )
      IF ( EXIST ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_EXISTS', 'SPD_EAJF: Error: The ' //
     :      'Extension structure ' // XCMP10 // ' already exists.',
     :      STATUS )
         GO TO 500
      END IF
      CALL DAT_THERE( XLOC, XCMP11, EXIST, STATUS )
      IF ( EXIST ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_EXISTS', 'SPD_EAJF: Error: The ' //
     :      'Extension structure ' // XCMP11 // ' already exists.',
     :      STATUS )
         GO TO 500
      END IF

*  Check that requested type is valid.
      IF ( TYPE(1) .NE. '_DOUBLE' .AND. TYPE(1) .NE. '_REAL' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAJF: Error creating coordinate values. ' //
     :      'Requested data type is invalid.', STATUS )
         GO TO 500
      END IF
      IF ( TYPE(2) .NE. '_DOUBLE' .AND. TYPE(2) .NE. '_REAL' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAJF: Error creating coordinate values. ' //
     :      'Requested data type is invalid.', STATUS )
         GO TO 500
      END IF

*  Get spectroscopic axis number. Work out which axes provide the
*  values.
      CALL SPD_EABA( NDF, .TRUE., SPAXIS, STATUS )
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


*  Find main NDF and create extension NDFs.
*  ========================================

*  Find the base of the main NDF and its bounds.
*  Modify the bounds for creating the Extension NDFs.
      CALL NDF_BASE(  NDF, BNDF, STATUS )
      CALL NDF_BOUND( BNDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      LBND(SPAXIS) = 1
      UBND(SPAXIS) = 1

*  Create the Extension NDFs.
      CALL NDF_PLACE( XLOC, XCMP10, PLACE, STATUS )
      CALL NDF_NEW( TYPE(1), NDIM, LBND, UBND, PLACE, XNDF(1), STATUS )
      CALL NDF_PLACE( XLOC, XCMP11, PLACE, STATUS )
      CALL NDF_NEW( TYPE(2), NDIM, LBND, UBND, PLACE, XNDF(2), STATUS )


*  Set label and unit.
*  ===================

*  For COORD1 and 2 respectively:
*  If base NDF has sufficient dimensionality.
*     Get label and units from base NDF axis, defaulting to 'unknown'.
*  Else (base NDF does not have that many axes).
*     Simply put 'unknown' as label and units.
      LABEL = 'unknown'
      UNITS = 'unknown'
      IF ( AXIS(1) .LE. NDIM ) THEN
         CALL NDF_ACGET( BNDF, 'LABEL', AXIS(1), LABEL, STATUS )
         CALL NDF_ACGET( BNDF, 'UNITS', AXIS(1), UNITS, STATUS )
      END IF
      CALL NDF_CPUT( LABEL, XNDF(1), XC10L, STATUS )
      CALL NDF_CPUT( UNITS, XNDF(1), XC10U, STATUS )
      LABEL = 'unknown'
      UNITS = 'unknown'
      IF ( AXIS(2) .LE. NDIM ) THEN
         CALL NDF_ACGET( BNDF, 'LABEL', AXIS(2), LABEL, STATUS )
         CALL NDF_ACGET( BNDF, 'UNITS', AXIS(2), UNITS, STATUS )
      END IF
      CALL NDF_CPUT( LABEL, XNDF(2), XC11L, STATUS )
      CALL NDF_CPUT( UNITS, XNDF(2), XC11U, STATUS )


*  Generate the data for COORD1.
*  =============================

*     The default data are copied from one axis of the
*     main NDF. That 1-D array is grown to fill the whole N-D array.
*     Get/initialise the dimensions of the target and source data.
         CALL NDF_DIM( XNDF(1), NDF__MXDIM, ODIM, NDIM, STATUS )
         ONELM = 1
         DO 1 I = 1, NDF__MXDIM
            LDIM(I)   = 1
            ONELM     = ONELM * ODIM(I)
 1       CONTINUE

*     Map the target data.
         CALL NDF_MAP( XNDF(1), 'DATA', TYPE(1), 'WRITE',
     :      OUTDAT, J, STATUS )

*     Map the source data. If they do not exist, but the dimensionality
*     of the base NDF is sufficient, then NDF routines will provide a
*     decent default. NDF will also do the type conversion for us.
*     Note that the relevant dimension of the source data is updated
*     here.

*     If there is a first non-spectroscopic axis.
         IF ( AXIS(1) .LE. NDIM ) THEN

*        Map the source data. If they do not exist then NDF routines
*        will provide a decent default. NDF will also do the type
*        conversion for us. Note that the relevant dimension of the
*        source data is updated here.
            CALL NDF_AMAP( BNDF, 'CENTRE', AXIS(1), TYPE(1), 'READ',
     :         INDAT, LDIM(AXIS(1)), STATUS )

*        Then grow from the source to the target, filling the target.
            IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
               CALL SPD_EBAAD( NDF__MXDIM, AXIS(1), ODIM, LDIM(AXIS(1)),
     :                         ONELM, %VAL( CNF_PVAL(INDAT) ),
     :                         %VAL( CNF_PVAL(OUTDAT) ), STATUS )
            ELSE
               CALL SPD_EBAAR( NDF__MXDIM, AXIS(1), ODIM, LDIM(AXIS(1)),
     :                         ONELM, %VAL( CNF_PVAL(INDAT) ),
     :                         %VAL( CNF_PVAL(OUTDAT) ), STATUS )
            END IF

*        And unmap the source data.
            CALL NDF_AUNMP( BNDF, 'CENTRE', AXIS(1), STATUS )

*     Else (base NDF presumably 1-D).
         ELSE

*        There are no source data to map. There is also no need to
*        update LDIM(AXIS(1)), since 1 will be fine.
*        Just grow the numer 1/2 (default centre of first pixel) into
*        the target.
            IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
               CALL SPD_EBAAD( NDF__MXDIM, AXIS(1), ODIM, LDIM(AXIS(1)),
     :            ONELM, 0.5D0, %VAL( CNF_PVAL(OUTDAT) ), STATUS )
            ELSE
               CALL SPD_EBAAR( NDF__MXDIM, AXIS(1), ODIM, LDIM(AXIS(1)),
     :            ONELM, 0.5, %VAL( CNF_PVAL(OUTDAT) ), STATUS )
            END IF

         END IF

*     Unmap the target data.
         CALL NDF_UNMAP( XNDF(1), 'DATA', STATUS )


*  Generate the data for COORD2.
*  =============================

*     See above.
         CALL NDF_DIM( XNDF(2), NDF__MXDIM, ODIM, NDIM, STATUS )
         ONELM = 1
         DO 2 I = 1, NDF__MXDIM
            LDIM(I)   = 1
            ONELM     = ONELM * ODIM(I)
 2       CONTINUE

         CALL NDF_MAP( XNDF(2), 'DATA', TYPE(2), 'WRITE',
     :      OUTDAT, J, STATUS )

         IF ( AXIS(2) .LE. NDIM ) THEN
            CALL NDF_AMAP( BNDF, 'CENTRE', AXIS(2), TYPE(2), 'READ',
     :         INDAT, LDIM(AXIS(2)), STATUS )
            IF ( TYPE(2) .EQ. '_DOUBLE' ) THEN
               CALL SPD_EBAAD( NDF__MXDIM, AXIS(2), ODIM, LDIM(AXIS(2)),
     :                         ONELM, %VAL( CNF_PVAL(INDAT) ),
     :                         %VAL( CNF_PVAL(OUTDAT) ), STATUS )
            ELSE
               CALL SPD_EBAAR( NDF__MXDIM, AXIS(2), ODIM, LDIM(AXIS(2)),
     :                         ONELM, %VAL( CNF_PVAL(INDAT) ),
     :                         %VAL( CNF_PVAL(OUTDAT) ), STATUS )
            END IF
            CALL NDF_AUNMP( BNDF, 'CENTRE', AXIS(2), STATUS )
         ELSE
            IF ( TYPE(2) .EQ. '_DOUBLE' ) THEN
               CALL SPD_EBAAD( NDF__MXDIM, AXIS(2), ODIM, LDIM(AXIS(2)),
     :             ONELM, 0.5D0, %VAL( CNF_PVAL(OUTDAT) ), STATUS )
            ELSE
               CALL SPD_EBAAR( NDF__MXDIM, AXIS(2), ODIM, LDIM(AXIS(2)),
     :            ONELM, 0.5, %VAL( CNF_PVAL(OUTDAT) ), STATUS )
            END IF
         END IF

         CALL NDF_UNMAP( XNDF(2), 'DATA', STATUS )


*  Tidy up.
*  ========

 500  CONTINUE

*  Annul the new NDFs and the main's base NDF.
      CALL NDF_ANNUL( XNDF(1), STATUS )
      CALL NDF_ANNUL( XNDF(2), STATUS )
      CALL NDF_ANNUL( BNDF,    STATUS )

*  Return.
      END
