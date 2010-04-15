      SUBROUTINE SPD_EAJE( NDF, XLOC, ACCESS, TYPE, LABEL, UNITS,
     :   PNTR, ONDF, NELM, STATUS )
*+
*  Name:
*     SPD_EAJE

*  Purpose:
*     Access existing coordinate arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAJE( NDF, XLOC, ACCESS, TYPE, LABEL, UNITS,
*        PNTR, ONDF, NELM, STATUS )

*  Description:
*     This routine will two arrays of coordinates from the COORD1/2
*     structures in the Specdre Extension if these are known to exist.
*     This routine will access sections with the corresponding bounds
*     as the main NDF.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access mode required. This can be 'READ', 'WRITE', or
*        'UPDATE'.
*     TYPE( 2 ) = CHARACTER * ( * ) (Given)
*        The numeric types for mapping the arrays. These can be any
*        NDF data type.
*     LABEL( 2 ) = CHARACTER * ( * ) (Given and Returned)
*        The Extension NDFs' data component labels.
*        If access is read, this argument is a returned argument.
*        If access is write, this argument is a given argument.
*        If access is update and this argument is given blank, this
*        argument is a returned argument. Otherwise this is a given
*        argument.
*     UNITS( 2 ) = CHARACTER * ( * ) (Given and Returned)
*        The Extension NDFs' data component units.
*        If access is read, this argument is a returned argument.
*        If access is write, this argument is a given argument.
*        If access is update and this argument is given blank, this
*        argument is a returned argument. Otherwise this is a given
*        argument.
*     PNTR( 2 ) = INTEGER (Returned)
*        The pointers to which the arrays are mapped.
*     ONDF( 2 ) = INTEGER (Returned)
*        The identifiers of the COORD1/2 NDFs, which are components of
*        the Specdre Extension.
*     NELM( 2 ) = INTEGER (Returned)
*        The numbers of elements in the mapped arrays.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set
*        -  if the main NDF is a base NDF and its shape does not match
*           the Extension NDF,
*        -  if the Extension NDF section to be returned is accessed for
*           read or update and contains bad values.

*  Notes:
*     This routine recognises the Specdre Extension v. 1.1.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     04 Aug 1994 (hme):
*        Original version, adapted from SPEEE.
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

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) ACCESS
      CHARACTER * ( * ) TYPE( 2 )

*  Arguments Given and Returned:
      CHARACTER * ( * ) LABEL( 2 )
      CHARACTER * ( * ) UNITS( 2 )

*  Arguments Returned:
      INTEGER PNTR( 2 )
      INTEGER ONDF( 2 )
      INTEGER NELM( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL ISBAS              ! True if given NDF is base
      LOGICAL MATCH              ! True if NDF dimensions match
      LOGICAL BADEX( 2 )         ! True if NDF sect. contains bad pixels
      INTEGER I                  ! Loop index
      INTEGER SPAXIS             ! Spectroscopic axis
      INTEGER TNDF               ! Temporary NDF identifier
      INTEGER NDIM1              ! Number of axes of NDF
      INTEGER LBND1( NDF__MXDIM ) ! Lower bounds of main NDF
      INTEGER UBND1( NDF__MXDIM ) ! Upper bounds of main NDF
      INTEGER NDIM2              ! Number of axes of NDF
      INTEGER LBND2( NDF__MXDIM ) ! Lower bounds of Extension NDF
      INTEGER UBND2( NDF__MXDIM ) ! Upper bounds of Extension NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out if the main NDF is a base NDF or a section.
      CALL NDF_ISBAS( NDF, ISBAS, STATUS )

*  Find the COORD1/2 NDFs.
      CALL NDF_FIND( XLOC, XCMP10, ONDF(1), STATUS )
      CALL NDF_FIND( XLOC, XCMP11, ONDF(2), STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Check that these two have equal dimensionality and bounds.
      CALL NDF_BOUND( ONDF(1), NDF__MXDIM, LBND2, UBND2, NDIM2, STATUS )
      CALL NDF_BOUND( ONDF(2), NDF__MXDIM, LBND1, UBND1, NDIM1, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( NDIM1 .NE. NDIM2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INCSHP', 'SPD_EAJE: Error: ' // XCMP10 //
     :      ' and ' // XCMP11 // ' shapes are inconsistent.', STATUS )
         GO TO 500
      END IF
      MATCH = .TRUE.
      DO 1 I = 1, NDF__MXDIM
         IF ( LBND2(I) .NE. LBND1(I) .OR. UBND2(I) .NE. UBND1(I) )
     :      MATCH = .FALSE.
 1    CONTINUE
      IF ( .NOT. MATCH ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INCSHP', 'SPD_EAJE: Error: ' // XCMP10 //
     :      ' and ' // XCMP11 // ' shapes are inconsistent.', STATUS )
         GO TO 500
      END IF

*  Find out which is the spectroscopic axis. Along this axis the shapes
*  for COORD1/2 differ from that of the main NDF.
      CALL SPD_EABA( NDF, .TRUE., SPAXIS, STATUS )

*  Get the main NDF's bounds.
      CALL NDF_BOUND(  NDF, NDF__MXDIM, LBND1, UBND1, NDIM1, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  If main NDF is base, check bounds.
      IF ( ISBAS ) THEN
         MATCH = .TRUE.
         DO 2 I = 1, NDF__MXDIM
            IF ( I .EQ. SPAXIS ) THEN
               IF ( LBND2(I) .NE. 1 .OR. UBND2(I) .NE. 1 )
     :            MATCH = .FALSE.
            ELSE
               IF ( LBND2(I) .NE. LBND1(I) .OR. UBND2(I) .NE. UBND1(I) )
     :            MATCH = .FALSE.
            END IF
 2       CONTINUE
         IF ( .NOT. MATCH ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPE_INCSHP', 'SPD_EAJE: Error: ' //
     :         'Main and Extension shapes are inconsistent.', STATUS )
            GO TO 500
         END IF

*  Else (if main NDF is section), take sections and abandon bases.
      ELSE
         DO 3 I = 1, NDF__MXDIM
            LBND2(I) = LBND1(I)
            UBND2(I) = UBND1(I)
 3       CONTINUE
         LBND2(SPAXIS) = 1
         UBND2(SPAXIS) = 1
         CALL NDF_SECT(  ONDF(1), NDIM1, LBND2, UBND2, TNDF, STATUS )
         CALL NDF_ANNUL( ONDF(1), STATUS )
         ONDF(1) = TNDF
         CALL NDF_SECT(  ONDF(2), NDIM1, LBND2, UBND2, TNDF, STATUS )
         CALL NDF_ANNUL( ONDF(2), STATUS )
         ONDF(2) = TNDF
      END IF

*  If the access is read or update, check that ONDF has no bad values.
      IF ( ACCESS(:5) .NE. 'WRITE' ) THEN
         BADEX(1) = .FALSE.
         CALL NDF_BAD( ONDF(1), 'DATA', .TRUE., BADEX(1), STATUS )
         BADEX(2) = .FALSE.
         CALL NDF_BAD( ONDF(2), 'DATA', .TRUE., BADEX(2), STATUS )
      END IF
      IF ( BADEX(1) .OR. BADEX(2) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVDAT', 'SPD_EAJE: Error: ' //
     :      'There are bad coordinate values present.', STATUS )
         GO TO 500
      END IF

*  Map the arrays.
      CALL NDF_MAP( ONDF(1), 'DATA', TYPE(1), ACCESS,
     :   PNTR(1), NELM(1), STATUS )
      CALL NDF_MAP( ONDF(2), 'DATA', TYPE(2), ACCESS,
     :   PNTR(2), NELM(2), STATUS )

*  Get or put labels.
      IF ( ACCESS .EQ. 'READ' .OR.
     :   ( ACCESS .EQ. 'UPDATE' .AND. LABEL(1) .EQ. ' ' ) ) THEN
         CALL NDF_CGET( ONDF(1), XC10L, LABEL(1), STATUS )
      ELSE
         CALL NDF_CPUT( LABEL(1), ONDF, XC10L, STATUS )
      END IF
      IF ( ACCESS .EQ. 'READ' .OR.
     :   ( ACCESS .EQ. 'UPDATE' .AND. LABEL(2) .EQ. ' ' ) ) THEN
         CALL NDF_CGET( ONDF(2), XC11L, LABEL(2), STATUS )
      ELSE
         CALL NDF_CPUT( LABEL(2), ONDF, XC11L, STATUS )
      END IF

*  Get or put unit.
      IF ( ACCESS .EQ. 'READ' .OR.
     :   ( ACCESS .EQ. 'UPDATE' .AND. UNITS(1) .EQ. ' ' ) ) THEN
         CALL NDF_CGET( ONDF(1), XC10U, UNITS(1), STATUS )
      ELSE
         CALL NDF_CPUT( UNITS(1), ONDF(1), XC10U, STATUS )
      END IF
      IF ( ACCESS .EQ. 'READ' .OR.
     :   ( ACCESS .EQ. 'UPDATE' .AND. UNITS(2) .EQ. ' ' ) ) THEN
         CALL NDF_CGET( ONDF(2), XC11U, UNITS(2), STATUS )
      ELSE
         CALL NDF_CPUT( UNITS(2), ONDF(2), XC11U, STATUS )
      END IF

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF_ANNUL( ONDF(1), STATUS )
         CALL NDF_ANNUL( ONDF(2), STATUS )
      END IF

*  Return.
      END
