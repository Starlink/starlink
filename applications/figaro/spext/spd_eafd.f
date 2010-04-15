      SUBROUTINE SPD_EAFD( NDF, XLOC, ACCESS, TYPE,
     :   PNTR, ONDF, NELM, STATUS )
*+
*  Name:
*     SPD_EAFD

*  Purpose:
*     Access widths in SPECWIDS structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAFD( NDF, XLOC, ACCESS, TYPE, PNTR, ONDF, NELM, STATUS )

*  Description:
*     This routine will access an array of spectroscopic pixel widths
*     from the SPECWIDS structure in the Specdre Extension. Should this
*     not exist, it will be created from (i) the SPECVALS structure
*     or (ii) the spectroscopic axis in the main NDF's AXIS structure.
*
*     The action of this routine depends on the presence of the SPECWIDS
*     structure, of the SPECVALS structure and on the access mode:
*     -  If the SPECWIDS structure already exists, then this routine
*        will access a section with the same bounds as the main NDF.
*     -  If SPECWIDS are absent, SPECVALS exist, and access is READ,
*        then a temporary NDF with the same bounds as the given main NDF
*        is created, its values are derived from SPECVALS.
*     -  If SPECWIDS are absent, SPECVALS exist, and access is WRITE or
*        UPDATE, then a permanent NDF with the same bounds as the base
*        NDF of which the main NDF is a section is created, its values
*        are derived from SPECVALS. Only a section of the new NDF is
*        accessed, its bounds match the main NDF.
*     -  If SPECWIDS and SPECVALS are absent, and access is READ, then a
*        temporary NDF with the same bounds as the given main NDF is
*        created, its values are derived from the main NDF's width
*        array.
*     -  If SPECWIDS and SPECVALS are absent, and access is WRITE or
*        UPDATE, then a permanent NDF with the same bounds as the base
*        NDF of which the main NDF is a section is created, its values
*        are derived from the width array of the main's base NDF.
*        Only a section of the new NDF is accessed, its bounds match the
*        main NDF.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF. If the Specdre Extension does not
*        exist, then this argument must be given as DAT__NOLOC.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access mode required. This can be 'READ', 'WRITE', or
*        'UPDATE'.
*     TYPE = CHARACTER * ( * ) (Given and Returned)
*        The numeric type for mapping the width array. This can be
*        '_REAL', '_DOUBLE', or blank. If given blank and if the array
*        is stored '_DOUBLE', then it is mapped in double precision. If
*        given blank and if the array is not stored '_DOUBLE', then it
*        is mapped '_REAL'. In effect, usually a blank type
*        specification causes the array to be mapped with the stored
*        type. On return, TYPE is '_REAL' or '_DOUBLE' and tells the
*        type actually used for mapping.
*     PNTR = INTEGER (Returned)
*        The pointer to which the width array is mapped.
*     ONDF = INTEGER (Returned)
*        The identifier of the SPECWIDS NDF, which is a component of the
*        Specdre Extension.
*     NELM = INTEGER (Returned)
*        The number of elements in the mapped array.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set if the type specification is
*        invalid.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02-JUL-1992 (HME):
*        Original version.
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
      CHARACTER * ( * ) ACCESS

*  Arguments Given and Returned:
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER PNTR
      INTEGER ONDF
      INTEGER NELM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL XTHERE             ! True if XLOC valid (Extension there)
      LOGICAL WTHERE             ! True if SPECWIDS structure exists
      LOGICAL VTHERE             ! True if SPECVALS structure exists
      INTEGER BNDF               ! Main's base NDF identifier
      INTEGER TNDF               ! Temporary NDF identifier
      INTEGER TPNTR              ! Temporary array pointer
      INTEGER BNELM              ! Size of base NDF
      CHARACTER * ( 64 ) LABEL   ! Returned by SPD_EAED
      CHARACTER * ( 64 ) UNITS   ! Returned by SPD_EAED

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that requested type is valid.
      IF ( TYPE .NE. '_REAL' .AND. TYPE .NE. '_DOUBLE' .AND.
     :     TYPE .NE. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAFD: Error creating spectroscopic widths. ' //
     :      'Requested data type is invalid.', STATUS )
         GO TO 500
      END IF

*  Find out whether SPECWIDS or SPECVALS structures exist.
      XTHERE = ( XLOC .NE. DAT__NOLOC )
      WTHERE = .FALSE.
      VTHERE = .FALSE.
      IF ( XTHERE ) THEN
         CALL DAT_THERE( XLOC, XCMP7, WTHERE, STATUS )
         CALL DAT_THERE( XLOC, XCMP6, VTHERE, STATUS )
      END IF

*  If type unspecified and SPECWIDS there, use existing type.
*  (More precisely, use _DOUBLE if stored as such, use default type XT7D
*  otherwise.)
      IF ( TYPE .EQ. ' ' .AND. WTHERE ) THEN
         CALL NDF_FIND( XLOC, XCMP7, TNDF, STATUS )
         CALL NDF_TYPE( TNDF, XC7D, TYPE, STATUS )
         IF ( TYPE .NE. '_DOUBLE' ) TYPE = XT7D
         CALL NDF_ANNUL( TNDF, STATUS )

*  Else if type unspecified and SPECVALS there (but SPECWIDS absent),
*  use existing type.
      ELSE IF ( TYPE .EQ. ' ' .AND. VTHERE ) THEN
         CALL NDF_FIND( XLOC, XCMP6, TNDF, STATUS )
         CALL NDF_TYPE( TNDF, XC6D, TYPE, STATUS )
         IF ( TYPE .NE. '_DOUBLE' ) TYPE = XT6D
         CALL NDF_ANNUL( TNDF, STATUS )

*  Else if type unspecified (and neither SPECWIDS nor SPECVALS there),
*  use default type.
      ELSE IF ( TYPE .EQ. ' ' ) THEN
         TYPE = XT7D
      END IF

*  If SPECWIDS exist, just access them.
      IF ( WTHERE ) THEN
         CALL SPD_EAFE( NDF, XLOC, ACCESS, TYPE,
     :      PNTR, ONDF, NELM, STATUS )

*  If SPECWIDS absent, SPECVALS there, access READ.
      ELSE IF ( ACCESS .EQ. 'READ' .AND. VTHERE ) THEN

*     Get read access to the base of SPECVALS.
*     Derive temporary SPECWIDS.
         CALL NDF_BASE( NDF, BNDF, STATUS )
         CALL SPD_EAED( BNDF, XLOC, 'READ', TYPE, LABEL, UNITS,
     :      TPNTR, TNDF, BNELM, STATUS )
         CALL NDF_ANNUL( BNDF, STATUS )
         CALL SPD_EAFG( NDF, TYPE, BNELM, %VAL( CNF_PVAL(TPNTR) ),
     :      PNTR, ONDF, NELM, STATUS )
         CALL NDF_ANNUL( TNDF, STATUS )

*  If SPECWIDS absent, SPECVALS absent, access READ.
      ELSE IF ( ACCESS .EQ. 'READ' ) THEN

*     Create temporarily from main NDF's width, and access.
         CALL SPD_EAFJ( NDF, XLOC, TYPE, PNTR, ONDF, NELM, STATUS )

*  If SPECWIDS absent, SPECVALS there, access WRITE or UPDATE.
      ELSE IF ( VTHERE ) THEN

*     Find the base of the main NDF (may be the main NDF itself).
*     Read-access base SPECVALS.
*     Create the SPECWIDS NDF and fill it with default values.
*     Discard base NDF.
         CALL NDF_BASE( NDF, BNDF, STATUS )
         CALL SPD_EAED( BNDF, XLOC, 'READ', TYPE, LABEL, UNITS,
     :      TPNTR, TNDF, BNELM, STATUS )
         CALL SPD_EAFF( BNDF, XLOC, TYPE, BNELM,
     :                  %VAL( CNF_PVAL(TPNTR) ), STATUS )
         CALL NDF_ANNUL( TNDF, STATUS )
         CALL NDF_ANNUL( BNDF, STATUS )

*     Access as if it existed in the first place. Only using 'WRITE'
*     won't work with the newly created NDF.
         CALL SPD_EAFE( NDF, XLOC, 'UPDATE', TYPE,
     :      PNTR, ONDF, NELM, STATUS )

*  If SPECWIDS absent, SPECVALS absent, access WRITE or UPDATE.
      ELSE

*     Create the SPECWIDS NDF and fill it with default values from the
*     main's base NDF's width array.
         CALL SPD_EAFH( NDF, XLOC, TYPE, STATUS )

*     Access as if it existed in the first place. Only using 'WRITE'
*     won't work with the newly created NDF.
         CALL SPD_EAFE( NDF, XLOC, 'UPDATE', TYPE,
     :      PNTR, ONDF, NELM, STATUS )
      END IF

*  Return.
 500  CONTINUE
      END
