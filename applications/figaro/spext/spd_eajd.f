      SUBROUTINE SPD_EAJD( NDF, XLOC, ACCESS, TYPE, LABEL, UNITS,
     :   PNTR, ONDF, NELM, STATUS )
*+
*  Name:
*     SPD_EAJD

*  Purpose:
*     Access coordinates in COORD1/2 structures.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAJD( NDF, XLOC, ACCESS, TYPE, LABEL, UNITS,
*        PNTR, ONDF, NELM, STATUS )

*  Description:
*     This routine will access arrays of first and second coordinates
*     from the COORD1/2 structures in the Specdre Extension. Should they
*     not exist, they will be created from the first two
*     non-spectroscopic axes in the main NDF's AXIS structure.
*
*     If the COORD1/2 structures already exist, then this routine will
*     access sections with the bounds corresponding to the main NDF.
*     (COORD1/2 have only one pixel along the spectroscopic axis.)
*     The action of this routine in the absence of the COORD1/2
*     structures depends on the access mode:
*     -  If read access is requested, then temporary NDFs are created.
*        They have the bounds corresponding to the main NDF and are
*        filled with default values derived from the main NDF's AXIS
*        structure.
*     -  If write or update access is requested, then permanent NDFs are
*        created. They will be located in the Specdre Extension of that
*        NDF that is the base of the main NDF. (The main NDF might be a
*        section only.) The Extension NDFs will also have the
*        shape corresponding to the main's base and will be filled with
*        default values from the main's base's AXIS structure. From
*        these new permanent Extension NDFs sections with the bounds
*        corresponding to the main NDF are accessed and returned
*        to the calling routine.

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
*     TYPE( 2 ) = CHARACTER * ( * ) (Given and Returned)
*        The numeric types for mapping the arrays. These can be
*        '_REAL', '_DOUBLE', or blank. If given blank and if the array
*        is stored '_DOUBLE', then it is mapped in double precision. If
*        given blank and if the array is not stored '_DOUBLE', then it
*        is mapped '_REAL'. In effect, usually a blank type
*        specification causes the array to be mapped with the stored
*        type. On return, TYPE is '_REAL' or '_DOUBLE' and tells the
*        type actually used for mapping.
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
*        The identifiers of the POSIT1/2 NDFs, which are components of
*        the Specdre Extension.
*     NELM( 2 ) = INTEGER (Returned)
*        The numbers of elements in the mapped arrays.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set if the type specification is
*        invalid.

*  Notes:
*     This routine recognises the Specdre Extension v. 1.1.
*
*     If only one of the two COORD1/2 structures exists, it will be
*     ignored and non-existence of both will be assumed. However, for
*     write and update access an error may occur when this routine tries
*     too create the structure that actually already exists.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     03 Aug 1994 (hme):
*        Original version, adapted from SPEED.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) ACCESS

*  Arguments Given and Returned:
      CHARACTER * ( * ) TYPE(  2 )
      CHARACTER * ( * ) LABEL( 2 )
      CHARACTER * ( * ) UNITS( 2 )

*  Arguments Returned:
      INTEGER PNTR( 2 )
      INTEGER ONDF( 2 )
      INTEGER NELM( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL XTHERE             ! True if XLOC valid (Extension there)
      LOGICAL THERE1             ! True if POSIT1 structure exists
      LOGICAL THERE2             ! True if POSIT2 structure exists
      INTEGER TNDF               ! Temporary NDF identifier

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check type specification.
      IF ( TYPE(1) .NE. '_REAL' .AND. TYPE(1) .NE. '_DOUBLE' .AND.
     :     TYPE(1) .NE. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAJD: Error: Requested type is invalid.', STATUS )
         GO TO 500
      END IF
      IF ( TYPE(2) .NE. '_REAL' .AND. TYPE(2) .NE. '_DOUBLE' .AND.
     :     TYPE(2) .NE. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAJD: Error: Requested type is invalid.', STATUS )
         GO TO 500
      END IF

*  Find out whether COORD1/2 structures exist. If either is absent, we
*  assume both are absent. An error may result if one of the two exists,
*  since we might try below to create both.
      XTHERE = ( XLOC .NE. DAT__NOLOC )
      THERE1 = .FALSE.
      THERE2 = .FALSE.
      IF ( XTHERE ) CALL DAT_THERE( XLOC, XCMP10, THERE1, STATUS )
      IF ( XTHERE ) CALL DAT_THERE( XLOC, XCMP11, THERE2, STATUS )
      THERE1 = THERE1 .AND. THERE2
      THERE2 = THERE1

*  If type unspecified and COORD1/2 there, use existing type.
*  (More precisely, use _DOUBLE if stored as such, use default type
*  otherwise.)
*  Else if type unspecified (and COORD1/2 not there), use default type.
      IF ( TYPE(1) .EQ. ' ' .AND. THERE1 ) THEN
         CALL NDF_FIND( XLOC, XCMP10, TNDF, STATUS )
         CALL NDF_TYPE( TNDF, XC10D, TYPE(1), STATUS )
         IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = XT10D
         CALL NDF_ANNUL( TNDF, STATUS )
      ELSE IF ( TYPE(1) .EQ. ' ' ) THEN
         TYPE(1) = XT10D
      END IF
      IF ( TYPE(2) .EQ. ' ' .AND. THERE2 ) THEN
         CALL NDF_FIND( XLOC, XCMP11, TNDF, STATUS )
         CALL NDF_TYPE( TNDF, XC11D, TYPE(2), STATUS )
         IF ( TYPE(2) .NE. '_DOUBLE' ) TYPE(2) = XT11D
         CALL NDF_ANNUL( TNDF, STATUS )
      ELSE IF ( TYPE(1) .EQ. ' ' ) THEN
         TYPE(2) = XT11D
      END IF

*  If COORD1/2 exist, just access them.
      IF ( THERE1 .AND. THERE2 ) THEN
         CALL SPD_EAJE( NDF, XLOC, ACCESS, TYPE, LABEL, UNITS,
     :      PNTR, ONDF, NELM, STATUS )

*  Else if read access (and COORD1/2 do not exist), create temporarily.
*  (The temporary NDFs match the main NDF, even if that is a section.)
      ELSE IF ( ACCESS .EQ. 'READ' ) THEN
         CALL SPD_EAJG( NDF, XLOC, TYPE, LABEL, UNITS,
     :      PNTR, ONDF, NELM, STATUS )

*  Else (write or update access and COORD1/2 do not exist), create
*  permanently. (The new NDF matches the main NDF's base NDF, but we
*  then access only a section matching the main NDF.)
      ELSE

*     Create COORD1/2.
*     Access as if it existed in the first place. Only using 'WRITE'
*     won't work with the newly created NDF.
         CALL SPD_EAJF( NDF, XLOC, TYPE, STATUS )
         CALL SPD_EAJE( NDF, XLOC, 'UPDATE', TYPE, LABEL, UNITS,
     :      PNTR, ONDF, NELM, STATUS )
      END IF

*  Return.
 500  CONTINUE
      END
