      SUBROUTINE SPD_EAED( NDF, XLOC, ACCESS, TYPE, LABEL, UNITS,
     :   PNTR, ONDF, NELM, STATUS )
*+
*  Name:
*     SPD_EAED

*  Purpose:
*     Access centres in SPECVALS structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAED( NDF, XLOC, ACCESS, TYPE, LABEL, UNITS,
*        PNTR, ONDF, NELM, STATUS )

*  Description:
*     This routine will access an array of spectroscopic centre values
*     from the SPECVALS structure in the Specdre Extension. Should this
*     not exist, it will be created from the spectroscopic axis in the
*     main NDF's AXIS structure.
*
*     If the SPECVALS structure already exists, then this routine will
*     access a section with the same bounds as the main NDF. The action
*     of this routine in the absence of the SPECVALS structure depends
*     on the access mode:
*     -  If read access is requested, then a temporary NDF is created.
*        It has the same bounds as the main NDF and is filled with
*        default values derived from the main NDF's AXIS structure.
*     -  If write or update access is requested, then a permanent NDF is
*        created. This will be located in the Specdre Extension of that
*        NDF that is the base of the main NDF. (The main NDF might be a
*        section only.) The Extension NDF will also have the same shape
*        as the main's base and will be filled with default values from
*        the main's base's AXIS structure. From this new permanent
*        Extension NDF a section with the same bounds as the main NDF is
*        accessed and returned to the calling routine.

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
*        The numeric type for mapping the centre array. This can be
*        '_REAL', '_DOUBLE', or blank. If given blank and if the array
*        is stored '_DOUBLE', then it is mapped in double precision. If
*        given blank and if the array is not stored '_DOUBLE', then it
*        is mapped '_REAL'. In effect, usually a blank type
*        specification causes the array to be mapped with the stored
*        type. On return, TYPE is '_REAL' or '_DOUBLE' and tells the
*        type actually used for mapping.
*     LABEL = CHARACTER * ( * ) (Given and Returned)
*        The Extension NDF's data component label.
*        If access is read, this argument is a returned argument.
*        If access is write, this argument is a given argument.
*        If access is update and this argument is given blank, this
*        argument is a returned argument. Otherwise this is a given
*        argument.
*     UNITS = CHARACTER * ( * ) (Given and Returned)
*        The Extension NDF's data component unit.
*        If access is read, this argument is a returned argument.
*        If access is write, this argument is a given argument.
*        If access is update and this argument is given blank, this
*        argument is a returned argument. Otherwise this is a given
*        argument.
*     PNTR = INTEGER (Returned)
*        The pointer to which the centre array is mapped.
*     ONDF = INTEGER (Returned)
*        The identifier of the SPECVALS NDF, which is a component of the
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
*     {enter_new_authors_here}

*  History:
*     02-JUL-1992 (HME):
*        Original version.
*        Make LABEL and UNITS given and returned. Use new SPEF[EFG]
*        routines.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'SPD_EPAR'           ! Specdre Extension parameters

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) ACCESS

*  Arguments Given and Returned:
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) LABEL
      CHARACTER * ( * ) UNITS

*  Arguments Returned:
      INTEGER PNTR
      INTEGER ONDF
      INTEGER NELM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL XTHERE             ! True if XLOC valid (Extension there)
      LOGICAL STHERE             ! True if SPECVALS structure exists
      INTEGER TNDF               ! Temporary NDF identifier

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check type specification.
      IF ( TYPE .NE. '_REAL' .AND. TYPE .NE. '_DOUBLE' .AND.
     :     TYPE .NE. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAED: Error: Requested type is invalid.', STATUS )
         GO TO 500
      END IF

*  Find out whether SPECVALS structure exists.
      XTHERE = ( XLOC .NE. DAT__NOLOC )
      STHERE = .FALSE.
      IF ( XTHERE ) CALL DAT_THERE( XLOC, XCMP6, STHERE, STATUS )

*  If type unspecified and SPECVALS there, use existing type.
*  (More precisely, use _DOUBLE if stored as such, use default type XT6D
*  otherwise.)
      IF ( TYPE .EQ. ' ' .AND. STHERE ) THEN
         CALL NDF_FIND( XLOC, XCMP6, TNDF, STATUS )
         CALL NDF_TYPE( TNDF, XC6D, TYPE, STATUS )
         IF ( TYPE .NE. '_DOUBLE' ) TYPE = XT6D
         CALL NDF_ANNUL( TNDF, STATUS )

*  Else if type unspecified (and SPECVALS not there), use default type.
      ELSE IF ( TYPE .EQ. ' ' ) THEN
         TYPE = XT6D
      END IF

*  If SPECVALS exist, just access them.
      IF ( STHERE ) THEN
         CALL SPD_EAEE( NDF, XLOC, ACCESS, TYPE, LABEL, UNITS,
     :      PNTR, ONDF, NELM, STATUS )

*  Else if read access (and SPECVALS do not exist), create temporarily.
*  (The temporary NDF matches the main NDF, even if that is a section.)
      ELSE IF ( ACCESS .EQ. 'READ' ) THEN
         CALL SPD_EAEG( NDF, XLOC, TYPE, LABEL, UNITS,
     :      PNTR, ONDF, NELM, STATUS )

*  Else (write or update access and SPECVALS do not exist), create
*  permanently. (The new NDF matches the main NDF's base NDF, but we
*  then access only a section matching the main NDF.)
      ELSE

*     Create SPECVALS.
*     Access as if it existed in the first place. Only using 'WRITE'
*     won't work with the newly created NDF.
         CALL SPD_EAEF( NDF, XLOC, TYPE, STATUS )
         CALL SPD_EAEE( NDF, XLOC, 'UPDATE', TYPE, LABEL, UNITS,
     :      PNTR, ONDF, NELM, STATUS )
      END IF

*  Return.
 500  CONTINUE
      END
