      SUBROUTINE SPD_EAGD( NDF, XLOC, ACCESS, TYPE,
     :   PNTR, ONDF, NELM, STATUS )
*+
*  Name:
*     SPD_EAGD

*  Purpose:
*     Access covariance row sums.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAGD( NDF, XLOC, ACCESS, TYPE, PNTR, ONDF, NELM, STATUS )

*  Description:
*     This routine will access an array of covariance row sums in the
*     Specdre Extension. The returned NDF identifier corresponds to an
*     NDF wich will have the same shape, size and bounds as the given
*     main NDF. If the main NDF was not a base NDF then the Extension
*     NDF will be an NDF section as well.

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
*     PNTR = INTEGER (Returned)
*        The pointer to which the array was mapped.
*     ONDF = INTEGER (Returned)
*        The identifier of the COVRS NDF, which is a component of the
*        Specdre Extension.
*     NELM = INTEGER (Returned)
*        The number of elements in the mapped array.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set
*        -  if the requested type is invalid,
*        -  if the structure is accessed for read and does not exist.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02-JUL-1992 (HME):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'           ! Specdre Extension parameters

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
      LOGICAL STHERE             ! True if COVRS there
      INTEGER TNDF               ! Temporary NDF identifier

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check type specification.
      IF ( TYPE .NE. '_REAL' .AND. TYPE .NE. '_DOUBLE' .AND.
     :     TYPE .NE. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAGD: Error: Requested type is invalid.', STATUS )
         GO TO 500
      END IF

*  Find out whether structure exists.
      XTHERE = ( XLOC .NE. DAT__NOLOC )
      STHERE = .FALSE.
      IF ( XTHERE ) CALL DAT_THERE( XLOC, XCMP8, STHERE, STATUS )

*  If type unspecified and structure there, use existing type.
*  (More precisely, use _DOUBLE if stored as such, use default type XT8D
*  otherwise.)
      IF ( TYPE .EQ. ' ' .AND. STHERE ) THEN
         CALL NDF_FIND( XLOC, XCMP8, TNDF, STATUS )
         CALL NDF_TYPE( TNDF, XC8D, TYPE, STATUS )
         IF ( TYPE .NE. '_DOUBLE' ) TYPE = XT8D
         CALL NDF_ANNUL( TNDF, STATUS )

*  Else if type unspecified (and SPECVALS not there), use default type.
      ELSE IF ( TYPE .EQ. ' ' ) THEN
         TYPE = XT8D
      END IF

*  If structure exists, access it.
      IF ( STHERE ) THEN
         CALL SPD_EAGE( NDF, XLOC, ACCESS, TYPE,
     :      PNTR, ONDF, NELM, STATUS )

*  Else if read access (and structure does not exist), report error.
      ELSE IF ( ACCESS .EQ. 'READ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_NOEXST', 'SPD_EAGD: Error accessing ' //
     :      'covariance row sums. The structure does not exist.',
     :      STATUS )
         GO TO 500

*  Else (write or update access and structure does not exist), create
*  permanently. (The new NDF matches the main NDF's base NDF, but we
*  then access only a section matching the main NDF.)
      ELSE

*     Create structure.
*     Access as if it existed in the first place. Only using 'WRITE'
*     won't work with the newly created NDF.
         CALL SPD_EAGF( NDF, XLOC, TYPE, STATUS )
         CALL SPD_EAGE( NDF, XLOC, 'UPDATE', TYPE,
     :      PNTR, ONDF, NELM, STATUS )
      END IF

*  Return.
 500  CONTINUE
      END
