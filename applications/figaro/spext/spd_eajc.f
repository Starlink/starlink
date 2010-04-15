      SUBROUTINE SPD_EAJC( NDF, ACCESS, TYPE, LABEL, UNITS,
     :   PNTR, NELM, STATUS )
*+
*  Name:
*     SPD_EAJC

*  Purpose:
*     Access coordinates in AXIS structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAJC( NDF, ACCESS, TYPE, LABEL, UNITS,
*        PNTR, NELM, STATUS )

*  Description:
*     This routine will access two arrays of centre values from the
*     NDF's axis structure. The axes are the first two non-spectroscopic
*     axes.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access mode required. This can be 'READ', 'WRITE', or
*        'UPDATE'.
*     TYPE( 2 ) = CHARACTER * ( * ) (Given and Returned)
*        The numeric types for mapping the centre arrays. These can be
*        '_REAL', '_DOUBLE', or blank. If given blank and if the array
*        is stored '_DOUBLE', then it is mapped in double precision. If
*        given blank and if the array is not stored '_DOUBLE', then it
*        is mapped '_REAL'. On return, TYPE is '_REAL' or '_DOUBLE' and
*        tells the type actually used for mapping.
*     LABEL( 2 ) = CHARACTER * ( * ) (Given and Returned)
*        The axes' labels.
*        If access is read, this argument is a returned argument.
*        If access is write, this argument is a given argument.
*        If access is update and this argument is given blank, this
*        argument is a returned argument. Otherwise this is a given
*        argument.
*     UNITS( 2 ) = CHARACTER * ( * ) (Given and Returned)
*        The axes' units.
*        If access is read, this argument is a returned argument.
*        If access is write, this argument is a given argument.
*        If access is update and this argument is given blank, this
*        argument is a returned argument. Otherwise this is a given
*        argument.
*     PNTR( 2 ) = INTEGER (Returned)
*        The pointers to which the centre arrays are mapped.
*     NELM( 2 ) = INTEGER (Returned)
*        The numbers of elements in the mapped arrays.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set if the type specification is
*        invalid.

*  Notes:
*     This routine recognises the Specdre Extension v. 1.1.
*
*     This routine will work only for NDFs with at least three
*     dimensions.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     03 Aug 1994 (hme):
*        Original version, adapted from SPEEC.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) ACCESS

*  Arguments Given and Returned:
      CHARACTER * ( * ) TYPE(  2 )
      CHARACTER * ( * ) LABEL( 2 )
      CHARACTER * ( * ) UNITS( 2 )

*  Arguments Returned:
      INTEGER PNTR( 2 )
      INTEGER NELM( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL XTHERE             ! True if Extension exists
      INTEGER AXIS( 2 )          ! First two non-spectroscopic axes
      INTEGER SPAXIS             ! Spectroscopic axis
      CHARACTER * ( DAT__SZLOC) XLOC ! Extensions locator

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check type specification.
      IF ( TYPE(1) .NE. '_REAL' .AND. TYPE(1) .NE. '_DOUBLE' .AND.
     :     TYPE(1) .NE. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAJC: Error: Requested type is invalid.', STATUS )
         GO TO 500
      END IF
      IF ( TYPE(2) .NE. '_REAL' .AND. TYPE(2) .NE. '_DOUBLE' .AND.
     :     TYPE(2) .NE. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAJC: Error: Requested type is invalid.', STATUS )
         GO TO 500
      END IF

*  Find out the two axis numbers, depend on spectroscopic axis number.
      CALL SPD_EAAA( NDF, 'READ', XTHERE, XLOC, STATUS )
      CALL SPD_EABA( NDF, XTHERE, SPAXIS, STATUS )
      IF ( XTHERE ) CALL DAT_ANNUL( XLOC, STATUS )
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

*  If type is unspecified, find out from the data.
*  _DOUBLE will be mapped accordingly, anything else will be mapped as
*  _REAL.
      IF ( TYPE(1) .EQ. ' ' ) THEN
         CALL NDF_ATYPE( NDF, 'CENTRE', AXIS(1), TYPE(1), STATUS )
         IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = '_REAL'
      END IF
      IF ( TYPE(2) .EQ. ' ' ) THEN
         CALL NDF_ATYPE( NDF, 'CENTRE', AXIS(2), TYPE(2), STATUS )
         IF ( TYPE(2) .NE. '_DOUBLE' ) TYPE(2) = '_REAL'
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Map the arrays.
      CALL NDF_AMAP( NDF, 'CENTRE', AXIS(1), TYPE, ACCESS,
     :   PNTR(1), NELM(1), STATUS )
      CALL NDF_AMAP( NDF, 'CENTRE', AXIS(2), TYPE, ACCESS,
     :   PNTR(2), NELM(2), STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get or put labels.
      IF ( ACCESS .EQ. 'READ' .OR.
     :   ( ACCESS .EQ. 'UPDATE' .AND. LABEL(1) .EQ. ' ' ) ) THEN
         LABEL(1) = 'unknown'
         CALL NDF_ACGET( NDF, 'LABEL', AXIS(1), LABEL(1), STATUS )
      ELSE
         CALL NDF_ACPUT( LABEL(1), NDF, 'LABEL', AXIS(1), STATUS )
      END IF
      IF ( ACCESS .EQ. 'READ' .OR.
     :   ( ACCESS .EQ. 'UPDATE' .AND. LABEL(2) .EQ. ' ' ) ) THEN
         LABEL(2) = 'unknown'
         CALL NDF_ACGET( NDF, 'LABEL', AXIS(2), LABEL(2), STATUS )
      ELSE
         CALL NDF_ACPUT( LABEL(2), NDF, 'LABEL', AXIS(2), STATUS )
      END IF

*  Get or put units.
      IF ( ACCESS .EQ. 'READ' .OR.
     :   ( ACCESS .EQ. 'UPDATE' .AND. UNITS(1) .EQ. ' ' ) ) THEN
         UNITS(1) = 'unknown'
         CALL NDF_ACGET( NDF, 'UNITS', AXIS(1), UNITS(1), STATUS )
      ELSE
         CALL NDF_ACPUT( UNITS(1), NDF, 'UNITS', AXIS(1), STATUS )
      END IF
      IF ( ACCESS .EQ. 'READ' .OR.
     :   ( ACCESS .EQ. 'UPDATE' .AND. UNITS(2) .EQ. ' ' ) ) THEN
         UNITS(2) = 'unknown'
         CALL NDF_ACGET( NDF, 'UNITS', AXIS(2), UNITS(2), STATUS )
      ELSE
         CALL NDF_ACPUT( UNITS(2), NDF, 'UNITS', AXIS(2), STATUS )
      END IF

*  Return.
 500  CONTINUE
      END
