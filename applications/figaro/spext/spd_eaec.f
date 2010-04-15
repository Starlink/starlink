      SUBROUTINE SPD_EAEC( NDF, AXIS, ACCESS, TYPE, LABEL, UNITS,
     :   PNTR, NELM, STATUS )
*+
*  Name:
*     SPD_EAEC

*  Purpose:
*     Access centres in AXIS structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAEC( NDF, AXIS, ACCESS, TYPE, LABEL, UNITS,
*        PNTR, NELM, STATUS )

*  Description:
*     This routine will access an array of centre values from the given
*     NDF's axis structure.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     AXIS = INTEGER (Given)
*        The number of the axis to be accessed.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access mode required. This can be 'READ', 'WRITE', or
*        'UPDATE'.
*     TYPE = CHARACTER * ( * ) (Given and Returned)
*        The numeric type for mapping the centre array. This can be
*        '_REAL', '_DOUBLE', or blank. If given blank and if the array
*        is stored '_DOUBLE', then it is mapped in double precision. If
*        given blank and if the array is not stored '_DOUBLE', then it
*        is mapped '_REAL'. On return, TYPE is '_REAL' or '_DOUBLE' and
*        tells the type actually used for mapping.
*     LABEL = CHARACTER * ( * ) (Given and Returned)
*        The axis' label.
*        If access is read, this argument is a returned argument.
*        If access is write, this argument is a given argument.
*        If access is update and this argument is given blank, this
*        argument is a returned argument. Otherwise this is a given
*        argument.
*     UNITS = CHARACTER * ( * ) (Given and Returned)
*        The axis' unit.
*        If access is read, this argument is a returned argument.
*        If access is write, this argument is a given argument.
*        If access is update and this argument is given blank, this
*        argument is a returned argument. Otherwise this is a given
*        argument.
*     PNTR = INTEGER (Returned)
*        The pointer to which the centre array is mapped.
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
*     17-MAY-1992 (HME):
*        Original version.
*     14-JUN-1992 (HME):
*        Make LABEL and UNITS given and returned.
*     29-JAN-1993 (HME):
*        Enter NDF_ACGET with 'unknown' rather than ' ', because ' '
*        would return as 'Axis N' and 'pixel'.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NDF
      INTEGER AXIS
      CHARACTER * ( * ) ACCESS

*  Arguments Given and Returned:
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) LABEL
      CHARACTER * ( * ) UNITS

*  Arguments Returned:
      INTEGER PNTR
      INTEGER NELM

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check type specification.
      IF ( TYPE .NE. '_REAL' .AND. TYPE .NE. '_DOUBLE' .AND.
     :     TYPE .NE. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP',
     :      'SPD_EAEC: Error: Requested type is invalid.', STATUS )
         GO TO 500
      END IF

*  If type is unspecified, find out from the data.
*  _DOUBLE will be mapped accordingly, anything else will be mapped as
*  _REAL.
      IF ( TYPE .EQ. ' ' ) THEN
         CALL NDF_ATYPE( NDF, 'CENTRE', AXIS, TYPE, STATUS )
         IF ( TYPE .NE. '_DOUBLE' ) TYPE = '_REAL'
      END IF

*  Map the array.
      CALL NDF_AMAP( NDF, 'CENTRE', AXIS, TYPE, ACCESS,
     :   PNTR, NELM, STATUS )

*  Get or put label.
      IF ( ACCESS .EQ. 'READ' .OR.
     :   ( ACCESS .EQ. 'UPDATE' .AND. LABEL .EQ. ' ' ) ) THEN
         LABEL = 'unknown'
         CALL NDF_ACGET( NDF, 'LABEL', AXIS, LABEL, STATUS )
      ELSE
         CALL NDF_ACPUT( LABEL, NDF, 'LABEL', AXIS, STATUS )
      END IF

*  Get or put unit.
      IF ( ACCESS .EQ. 'READ' .OR.
     :   ( ACCESS .EQ. 'UPDATE' .AND. UNITS .EQ. ' ' ) ) THEN
         UNITS = 'unknown'
         CALL NDF_ACGET( NDF, 'UNITS', AXIS, UNITS, STATUS )
      ELSE
         CALL NDF_ACPUT( UNITS, NDF, 'UNITS', AXIS, STATUS )
      END IF

*  Return.
 500  CONTINUE
      END
