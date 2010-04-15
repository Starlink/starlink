      SUBROUTINE SPD_EAFA( NDF, XLOC, AXIS, ACCESS, TYPE,
     :   PNTR, ONDF, NELM, STATUS )
*+
*  Name:
*     SPD_EAFA

*  Purpose:
*     Access widths in AXIS or SPECWIDS structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAFA( NDF, XLOC, AXIS, ACCESS, TYPE,
*        PNTR, ONDF, NELM, STATUS )

*  Description:
*     This routine will access an array of width values wherever they
*     are. Mostly this will be from the AXIS structure of the given NDF.
*     This routine will however find out whether the axis number is that
*     of the spectroscopic axis. It accesses the SPECWIDS structure in
*     the Specdre Extension instead, if (i) the given HDS locator is
*     valid, and (ii) the requested axis is the spectroscopic axis, and
*     (iii) the SPECWIDS or SPECVALS structures exist in the Specdre
*     Extension. Which source was used - and thus whether the mapped
*     centre array is one- or multi-dimensional - is signalled to the
*     calling routine by the returned NDF identifier being valid or
*     NDF__NOID. If the returned Extension NDF identifier is valid, then
*     that NDF will have the same shape, size and bounds as the given
*     main NDF. If the main NDF was not a base NDF then the Extension
*     NDF will be an NDF section as well.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF. If the Specdre Extension does not
*        exist, then this argument must be given as DAT__NOLOC.
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
*        is mapped '_REAL'. In effect, usually a blank type
*        specification causes the array to be mapped with the stored
*        type. On return, TYPE is '_REAL' or '_DOUBLE' and tells the
*        type actually used for mapping.
*     PNTR = INTEGER (Returned)
*        The pointer to which the width array is mapped.
*     ONDF = INTEGER (Returned)
*        The identifier of the SPECWIDS NDF, which is a component of the
*        Specdre Extension. This will be returned as NDF__NOID if no NDF
*        was accessed and the array was mapped from the main NDF's AXIS
*        structure.
*     NELM = INTEGER (Returned)
*        The number of elements in the mapped array. If taken from the
*        main NDF's AXIS structure, this will be the length of the axis.
*        If taken from the main NDF's Specdre Extension, this will be
*        the size of the NDF identified by ONDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     17-MAY-1992 (HME):
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
      INTEGER AXIS
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
      LOGICAL VTHERE             ! True if SPECVALS there
      LOGICAL WTHERE             ! True if SPECWIDS there
      INTEGER SPAXIS             ! Number of spectroscopic axis

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out which is the spectroscopic axis and whether SPECWIDS and
*  SPECVALS structures exist.
      XTHERE = ( XLOC .NE. DAT__NOLOC )
      CALL SPD_EABA( NDF, XTHERE, SPAXIS, STATUS )
      WTHERE = .FALSE.
      VTHERE = .FALSE.
      IF ( XTHERE ) THEN
         CALL DAT_THERE( XLOC, XCMP7, WTHERE, STATUS )
         CALL DAT_THERE( XLOC, XCMP6, VTHERE, STATUS )
      END IF

*  If axis is spectroscopic and SPECWIDS or SPECVALS do exist, get
*  information from the SPECWIDS NDF, which is a component of XLOC.
      IF ( AXIS .EQ. SPAXIS .AND. ( VTHERE .OR. WTHERE ) ) THEN
         CALL SPD_EAFD( NDF, XLOC, ACCESS, TYPE,
     :      PNTR, ONDF, NELM, STATUS )

*  Else (axis is not spectroscopic or neither SPECWIDS nor SPECVALS
*  exist), get information from main NDF's AXIS structure.
      ELSE

*     Signal that this will not be an Extension NDF.
         ONDF = NDF__NOID

*     Check type specification.
         IF ( TYPE .NE. '_REAL' .AND. TYPE .NE. '_DOUBLE' .AND.
     :        TYPE .NE. ' ' ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPE_INVTYP',
     :         'SPD_EAEC: Error: Requested type is invalid.', STATUS )
            GO TO 500
         END IF

*     If type is unspecified, find out from the data.
*     _DOUBLE will be mapped accordingly, anything else will be mapped as
*     _REAL.
         IF ( TYPE .EQ. ' ' ) THEN
            CALL NDF_ATYPE( NDF, 'WIDTH', AXIS, TYPE, STATUS )
            IF ( TYPE .NE. '_DOUBLE' ) TYPE = '_REAL'
         END IF

*     Now map the axis array.
         CALL NDF_AMAP( NDF, 'WIDTH', AXIS, TYPE, ACCESS,
     :      PNTR, NELM, STATUS )
      END IF

*  Return.
 500  CONTINUE
      END
