      SUBROUTINE SPD_EAEA( NDF, XLOC, AXIS, ACCESS, TYPE, LABEL, UNITS,
     :   PNTR, ONDF, NELM, STATUS )
*+
*  Name:
*     SPD_EAEA

*  Purpose:
*     Access centres in AXIS or SPECVALS structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAEA( NDF, XLOC, AXIS, ACCESS, TYPE, LABEL, UNITS,
*        PNTR, ONDF, NELM, STATUS )

*  Description:
*     This routine will access an array of centre values wherever they
*     are. Mostly this will be from the AXIS structure of the given NDF.
*     This routine will however find out whether the axis number is that
*     of the spectroscopic axis. It accesses the SPECVALS structure in
*     the Specdre Extension instead, if (i) the given HDS locator is
*     valid, and (ii) the requested axis is the spectroscopic axis, and
*     (iii) the SPECVALS structure exists in the Specdre Extension.
*     Which source was used - and thus whether the mapped centre array
*     is one- or multi-dimensional - is signalled to the calling routine
*     by the returned NDF identifier being valid or NDF__NOID.
*     If the returned Extension NDF identifier is valid, then that NDF
*     will have the same shape, size and bounds as the given main NDF.
*     If the main NDF was not a base NDF then the Extension NDF will be
*     an NDF section as well.

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
*     LABEL = CHARACTER * ( * ) (Given and Returned)
*        The label that goes with the mapped array. If taken from the
*        main NDF's AXIS structure, this will be the label of the axis.
*        If taken from the main NDF's Specdre Extension, this will be
*        the label of the data component of the NDF identified by ONDF.
*        If access is read, this argument is a returned argument.
*        If access is write, this argument is a given argument.
*        If access is update and this argument is given blank, this
*        argument is a returned argument. Otherwise this is a given
*        argument.
*     UNITS = CHARACTER * ( * ) (Given and Returned)
*        The unit that goes with the mapped array. If taken from the
*        main NDF's AXIS structure, this will be the unit of the axis.
*        If taken from the main NDF's Specdre Extension, this will be
*        the unit of the data component of the NDF identified by ONDF.
*        If access is read, this argument is a returned argument.
*        If access is write, this argument is a given argument.
*        If access is update and this argument is given blank, this
*        argument is a returned argument. Otherwise this is a given
*        argument.
*     PNTR = INTEGER (Returned)
*        The pointer to which the centre array was mapped.
*     ONDF = INTEGER (Returned)
*        The identifier of the SPECVALS NDF, which is a component of the
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
*     14-JUN-1992 (HME):
*        Make LABEL and UNITS given and returned.
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
      LOGICAL STHERE             ! True if SPECVALS there
      INTEGER SPAXIS             ! Number of spectroscopic axis

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out which is the spectroscopic axis and whether SPECVALS
*  structure exists.
      XTHERE = ( XLOC .NE. DAT__NOLOC )
      CALL SPD_EABA( NDF, XTHERE, SPAXIS, STATUS )
      STHERE = .FALSE.
      IF ( XTHERE ) CALL DAT_THERE( XLOC, XCMP6, STHERE, STATUS )

*  If axis is spectroscopic and SPECVALS do exist, get information
*  from the SPECVALS NDF, which is a component of XLOC.
      IF ( AXIS .EQ. SPAXIS .AND. STHERE ) THEN
         CALL SPD_EAED( NDF, XLOC, ACCESS, TYPE, LABEL, UNITS,
     :      PNTR, ONDF, NELM, STATUS )

*  Else (axis is not spectroscopic or SPECVALS don't exist), get
*  information from main NDF's AXIS structure.
      ELSE
         ONDF = NDF__NOID
         CALL SPD_EAEC( NDF, AXIS, ACCESS, TYPE, LABEL, UNITS,
     :      PNTR, NELM, STATUS )
      END IF

*  Return.
 500  CONTINUE
      END
