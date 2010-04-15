      SUBROUTINE MAPCF2( NDFOUT, IDA, SCS, AREF, BREF, BAND0, MAXSOP,
     :                   MINSOP, OUNITS, STATUS )
*+
*  Name:
*     MAPCF2

*  Purpose:
*     Create a MAPCRDD IRAS NDF extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCF2( NDFOUT, IDA, SCS, AREF, BREF, BAND0, MAXSOP, MINSOP,
*                  OUNITS, STATUS )

*  Description:
*     The extension is created by IRI_NEW holding the mandatory items,
*     together with the following optional items; FIELDLON, FIELDLAT,
*     FIELDSCS, MAXSOP, MINSOP. An astrometry structure is created and
*     the astrometry information identified by IDA is copied to it.

*  Arguments:
*     NDFOUT = INTEGER (Given)
*        The NDF identifier for the output map.
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information describing
*        sky positions within the output map.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system associated with IDA.
*     AREF = DOUBLE PRECISION (Given)
*        The longitude of the reference point of the first usable CRDD
*        file included in the output map. This should be in the sky
*        coordinate system specified by SCS.
*     BREF = DOUBLE PRECISION (Given)
*        The latitude of the reference point of the first usable CRDD
*        file included in the output map. This should be in the sky
*        coordinate system specified by SCS.
*     BAND0 = INTEGER (Given)
*        The IRAS band number (1-4).
*     MAXSOP = INTEGER (Given)
*        The maximum SOP no. included in the output map.
*     MINSOP = INTEGER (Given)
*        The minimum SOP no. included in the output map.
*     OUNITS = CHARACTER * ( * ) (Given)
*        The units used for the output NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRI_PAR'          ! IRI constants.

*  Arguments Given:
      INTEGER NDFOUT
      INTEGER IDA
      CHARACTER SCS*(*)
      DOUBLE PRECISION AREF
      DOUBLE PRECISION BREF
      INTEGER BAND0
      INTEGER MAXSOP
      INTEGER MINSOP
      CHARACTER OUNITS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER IILOC*(DAT__SZLOC)! HDS locator to the IMAGE_INFO
                                 ! structure.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create the IRAS extension with the mandatory components.
      CALL IRI_NEW( NDFOUT, 'SURVEY', BAND0, IRI__MAPCR, OUNITS, IILOC,
     :              STATUS )

*  Create the FIELDLAT component within IMAGE_INFO, and store the
*  latitude at the reference position from the first usable CRDD file.
      CALL DAT_NEW0D( IILOC, 'FIELDLAT', STATUS )
      CALL CMP_PUT0D( IILOC, 'FIELDLAT', BREF, STATUS )

*  Create the FIELDLON component within IMAGE_INFO, and store the
*  longitude at the reference position from the first usable CRDD file.
      CALL DAT_NEW0D( IILOC, 'FIELDLON', STATUS )
      CALL CMP_PUT0D( IILOC, 'FIELDLON', AREF, STATUS )

*  Create the FIELDSCS component within IMAGE_INFO, and store the
*  SCS in which the FIELDLON and FIELDLAT values are stored.
      CALL DAT_NEW0C( IILOC, 'FIELDSCS', IRA__SZSCS, STATUS )
      CALL CMP_PUT0C( IILOC, 'FIELDSCS', SCS, STATUS )

*  Create the MAXSOP component within IMAGE_INFO, and store the
*  maximum SOP number included in the output map.
      CALL DAT_NEW0I( IILOC, 'MAXSOP', STATUS )
      CALL CMP_PUT0I( IILOC, 'MAXSOP', MAXSOP, STATUS )

*  Create the MINSOP component within IMAGE_INFO, and store the
*  minimum SOP number included in the output map.
      CALL DAT_NEW0I( IILOC, 'MINSOP', STATUS )
      CALL CMP_PUT0I( IILOC, 'MINSOP', MINSOP, STATUS )

*  Annul the locator to the IMAGE_INFO structure.
      CALL DAT_ANNUL( IILOC, STATUS )

*  Store the astrometry information in the output NDF.
      CALL IRA_EXPRT( IDA, NDFOUT, STATUS )

      END
