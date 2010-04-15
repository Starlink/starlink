      SUBROUTINE MV4_HEADRD( )
*+
*  Name:
*     MV4_HEADRD

*  Purpose:
*     Read map header from file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_HEADRD( )

*  Description:
*     This routine reads the map header from the map file for Specx.

*  Arguments:
*     None.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     13 Aug 1994 (hme):
*        Original version.
*     31 Aug 1994 (hme):
*        NDF/HDS-based sparse cube.
*     01 Sep 1994 (hme):
*        Add the NELM argument in the CMP_GET1x calls.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'MAPHD'            ! Specx map header
      INCLUDE 'MAPV4'            ! Map locators etc.

*  Local Variables:
      INTEGER STATUS             ! Starlink status
      INTEGER NELM               ! Ignored return value
      CHARACTER * ( DAT__SZLOC ) TLOC ! An HDS locator

*.

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Locate the map header extension.
      CALL NDF_XLOC( IDXNDF, 'SPECX_MAP', 'UPDATE', TLOC, STATUS )

*  Read each component.
      CALL CMP_GET0C( TLOC, 'NAME',        MAP_OWNER_NAME,   STATUS )
      CALL CMP_GET0C( TLOC, 'ID',          MAP_ID,           STATUS )
      CALL CMP_GET0R( TLOC, 'VERSION',     MAP_VERSION,      STATUS )
      CALL CMP_GET0I( TLOC, 'IHEAD',       IHEAD,            STATUS )
      CALL CMP_GET1D( TLOC, 'RAM_DECM', 2, RAM,        NELM, STATUS )
      CALL CMP_GET1R( TLOC, 'CELLSIZE', 2, CELL_XSIZE, NELM, STATUS )
      CALL CMP_GET0R( TLOC, 'POSANGLE',    POS_ANGLE,        STATUS )
      CALL CMP_GET1I( TLOC, 'MNSTEP',   2, MSTEP,      NELM, STATUS )
      CALL CMP_GET0I( TLOC, 'NPTS1',       NPTS1,            STATUS )
      CALL CMP_GET0I( TLOC, 'NSPEC',       NSPEC,            STATUS )
      CALL CMP_GET0I( TLOC, 'NREDT',       NREDT,            STATUS )
      CALL CMP_GET0I( TLOC, 'ID1',         ID1,              STATUS )

*  Release the extension.
      CALL DAT_ANNUL( TLOC, STATUS )

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
      END IF
      CALL ERR_RLSE

*  Return.
      END
