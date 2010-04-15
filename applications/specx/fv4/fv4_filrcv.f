      SUBROUTINE FV4_FILRCV( IFAIL )
      ENTRY          FSYREC( IFAIL )
*+
*  Name:
*     FV4_FILRCV

*  Purpose:
*     Unmark (un-delete) all spectra in a file for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_FILRCV( IFAIL )

*  Description:
*     This routine serves the RECOVER-FILE command of Specx. All spectra
*     in the file are unmarked (un-deleted) by setting their scan
*     numbers LSCAN to their absolute values.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     rpt: Remo Tilanus (JAC, Hilo)
*     {enter_new_authors_here}

*  History:
*     08 Dec 1993 (hme):
*        Original version.
*     10 May 1995 (rpt):
*        Added support for FILHD.
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
      INCLUDE 'FILES'            ! Open file information

*  FILHD Variables:
      INCLUDE 'FILHD'            ! NSCAN, NAME, ID, VERSION, IREC1

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER ICELL              ! Loop index
      INTEGER IFILE              ! Internal number for this file
      INTEGER LSCAN              ! Scan number in header
      INTEGER STATUS             ! Starlink status
      CHARACTER * ( DAT__SZLOC ) TLOC( 4 ) ! HDS locators

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Find out which file.
      CALL GETFIL( 'W', IFILE, IFAIL )
      IF ( IFAIL .NE. 0 ) GO TO 500

*  Get some information from file header.
      CALL FV4_FILINF( IFILE, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         IFAIL = 38
         GO TO 500
      END IF

*  For each cell, get and put LSCAN.
      DO 1001 ICELL = 1, NSCAN
         CALL DAT_CELL( SPXLOC(IFILE), 1, ICELL, TLOC(1), STATUS )
         CALL DAT_FIND( TLOC(1), 'MORE',  TLOC(2), STATUS )
         CALL DAT_FIND( TLOC(2), 'SPECX', TLOC(3), STATUS )
         CALL DAT_FIND( TLOC(3), 'LSCAN', TLOC(4), STATUS )
         CALL DAT_GET0I( TLOC(4), LSCAN, STATUS )
         LSCAN = +ABS( LSCAN )
         CALL DAT_PUT0I( TLOC(4), LSCAN, STATUS )
         CALL DAT_ANNUL( TLOC(4), STATUS )
         CALL DAT_ANNUL( TLOC(3), STATUS )
         CALL DAT_ANNUL( TLOC(2), STATUS )
         CALL DAT_ANNUL( TLOC(1), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            IFAIL = 2
            GO TO 500
         END IF
 1001 CONTINUE

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
