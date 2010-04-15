      SUBROUTINE FV4_SPECDL( IFAIL )
      ENTRY          FSYDEL( IFAIL )
*+
*  Name:
*     FV4_SPECDL

*  Purpose:
*     Mark spectra in a file as deleted for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_SPECDL( IFAIL )

*  Description:
*     This routine serves the DELETE-SPECTRUM-FROM-FILE command of
*     Specx. A spectrum or range of spectra in the file is marked as
*     deleted by setting their scan numbers LSCAN to their negated
*     absolute values. Such deleted spectra can be recovered later. They
*     can also be overwritten with a non-deleted spectra from the Specx
*     stack X register. Deleted spectra are removed from a file only
*     when the file is compressed.

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
*        Added support FILHD
*     21 Sep 2000 (ajc):
*        Unused NDIM
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
      INTEGER LIST( 2 )          ! First and last scan to delete
      INTEGER NELM               ! Temporary integer
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

*  Find out scan number range.
      CALL GEN_GETI4A2( 'Scan number or range (S1,S2)..?',
     :   LIST, 2, ' ', LIST, NELM, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         IFAIL = 74
         GO TO 500
      END IF
      IF ( NELM .EQ. 1 ) LIST(2) = LIST(1)

*  Get some information from file header.
      CALL FV4_FILINF ( IFILE, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         IFAIL = 38
         GO TO 500
      END IF

*  Transform scan number range to cell number range, apply restriction
*  according to how many scans there are in the file.
      LIST(1) = LIST(1) - IREC1 + 1
      LIST(2) = LIST(2) - IREC1 + 1
      LIST(1) = MAX( LIST(1),     1 )
      LIST(2) = MIN( LIST(2), NSCAN )
      IF ( LIST(2) .LT. LIST(1) ) THEN
         IFAIL = 2
         GO TO 500
      END IF

*  For each cell, get and put LSCAN.
      DO 1001 ICELL = LIST(1), LIST(2)
         CALL DAT_CELL( SPXLOC(IFILE), 1, ICELL, TLOC(1), STATUS )
         CALL DAT_FIND( TLOC(1), 'MORE',  TLOC(2), STATUS )
         CALL DAT_FIND( TLOC(2), 'SPECX', TLOC(3), STATUS )
         CALL DAT_FIND( TLOC(3), 'LSCAN', TLOC(4), STATUS )
         CALL DAT_GET0I( TLOC(4), LSCAN, STATUS )
         LSCAN = -ABS( LSCAN )
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
