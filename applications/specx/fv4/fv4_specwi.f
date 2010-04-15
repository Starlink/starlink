      SUBROUTINE FV4_SPECWI( IFAIL )
      ENTRY         REWRITE( IFAIL )
*+
*  Name:
*     FV4_SPECWI

*  Purpose:
*     Re-write a spectrum to a file for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_SPECWI( IFILE, IFAIL )

*  Description:
*     This routine serves the REWRITE-SPECTRUM command of Specx. It
*     replaces a spectrum in a file of format version 4 with the
*     spectrum in the X register.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     rpt: Remo Tilanus (JAC, Hilo)
*     {enter_new_authors_here}

*  History:
*     07 Dec 1993 (hme):
*        Original version.
*     10 May 1995 (rpt):
*        Added support for ISEQ and FILHD
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
      INCLUDE 'STACKCOMM'        ! Specx stack
      INCLUDE 'FILES'            ! Open files information

*  FILHD Variables:
      INCLUDE 'FILHD'            ! NSCAN, NAME, ID, VERSION, IREC1

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER IFILE              ! Internal file number
      INTEGER ICELL              ! Cell number ISCAN-IREC1+1
      INTEGER STATUS             ! Starlink status
      CHARACTER * ( DAT__SZLOC ) CELLOC ! HDS locator

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

*  Find out which scan and cell.
      ICELL = LSCAN - IREC1 + 1
      IF ( ICELL .LT. 1 .OR. ICELL .GT. NSCAN ) THEN
         IFAIL = 2
         GO TO 500
      END IF

*  Get hold of cell. Write spectrum, annul cell.
      CALL DAT_CELL(  SPXLOC(IFILE), 1, ICELL, CELLOC, STATUS )
      CALL FV4_SPEC2C( CELLOC, STATUS )
      CALL DAT_ANNUL( CELLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 4
         GO TO 500
      END IF

*  Report back.
      WRITE( *, 101 ) LSCAN, FILNAMS(IFILE)
 101  FORMAT( ' Filed as scan ', I3, ' of ', A )

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
