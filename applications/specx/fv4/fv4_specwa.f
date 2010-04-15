      SUBROUTINE FV4_SPECWA( IFILE, IFAIL )
      ENTRY       WRITESCAN( IFILE, IFAIL )
*+
*  Name:
*     FV4_SPECWA

*  Purpose:
*     Write a spectrum to (end of) a file for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_SPECWA( IFILE, IFAIL )

*  Description:
*     This routine serves the WRITE-SPECTRUM command of Specx. It
*     appends the spectrum from the X register at the end of a file of
*     format version 4.

*  Arguments:
*     IFILE = INTEGER (Given)
*        The internal file number.
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
      INCLUDE 'FILES'            ! Open files information

*  FILHD Variables:
      INCLUDE 'FILHD'            ! NSCAN, NAME, ID, VERSION, IREC1

*  Arguments Given:
      INTEGER IFILE

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER ISCAN              ! Scan number in output file
      INTEGER STATUS             ! Starlink status
      CHARACTER * ( DAT__SZLOC ) CELLOC, TLOC( 2 ) ! HDS locators

*  Internal References:
      LOGICAL CHKACC             ! Check file access

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Check given file number.
      IF ( .NOT. ( CHKACC(IFILE,'W') .AND. FILELUNS(IFILE).NE.0 ) ) THEN
         IFAIL = 3
         GO TO 500
      END IF

*  Get some information from file header.
*  Work out new scan number.
      CALL FV4_FILINF( IFILE, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         IFAIL = 38
         GO TO 500
      END IF
      ISCAN = NSCAN + IREC1
      NSCAN = NSCAN + 1

*  Alter shape of SPECTRUM array to accomodate another spectrum.
*  Copy template from previous last to new last cell.
      CALL DAT_ALTER( SPXLOC(IFILE), 1, NSCAN+1, STATUS )
      CALL DAT_CELL(  SPXLOC(IFILE), 1, NSCAN, CELLOC,  STATUS )
      CALL DAT_CELL(  SPXLOC(IFILE), 1, NSCAN+1, TLOC(1), STATUS )
      CALL DAT_FIND(  CELLOC,  'DATA_ARRAY', TLOC(2), STATUS )
      CALL DAT_COPY(  TLOC(2), TLOC(1), 'DATA_ARRAY', STATUS )
      CALL DAT_ANNUL( TLOC(2), STATUS )
      CALL DAT_FIND(  CELLOC,  'MORE', TLOC(2), STATUS )
      CALL DAT_COPY(  TLOC(2), TLOC(1), 'MORE', STATUS )
      CALL DAT_ANNUL( TLOC(2), STATUS )
      CALL DAT_ANNUL( TLOC(1), STATUS )

*  Write spectrum, annul cell.
      CALL FV4_SPEC2C( CELLOC, STATUS )
      CALL DAT_ANNUL( CELLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 4
         GO TO 500
      END IF

*  Report back.
      WRITE( *, 101 ) ISCAN, FILNAMS(IFILE)
 101  FORMAT( ' Filed as scan ', I3, ' of ', A )

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
