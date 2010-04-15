      SUBROUTINE FV4_FILCMP( IFAIL )
      ENTRY          FSYCOM( IFAIL )
*+
*  Name:
*     FV4_FILCMP

*  Purpose:
*     Remove deleted spectra from a file for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_FILCMP( IFAIL )

*  Description:
*     This routine the COMPRESS-FILE command of Specx. It actually
*     removes deleted spectra from a file of spectra.
*
*     This routine does not reduce the size of the file. Since files
*     with spectra are Starlink Data Files, they keep their size and
*     unused space will be re-used later.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global Specx status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     rpt: Remo Tilanus (JAC, Hilo)
*     {enter_new_authors_here}

*  History:
*     08 Dec 1993 (hme):
*        Original version.
*     09 Dec 1993 (hme):
*        Fix bug whereby scans were not reported to list file if they
*        stayed in place.
*     10 May 1995 (rpt):
*        Added support for ISEQ and FILHD
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Skip asking for seq and offset limits if number of scans less than:
      INTEGER    MXSCAN
      PARAMETER  (MXSCAN=25)

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'FLAGCOMM'         ! List file unit ILOUT
      INCLUDE 'FILES'            ! Open file information

*  FILHD Variables:
      INCLUDE 'FILHD'            ! NSCAN, NAME, ID, VERSION, IREC1

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER IFILE              ! Internal file number
      INTEGER JDEF               ! Returned by GEN_*
      INTEGER NTICKS             ! Current date and time
      INTEGER ISCAN, JSCAN       ! Loop indices
      INTEGER LSCAN              ! Scan number in header
      INTEGER STATUS             ! Starlink status
      INTEGER RSCAN(2), IDUM     ! Scan sequence boundaries
      REAL    RPOS(4), RDUM      ! Offset boundaries
      CHARACTER * ( 24 ) TIMNOW  ! Current date and time
      CHARACTER * ( DAT__SZLOC ) ICLOC, JCLOC ! Cell locators
      CHARACTER * ( DAT__SZLOC ) TLOC( 2 ) ! Temporary locators

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Find out which file.
      CALL GETFIL( 'W', IFILE, IFAIL )
      IF ( IFAIL .NE. 0 ) GO TO 500

*  Get information from file header.
      CALL FV4_FILINF( IFILE, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         IFAIL = 38
         GO TO 500
      END IF

*  Get scan and offset (xlo, xhi, ylo, yhi) boundaries.
      RSCAN(1) =  1
      RSCAN(2) =  NSCAN
      RPOS(1)  = -99999.0
      RPOS(2)  =  99999.0
      RPOS(3)  = -99999.0
      RPOS(4)  =  99999.0

      IF ( NSCAN .GT. MXSCAN ) THEN
         CALL GEN_GETI4A( '(Listing) Scan range (low,high)? (All)',
     :                    RSCAN, 2, ' ', RSCAN, JDEF)
         IF ( RSCAN(1) .GT. RSCAN(2) ) THEN
            IDUM = RSCAN(1)
            RSCAN(1) = RSCAN(2)
            RSCAN(2) = IDUM
         ENDIF

         CALL GEN_GETR4A(
     :         '(Listing) Offset limits (xlo, xhi, ylo, yhi)? (None)',
     :                    RPOS, 4, ' ', RPOS, JDEF)
         IF ( RPOS(1) .GT. RPOS(2) ) THEN
            RDUM = RPOS(1)
            RPOS(1) = RPOS(2)
            RPOS(2) = RDUM
         ENDIF
         IF ( RPOS(3) .GT. RPOS(4) ) THEN
            RDUM = RPOS(3)
            RPOS(3) = RPOS(4)
            RPOS(4) = RDUM
         ENDIF
      ENDIF

*  Find out current time.
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_CTIME( NTICKS, TIMNOW, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 18
         GO TO 500
      END IF

*  Write file header information.
      WRITE( ILOUT, 101 ) FILNAMS(IFILE), ID, NAME, NSCAN, TIMNOW
 101  FORMAT( // ' INDEX LISTING OF FILE: ', A //
     :   ' File title: ', A /
     :   ' Owner:      ', A /
     :   ' No. of scans in file =', I5 /
     :   ' Date of listing:      ', A // )

*  Write table heading.
      WRITE( ILOUT, 103 )
 103  FORMAT(
     :   '  Seq     Scan     Title   NPT IQ       Fcen     Finc',
     :   '      Offsets    INTT  El' /,
     :   ' ---- ------------------ ----- --  ---------- -------',
     :   ' ------------- ------  --' / )

*  If no scans in file.
      IF ( NSCAN .LE. 0 ) THEN
         WRITE ( ILOUT, * ) '  -- FV4_FILCMP -- No scans in file.'
         GO TO 500
      END IF

*  JSCAN counts the surviving scans, ISCAN counts all scans.
      JSCAN = 0
      DO 1001 ISCAN = 1, NSCAN

*     Locate the next cell.
         CALL DAT_CELL(  SPXLOC(IFILE), 1, ISCAN, ICLOC, STATUS )

*     Find out scan number in header. If it is negative the scan was
*     deleted.
         CALL DAT_FIND(  ICLOC,   'MORE',  TLOC(1), STATUS )
         CALL DAT_FIND(  TLOC(1), 'SPECX', TLOC(2), STATUS )
         CALL CMP_GET0I( TLOC(2), 'LSCAN', LSCAN, STATUS )
         CALL DAT_ANNUL( TLOC(2), STATUS )
         CALL DAT_ANNUL( TLOC(1), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            IFAIL = 38
            CALL DAT_ANNUL( ICLOC, STATUS )
            GO TO 500
         END IF

*     If scan should survive.
         IF ( LSCAN .GT. 0 ) THEN
            JSCAN = JSCAN + 1
            LSCAN = JSCAN + IREC1 - 1

*        If scan has to be copied to earlier cell in order to survive.
            IF ( JSCAN .LT. ISCAN ) THEN

*           Locate destination cell.
               CALL DAT_CELL( SPXLOC(IFILE), 1, JSCAN, JCLOC, STATUS )

*           Remove contents of destination cell.
               CALL DAT_ERASE( JCLOC, 'MORE',       STATUS )
               CALL DAT_ERASE( JCLOC, 'DATA_ARRAY', STATUS )

*           Copy the data.
               CALL DAT_FIND(  ICLOC, 'DATA_ARRAY', TLOC(1), STATUS )
               CALL DAT_COPY(  TLOC(1), JCLOC, 'DATA_ARRAY', STATUS )
               CALL DAT_ANNUL( TLOC(1), STATUS )

*           Find the original header, update scan number.
               CALL DAT_FIND(  ICLOC, 'MORE', TLOC(1), STATUS )
               CALL DAT_FIND(  TLOC(1), 'SPECX', TLOC(2), STATUS )
               CALL CMP_PUT0I( TLOC(2), 'LSCAN', LSCAN, STATUS )

*           Report to list file.
               IF (ISCAN .GE. RSCAN(1) .AND. ISCAN .LE. RSCAN(2)) THEN
                  CALL FV4_SPECIX( TLOC(2), JSCAN, RPOS, 1, ILOUT,
     :                 IFAIL, STATUS )
               ENDIF
               CALL DAT_ANNUL(  TLOC(2), STATUS )

*           Copy the header.
               CALL DAT_COPY(  TLOC(1), JCLOC, 'MORE', STATUS )
               CALL DAT_ANNUL( TLOC(1), STATUS )

*           Annul destination cell.
               CALL DAT_ANNUL( JCLOC, STATUS )

*        Else (spectrum can stay put to survive).
            ELSE

*           Report to list file.
               CALL DAT_FIND(   ICLOC,   'MORE',  TLOC(1), STATUS )
               CALL DAT_FIND(   TLOC(1), 'SPECX', TLOC(2), STATUS )
               IF (ISCAN .GE. RSCAN(1) .AND. ISCAN .LE. RSCAN(2)) THEN
                  CALL FV4_SPECIX( TLOC(2), JSCAN, RPOS, 1, ILOUT,
     :                 IFAIL, STATUS )
               ENDIF
               CALL DAT_ANNUL(  TLOC(2), STATUS )
               CALL DAT_ANNUL(  TLOC(1), STATUS )
            END IF
         END IF

*     Annul the cell.
         CALL DAT_ANNUL( ICLOC, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            IFAIL = 2
            GO TO 500
         END IF
 1001 CONTINUE

*  If there are now fewer scans in the file.
      IF ( JSCAN .LT. NSCAN ) THEN

*     Increment destination cell one further, i.e. make it point to the
*     new template spectrum.
         JSCAN = JSCAN + 1

*     Locate destination cell.
*     Remove contents of destination cell.
         CALL DAT_CELL( SPXLOC(IFILE), 1, JSCAN, JCLOC, STATUS )
         CALL DAT_ERASE( JCLOC, 'MORE',       STATUS )
         CALL DAT_ERASE( JCLOC, 'DATA_ARRAY', STATUS )

*     Locate template cell.
         CALL DAT_CELL( SPXLOC(IFILE), 1, NSCAN+1, ICLOC, STATUS )

*     Move the data and header.
         CALL DAT_FIND(  ICLOC, 'DATA_ARRAY', TLOC(1), STATUS )
         CALL DAT_MOVE(  TLOC(1), JCLOC, 'DATA_ARRAY', STATUS )
         CALL DAT_FIND(  ICLOC, 'MORE', TLOC(1), STATUS )
         CALL DAT_MOVE(  TLOC(1), JCLOC, 'MORE', STATUS )

*     Annul the two cells.
         CALL DAT_ANNUL( ICLOC, STATUS )
         CALL DAT_ANNUL( JCLOC, STATUS )

*     Remove the contents of all unused cells.
         DO 1002 ISCAN = JSCAN + 1, NSCAN
            CALL DAT_CELL( SPXLOC(IFILE), 1, ISCAN, ICLOC, STATUS )
            CALL DAT_ERASE( ICLOC, 'MORE',       STATUS )
            CALL DAT_ERASE( ICLOC, 'DATA_ARRAY', STATUS )
            CALL DAT_ANNUL( ICLOC, STATUS )
 1002    CONTINUE

*     Remove the unused cells. The new length of the array reflects the
*     new number of scans in the file.
         NSCAN = JSCAN
         CALL DAT_ALTER( SPXLOC(IFILE), 1, JSCAN, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            IFAIL = 2
            GO TO 500
         END IF
      END IF

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
