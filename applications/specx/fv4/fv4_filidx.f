      SUBROUTINE FV4_FILIDX( IFILE, IFAIL )
      ENTRY          INDXFL( IFILE, IFAIL )
*+
*  Name:
*     FV4_FILIDX

*  Purpose:
*     List spectra in a file of spectra for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_FILIDX( IFILE, IFAIL )

*  Description:
*     This routine serves the INDEX-FILE command of Specx. It lists
*     short or long format information for each (non-deleted) spectrum
*     in an open file of spectra (format version 4).

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
*     20 July 2000 (ajc):
*        Correct extra-long line CALL FV4_SPECIX
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
      INCLUDE 'FILES'            ! Open files information

*  FILHD Variables:
      INCLUDE 'FILHD'            ! NSCAN, NAME, ID, VERSION, IREC1

*  Arguments Given:
      INTEGER IFILE

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER ISCAN              ! Loop index
      INTEGER JDEF               ! Returned by GEN_*
      INTEGER LENGTH             ! 1/2 for short/long format
      INTEGER NTICKS             ! Current date and time
      INTEGER STATUS             ! Starlink status
      INTEGER RSCAN(2), IDUM     ! Scan sequence boundaries
      REAL    RPOS(4), RDUM      ! Offset boundaries
      CHARACTER * ( 1 ) ILS      ! Short/long format switch
      CHARACTER * ( 24 ) TIMNOW  ! Current date and time
      CHARACTER * ( DAT__SZLOC ) TLOC( 3 ) ! HDS locators

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Get information from file header.
      CALL FV4_FILINF( IFILE, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         IFAIL = 38
         GO TO 500
      END IF

*  Find out about short or long format.
      ILS = 'S'
      WRITE( *, * )
     :   'Listing of spectrum headers on logical unit ', ILOUT
      CALL GEN_GETSTR( 'Long or short listings? (L/S)',
     :   ILS, ' ', ILS, JDEF )
      CALL CHR_UCASE( ILS )
      IF ( ILS .EQ. 'L' ) THEN
         LENGTH = 2
      ELSE
         LENGTH = 1
      END IF

*  Get scan and offset (xlo, xhi, ylo, yhi) boundaries.
      RSCAN(1) =  1
      RSCAN(2) =  NSCAN
      RPOS(1)  = -99999.0
      RPOS(2)  =  99999.0
      RPOS(3)  = -99999.0
      RPOS(4)  =  99999.0

      IF ( NSCAN .GT. MXSCAN ) THEN
         CALL GEN_GETI4A( 'Scan range (low,high)? (All)',
     :                    RSCAN, 2, ' ', RSCAN, JDEF)
         IF ( RSCAN(1) .GT. RSCAN(2) ) THEN
            IDUM = RSCAN(1)
            RSCAN(1) = RSCAN(2)
            RSCAN(2) = IDUM
         ENDIF

         CALL GEN_GETR4A( 'Offset limits (xlo xhi ylo yhi)? (None)',
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
      IF ( LENGTH .EQ. 2 ) THEN
         WRITE( ILOUT, 102 )
      ELSE
         WRITE( ILOUT, 103 )
      END IF
 102  FORMAT(
     :   '  Seq     Scan             Title',
     :   '  R.A.  (1950.0)  Dec.        Offsets       Date',
     :   '           Time IQ   NPT        FCen     FInc',
     :   '      INTT  El' /,
     :   ' ---- --------------------------',
     :   ' ---------------------- -------------  ---------',
     :   '       -------- -- -----  ----------  -------',
     :   '  --------  --' / )
 103  FORMAT(
     :   '  Seq     Scan     Title   NPT IQ       Fcen     Finc',
     :   '      Offsets    INTT  El' /,
     :   ' ---- ------------------ ----- --  ---------- -------',
     :   ' ------------- ------  --' / )

*  If no scans in file.
      IF ( NSCAN .LE. 0 ) THEN
         WRITE ( ILOUT, * ) '  -- FV4_FILIDX -- No scans in file.'
         GO TO 500
      END IF

*  Loop through file, enquiring each scan in turn.
      DO 1001 ISCAN = RSCAN(1), RSCAN(2)

*     Locate the cell's SPECX extension.
         CALL DAT_CELL( SPXLOC(IFILE), 1, ISCAN, TLOC(1), STATUS )
         CALL DAT_FIND(  TLOC(1), 'MORE',  TLOC(2), STATUS )
         CALL DAT_FIND(  TLOC(2), 'SPECX', TLOC(3), STATUS )
         CALL DAT_ANNUL( TLOC(1), STATUS )
         CALL DAT_ANNUL( TLOC(2), STATUS )

*     Get and write header information.
         CALL FV4_SPECIX( TLOC(3), ISCAN, RPOS, LENGTH, ILOUT, IFAIL,
     &     STATUS )
         CALL DAT_ANNUL( TLOC(3), STATUS )
         IF ( IFAIL .NE. 0 ) GO TO 500
 1001 CONTINUE

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
