
      SUBROUTINE FIND23( IPLAT, FD, OUTNAM, STATUS )
*+
*  Name:
*     FIND23

*  Purpose:
*     This subroutine creates an output file for a given plate record
*     which can be read by the Survey Data Extraction Program,
*     EXCRDD. This HDS file contains details of the scans required
*     sorted in start UTCS order, and details of the associated sources.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND23( IPLAT, FD, OUTNAM, STATUS )

*  Description:
*     -  This subroutine creates an output file for a given plate record
*     which can be read by the Survey Data Extraction Program,
*     EXCRDD. This HDS file contains details of the scans required
*     sorted in start UTCS order, and details of the associated sources.
*
*     -  A list of the sources associated with the scans for the plate
*     record is first prepared. Each scan required is examined and a
*     check is made that its source is present in the list. If it is
*     not the source is added to the list. In either case the scan
*     source number is ammended to point to the position of the
*     associated source in this list. The number of sources is counted.
*
*     -  The subroutine sets up a HDS file containing places for
*     plate details, scan details and source details. The plate details
*     are entered from the plate record.
*
*     -  The scan details are entered in start order using the linked
*     list generated in FIND29.
*
*     - The source details are entered in the order in which they were
*     specified in the list.
*

*  Arguments:
*     IPLAT = INTEGER (Given)
*        Plate position in plate common
*     FD = INTEGER (Given)
*        File descriptor obtained from FIO
*     OUTNAM = CHARACTER * ( * )
*        Output file name for plate HDS file
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     FINDCRDD:
*        FIND33, FIND34, FIND35, FIND36, FIND37
*     CHR:
*        CHR_ITOC
*     DAT:
*        DAT_ANNUL, DAT_FIND, DAT_MAPC, DAT_MAPV, DAT_NEW, DAT_NEWC,
*        DAT_PUT0C, DAT_PUT0I
*     FIO:
*        FIO_WRITE
*     HDS:
*        HDS_CLOSE, HDS_NEW
*     MSG:
*        MSG_FMTC, MSG_OUT

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1992 (DCP):
*        Original version.
*     20-MAY-1993 (DCP):
*        Modifications to the calls to FIND33 to give call
*        FIND33( NOLISO, ISOURC, %VAL(PNTRSO(1)), SONAME(SOPOS),
*        STATUS, %VAL( length of array element))
*        This then makes it compatible with UNIX
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'FIO_PAR'          ! FIO constants
      INCLUDE 'FIO_ERR'          ! FIO errors
      INCLUDE 'PAR_ERR'          ! Parameter errors
      INCLUDE 'DAT_PAR'          ! Data system constants
      INCLUDE 'DAT_ERR'          ! Data system errors
      INCLUDE 'CMP_ERR'          ! CMP errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      INTEGER IPLAT
      INTEGER FD
      CHARACTER * ( * ) OUTNAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER ENDVAL             ! Value of pointer which indicates end
                                 ! of linked list
      PARAMETER ( ENDVAL = 99999 )

*  Local Variables:
      CHARACTER * ( 97 ) BUFFER  ! Output buffer for use with FIO
      INTEGER ELSCA( 7 )         ! Number of elements in SCAN vectors
                                 ! in HDS mapping
      INTEGER ELSOU( 13 )        ! Number of elements in SOURCE vectors
                                 ! in HDS mapping
      INTEGER ILISO              ! Do loop variable for list of sources
      INTEGER ISCAN              ! Position in HDS scan vector
      INTEGER ISOURC             ! Position in HDS source vector
      INTEGER LISO( MAXSO )      ! List of source records in source
                                 ! common to be output to this plate's
                                 ! HDS file.
      CHARACTER * ( DAT__SZLOC ) LOC   ! Locator to top level element
                                       ! in HDS file
      CHARACTER * ( DAT__SZLOC ) LOCO  ! Locator to HDS file
      CHARACTER * ( DAT__SZLOC ) LOCSC ! Locator to SCAN component of
                                       ! HDS file
      CHARACTER * ( DAT__SZLOC ) LOCSCA( 7 )  ! Locators to vectors
                                              ! within the SCAN
                                              ! component of the HDS
                                              ! file.
      CHARACTER * ( DAT__SZLOC ) LOCSO ! Locator to SOURCE component of
                                       ! HDS file
      CHARACTER * ( DAT__SZLOC ) LOCSOU( 13 ) ! Locators to vectors
                                              ! within the SOURCE
                                              ! component of the HDS
                                              ! file.
      INTEGER NOLISO             ! Number of sources in source list
      INTEGER PLNSC              ! Number of scans for this plate
      INTEGER PNTRSC( 7 )        ! Pointers to start of each SCAN vector
                                 ! in HDS
      INTEGER PNTRSO( 13 )       ! Pointers to start of each SOURCE
                                 ! vector in HDS
      INTEGER SCPOS              ! Current scan index
      LOGICAL SOFOUN             ! Source found in source list flag
      INTEGER SOPOS              ! Current source index
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* *********************************************************************
*  Check whether there are any scans for this plate
* *********************************************************************

*  If the pointer in the plate record pointing to the first scan record
*  in the linked list is not the end list marker then there are scans
*  for this plate
      IF ( PLFSCP( IPLAT ) .NE. ENDVAL ) THEN

* *********************************************************************
*  Form a list of the sources required to be output with this plate by
*  examining each scan required.
* *********************************************************************

*  Zeroise the count of the number of sources in the list and the
*  contents of the list
         DO 100 ILISO = 1, MAXSO
            LISO( ILISO ) = 0
 100     CONTINUE
         NOLISO = 0

* *********************************************************************
*  Examine first scan in the linked list and add its source to source
*  list
* *********************************************************************

*  For first scan in linked list. Set the current scan marker to the
*  scan pointed to by the plate record
         SCPOS = PLFSCP( IPLAT )

*  Add the source for the first scan to the list of sources.
         LISO( 1 ) = SCSOI( SCPOS )

*  Set the pointer for the position in the list of sources, in the scan
*  to point to this source
         SCSOLI( SCPOS ) = 1

*  Make the number of sources in the source list one
         NOLISO = 1

* *********************************************************************
*  Examine subsequent scans in the scan linked list and add their
*  sources to the list.
* *********************************************************************
 200     CONTINUE

*  Are there any more scans or does the scan pointer in the current scan
*  contain the end of list value
         IF ( SCNSCP( SCPOS ) .NE. ENDVAL ) THEN

*  Set the current scan to the next one pointed to
            SCPOS = SCNSCP( SCPOS )

*  Set the source found flag to .FALSE.
            SOFOUN = .FALSE.

*  Check whether the source for this scan is already in the list of
*  sources
            DO 300  ILISO = 1, NOLISO
               IF ( SCSOI( SCPOS ) .EQ. LISO( ILISO ) ) THEN

*  Set the source found in the source list to .TRUE.
                  SOFOUN = .TRUE.

*  Set the pointer for the position in the list of sources, in the scan
*  to point to this source
                  SCSOLI( SCPOS ) = ILISO
               END IF
 300        CONTINUE

*  Check whether the source for this scan has not been found
            IF ( .NOT. SOFOUN ) THEN

*  Add the source for this scan to the source list
               NOLISO = NOLISO + 1
               LISO( NOLISO ) = SCSOI( SCPOS )

*  Set the pointer for the position in the list of sources, in the scan
*  to point to this source
               SCSOLI( SCPOS ) = NOLISO
            END IF

*  GO TO examine next scan in linked list
            GO TO 200

*  End if for check on whether the scan last examined was the last in
*  the linked list
         END IF

* *********************************************************************
*  End of examining all scans and making source list
* *********************************************************************

* *********************************************************************
*  Set up HDS object for output
* *********************************************************************

*  Put the number of scans for this plate into a suitable variable
         PLNSC = PLNOSC( IPLAT )

*  Create an HDS file with a top level of IRAS_SCANS_REQU
         CALL HDS_NEW( OUTNAM, OUTNAM, 'IRAS_SCANS_REQU',
     :                 0, 0, LOCO, STATUS )

*  Create components in the top level object
         CALL DAT_NEW( LOCO, 'PLATE_NUMBER', '_INTEGER', 0, 0, STATUS )
         CALL DAT_NEWC( LOCO, 'TAPE_ID', 6, 0, 0, STATUS )
         CALL DAT_NEW( LOCO, 'POSN_ON_TAPE', '_INTEGER', 0, 0, STATUS )
         CALL DAT_NEW( LOCO, 'SCAN', 'SCAN', 0, 0, STATUS )
         CALL DAT_NEW( LOCO, 'SOURCE', 'SOURCE', 0, 0, STATUS )
         CALL DAT_NEW( LOCO, 'HISTORY', 'HISTORY', 0, 0, STATUS )

*  Get locator to the SCAN component
         CALL DAT_FIND( LOCO, 'SCAN', LOCSC, STATUS )

*  Create subcomponents of the SCAN component
         CALL DAT_NEW( LOCSC, 'SC_SOP', '_INTEGER', 1, PLNSC, STATUS )
         CALL DAT_NEW( LOCSC, 'SC_OBS', '_INTEGER', 1, PLNSC, STATUS )
         CALL DAT_NEW( LOCSC, 'SC_XSCAN', '_REAL', 1, PLNSC, STATUS )
         CALL DAT_NEW( LOCSC, 'SC_START_UTCS', '_DOUBLE', 1, PLNSC,
     :   STATUS )
         CALL DAT_NEW( LOCSC, 'SC_END_UTCS', '_DOUBLE', 1, PLNSC,
     :   STATUS )
         CALL DAT_NEW( LOCSC, 'SC_SOURCE', '_INTEGER', 1, PLNSC,
     :   STATUS )
         CALL DAT_NEW( LOCSC, 'SC_OVERLAP', '_LOGICAL', 1, PLNSC,
     :   STATUS )

*  Get locator to the SOURCE component
         CALL DAT_FIND( LOCO, 'SOURCE', LOCSO, STATUS )

*  Create subcomponents of the SOURCE component
         CALL DAT_NEWC( LOCSO, 'SO_NAME', NMLEN, 1, NOLISO, STATUS )
         CALL DAT_NEWC( LOCSO, 'SO_TITLE', TILEN, 1, NOLISO, STATUS )
         CALL DAT_NEWC( LOCSO, 'SO_COORD1', IRA__SZFSC, 1, NOLISO,
     :   STATUS )
         CALL DAT_NEWC( LOCSO, 'SO_COORD2', IRA__SZFSC, 1, NOLISO,
     :   STATUS )
         CALL DAT_NEWC( LOCSO, 'SO_COORD_SYS', IRA__SZSCS, 1, NOLISO,
     :   STATUS )
         CALL DAT_NEW( LOCSO, 'SO_RA', '_REAL', 1, NOLISO, STATUS )
         CALL DAT_NEW( LOCSO, 'SO_DEC', '_REAL',1, NOLISO, STATUS )
         CALL DAT_NEW( LOCSO, 'SO_INSCAN_SIZE', '_REAL', 1, NOLISO,
     :   STATUS )
         CALL DAT_NEW( LOCSO, 'SO_XSCAN_SIZE', '_REAL',1, NOLISO,
     :   STATUS )
         CALL DAT_NEW( LOCSO, 'SO_WAVE1_REQ', '_LOGICAL', 1, NOLISO,
     :   STATUS )
         CALL DAT_NEW( LOCSO, 'SO_WAVE2_REQ', '_LOGICAL', 1, NOLISO,
     :   STATUS )
         CALL DAT_NEW( LOCSO, 'SO_WAVE3_REQ', '_LOGICAL', 1, NOLISO,
     :   STATUS )
         CALL DAT_NEW( LOCSO, 'SO_WAVE4_REQ', '_LOGICAL', 1, NOLISO,
     :   STATUS )

* *********************************************************************
*  Output data to HDS object
* *********************************************************************

* *********************************************************************
*  Data for components in top level
* *********************************************************************
         CALL DAT_FIND( LOCO, 'PLATE_NUMBER', LOC, STATUS )
         CALL DAT_PUT0I( LOC, PLNUM( IPLAT ), STATUS )
         CALL DAT_ANNUL( LOC, STATUS )

         CALL DAT_FIND( LOCO, 'TAPE_ID', LOC, STATUS )
         CALL DAT_PUT0C( LOC, PLID( IPLAT ), STATUS )
         CALL DAT_ANNUL( LOC, STATUS )

         CALL DAT_FIND( LOCO, 'POSN_ON_TAPE', LOC, STATUS )
         CALL DAT_PUT0I( LOC, PLPOTA( IPLAT ), STATUS )
         CALL DAT_ANNUL( LOC, STATUS )

* *********************************************************************
*  Data for SCAN component
* *********************************************************************

*  Find the locators for all vectors in the SCAN component
         CALL DAT_FIND( LOCSC, 'SC_SOP', LOCSCA( 1 ), STATUS )
         CALL DAT_FIND( LOCSC, 'SC_OBS', LOCSCA( 2 ), STATUS )
         CALL DAT_FIND( LOCSC, 'SC_XSCAN', LOCSCA( 3 ), STATUS )
         CALL DAT_FIND( LOCSC, 'SC_START_UTCS', LOCSCA( 4 ), STATUS )
         CALL DAT_FIND( LOCSC, 'SC_END_UTCS', LOCSCA( 5 ), STATUS )
         CALL DAT_FIND( LOCSC, 'SC_SOURCE', LOCSCA( 6 ), STATUS )
         CALL DAT_FIND( LOCSC, 'SC_OVERLAP', LOCSCA( 7 ), STATUS )

*  Map all vectors in the SCAN component
         CALL DAT_MAPV( LOCSCA( 1 ), '_INTEGER', 'WRITE',
     :                 PNTRSC( 1 ), ELSCA( 1 ) , STATUS )
         CALL DAT_MAPV( LOCSCA( 2 ), '_INTEGER', 'WRITE',
     :                 PNTRSC( 2 ), ELSCA( 2 ) , STATUS )
         CALL DAT_MAPV( LOCSCA( 3 ), '_REAL',    'WRITE',
     :                 PNTRSC( 3 ), ELSCA( 3 ) , STATUS )
         CALL DAT_MAPV( LOCSCA( 4 ), '_DOUBLE',  'WRITE',
     :                 PNTRSC( 4 ), ELSCA( 4 ) , STATUS )
         CALL DAT_MAPV( LOCSCA( 5 ), '_DOUBLE',  'WRITE',
     :                 PNTRSC( 5 ), ELSCA( 5 ) , STATUS )
         CALL DAT_MAPV( LOCSCA( 6 ), '_INTEGER', 'WRITE',
     :                 PNTRSC( 6 ), ELSCA( 6 ) , STATUS )
         CALL DAT_MAPV( LOCSCA( 7 ), '_LOGICAL', 'WRITE',
     :                 PNTRSC( 7 ), ELSCA( 7 ) , STATUS )

*  For first scan in linked list
         SCPOS = PLFSCP( IPLAT )
         ISCAN = 1

         CALL FIND35( ELSCA( 1 ), ISCAN, SCSOP( SCPOS ),
     :   %VAL( PNTRSC( 1 )), STATUS )
         CALL FIND35( ELSCA( 2 ), ISCAN, SCOBS( SCPOS ),
     :   %VAL( PNTRSC( 2 )), STATUS )
         CALL FIND37( ELSCA( 3 ), ISCAN, SCXSC( SCPOS ),
     :   %VAL( PNTRSC( 3 )), STATUS )
         CALL FIND34( ELSCA( 4 ), ISCAN, SCSTUT( SCPOS ),
     :   %VAL( PNTRSC( 4 )), STATUS )
         CALL FIND34( ELSCA( 5 ), ISCAN, SCENUT( SCPOS ),
     :   %VAL( PNTRSC( 5 )), STATUS )
         CALL FIND35( ELSCA( 6 ), ISCAN, SCSOLI( SCPOS ),
     :   %VAL( PNTRSC( 6 )), STATUS )
         CALL FIND36( ELSCA( 7 ), ISCAN, SCOVFL( SCPOS ),
     :   %VAL( PNTRSC( 7 )), STATUS )

*  For subsequent scans in linked list
 400     CONTINUE
*  Are there any more scans or does the scan pointer in the current scan
*  contain the end of list value
         IF ( SCNSCP( SCPOS ) .NE. ENDVAL ) THEN

*  Set the current scan to the next one pointed to
            SCPOS = SCNSCP( SCPOS )
            ISCAN = ISCAN + 1

*  Enter data
            CALL FIND35( ELSCA( 1 ), ISCAN, SCSOP( SCPOS ),
     :      %VAL( PNTRSC( 1 )), STATUS )
            CALL FIND35( ELSCA( 2 ), ISCAN, SCOBS( SCPOS ),
     :      %VAL( PNTRSC( 2 )), STATUS )
            CALL FIND37( ELSCA( 3 ), ISCAN, SCXSC( SCPOS ),
     :      %VAL( PNTRSC( 3 )), STATUS )
            CALL FIND34( ELSCA( 4 ), ISCAN, SCSTUT( SCPOS ),
     :      %VAL( PNTRSC( 4 )), STATUS )
            CALL FIND34( ELSCA( 5 ), ISCAN, SCENUT( SCPOS ),
     :      %VAL( PNTRSC( 5 )), STATUS )
            CALL FIND35( ELSCA( 6 ), ISCAN, SCSOLI( SCPOS ),
     :      %VAL( PNTRSC( 6 )), STATUS )
            CALL FIND36( ELSCA( 7 ), ISCAN, SCOVFL( SCPOS ),
     :      %VAL( PNTRSC( 7 )), STATUS )

*  GOTO start of adding data for subsequent scans in linked list
            GO TO 400
         END IF

*  Annul locators to SCAN vectors
         CALL DAT_ANNUL( LOCSCA( 1 ), STATUS )
         CALL DAT_ANNUL( LOCSCA( 2 ), STATUS )
         CALL DAT_ANNUL( LOCSCA( 3 ), STATUS )
         CALL DAT_ANNUL( LOCSCA( 4 ), STATUS )
         CALL DAT_ANNUL( LOCSCA( 5 ), STATUS )
         CALL DAT_ANNUL( LOCSCA( 6 ), STATUS )
         CALL DAT_ANNUL( LOCSCA( 7 ), STATUS )

* *********************************************************************
*  Data for SOURCE component
* *********************************************************************

*  Find the locators for all vectors in the SOURCE component
         CALL DAT_FIND( LOCSO, 'SO_NAME', LOCSOU( 1 ), STATUS )
         CALL DAT_FIND( LOCSO, 'SO_TITLE', LOCSOU( 2 ), STATUS )
         CALL DAT_FIND( LOCSO, 'SO_COORD1', LOCSOU( 3 ), STATUS )
         CALL DAT_FIND( LOCSO, 'SO_COORD2', LOCSOU( 4 ), STATUS )
         CALL DAT_FIND( LOCSO, 'SO_COORD_SYS', LOCSOU( 5 ), STATUS )
         CALL DAT_FIND( LOCSO, 'SO_RA', LOCSOU( 6 ), STATUS )
         CALL DAT_FIND( LOCSO, 'SO_DEC', LOCSOU( 7 ), STATUS )
         CALL DAT_FIND( LOCSO, 'SO_INSCAN_SIZE', LOCSOU( 8 ), STATUS )
         CALL DAT_FIND( LOCSO, 'SO_XSCAN_SIZE', LOCSOU( 9 ), STATUS )
         CALL DAT_FIND( LOCSO, 'SO_WAVE1_REQ', LOCSOU( 10 ), STATUS )
         CALL DAT_FIND( LOCSO, 'SO_WAVE2_REQ', LOCSOU( 11 ), STATUS )
         CALL DAT_FIND( LOCSO, 'SO_WAVE3_REQ', LOCSOU( 12 ), STATUS )
         CALL DAT_FIND( LOCSO, 'SO_WAVE4_REQ', LOCSOU( 13 ), STATUS )

*  Map all vectors in the SOURCE component
         CALL DAT_MAPC( LOCSOU( 1 ), 'WRITE', 1, NOLISO,
     :                  PNTRSO( 1 ), STATUS )
         CALL DAT_MAPC( LOCSOU( 2 ), 'WRITE', 1, NOLISO,
     :                  PNTRSO( 2 ), STATUS )
         CALL DAT_MAPC( LOCSOU( 3 ), 'WRITE', 1, NOLISO,
     :                  PNTRSO( 3 ), STATUS )
         CALL DAT_MAPC( LOCSOU( 4 ), 'WRITE', 1, NOLISO,
     :                  PNTRSO( 4 ), STATUS )
         CALL DAT_MAPC( LOCSOU( 5 ), 'WRITE', 1, NOLISO,
     :                  PNTRSO( 5 ), STATUS )
         CALL DAT_MAPV( LOCSOU( 6 ), '_REAL', 'WRITE',
     :                  PNTRSO( 6 ), ELSOU( 6 ), STATUS )
         CALL DAT_MAPV( LOCSOU( 7 ), '_REAL', 'WRITE',
     :                  PNTRSO( 7 ), ELSOU( 7 ), STATUS )
         CALL DAT_MAPV( LOCSOU( 8 ), '_REAL', 'WRITE',
     :                  PNTRSO( 8 ), ELSOU( 8 ), STATUS )
         CALL DAT_MAPV( LOCSOU( 9 ), '_REAL', 'WRITE',
     :                  PNTRSO( 9 ), ELSOU( 9 ), STATUS )
         CALL DAT_MAPV( LOCSOU( 10 ), '_LOGICAL', 'WRITE',
     :                  PNTRSO( 10 ), ELSOU( 10 ), STATUS )
         CALL DAT_MAPV( LOCSOU( 11 ), '_LOGICAL', 'WRITE',
     :                  PNTRSO( 11 ), ELSOU( 11 ), STATUS )
         CALL DAT_MAPV( LOCSOU( 12 ), '_LOGICAL', 'WRITE',
     :                  PNTRSO( 12 ), ELSOU( 12 ), STATUS )
         CALL DAT_MAPV( LOCSOU( 13 ), '_LOGICAL', 'WRITE',
     :                  PNTRSO( 13 ), ELSOU( 13 ), STATUS )

*  Set position in the output HDS SOURCE to zero
         ISOURC = 0

*  For each source in the source list
         DO 500 ILISO = 1, NOLISO

*  Set the current source to that pointed to by the current position in
*  the source list
            SOPOS = LISO( ILISO )

*  Set next position in the output HDS SOURCE
            ISOURC = ISOURC + 1

            CALL FIND33( NOLISO,  ISOURC, %VAL( PNTRSO( 1 )),
     :      SONAME( SOPOS ), STATUS, %VAL( NMLEN ) )
            CALL FIND33( NOLISO,  ISOURC, %VAL( PNTRSO( 2 )),
     :      SOTITL( SOPOS ), STATUS , %VAL( TILEN ) )
            CALL FIND33( NOLISO,  ISOURC, %VAL( PNTRSO( 3 )),
     :      SOCO1( SOPOS ), STATUS , %VAL( IRA__SZFSC ) )
            CALL FIND33( NOLISO,  ISOURC, %VAL( PNTRSO( 4 )),
     :      SOCO2( SOPOS ), STATUS, %VAL( IRA__SZFSC  ) )
            CALL FIND33( NOLISO,  ISOURC, %VAL( PNTRSO( 5 )),
     :      SOCOSY( SOPOS ), STATUS, %VAL( IRA__SZSCS  ) )
            CALL FIND37( ELSOU( 6 ),  ISOURC, SORA( SOPOS ),
     :      %VAL( PNTRSO( 6 )), STATUS )
            CALL FIND37( ELSOU( 7 ),  ISOURC, SODEC( SOPOS ),
     :      %VAL( PNTRSO( 7 )), STATUS )
            CALL FIND37( ELSOU( 8 ),  ISOURC, SOINSZ( SOPOS ),
     :      %VAL( PNTRSO( 8 )), STATUS )
            CALL FIND37( ELSOU( 9 ),  ISOURC, SOCRSZ( SOPOS ),
     :      %VAL( PNTRSO( 9 )), STATUS )
            CALL FIND36( ELSOU( 10 ), ISOURC, SOWAB1( SOPOS ),
     :      %VAL( PNTRSO( 10 )), STATUS )
            CALL FIND36( ELSOU( 11 ), ISOURC, SOWAB2( SOPOS ),
     :      %VAL( PNTRSO( 11 )), STATUS )
            CALL FIND36( ELSOU( 12 ), ISOURC, SOWAB3( SOPOS ),
     :      %VAL( PNTRSO( 12 )), STATUS )
            CALL FIND36( ELSOU( 13 ), ISOURC, SOWAB4( SOPOS ),
     :      %VAL( PNTRSO( 13 )), STATUS )
 500     CONTINUE

*  Annul locators to SOURCE vectors
         CALL DAT_ANNUL( LOCSOU( 1  ), STATUS )
         CALL DAT_ANNUL( LOCSOU( 2  ), STATUS )
         CALL DAT_ANNUL( LOCSOU( 3  ), STATUS )
         CALL DAT_ANNUL( LOCSOU( 4  ), STATUS )
         CALL DAT_ANNUL( LOCSOU( 5  ), STATUS )
         CALL DAT_ANNUL( LOCSOU( 6  ), STATUS )
         CALL DAT_ANNUL( LOCSOU( 7  ), STATUS )
         CALL DAT_ANNUL( LOCSOU( 8  ), STATUS )
         CALL DAT_ANNUL( LOCSOU( 9  ), STATUS )
         CALL DAT_ANNUL( LOCSOU( 10 ), STATUS )
         CALL DAT_ANNUL( LOCSOU( 11 ), STATUS )
         CALL DAT_ANNUL( LOCSOU( 12 ), STATUS )
         CALL DAT_ANNUL( LOCSOU( 13 ), STATUS )

*  Annul locators to top level objects
         CALL DAT_ANNUL( LOCSO, STATUS )
         CALL DAT_ANNUL( LOCSC, STATUS )

*  Shut down HDS file
         CALL HDS_CLOSE( LOCO, STATUS )

*  Display message to say HDS file has been made and show tape required
*  for extraction
         CALL MSG_FMTC( 'C1', 'A10', OUTNAM )
         CALL MSG_FMTC( 'C2', 'A6', PLID( IPLAT) )
         CALL MSG_OUT( ' ', 'A scans required file has been prepared'//
     :    ' for ^C1, you will need to use ^C2 for EXCRDD'//
     :    ' extraction', STATUS )

*  Write the message on description of plates file
          BUFFER = ' A scans required file has been prepared for
     :           , you will need to use         for EXCRDD'
               WRITE ( BUFFER(46:54), '(A9)' ) OUTNAM
               WRITE ( BUFFER(80:85), '(A6)' ) PLID( IPLAT)
               CALL FIO_WRITE( FD, BUFFER, STATUS )

* **********************************************************************
*  If the pointer to the first scan is the end list marker ie there are
*  no scans associated with this plate
* **********************************************************************
      ELSE
*  Display message to say that no scans were required for this plate
*  and no HDS file has been made
         CALL MSG_FMTC( 'C1', 'A10', OUTNAM )
         CALL MSG_OUT( ' ', 'WARNING - No scans were required '//
     :    'for ^C1, and no file has been prepared',
     :    STATUS )

*  Write the message on description of plates file
          BUFFER = 'WARNING - No scans were required for           , an
     :    d no file has been prepared                 '
               WRITE ( BUFFER(38:47), '(A10)' ) OUTNAM
               CALL FIO_WRITE( FD, BUFFER, STATUS )
      END IF

      END
