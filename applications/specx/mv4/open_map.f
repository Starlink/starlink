      SUBROUTINE OPEN_SPECX_MAP( IFAIL )
*+
*  Name:
*     OPEN_SPECX_MAP

*  Purpose:
*     Open an existing map file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL OPEN_SPECX_MAP( IFAIL )

*  Description:
*     Routine to open a Specx map file, extract the current map header
*     and scan header (if it exists), and create dynamic memory for the
*     index array.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     rp: Rachael Padman (UCB, MRAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JAC)
*     {enter_new_authors_here}

*  History:
*     {date} (rp):
*        Original version.
*     22 Nov 1993 (hme):
*        Replace LIB${GET|FREE}_LUN with FIO_{G|P}UNIT.
*     13 Dec 1993 (hme):
*        On account of the OPEN statement with record length, have
*        separate files for all systems.
*        Adapting to latest version from Rachael: remove IOSTAT from
*        OPEN, add IFAIL=62 in error return.
*     17 Dec 1993 (hme):
*        Remove support of older versions.
*     20 Dec 1993 (hme):
*        Use RAM rather than NSPEC in WFILE, since it is now at the
*        start of the common block.
*     09 Jan 1994 (rp):
*        Replace FIO_{G|P}UNIT with I{GET|FREE}LUN
*     15 Jan 1994 (rp):
*        Replace OPEN statement with call to UOPENUF
*     28 Jan 1994 (hme):
*        Remove the desparate TYPE* statements.
*     31 Jan 1994 (hme):
*        Do not use <> in format.
*     13 Aug 1994 (hme):
*        Complete review for map version 4.1, and the mv4 library.
*     01 Sep 1994 (hme):
*        Don't report garbage unit number on open success.
*     10 June 2003 (timj):
*        Compare NSPEC and NPTS1 from POSN with that from header.
*        This was added because some people have maps where NSPEC is not equal
*        to the number of spectra in the map itself!
*     3 Nov 2003 (timj):
*        Test of NSPEC was too stringent. Test simply requires that
*        header value is less than data array size. Also, NPTS1 test had
*        incorrect consequences.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'CUBE'
      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'PROTOTYPE'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER IFAIL

*  Local Variables:
      INTEGER ISTAT
      INTEGER I, J
      INTEGER TMPNSPEC, TMPNPTS1

*  Internal References:
      INTEGER GEN_ILEN
      INTEGER IGETVM
      INTEGER IFREEVM

*.

*  Reset global status.
      IFAIL = 0

*  If a map is open, close it.
      IF ( MAP_OPEN ) CALL CLOSE_SPECX_MAP( IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         WRITE( *, * ) '--- open_specx_map ---'
         WRITE( *, * ) '    Trouble closing already open map!'
         GO TO 999
      END IF

*  Open the map file. This also maps the cube and the index.
*  (If the open routine fails, it will close the file itself.)
      NSPEC = 0
      NPTS1 = 0
      CALL MV4_MAPOPN( IFAIL )
      IF ( IFAIL .NE. 0 ) GO TO 999
      WRITE( *, * )
      WRITE( *, '('' Map file '', A, '' opened.'')' )
     :   NAMEMP(:GEN_ILEN(NAMEMP))

*  Need to store a copy of NSPEC and NPTS1 from the map read
*  so that we can compare it with the header read. This is twisted
*  because the data array is read before the header is read
      TMPNSPEC = NSPEC
      TMPNPTS1 = NPTS1

*  Read the map header.
      CALL MV4_HEADRD( )
      WRITE( *, * ) 'Map version number: ', MAP_VERSION

*  Read the prototype spectrum header if it exists.
      IF ( IHEAD .EQ. 1 ) THEN
         CALL MV4_PROTRD( )
         IF ( NPTS1 .EQ. 0 ) NPTS1 = NPTS(1)
      END IF

*  Make sure everything is in order with the POSN array to prevent
*  buffer overruns. This is only important if POSN is less than NSPEC.
      IF (TMPNSPEC .LT. NSPEC) THEN
         print *,'Dangerous header inconsistency. SPECX.NSPEC [',NSPEC,
     :        '] > DIMS(POSN) [',TMPNSPEC,']. Correcting error.'
         NSPEC = TMPNSPEC
      END IF

      IF (TMPNPTS1 .NE. NPTS1) THEN
         print *,'Header inconsistency. SPECX.NPTS1 [',NPTS1,
     :        '] != DIMS(POSN) [',TMPNPTS1,']. Correcting error.'
         NPTS1 = TMPNPTS1
      END IF

*  Reset some flags.
      CUBE_IN_MEMORY  = .FALSE.
      NEW_CUBE_LOADED = .FALSE.

*  Set some globals.
      NCUBE  = 4 * MSTEP * NSTEP * NPTS1
      NINDEX = 256 * (((MSTEP * NSTEP) - 1)/64 + 1)

*  Get virtual memory for the index.
      ISTAT = IGETVM( NINDEX, .FALSE., 'OPEN_SPECX_MAP',
     :   INDEX_ADDRESS )
      IF ( ISTAT .NE. 0 ) THEN
         WRITE( *, * ) ' -- open_specx_map --',
     :      '    Error creating VM for index'
         ISTAT = IFREEVM( INDEX_ADDRESS )
         GO TO 999
      END IF

*  Get virtual memory for the cube.
      ISTAT = IGETVM( NCUBE, .FALSE., 'OPEN_SPECX_MAP',
     :   CUBE_ADDRESS )
      IF ( ISTAT .NE. 0 ) THEN
         WRITE( *, * ) ' -- open_specx_map --',
     :      '    Error creating VM for cube'
         ISTAT = IFREEVM( INDEX_ADDRESS )
         ISTAT = IFREEVM( CUBE_ADDRESS )
         GO TO 999
      END IF

*  Get virtual memory for the inverted index.
      ISTAT = IGETVM( NINDEX, .FALSE., 'OPEN_SPECX_MAP',
     :   INVINDEX_ADDRESS )
      IF ( ISTAT .NE. 0 ) THEN
         WRITE( *, * ) ' -- open_specx_map --',
     :      '    Error creating VM for inverse index'
         ISTAT = IFREEVM( INDEX_ADDRESS )
         ISTAT = IFREEVM( CUBE_ADDRESS )
         ISTAT = IFREEVM( INVINDEX_ADDRESS )
         GO TO 999
      END IF

*  Read the index and each spectrum.
*  Absent spectra will be returned as all-zero rows.
      CALL MV4_INDXRD( %VAL(CNF_PVAL(INDEX_ADDRESS)) )
      DO 2 J = 1, NSTEP
         DO 1 I = 1, MSTEP
            CALL MV4_SPECRD( I, J, %VAL(CNF_PVAL(INDEX_ADDRESS)),
     :         %VAL(CNF_PVAL(CUBE_ADDRESS)) )
 1       CONTINUE
 2    CONTINUE

*  Update some globals and flags.
      CUBE_IN_MEMORY        = .TRUE.
      CURRENT_CUBE_ADDRESS  = CUBE_ADDRESS
      CURRENT_INDEX_ADDRESS = INDEX_ADDRESS
      MAP_ROTATED           = .FALSE.
      MAP_INTERPOLATED      = .FALSE.

*  Flag map as open.
      MAP_OPEN = .TRUE.

*  Return.
 999  CONTINUE

      END
