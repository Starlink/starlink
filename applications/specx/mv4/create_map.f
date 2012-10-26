      SUBROUTINE CREATE_SPECX_MAP( IFAIL )
*+
*  Name:
*     CREATE_SPECX_MAP

*  Purpose:
*     Open and initialise a new map file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CREATE_SPECX_MAP( IFAIL )

*  Description:
*     Routine to make a Specx map file. Also sets up file for immediate
*     use.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     rp: Rachael Padman (UCB, MRAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     {date} (rp):
*        Original version.
*     13 Dec 1993 (hme):
*        On account of the OPEN statement with record length, have
*        separate files for all systems.
*        Replace LIB${GET|FREE}_LUN with FIO_{G|P}UNIT.
*     17 Dec 1993 (hme):
*        Change MAP_VERSION to 4.0.
*     20 Dec 1993 (hme):
*        Use RAM rather than NSPEC in WFILE, since it is now at the
*        start of the common block.
*     09 Jan 1994 (rp):
*        Replace FIO_ routines with IGETLUN/IFREELUN
*     15 Jan 1994 (rp):
*        Replace OPEN statement with call to UOPENUF routines
*     28 Jan 1994 (hme):
*        Remove the desparate TYPE* statements.
*     12 Aug 1994 (hme):
*        Complete review for map version 4.1, and the mv4 library.
*     2012-10-26 (TIMJ):
*        Use CNF
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'CNF_PAR'
      INCLUDE 'CUBE'
      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'FLAGCOMM'

*  Status:
      INTEGER IFAIL

*  Local Variables:
      INTEGER ISTAT
      INTEGER I, J

*  Internal References:
      INTEGER IGETVM
      INTEGER IFREEVM

*.

*  Reset inherited global status.
      IFAIL = 0

*  Get necessary information from terminal.
      CALL ASK_MAP( MAP_ID, MAP_OWNER_NAME, CELL_XSIZE, CELL_YSIZE,
     :   POS_ANGLE, MSTEP, NSTEP, NPTS1, RAM, DECM, IFAIL )
      IF ( IFAIL .NE. 0 ) GO TO 999

*  Complete the header.
      NREDT = ( MSTEP * NSTEP - 1 ) / 64 + 1
      NSPEC = 0
      IHEAD = 0

*  Open a new map file. This also
*  creates and writes the map header,
*  creates, maps, initialises the index.
      CALL MV4_MAPNEW( IFAIL )
      IF ( IFAIL .NE. 0 ) GO TO 999

*  Reset some flags.
      CUBE_IN_MEMORY  = .FALSE.
      NEW_CUBE_LOADED = .FALSE.

*  Set some globals.
      NCUBE  = 4 * MSTEP * NSTEP * NPTS1
      NINDEX = 256 * (((MSTEP * NSTEP) - 1)/64 + 1)

*  Get virtual memory for the index.
      ISTAT = IGETVM( NINDEX, .FALSE., 'CREATE_SPECX_MAP',
     :   INDEX_ADDRESS )
      IF ( ISTAT .NE. 0 ) THEN
         WRITE( *, * ) ' -- create_specx_map --',
     :      '    Error creating VM for index'
         ISTAT = IFREEVM( INDEX_ADDRESS )
         IFAIL = 51
         GO TO 999
      END IF

*  Get virtual memory for the cube.
      ISTAT = IGETVM( NCUBE, .FALSE., 'CREATE_SPECX_MAP',
     :   CUBE_ADDRESS )
      IF ( ISTAT .NE. 0 ) THEN
         WRITE( *, * ) ' -- create_specx_map --',
     :      '    Error creating VM for cube'
         ISTAT = IFREEVM( INDEX_ADDRESS )
         ISTAT = IFREEVM( CUBE_ADDRESS )
         IFAIL = 51
         GO TO 999
      END IF

*  Get virtual memory for the inverted index.
      ISTAT = IGETVM( NINDEX, .FALSE., 'CREATE_SPECX_MAP',
     :   INVINDEX_ADDRESS )
      IF ( ISTAT .NE. 0 ) THEN
         WRITE( *, * ) ' -- create_specx_map --',
     :      '    Error creating VM for inverse index'
         ISTAT = IFREEVM( INDEX_ADDRESS )
         ISTAT = IFREEVM( CUBE_ADDRESS )
         ISTAT = IFREEVM( INVINDEX_ADDRESS )
         IFAIL = 51
         GO TO 999
      END IF

*  Read the index and each spectrum.
*  Non-existent spectra (according to the index) will be returned as an
*  all-zero row in the cube.
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
