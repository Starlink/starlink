      SUBROUTINE ARCGENDB( STATUS )
*+
*  Name:
*     ARCGENDB

*  Purpose:
*     Convert list of laboratory values to feature data base.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ARCGENDB( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine converts an arc line list - i.e. an ASCII list of
*     laboratory wavelengths or frequencies of known features in an arc
*     spectrum - into a feature data base. That can be used for
*     automatic identification of features in an observed arc spectrum.
*
*     Since generating the feature data base may take some time, you may
*     want to do it once for any line lists you often use, and keep the
*     feature data bases. On the other hand, the feature data bases may
*     be rather big.
*
*     This routine reads a list of laboratory values (wavelengths or
*     frequencies). The list must be an unformatted ASCII file. From the
*     beginning of each line one value is read. If this fails, the line
*     is ignored. Comment lines can be inserted by prefixing them with
*     "*", "!" or "#". The value can be followed by any comment, but can
*     be preceded only by blanks. The list must be strictly
*     monotonically increasing.
*
*     The list should to some degree match an expected observation. Its
*     spectral extent should be wider than that of an expected
*     observation. But it should not contain a significant number of
*     features that are usually not detected. This is because the
*     automatic identification algorithm uses relative distances between
*     neighbouring features. If most neighbours in the list of
*     laboratory values are not detected in the actual arc observation,
*     then the algorithm may fail to find a solution or may return the
*     wrong solution.
*
*     The given list is converted to a feature data base according to
*     Mills (1992). The data base contains information about the
*     distances between neighbours of features. The scope of the feature
*     data base is the number of neighbours about which information is
*     stored. The feature data base is stored in an extension to a dummy
*     NDF. The NDF itself has only the obligatory data array. The data
*     array is one-dimensional with 1 pixel. All the actual information
*     is in an extension with the name "ECHELLE" and of type
*     "ECH_FTRDB". Its HDS components are:
*
*     -  FTR_WAVE(NLINES)           <_REAL>
*     -  FTR_DB(10,10,NLINES)       <_REAL>
*     -  FTR_LEFT(10,10,NLINES)     <_BYTE>
*     -  FTR_RIGHT(10,10,NLINES)    <_BYTE>
*     -  WAVE_INDEX(10,10,NLINES)   <_UWORD>
*     -  QUICK_INDEX(5000)          <_INTEGER>
*     -  QENTRIES(5000)             <_REAL>
*
*     NLINES is the number of features listed in the input file. The
*     scope (=10) controls about how many neighbours information is
*     stored in the data base. The index size is fixed to 5000, which
*     seems sufficient for NLINES = 3500. The size of the FDB is
*
*        (804 * NLINES + 40000) bytes
*
*     plus a small overhead for the HDS structure and the nominal NDF.
*     So it is 10 to 100 times bigger than the original ASCII list. The
*     point about the FDB is the reduced computing time when
*     auto-identifying features in an observed arc spectrum.

*  Usage:
*     arcgendb in fdb

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        If true, informational messages will be issued.
*     IN = FILENAME (Read)
*        The name of the input ASCII list of wavelengths or frequencies.
*        The list must be strictly monotonically increasing.
*     FDB = NDF (Read)
*        The name of the output file to hold the feature data base.
*        This is formally an NDF.

*  Examples:
*     arcgendb $FIGARO_PROG_S/thar.arc thar_arc
*        This will convert the Th-Ar list from the Figaro release into a
*        "feature data base" by the name of "thar_arc.sdf".

*  References:
*     Mills, D., 1992, Automatic ARC wavelength calibration, in P.J.
*     Grosbol, R.C.E. de Ruijsscher (eds), 4th ESO/ST-ECF Data Analysis
*     Workshop, Garching, 13 - 14 May 1992, ESO Conference and Workshop
*     Proceedings No. 41, Garching bei Muenchen, 1992

*  Authors:
*     djm: Dave Mills (UCL)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10 Jun 1992 (djm):
*        Original version (GENERATE_FDB.FOR, PROGRAM TEST).
*     09 Jun 1993 (hme):
*        Specdre application.
*     25 Nov 1994 (hme):
*        Use new libraries.
*     30 Jan 1995 (hme):
*        Move internal references above DATA statements.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER SCOPE              ! Controls number of neighbours
      PARAMETER ( SCOPE = 10 )
      INTEGER INDXSZ             ! Size of index arrays
      PARAMETER ( INDXSZ = 5000 )

*  Local Variables:
*  PLACE(1): Workspace for FTR_DB_TEMP_L/R.
*  PLACE(2): Workspace for FTR_DB_TEMP_WAVE.
*  PLACE(3): Workspace for input values.
*  NDF(1): Output NDF.
*  NDF(2): Workspace for FTR_DB_TEMP_L/R.
*  NDF(3): Workspace for FTR_DB_TEMP_WAVE.
*  NDF(4): Workspace for input values.
*  PNTR(1...7): FDB arrays.
*  PNTR(8...10): FTR_DB_TEMP_* arrays.
*  PNTR(11): Array for input values.
      LOGICAL INFO
      INTEGER FD, FU
      INTEGER RDSTAT             ! Status from SPD_UAAK
      INTEGER FDBSTA             ! FDB status from SPD_WZLB
      INTEGER MONO               ! Monotonicity code
      INTEGER FIRST              ! First monotonicity violator
      INTEGER I                  ! Loop index
      INTEGER NLINES             ! How many arc lines in input
      INTEGER ONE( 3 )           ! Itself
      INTEGER DBDIM( 3 )         ! FDB array dimensions
      INTEGER PLACE( 3 )         ! Temp. NDF place holder
      INTEGER NDF( 4 )           ! NDF identifiers
      INTEGER PNTR( 11 )         ! Array pointers
      REAL VALI                  ! Value read from ASCII file
      CHARACTER * ( 80 ) COMM    ! Comment read from ASCII file
      CHARACTER * ( DAT__SZLOC ) XLOC ! Extension locator
      CHARACTER * ( DAT__SZLOC ) LOC( 7 ) ! FDB array locators

*  Internal References:
      REAL SPD_UAAGR             ! An array element

*  Local Data:
      DATA ONE / 1, 1, 1 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN

*  Modal parameters.
      CALL PAR_GET0L( 'INFO', INFO, STATUS )

*  Get hold of the input file.
      CALL FIO_ASSOC( 'IN', 'READ', 'LIST', 0, FD, STATUS )
      CALL FIO_UNIT( FD, FU, STATUS )

*  Get hold of the output NDF, which has a nominal 1-length array.
*  That array must be in a defined state.
      CALL NDF_CREAT( 'FDB', '_REAL', 1, 1, 1, NDF(1), STATUS )
      CALL NDF_MAP( NDF(1), 'DATA', '_REAL', 'WRITE/ZERO',
     :   PNTR(1), I, STATUS )
      CALL NDF_UNMAP( NDF(1), 'DATA', STATUS )

*  Check status.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  First pass through input file, find out how many lines. Then rewind.
*  This is an endless loop broken when the EOF was found.
      NLINES = 0
 1    CONTINUE
         CALL SPD_UAAK( FU, 1, 1, 0., 1, VALI, COMM, RDSTAT )

*     If a value could be read from the line, count it.
*     Else if EOF found, previous line was the last; break out of loop.
*     Else (comment line, line with too few columns, other error),
*     ignore the line.
         IF ( RDSTAT .EQ. 0 ) THEN
            NLINES = NLINES + 1
         ELSE IF ( RDSTAT .EQ. 1 ) THEN
            GO TO 2
         END IF
      GO TO 1
 2    CONTINUE
      REWIND ( UNIT = FU )

*  Check that there are lines in the input.
      IF ( NLINES .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCGENDB_E01', 'ARCGENDB: Error reading ' //
     :      'input file: file is empty.', STATUS )
         GO TO 500
      END IF

*  Create the ECHELLE extension in the output NDF.
      DBDIM(1) = SCOPE
      DBDIM(2) = SCOPE
      DBDIM(3) = NLINES
      CALL NDF_XNEW( NDF(1), 'ECHELLE', 'ECH_FTRDB', 0,0, XLOC, STATUS )
      CALL DAT_NEW( XLOC, 'FTR_WAVE',    '_REAL',    1, NLINES, STATUS )
      CALL DAT_NEW( XLOC, 'FTR_DB',      '_REAL',    3, DBDIM,  STATUS )
      CALL DAT_NEW( XLOC, 'FTR_LEFT',    '_BYTE',    3, DBDIM,  STATUS )
      CALL DAT_NEW( XLOC, 'FTR_RIGHT',   '_BYTE',    3, DBDIM,  STATUS )
      CALL DAT_NEW( XLOC, 'WAVE_INDEX',  '_UWORD',   3, DBDIM,  STATUS )
      CALL DAT_NEW( XLOC, 'QUICK_INDEX', '_INTEGER', 1, INDXSZ, STATUS )
      CALL DAT_NEW( XLOC, 'QENTRIES',    '_REAL',    1, INDXSZ, STATUS )

*  Locate the new arrays in the ECHELLE extension.
      CALL DAT_FIND( XLOC, 'FTR_WAVE',    LOC(1), STATUS )
      CALL DAT_FIND( XLOC, 'FTR_DB',      LOC(2), STATUS )
      CALL DAT_FIND( XLOC, 'FTR_LEFT',    LOC(3), STATUS )
      CALL DAT_FIND( XLOC, 'FTR_RIGHT',   LOC(4), STATUS )
      CALL DAT_FIND( XLOC, 'WAVE_INDEX',  LOC(5), STATUS )
      CALL DAT_FIND( XLOC, 'QUICK_INDEX', LOC(6), STATUS )
      CALL DAT_FIND( XLOC, 'QENTRIES',    LOC(7), STATUS )

*  Map the new arrays in the ECHELLE extension.
      CALL DAT_MAP( LOC(1),'_REAL', 'WRITE', 1,NLINES,PNTR(1), STATUS )
      CALL DAT_MAP( LOC(2),'_REAL', 'WRITE', 3,DBDIM, PNTR(2), STATUS )
      CALL DAT_MAP( LOC(3),'_BYTE', 'WRITE', 3,DBDIM, PNTR(3), STATUS )
      CALL DAT_MAP( LOC(4),'_BYTE', 'WRITE', 3,DBDIM, PNTR(4), STATUS )
      CALL DAT_MAP( LOC(5),'_UWORD','WRITE', 3,DBDIM, PNTR(5), STATUS )
      CALL DAT_MAP( LOC(6),'_INTEGER','WRITE',1,INDXSZ,PNTR(6),STATUS )
      CALL DAT_MAP( LOC(7),'_REAL', 'WRITE', 1,INDXSZ,PNTR(7), STATUS )

*  Check status.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get a workspace for FTR_DB_TEMP_L/R (data/variance).
      CALL NDF_TEMP( PLACE(1), STATUS )
      CALL NDF_NEW( '_BYTE', 3, ONE, DBDIM, PLACE(1), NDF(2), STATUS )
      CALL NDF_MAP( NDF(2), 'DATA,VARIANCE', '_BYTE', 'WRITE',
     :   PNTR(8), I, STATUS )

*  Get a workspace for FTR_DB_TEMP_WAVE.
      CALL NDF_TEMP( PLACE(2), STATUS )
      CALL NDF_NEW( '_UWORD', 3, ONE, DBDIM, PLACE(2), NDF(3), STATUS )
      CALL NDF_MAP( NDF(3), 'DATA', '_UWORD', 'WRITE',
     :   PNTR(10), I, STATUS )

*  Get a workspace for the input values.
      CALL NDF_TEMP( PLACE(3), STATUS )
      CALL NDF_NEW( '_REAL', 1, 1, NLINES, PLACE(3), NDF(4), STATUS )
      CALL NDF_MAP( NDF(4), 'DATA', '_REAL', 'WRITE',
     :   PNTR(11), I, STATUS )

*  Check status.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Second pass through input file, read the values.
      CALL SPD_WZLA( FU, NLINES, %VAL( CNF_PVAL(PNTR(11)) ), STATUS )

*  Check that strictly monotonically increasing.
      MONO = 2
      CALL SPD_UAASR( NLINES, %VAL( CNF_PVAL(PNTR(11)) ), MONO, FIRST,
     :                STATUS )
      IF ( MONO .NE. 2 ) THEN
         VALI = SPD_UAAGR( %VAL( CNF_PVAL(PNTR(11)) ), FIRST, STATUS )
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ARCGENDB_T01', FIRST )
         CALL MSG_SETR( 'ARCGENDB_T02', VALI )
         CALL ERR_REP( 'ARCGENDB_E02', 'ARCGENDB: Error: The input ' //
     :      'values are not strictly monotonically increasing. ' //
     :      'First violation near element ^ARCGENDB_T01, value ' //
     :      '^ARCGENDB_T02.', STATUS )
         GO TO 500
      END IF

*  Generate the data base by calling SPD_WZLB.
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL SPD_WZLB( INFO, SCOPE, NLINES, INDXSZ,
     :               %VAL( CNF_PVAL(PNTR(1)) ),
     :               %VAL( CNF_PVAL(PNTR(2)) ),
     :               %VAL( CNF_PVAL(PNTR(3)) ),
     :               %VAL( CNF_PVAL(PNTR(4)) ),
     :               %VAL( CNF_PVAL(PNTR(5)) ),
     :               %VAL( CNF_PVAL(PNTR(6)) ),
     :               %VAL( CNF_PVAL(PNTR(7)) ),
     :               %VAL( CNF_PVAL(PNTR(8)) ),
     :               %VAL( CNF_PVAL(PNTR(9)) ),
     :               %VAL( CNF_PVAL(PNTR(10)) ),
     :               NLINES, %VAL( CNF_PVAL(PNTR(11)) ), FDBSTA )

*  Tidy up.
 500  CONTINUE

*  Release FDB arrays.
*  Release ECHELLE extension.
      DO 3 I = 1, 7
         CALL DAT_ANNUL( LOC(I), STATUS )
 3    CONTINUE
      CALL DAT_ANNUL( XLOC, STATUS )

*  Release output NDF.
*  Release workspaces.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARCGENDB_E03', 'Deleting output feature ' //
     :      'data base.', STATUS )
         CALL NDF_DELET( NDF(1), STATUS )
      END IF
      DO 4 I = 1, 4
         CALL NDF_ANNUL( NDF(I), STATUS )
 4    CONTINUE

*  Close input file.
      CALL FIO_CANCL( 'IN', STATUS )
      CALL NDF_END( STATUS )

*  Return.
      END
