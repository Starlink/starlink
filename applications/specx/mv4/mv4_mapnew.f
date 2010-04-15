      SUBROUTINE MV4_MAPNEW( IFAIL )
*+
*  Name:
*     MV4_MAPNEW

*  Purpose:
*     Open a new map file for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_MAPNEW( IFAIL )

* Description:
*     This routine opens a new map file for Specx. This also creates,
*     maps and initialises the index, creates and writes the map header,
*     and creates the position array (the sequence of spectra). The
*     position array is not initialised, it will be a vector of NSPEC
*     undefined NDFs. The prototype is not created or written.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        Must be zero on entry, returned non-zero on failure.

*  Prior Requirements:
*     The HDS system must have been started and the NDF system must have
*     been begun.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JACH)
*     {enter_new_authors_here}

*  History:
*     12 Aug 1994 (hme):
*        Original version.
*     31 Aug 1994 (hme):
*        NDF/HDS-based sparse cube.
*     01 Sep 1994 (hme):
*        Take care that position array has at least one cell. Also
*        initialise all cells to have what looks like an NDF.
*        IFAIL=STATUS was inadequate.
*        HDS_NEW must be given a structure name short enough.
*     05 Oct 1995 (timj)
*        Modify map format to improve speed.
*        Use large POSN array instead of NSPEC NDF entries.
*        Add blank spectrum prototype header at this stage
*     21 Sep 2000 (ajc)
*        Unused I, J, TNDF, DATPTR
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
      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPHD'
      INCLUDE 'MAPV4'

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER K                  ! Temporary integer
      INTEGER STATUS             ! Starlink status
      INTEGER NELM               ! Size of mapped array
      INTEGER NPT                ! Initial size of array (NPTS1)
      INTEGER ONE( 2 )           ! Itself
      INTEGER DIM( 2 )           ! Index dimensions
      INTEGER MPLACE             ! NDF placeholder of map file
      INTEGER SPLACE             ! NDF placeholder of POSN
      INTEGER POSDIM( 2 )        ! Size of POSN array
      INTEGER NPOINTS            ! Temporary number
      CHARACTER * ( DAT__SZLOC ) TLOC( 3 ) ! Temporary HDS locators


*  Local Data:
      DATA ONE / 1, 1 /

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Open a new file.
      CALL NDF_OPEN( DAT__ROOT, NAMEMP, 'WRITE', 'NEW', IDXNDF,
     :               MPLACE, STATUS )

*  Create, map, initialise the index.
      DIM(1) = MSTEP
      DIM(2) = NSTEP
      CALL NDF_NEW( '_INTEGER', 2, ONE, DIM, MPLACE, IDXNDF, STATUS )
      CALL NDF_MAP(   IDXNDF, 'DATA', '_INTEGER', 'WRITE/BAD',
     :   IDXPTR, NELM, STATUS )

*  Create and write the map header.
      CALL NDF_XNEW( IDXNDF, 'SPECX_MAP', 'SPECX_EXT2',
     :   0, 0, TLOC(1), STATUS )
      CALL DAT_NEW(    TLOC(1), 'NAME',     '_CHAR*12', 0, 0, STATUS )
      CALL DAT_NEW(    TLOC(1), 'ID',       '_CHAR*40', 0, 0, STATUS )
      CALL DAT_NEW(    TLOC(1), 'VERSION',  '_REAL',    0, 0, STATUS )
      CALL DAT_NEW(    TLOC(1), 'IHEAD',    '_INTEGER', 0, 0, STATUS )
      CALL DAT_NEW(    TLOC(1), 'RAM_DECM', '_DOUBLE',  1, 2, STATUS )
      CALL DAT_NEW(    TLOC(1), 'CELLSIZE', '_REAL',    1, 2, STATUS )
      CALL DAT_NEW(    TLOC(1), 'POSANGLE', '_REAL',    0, 0, STATUS )
      CALL DAT_NEW(    TLOC(1), 'MNSTEP',   '_INTEGER', 1, 2, STATUS )
      CALL DAT_NEW(    TLOC(1), 'NPTS1',    '_INTEGER', 0, 0, STATUS )
      CALL DAT_NEW(    TLOC(1), 'NSPEC',    '_INTEGER', 0, 0, STATUS )
      CALL DAT_NEW(    TLOC(1), 'NREDT',    '_INTEGER', 0, 0, STATUS )
      CALL DAT_NEW(    TLOC(1), 'ID1',      '_INTEGER', 0, 0, STATUS )
      CALL DAT_ANNUL(  TLOC(1), STATUS )
      CALL MV4_HEADWR( )

*  Create a blank spectrum header
*  This has the advantage that the data is then always on the end of
*  the file.

         CALL NDF_XNEW( IDXNDF, 'SPECX', 'SPECX_EXT1', 0, 0,
     :                  TLOC(2), STATUS )

*     Create each component in PROTOTYPE. Code stolen from fv4.
         CALL DAT_NEW(   TLOC(2), 'LSCAN',  '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW(   TLOC(2), 'NPTS',   '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   TLOC(2), 'TSYS',   '_REAL',      1, 1, STATUS )
         CALL DAT_NEW(   TLOC(2), 'LOFREQ', '_DOUBLE',    1, 1, STATUS )
         CALL DAT_NEW(   TLOC(2), 'IFFREQ', '_DOUBLE',    1, 1, STATUS )
         CALL DAT_NEW(   TLOC(2), 'AZ_EL',  '_REAL',      1, 2, STATUS )
         CALL DAT_NEW(   TLOC(2), 'JFREST', '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   TLOC(2), 'JFCEN',  '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   TLOC(2), 'JFINC',  '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   TLOC(2), 'INTT',   '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW(   TLOC(2), 'ITREC',  '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   TLOC(2), 'ITSKY',  '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   TLOC(2), 'ITTEL',  '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   TLOC(2), 'RA_DEC', '_DOUBLE',    1, 2, STATUS )
         CALL DAT_NEW(   TLOC(2), 'DPOS',   '_REAL',      1, 2, STATUS )
         CALL DAT_NEW(   TLOC(2), 'V_SETL', '_REAL',      1, 4, STATUS )
         CALL DAT_NEW(   TLOC(2), 'IMODE',  '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW(   TLOC(2), 'ICALZD', '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW(   TLOC(2), 'LSRFLG', '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW(   TLOC(2), 'IQCEN',  '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW0C( TLOC(2), 'ITITLE',                 26, STATUS )
         CALL DAT_NEW0C( TLOC(2), 'IDATE',                   9, STATUS )
         CALL DAT_NEW(   TLOC(2), 'IUTFLG', '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW0C( TLOC(2), 'ITIME',                   8, STATUS )

*     Release PROTOTYPE.
         CALL DAT_ANNUL( TLOC(2), STATUS )


*  Sort out the POSN part of the file

*  Define size of POSN data_array
      NELM = MAX( 1, NSPEC )
      NPT  = MAX( 1, NPTS1 )
      POSDIM(1) = NPT
      POSDIM(2) = NELM

* Create the POSN extension
      CALL NDF_XNEW( IDXNDF, 'POSN', 'NDF', 0, 1, TLOC(3), STATUS )

      CALL NDF_PLACE( TLOC(3), ' ', SPLACE, STATUS )
      CALL NDF_NEW( '_REAL', 2, ONE, POSDIM, SPLACE, POSNDF, STATUS )
      CALL NDF_MAP(   POSNDF, 'DATA', '_REAL', 'WRITE/BAD',
     :   POSPTR, NPOINTS, STATUS )

      CALL DAT_ANNUL( TLOC(3), STATUS )

* Find out current bounds of map for use when spectra are written to it
* Find out lower bound just in case it is not equal to 1

      CALL NDF_BOUND( POSNDF, 2, MAPLBND, MAPUBND, K, STATUS )



*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 62
         CALL NDF_ANNUL( POSNDF, STATUS )
         CALL NDF_ANNUL( IDXNDF, STATUS )
         CALL ERR_FLUSH( STATUS )
      END IF
      CALL ERR_RLSE

*  Return.
      END
