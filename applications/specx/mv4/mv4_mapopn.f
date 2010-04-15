      SUBROUTINE MV4_MAPOPN( IFAIL )
*+
*  Name:
*     MV4_MAPOPN

*  Purpose:
*     Open a map file for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_MAPOPN( IFAIL )

* Description:
*     This routine opens a map file for Specx. This also maps the index
*     and the position array.

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
*     13 Aug 1994 (hme):
*        Original version.
*     31 Aug 1994 (hme):
*        NDF/HDS-based sparse cube.
*     01 Sep 1994 (hme):
*        Must suppress Starlink error reports, since this routine is
*        used to find out whether a map file is old. Also, IFAIL=STATUS
*        is inappropriate.
*     02 Sep 1994 (hme):
*        Actually, do return the Starlink status. The calling routine
*        must decide why this one failed.
*     09 Oct 1995 (timj):
*        Upgrade to map version 4.2
*     10 June 2003 (timj):
*        Add bounds check and store NSPEC and NPTS1 from POSN
*        for size verification against header. This was added
*        because some people have maps where NSPEC is not equal
*        to the number of spectra in the map itself!
*     3 Nov 2003 (timj):
*        Should only compare bounds when STATUS is good (eg when
*        opening a new file you get bad status!)
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

*  Constants:
      INTEGER NUM_POSN_DIMS      ! Expected number of dims for POSN
      PARAMETER ( NUM_POSN_DIMS = 2 )

*  Local Variables:
      INTEGER K                  ! Temporary integer
      INTEGER STATUS             ! Starlink status
      INTEGER NELM               ! Size of mapped array
      INTEGER MPLACE             ! Placeholder of map
      INTEGER NPOINTS            ! Number of points in POSN
      REAL MAPVERSION            ! Map version number
      INTEGER IMAPV              ! INT form of version number (*10)
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary locator
      INTEGER SPDIM( NUM_POSN_DIMS ) ! Dimensions of POSN
      INTEGER NSPDIMS            ! Actual number of dimensions

      LOGICAL CONTINUE           ! Yes/no
      INTEGER ISTAT              ! Genlib thing

*.
      MAPVERSION = 0.0

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Open the file.
      CALL NDF_OPEN( DAT__ROOT, NAMEMP, 'UPDATE', 'OLD', IDXNDF,
     :               MPLACE, STATUS )

*  Check to make sure the map format is NOT v4.1
      CALL NDF_XGT0R( IDXNDF, 'SPECX_MAP', 'VERSION', MAPVERSION,
     :                STATUS )

*  Force 1 decimal place for version number check
*  but round up second decimal place
      IMAPV = INT( (MAPVERSION + 0.01 ) * 10.0 )

*  Convert v4.1 to version 4.2
*  Use INTEGER comparison
      IF (IMAPV .EQ. 41) THEN
         PRINT *,'Map version 4.1 is no longer supported'
         PRINT *,'This map must be converted in order to continue'

         CALL GEN_YESNO ('Copy the map to a new format file?',
     :        .TRUE., CONTINUE, ISTAT )

         IF ( CONTINUE ) THEN
*  Need to close file in order to change access mode before converting
            CALL NDF_ANNUL( IDXNDF, STATUS )

*  Convert the file (NAMEMP is changed on return)
            CALL MV4_41TO42( NAMEMP, STATUS )

*  Open the newly converted map
            IF ( STATUS .EQ. SAI__OK ) THEN
               PRINT *,'Now opening new file...'
            END IF
            CALL NDF_OPEN( DAT__ROOT, NAMEMP, 'UPDATE', 'OLD', IDXNDF,
     :                     MPLACE, STATUS )
         ELSE
            STATUS = SAI__ERROR
         END IF
      END IF

*  Everything is okay so I can now map the index.
      CALL NDF_MAP(  IDXNDF, 'DATA', '_INTEGER', 'UPDATE',
     :               IDXPTR, NELM, STATUS )

*  Locate the POSN array.
      CALL NDF_XLOC(  IDXNDF, 'POSN', 'UPDATE', TLOC, STATUS )

* Map POSN array to POSPTR
      CALL NDF_OPEN( TLOC, ' ', 'UPDATE', 'OLD', POSNDF,
     :               MPLACE, STATUS )

      CALL NDF_MAP(   POSNDF, 'DATA', '_REAL', 'UPDATE',
     :                POSPTR, NPOINTS, STATUS )

*  Verify the dimensions of the spectral data
      CALL NDF_DIM( POSNDF, NUM_POSN_DIMS, SPDIM, NSPDIMS, STATUS)
      IF (STATUS .EQ. SAI__OK .AND. NSPDIMS .NE. NUM_POSN_DIMS) THEN
*  Abort if we do not get the right number of dimensions
         print *,'Fatal error in map. Number of dimensions is ',
     :        NSPDIMS, ' and not ', NUM_POSN_DIMS
         IFAIL = 18
         RETURN
      END IF

*  Set NSPEC from here prior to reading the header itself
*  Ideally the header should already have been read.
      IF (STATUS .EQ. SAI__OK) THEN
         NSPEC = SPDIM(2)
         NPTS1 = SPDIM(1)
      END IF

      CALL DAT_ANNUL( TLOC, STATUS )

* Find out current bounds of map for use when spectra are written to it
* Find out lower bound just in case it is not equal to 1

      CALL NDF_BOUND( POSNDF, NUM_POSN_DIMS, MAPLBND, MAPUBND, K,STATUS)



*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = STATUS
         CALL NDF_ANNUL( IDXNDF, STATUS )
	 CALL NDF_ANNUL( POSNDF, STATUS )
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL ERR_RLSE

*  Return.
      END
