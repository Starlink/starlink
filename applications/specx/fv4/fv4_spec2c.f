      SUBROUTINE FV4_SPEC2C( CELLOC, STATUS )
*+
*  Name:
*     FV4_SPEC2C

*  Purpose:
*     Write X register to located cell in file for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_SPEC2C( CELLOC, STATUS )

*  Description:
*     This routine writes the contents of the X register of the Specx
*     stack to an NDF that has been located via the given HDS locator.
*     The NDF must be intact, especially it must have a complete SPECX
*     extension. It will be overwritten without checks.

*  Arguments:
*     CELLOC = CHARACTER * ( * ) (Given)
*        The HDS locator to the NDF to be written. Usually this will be
*        a cell (an element) in the SPECTRUM array of a Starlink Data
*        File, which is a version-4 file for spectra for Specx.
*     STATUS = INTEGER (Given and Returned)
*        The global Starlink status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     07 Dec 1993 (hme):
*        Original version.
*     02 Sep 1994 (hme):
*        Replace the XCOPY with a new routine MV4_SPECW2 to convert bad
*        values properly.
*     16 Aug 2004 (timj):
*        Use CNF_PVAL for HDS mapped data arrays
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL

*  Global Variables:
      INCLUDE 'STACKCOMM'        ! Specx stack

*  Arguments Given:
      CHARACTER * ( * ) CELLOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Temporary integer
      INTEGER TNDF               ! NDF identifier
      INTEGER DPNTR              ! Pointer to mapped data array
      INTEGER NELM               ! Array size
      CHARACTER * ( DAT__SZLOC ) XLOC( 2 ) ! Locator to/in extension

*  Internal References:
      INTEGER NTOT               ! Sum(NPTS) 1st to nominated quadrant

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import cell as NDF.
      CALL NDF_IMPRT( CELLOC, TNDF, STATUS )

*  Reshape, map, copy, unmap data.
      NELM = NTOT(NQUAD)
      CALL NDF_SBND( 1, 1, NELM, TNDF, STATUS )
      CALL NDF_MAP( TNDF, 'DATA', '_REAL', 'WRITE',
     :   DPNTR, NELM, STATUS )
*     CALL XCOPY( 4*NELM, DATA, %VAL(CNF_PVAL(DPNTR)) )
      CALL FV4_SPECW2( NELM, DATA, %VAL(CNF_PVAL(DPNTR)) )
      CALL NDF_UNMAP( TNDF, 'DATA', STATUS )

*  Locate extension XLOC(1).
      CALL NDF_XLOC( TNDF, 'SPECX', 'UPDATE', XLOC(1), STATUS )

*  For each extension item: reshape and put. Use XLOC(2) short term.
      CALL CMP_PUT0I( XLOC(1), 'LSCAN', LSCAN, STATUS )

      CALL DAT_FIND(  XLOC(1), 'NPTS', XLOC(2), STATUS )
      CALL DAT_ALTER( XLOC(2), 1, NQUAD, STATUS )
      CALL DAT_PUT1I( XLOC(2),    NQUAD, NPTS, STATUS )
      CALL DAT_ANNUL( XLOC(2), STATUS )

      CALL DAT_FIND(  XLOC(1), 'TSYS', XLOC(2), STATUS )
      CALL DAT_ALTER( XLOC(2), 1, NQUAD, STATUS )
      CALL DAT_PUT1R( XLOC(2),    NQUAD, TSYS, STATUS )
      CALL DAT_ANNUL( XLOC(2), STATUS )

      CALL DAT_FIND(  XLOC(1), 'LOFREQ', XLOC(2), STATUS )
      CALL DAT_ALTER( XLOC(2), 1, NQUAD, STATUS )
      CALL DAT_PUT1D( XLOC(2),    NQUAD, LOFREQ, STATUS )
      CALL DAT_ANNUL( XLOC(2), STATUS )

      CALL DAT_FIND(  XLOC(1), 'IFFREQ', XLOC(2), STATUS )
      CALL DAT_ALTER( XLOC(2), 1, NQUAD, STATUS )
      CALL DAT_PUT1D( XLOC(2),    NQUAD, IFFREQ, STATUS )
      CALL DAT_ANNUL( XLOC(2), STATUS )

      CALL CMP_PUT1R( XLOC(1), 'AZ_EL', 2, AZ, STATUS )

      CALL DAT_FIND(  XLOC(1), 'JFREST', XLOC(2), STATUS )
      CALL DAT_ALTER( XLOC(2), 1, NQUAD, STATUS )
      CALL DAT_PUT1I( XLOC(2),    NQUAD, JFREST, STATUS )
      CALL DAT_ANNUL( XLOC(2), STATUS )

      CALL DAT_FIND(  XLOC(1), 'JFCEN', XLOC(2), STATUS )
      CALL DAT_ALTER( XLOC(2), 1, NQUAD, STATUS )
      CALL DAT_PUT1I( XLOC(2),    NQUAD, JFCEN, STATUS )
      CALL DAT_ANNUL( XLOC(2), STATUS )

      CALL DAT_FIND(  XLOC(1), 'JFINC', XLOC(2), STATUS )
      CALL DAT_ALTER( XLOC(2), 1, NQUAD, STATUS )
      CALL DAT_PUT1I( XLOC(2),    NQUAD, JFINC, STATUS )
      CALL DAT_ANNUL( XLOC(2), STATUS )

      CALL CMP_PUT0I( XLOC(1), 'INTT', INTT, STATUS )

      CALL DAT_FIND(  XLOC(1), 'ITREC', XLOC(2), STATUS )
      CALL DAT_ALTER( XLOC(2), 1, NQUAD, STATUS )
      CALL DAT_PUT1I( XLOC(2),    NQUAD, ITREC, STATUS )
      CALL DAT_ANNUL( XLOC(2), STATUS )

      CALL DAT_FIND(  XLOC(1), 'ITSKY', XLOC(2), STATUS )
      CALL DAT_ALTER( XLOC(2), 1, NQUAD, STATUS )
      CALL DAT_PUT1I( XLOC(2),    NQUAD, ITSKY, STATUS )
      CALL DAT_ANNUL( XLOC(2), STATUS )

      CALL DAT_FIND(  XLOC(1), 'ITTEL', XLOC(2), STATUS )
      CALL DAT_ALTER( XLOC(2), 1, NQUAD, STATUS )
      CALL DAT_PUT1I( XLOC(2),    NQUAD, ITTEL, STATUS )
      CALL DAT_ANNUL( XLOC(2), STATUS )

      CALL CMP_PUT1D( XLOC(1), 'RA_DEC', 2, RA,  STATUS )
      CALL CMP_PUT1R( XLOC(1), 'DPOS',   2, DRA, STATUS )
      CALL CMP_PUT1R( XLOC(1), 'V_SETL', 4, VSL, STATUS )

      CALL CMP_PUT0I( XLOC(1), 'IMODE',  IMODE,  STATUS )
      CALL CMP_PUT0I( XLOC(1), 'ICALZD', ICALZD, STATUS )
      CALL CMP_PUT0I( XLOC(1), 'LSRFLG', LSRFLG, STATUS )
      CALL CMP_PUT0I( XLOC(1), 'IQCEN',  IQCEN,  STATUS )

      CALL CMP_PUT0C( XLOC(1), 'ITITLE', ITITLE, STATUS )
      CALL CMP_PUT0C( XLOC(1), 'IDATE',  IDATE,  STATUS )
      I = IUTFLG
      CALL CMP_PUT0I( XLOC(1), 'IUTFLG', I,      STATUS )
      CALL CMP_PUT0C( XLOC(1), 'ITIME',  ITIME,  STATUS )

*  Annull extension and NDF.
      CALL DAT_ANNUL( XLOC(1), STATUS )
      CALL NDF_ANNUL( TNDF, STATUS )

*  Return.
      END



      SUBROUTINE FV4_SPECW2( NELM, MEMDAT, FILDAT )

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'
      INCLUDE 'FLAGCOMM'

      INTEGER NELM
      REAL MEMDAT( NELM )
      REAL FILDAT( NELM )

      INTEGER K

      DO 2 K = 1, NELM
         IF ( MEMDAT(K) .EQ. BADPIX_VAL ) THEN
            FILDAT(K) = VAL__BADR
         ELSE
            FILDAT(K) = MEMDAT(K)
         END IF
 2    CONTINUE

      END
