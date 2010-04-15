      SUBROUTINE FV4_SPECRD( IFILE, IFAIL )
      ENTRY        READSCAN( IFILE, IFAIL )
*+
*  Name:
*     FV4_SPECRD

*  Purpose:
*     Read a spectrum from a file for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_SPECRD( IFILE, IFAIL )

*  Description:
*     This routine serves the READ-SPECTRUM command of Specx. It reads a
*     spectrum from a file of format version 4.

*  Arguments:
*     IFILE = INTEGER (Given)
*        The internal file number for the file to be read from.
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     rpt: Remo Tilanus (JAC, Hilo)
*     timj: Tim Jenness (JAC, Hilo)
*     {enter_new_authors_here}

*  History:
*     06 Dec 1993 (hme):
*        Original version.
*     10 May 1995 (rpt):
*        Added support FILHD
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
      INCLUDE 'STAKPAR'          ! Specx stack control information
      INCLUDE 'SPECX_PARS'       ! LHEAD, NQMAX
      INCLUDE 'FILES'            ! Open files information

*  FILHD Variables:
      INCLUDE 'FILHD'            ! NSCAN, NAME, ID, VERSION, IREC1

*  Arguments Given:
      INTEGER IFILE

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER I, NDIM, NELM      ! Temporary integers
      INTEGER ISCAN              ! Scan number
      INTEGER ICELL              ! Cell number ISCAN-IREC1+1
      INTEGER NDF                ! NDF identifier
      INTEGER DPNTR              ! Array pointer
      INTEGER IQUAD              ! Provisional number of quadrants
      INTEGER STATUS             ! Starlink status
      INTEGER JDEF               ! Returned by GEN_*
      CHARACTER * ( DAT__SZLOC ) CELLOC, XLOC, TLOC ! HDS locators

*  Internal References:
      LOGICAL CHKACC             ! Check file access

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Check given file number.
      IF ( .NOT. ( CHKACC(IFILE,'R') .AND. FILELUNS(IFILE).NE.0 ) ) THEN
         IFAIL = 3
         GO TO 500
      END IF

*  Get some information from file header (fill FILHD)
      CALL FV4_FILINF ( IFILE, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         IFAIL = 38
         GO TO 500
      END IF

*  Find out which scan and cell.
      CALL GEN_GETI4( 'Scan? ', 0, ' ', ISCAN, JDEF )
      ICELL = ISCAN - IREC1 + 1
      IF ( ICELL .LT. 1 .OR. ICELL .GT. NSCAN ) THEN
         IFAIL = 2
         GO TO 500
      END IF

*  Get hold of cell (as an NDF), locate the extension.
*  Check that not more data in NDF than fit into a stack position.
      CALL DAT_CELL(  SPXLOC(IFILE), 1, ICELL, CELLOC, STATUS )
      CALL NDF_IMPRT( CELLOC, NDF, STATUS )
      CALL DAT_ANNUL( CELLOC, STATUS )
      CALL NDF_XLOC(  NDF, 'SPECX', 'READ', XLOC, STATUS )
      CALL NDF_SIZE(  NDF, NELM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 2
         GO TO 400
      ELSE IF ( NELM .GT. LSTK - LHEAD ) THEN
         IFAIL = 25
         GO TO 400
      END IF

*  Check that not more quadrants than the programme can cope with.
      CALL DAT_FIND( XLOC, 'NPTS', TLOC, STATUS )
      CALL DAT_SHAPE( TLOC, 1, IQUAD, NDIM, STATUS )
      CALL DAT_ANNUL( TLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 2
         GO TO 400
      ELSE IF ( IQUAD .GT. NQMAX ) THEN
         IFAIL = 25
         GO TO 400
      END IF

*  Push the stack.
      IF ( .NOT. XCLEAR ) CALL PUSH
      IF ( JTOP .EQ. 0 ) JTOP = 1

*  Map, copy, unmap data.
*  Fill unused stack tail with zeros.
      CALL NDF_MAP( NDF, 'DATA', '_REAL', 'READ', DPNTR, NELM, STATUS )
*     CALL XCOPY( 4*NELM, %VAL(CNF_PVAL(DPNTR)), DATA )
      CALL FV4_SPECR2( NELM, %VAL(CNF_PVAL(DPNTR)), DATA )
      CALL NDF_UNMAP( NDF, 'DATA', STATUS )
      DO 1001 I = NELM + 1, LSTK - LHEAD
         DATA(I) = 0.
 1001 CONTINUE

*  Read all the header information.
      LSCAN = ISCAN
      NQUAD = IQUAD
      IST   = 0
      IEND  = 0
      CALL CMP_GET1I( XLOC, 'NPTS',   NQUAD, NPTS,   NELM, STATUS )
      CALL CMP_GET1R( XLOC, 'TSYS',   NQUAD, TSYS,   NELM, STATUS )
      CALL CMP_GET1D( XLOC, 'LOFREQ', NQUAD, LOFREQ, NELM, STATUS )
      CALL CMP_GET1D( XLOC, 'IFFREQ', NQUAD, IFFREQ, NELM, STATUS )
      CALL CMP_GET1R( XLOC, 'AZ_EL',      2, AZ,     NELM, STATUS )
      CALL CMP_GET1I( XLOC, 'JFREST', NQUAD, JFREST, NELM, STATUS )
      CALL CMP_GET1I( XLOC, 'JFCEN',  NQUAD, JFCEN,  NELM, STATUS )
      CALL CMP_GET1I( XLOC, 'JFINC',  NQUAD, JFINC,  NELM, STATUS )
      CALL CMP_GET0I( XLOC, 'INTT',   INTT,  STATUS )
      CALL CMP_GET1I( XLOC, 'ITREC',  NQUAD, ITREC,  NELM, STATUS )
      CALL CMP_GET1I( XLOC, 'ITSKY',  NQUAD, ITSKY,  NELM, STATUS )
      CALL CMP_GET1I( XLOC, 'ITTEL',  NQUAD, ITTEL,  NELM, STATUS )
      CALL CMP_GET1D( XLOC, 'RA_DEC',     2, RA,     NELM, STATUS )
      CALL CMP_GET1R( XLOC, 'DPOS',       2, DRA,    NELM, STATUS )
      CALL CMP_GET1R( XLOC, 'V_SETL',     4, VSL,    NELM, STATUS )
      CALL CMP_GET0I( XLOC, 'IMODE',  IMODE,  STATUS )
      CALL CMP_GET0I( XLOC, 'ICALZD', ICALZD, STATUS )
      CALL CMP_GET0I( XLOC, 'LSRFLG', LSRFLG, STATUS )
      CALL CMP_GET0I( XLOC, 'IQCEN',  IQCEN,  STATUS )
      CALL CMP_GET0C( XLOC, 'ITITLE', ITITLE, STATUS )
      CALL CMP_GET0C( XLOC, 'IDATE',  IDATE,  STATUS )
      CALL CMP_GET0I( XLOC, 'IUTFLG', I,      STATUS )
      IUTFLG = I
      CALL CMP_GET0C( XLOC, 'ITIME',  ITIME,  STATUS )

*  Fill header elements for unused quadrants.
      DO 1002 I = NQUAD+1, NQMAX
         NPTS(I)   = 0
         TSYS(I)   = 0.
         LOFREQ(I) = 0D0
         IFFREQ(I) = 0D0
         JFREST(I) = 0
         JFCEN(I)  = 0
         JFINC(I)  = 0
         ITREC(I)  = 0
         ITSKY(I)  = 0
         ITTEL(I)  = 0
 1002 CONTINUE

*  If any failure since pushing the stack, pull it.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( .NOT. XCLEAR ) CALL POP
         IFAIL = 38
         GO TO 400
      END IF

*  Mark the X register as used.
      XCLEAR = .FALSE.

*  Tidy up.
*  --------

*  Annull the NDF extension and the NDF.
 400  CONTINUE
      CALL DAT_ANNUL( XLOC, STATUS )
      CALL NDF_ANNUL( NDF,  STATUS )

*  Do Starlink error reporting and close Starlink error context.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END



      SUBROUTINE FV4_SPECR2( NELM, FILDAT, MEMDAT )

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'
      INCLUDE 'FLAGCOMM'

      INTEGER NELM
      REAL FILDAT( NELM )
      REAL MEMDAT( NELM )

      INTEGER K

      DO 2 K = 1, NELM
         IF ( FILDAT(K) .EQ. VAL__BADR ) THEN
            MEMDAT(K) = BADPIX_VAL
         ELSE
            MEMDAT(K) = FILDAT(K)
         END IF
 2    CONTINUE

      END
