      SUBROUTINE MV4_PROTRD( )
*+
*  Name:
*     MV4_PROTRD

*  Purpose:
*     Read prototype spectrum header from map file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_PROTRD( )

*  Description:
*     This routine reads the prototype spectrum header from the map file
*     for Specx. The relevant structure must exist in the map file.

*  Arguments:
*     None.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     13 Aug 1994 (hme):
*        Original version.
*     31 Aug 1994 (hme):
*        NDF/HDS-based sparse cube.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'SPECX_PARS'       ! General Specx parameters

*  Global Variables:
      INCLUDE 'MAPHD'            ! Specx map header
      INCLUDE 'PROTOTYPE'        ! Specx prototype spectrum header
      INCLUDE 'MAPV4'            ! Map locators etc.

*  Local Variables:
      INTEGER STATUS             ! Starlink status
      INTEGER I                  ! Casting buffer
      INTEGER NDIM, NELM         ! Dimensionality, size of HDS objects
      CHARACTER * ( DAT__SZLOC ) XLOC( 2 ) ! Temporary locators

*.

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Locate the prototype.
      CALL NDF_XLOC( IDXNDF, 'SPECX', 'READ', XLOC(1), STATUS )

*  First find out the number of quadrants, which is the length of
*  several vectors in the file. Incidentally, in a map there is always
*  only one quadrant.
      CALL DAT_FIND(  XLOC(1), 'NPTS', XLOC(2), STATUS )
      CALL DAT_SHAPE( XLOC(2), 1, NQUAD, NDIM, STATUS )
      CALL DAT_ANNUL( XLOC(2), STATUS )

*  The remaining reading code is stolen from fv4.
      IST   = 0
      IEND  = 0
      CALL CMP_GET1I( XLOC(1), 'NPTS',   NQUAD, NPTS,   NELM, STATUS )
      CALL CMP_GET1R( XLOC(1), 'TSYS',   NQUAD, TSYS,   NELM, STATUS )
      CALL CMP_GET1D( XLOC(1), 'LOFREQ', NQUAD, LOFREQ, NELM, STATUS )
      CALL CMP_GET1D( XLOC(1), 'IFFREQ', NQUAD, IFFREQ, NELM, STATUS )
      CALL CMP_GET1R( XLOC(1), 'AZ_EL',      2, AZ,     NELM, STATUS )
      CALL CMP_GET1I( XLOC(1), 'JFREST', NQUAD, JFREST, NELM, STATUS )
      CALL CMP_GET1I( XLOC(1), 'JFCEN',  NQUAD, JFCEN,  NELM, STATUS )
      CALL CMP_GET1I( XLOC(1), 'JFINC',  NQUAD, JFINC,  NELM, STATUS )
      CALL CMP_GET0I( XLOC(1), 'INTT',   INTT,  STATUS )
      CALL CMP_GET1I( XLOC(1), 'ITREC',  NQUAD, ITREC,  NELM, STATUS )
      CALL CMP_GET1I( XLOC(1), 'ITSKY',  NQUAD, ITSKY,  NELM, STATUS )
      CALL CMP_GET1I( XLOC(1), 'ITTEL',  NQUAD, ITTEL,  NELM, STATUS )
      CALL CMP_GET1D( XLOC(1), 'RA_DEC',     2, RA,     NELM, STATUS )
      CALL CMP_GET1R( XLOC(1), 'DPOS',       2, DRA,    NELM, STATUS )
      CALL CMP_GET1R( XLOC(1), 'V_SETL',     4, VSL,    NELM, STATUS )
      CALL CMP_GET0I( XLOC(1), 'IMODE',  IMODE,  STATUS )
      CALL CMP_GET0I( XLOC(1), 'ICALZD', ICALZD, STATUS )
      CALL CMP_GET0I( XLOC(1), 'LSRFLG', LSRFLG, STATUS )
      CALL CMP_GET0I( XLOC(1), 'IQCEN',  IQCEN,  STATUS )
      CALL CMP_GET0C( XLOC(1), 'ITITLE', ITITLE, STATUS )
      CALL CMP_GET0C( XLOC(1), 'IDATE',  IDATE,  STATUS )
      CALL CMP_GET0I( XLOC(1), 'IUTFLG', I,      STATUS )
      IUTFLG = I
      CALL CMP_GET0C( XLOC(1), 'ITIME',  ITIME,  STATUS )

*  Fill header elements for unused quadrants.
*  Also stolen from fv4.
      DO 1 I = NQUAD+1, NQMAX
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
 1    CONTINUE

*  Release the prototype.
      CALL DAT_ANNUL( XLOC(1), STATUS )

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
      END IF
      CALL ERR_RLSE

*  Return.
      END
