      SUBROUTINE MV4_PROTWR( )
*+
*  Name:
*     MV4_PROTWR

*  Purpose:
*     Write prototype spectrum header to map file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_PROTWR( )

*  Description:
*     This routine writes the prototype spectrum header to the map file
*     for Specx. If the relevant structure in the map file exists, then
*     only its values will be changed. If the stucture does not exist,
*     then it will be created.
*
*     Note that this routine writes from the /STACKCOMM/ common block,
*     while the sister routine reads the prototype into the /PROTOTYPE/
*     common block. The point is that the prototype is written to a map
*     file only once, when the first spectrum ever is written to it. So
*     it is written from that spectrum's header. The prototype is read
*     back into /PROTOTYPE/ whenever a map file is opened.
*
*     This begs the question, when /STACKCOMM/ is written to the file as
*     the prototype, should it not also be copied to /PROTOTYPE/? And
*     how can that be done?
*
*     The answer is that this routine calls its sister MV4_PROTRD after
*     the prototype is safely in the file.

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

*  Global Variables:
      INCLUDE 'MAPHD'            ! Specx map header
      INCLUDE 'STACKCOMM'        ! Specx spectrum header
      INCLUDE 'MAPV4'            ! Map locators etc.

*  Local Variables:
      LOGICAL THERE              ! True if an HDS component exists
      INTEGER STATUS             ! Starlink status
      INTEGER I                  ! Casting buffer
      CHARACTER * ( DAT__SZLOC ) XLOC( 2 ) ! Temporary locators

*.

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK


*  See if PROTOTYPE exists, create if necessary.
*  =============================================


      CALL NDF_XSTAT( IDXNDF, 'SPECX', THERE, STATUS )


      IF ( .NOT. THERE ) THEN

*     Create and locate PROTOTYPE itself.
         CALL NDF_XNEW( IDXNDF, 'SPECX', 'SPECX_EXT1', 0, 0,
     :                  XLOC(1), STATUS )

*     Create each component in PROTOTYPE. Code stolen from fv4.
         CALL DAT_NEW(   XLOC(1), 'LSCAN',  '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW(   XLOC(1), 'NPTS',   '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   XLOC(1), 'TSYS',   '_REAL',      1, 1, STATUS )
         CALL DAT_NEW(   XLOC(1), 'LOFREQ', '_DOUBLE',    1, 1, STATUS )
         CALL DAT_NEW(   XLOC(1), 'IFFREQ', '_DOUBLE',    1, 1, STATUS )
         CALL DAT_NEW(   XLOC(1), 'AZ_EL',  '_REAL',      1, 2, STATUS )
         CALL DAT_NEW(   XLOC(1), 'JFREST', '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   XLOC(1), 'JFCEN',  '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   XLOC(1), 'JFINC',  '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   XLOC(1), 'INTT',   '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW(   XLOC(1), 'ITREC',  '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   XLOC(1), 'ITSKY',  '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   XLOC(1), 'ITTEL',  '_INTEGER',   1, 1, STATUS )
         CALL DAT_NEW(   XLOC(1), 'RA_DEC', '_DOUBLE',    1, 2, STATUS )
         CALL DAT_NEW(   XLOC(1), 'DPOS',   '_REAL',      1, 2, STATUS )
         CALL DAT_NEW(   XLOC(1), 'V_SETL', '_REAL',      1, 4, STATUS )
         CALL DAT_NEW(   XLOC(1), 'IMODE',  '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW(   XLOC(1), 'ICALZD', '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW(   XLOC(1), 'LSRFLG', '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW(   XLOC(1), 'IQCEN',  '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW0C( XLOC(1), 'ITITLE',                 26, STATUS )
         CALL DAT_NEW0C( XLOC(1), 'IDATE',                   9, STATUS )
         CALL DAT_NEW(   XLOC(1), 'IUTFLG', '_INTEGER',   0, 0, STATUS )
         CALL DAT_NEW0C( XLOC(1), 'ITIME',                   8, STATUS )

*     Release PROTOTYPE.
         CALL DAT_ANNUL( XLOC(1), STATUS )

      END IF


*  Write to PROTOTYPE. This alters shapes and puts data. Code stolen
*  from fv4.
*  =================================================================

      CALL NDF_XLOC(  IDXNDF, 'SPECX', 'UPDATE', XLOC(1), STATUS )

      CALL NDF_XPT0I( LSCAN, IDXNDF, 'SPECX', 'LSCAN', STATUS )

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

      CALL DAT_ANNUL( XLOC(1), STATUS )



*  So the prototype is in the file now, but we need it in the
*  /PROTOTYPE/ common block as well.
*  ==========================================================

      CALL MV4_PROTRD( )


*  Tidy up.
*  ========

 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
      END IF
      CALL ERR_RLSE

*  Return.
      END
