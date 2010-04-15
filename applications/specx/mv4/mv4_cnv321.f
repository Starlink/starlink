      SUBROUTINE MV4_CNV321( IFAIL )
*+
*  Name:
*     MV4_CNV321

*  Purpose:
*     Convert a binary file with a Specx map to version 4.1 format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_CNV321( IFAIL )

*  Description:
*     This routine provides the Specx command "convert-vax-map". It
*     will convert an existing file in format versions 1, 2, or 3 to
*     format version 4.1.
*
*     Versions 1, 2, and 3 are VAX binary files, unformatted
*     direct-access files with a record length of 64 4-byte words (256
*     bytes) to be more precise.
*
*     Version 4.1 is a portable format, implemented as an NDF with two
*     extension structures in a Starlink Data File.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global Specx status.

*  Notes:
*     The conversion of VAX binary formats is coded portably and in
*     Fortran. This is rather inefficient. Therefore this routine should
*     be used only for one-off conversions, not for every-day use.
*
*     This routine assumes that the last '/' in a file name
*     separates the name from the path. It is thus appropriate only for
*     Unix systems.
*
*     This routine can also read maps in format version 4.0, provided
*     they were written on the same platform as they are read. Map
*     format 4.0 is a non-portable binary format. It was written only by
*     the beta test version of Specx v.6.4. The first Unix release of
*     Specx was version 6.4-1, which introduced the portable map format
*     version 4.1.

*  Prior Requirements:
*     The HDS system must have been started.
*     The NDF system must have been begun.

*  Side Effects:
*     In order to convert properly between machine-dependent binary
*     formats and between different Specx file format versions, this
*     routine may have to be compiled without optimisation.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15 Aug 1994 (hme):
*        Original version.
*     31 Aug 1994 (hme):
*        NDF/HDS-based sparse cube.
*     01 Sep 1994 (hme):
*        Small bug about using RVAXV when it was garbage.
*        This routine must refuse if a map file is open.
*     02 Sep 1994 (hme):
*        Two references to xBUFER(I) had to be (1). This bug was copied
*        from fv4. IMHEAD was not equivalenced to BMHEAD. Owner and ID
*        were copied two bytes too early when converting 3.0 to 4.0.
*        That would also mess up ID1. For map header RA/DEC conversion
*        2.0 to 3.0 DMS_TO_RAD was erroneously assumed to return a
*        DOUBLE. It does return a REAL. (Which means that the fourth
*        integer for centi-arcsec is barely significant in the
*        intermediate storage.)
*     12 Oct 1994 (hme):
*        If the output file existed, the call to close it did not have
*        the STATUS argument (DAT_ANNUL).
*        Close output file only if it was opened properly.
*     16 Aug 2004 (timj):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DAT_ERR'          ! Standard DAT error codes
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL

*  Global Variables:
      INCLUDE 'MAPV4'            ! Specx NDF-based map variables
      INCLUDE 'FLAGCOMM'         ! Specx flags
      INCLUDE 'STACKCOMM'        ! Specx stack
      INCLUDE 'MAPHD'            ! Specx map header

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Constants:
      DOUBLE PRECISION RAD2DG    ! Degrees per radian
      PARAMETER ( RAD2DG = 57.29577951D0 )

*  Local Variables:
      LOGICAL OUTOPN             ! True if output file open
      INTEGER STATUS             ! Starlink status
      INTEGER I, J               ! Temporary integers
      INTEGER JDEF               ! Returend by gen_getstr
      INTEGER UNIT               ! Fortran unit number for input file
      INTEGER CUBPTR             ! Pointer to cube buffer
      REAL OLD_VERSION           ! Format version of input
      REAL TREAL                 ! Temporary REAL
      CHARACTER * ( 80 ) FILE1   ! Input file name
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary HDS locator

      BYTE BMHEAD( 128 )         ! Input map header
      INTEGER IMHEAD( 32 )
      INTEGER MSIZ, NSIZ
      REAL CELLX, CELLY
      EQUIVALENCE( IMHEAD(1), BMHEAD(1) )
      EQUIVALENCE( CELLX, MSIZ, BMHEAD(69) )
      EQUIVALENCE( CELLY, NSIZ, BMHEAD(73) )

      BYTE BMAPHD( 128 )         ! Output map header
      EQUIVALENCE( RAM, BMAPHD(1) )

      BYTE BVAXV( 4 )            ! Input version if 1,2,3
      REAL RVAXV
      EQUIVALENCE( RVAXV, BVAXV(1) )

      BYTE BUNXV( 4 )            ! Input version if 4.0
      REAL RUNXV
      EQUIVALENCE( RUNXV, BUNXV(1) )

      BYTE        BSHEAD( 512 )  ! Input prototype
      INTEGER*2   WSHEAD( 256 )
      INTEGER     ISHEAD( 128 )
      CHARACTER * ( 512 ) CSHEAD
      EQUIVALENCE ( BSHEAD(1), WSHEAD(1), ISHEAD(1), CSHEAD )

      DOUBLE PRECISION DBUFER    ! 8-byte buffer
      REAL             RBUFER( 2 )
      INTEGER          IBUFER( 2 )
      EQUIVALENCE ( IBUFER(1), RBUFER(1), DBUFER )

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN
      OUTOPN = .FALSE.

*  Check no map open.
      IF ( MAP_OPEN ) THEN
         IFAIL = 62
         WRITE( *, * )
     :      'Cannot convert VAX map while regular map is open.'
         WRITE( *, * ) 'Use the command CLOSE-MAP first.'
         RETURN
      END IF

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK


*  Get input file.
*  ===============

*  Get a file name. The user gives the input file without extension. The
*  old extension is ".map", the new one is "_map.sdf". But the ".sdf"
*  need not be handled, since the HDS routines take care of it.
      FILE1 = ' '
      CALL GEN_GETSTR( 'File name?', FILE1, ' ', FILE1, JDEF )
      NAMEMP = FILE1
      J = CHR_LEN(NAMEMP)
      FILE1(J+1:)  = '.map'
      NAMEMP(J+1:) = '_map'

*  Get a free unit and open the file.
      CALL FIO_GUNIT( UNIT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         WRITE( *, * ) 'No unit free to open file.'
         IFAIL = 10
         GO TO 500
      END IF
      CALL FV4_OPF321( UNIT, FILE1, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         WRITE( *, * ) 'Failed to open input file, IOSTAT =', IFAIL
         WRITE( *, * ) 'File name was ', FILE1(:CHR_LEN(FILE1))
         IFAIL = 10
         GO TO 500
      END IF


*  Convert map header.
*  ===================

*  Read the input file header. Don't convert yet to native or version 3.
*  (RFILE is weird in that it counts records from zero and uses two-byte
*  words as unit. This reads the first 128 byte of the first record.)
      CALL RFILE( UNIT, 0, BMHEAD, 0, 64 )

*  Work out the map format version. In formats 1, 2 and 3 the map
*  version is a REAL*4 in bytes 5 to 8 and in VAX binary format. In
*  format 4.0 it is a REAL*4 in bytes 29 to 32; if that is not 4.0 in
*  native format, then the file is unreadable.
      DO 1201 I = 1, 4
         BVAXV(I) = BMHEAD(4+I)
         BUNXV(I) = BMHEAD(28+I)
 1201 CONTINUE
      CALL FV4_NATIVR( RVAXV )
      IF ( RVAXV .GT. 0. .AND. RVAXV .LT. 4.0 .AND.
     :                         RUNXV .NE. 4.0 ) THEN
         MAP_VERSION = RVAXV
      ELSE IF ( ( RVAXV .LE. 0. .OR. RVAXV .GE. 4.0 )
     :                         .AND. RUNXV .EQ. 4.0 ) THEN
         MAP_VERSION = RUNXV
      ELSE
         WRITE( *, * ) 'File format version does not make sense.'
         IFAIL = 38
         GO TO 400
      END IF

*  Save knowledge of input format for later use.
      OLD_VERSION = MAP_VERSION

*  Convert header to native. But leave it at the old version.
      IF ( MAP_VERSION .LT. 2.0 ) THEN
         CALL FV4_NATIVI( BMHEAD(1) )
         CALL FV4_NATIVR( BMHEAD(5) )
         CALL FV4_NATIVI( BMHEAD(9) )
         CALL FV4_NATIVR( BMHEAD(65) )
         CALL FV4_NATIVI( BMHEAD(69) )
         CALL FV4_NATIVI( BMHEAD(73) )
         DO 1205 I = 77, 125, 4
            CALL FV4_NATIVI( BMHEAD(I) )
 1205    CONTINUE
      ELSE IF ( MAP_VERSION .LT. 3.0 ) THEN
         CALL FV4_NATIVI( BMHEAD(1) )
         CALL FV4_NATIVR( BMHEAD(5) )
         CALL FV4_NATIVI( BMHEAD(9) )
         CALL FV4_NATIVR( BMHEAD(65) )
         CALL FV4_NATIVR( BMHEAD(69) )
         CALL FV4_NATIVR( BMHEAD(73) )
         DO 1206 I = 77, 125, 4
            CALL FV4_NATIVI( BMHEAD(I) )
 1206    CONTINUE
      ELSE IF ( MAP_VERSION .LT. 4.0 ) THEN
         CALL FV4_NATIVI( BMHEAD(1) )
         CALL FV4_NATIVR( BMHEAD(5) )
         CALL FV4_NATIVI( BMHEAD(9) )
         CALL FV4_NATIVR( BMHEAD(65) )
         CALL FV4_NATIVR( BMHEAD(69) )
         CALL FV4_NATIVR( BMHEAD(73) )
         DO 1207 I = 77, 89, 4
            CALL FV4_NATIVI( BMHEAD(I) )
 1207    CONTINUE
         CALL FV4_NATIVD( BMHEAD(93) )
         CALL FV4_NATIVD( BMHEAD(101) )
         DO 1208 I = 109, 125, 4
            CALL FV4_NATIVI( BMHEAD(I) )
 1208    CONTINUE
      END IF

*  Convert version 1 to version 2.
*  Cell sizes changed type from integer to real.
      IF ( MAP_VERSION .LT. 2.0 ) THEN
         CELLX = MSIZ
         CELLY = NSIZ
         MAP_VERSION = 2.0
      END IF

*  Convert version 2 to version 3.
*  Representation of RA and DEC changed from quadruplet of integers to
*  one double precision number.
      IF ( MAP_VERSION .LT. 3.0 ) THEN
         CALL DMS_TO_RAD( IMHEAD(24), TREAL )
         DBUFER = TREAL * RAD2DG * 15D0
         IMHEAD(24) = IBUFER(1)
         IMHEAD(25) = IBUFER(2)
         CALL DMS_TO_RAD( IMHEAD(28), TREAL )
         DBUFER = TREAL * RAD2DG
         IMHEAD(26) = IBUFER(1)
         IMHEAD(27) = IBUFER(2)
         IMHEAD(28) = 0
         IMHEAD(29) = 0
         IMHEAD(30) = 0
         IMHEAD(31) = 0
         MAP_VERSION = 3.0
      END IF

*  Convert version 3 to version 4.0.
*  This is merely a re-sorting of the variables.
*  It is now that the input header is copied to the output header.
      IF ( MAP_VERSION .LT. 4.0 ) THEN
         DO 1202 I = 1, 16
            BMAPHD(I)    = BMHEAD(92+I)       ! RAM, DECM
            BMAPHD(32+I) = 0                  ! MDUMMY
 1202    CONTINUE
         DO 1203 I = 1, 4
            BMAPHD(16+I) = BMHEAD(68+I)       ! CELL_XSIZE
            BMAPHD(20+I) = BMHEAD(72+I)       ! CELL_YSIZE
            BMAPHD(24+I) = BMHEAD(64+I)       ! POSANG
            BMAPHD(28+I) = BMHEAD( 4+I)       ! MAP_VERSION
            BMAPHD(48+I) = BMHEAD(76+I)       ! MSTEP
            BMAPHD(52+I) = BMHEAD(80+I)       ! NSTEP
            BMAPHD(56+I) = BMHEAD(124+I)      ! NPTS1
            BMAPHD(60+I) = BMHEAD(I)          ! NSPEC
            BMAPHD(64+I) = BMHEAD(84+I)       ! NREDT
            BMAPHD(68+I) = BMHEAD(88+I)       ! IHEAD
            BMAPHD(72+I) = BMHEAD( 8+I)       ! ID1
 1203    CONTINUE
         DO 1204 I = 1, 52
            BMAPHD(76+I) = BMHEAD(12+I)       ! MAP_OWNER_NAME, MAP_ID
 1204    CONTINUE
         MAP_VERSION = 4.0
      ELSE
         DO 1209 I = 1, 128
            BMAPHD(I) = BMHEAD(I)
 1209    CONTINUE
      END IF

*  Convert version 4.0 to version 4.2.
*  Internally these are identical.
      IF ( MAP_VERSION .LT. 4.1 ) THEN
         MAP_VERSION = 4.2
      END IF


*  Open output file, write header.
*  ===============================

*  Before we seriously try to create the new map file we try to open it
*  for read-only. This should fail.
      CALL HDS_OPEN( NAMEMP, 'READ', TLOC, STATUS )
      IF ( STATUS .NE. DAT__FILNF ) THEN
         CALL DAT_ANNUL( TLOC, STATUS )
         WRITE( *, * ) 'Output file exists, no conversion!'
         IFAIL = 10
         GO TO 400
      END IF
      CALL ERR_ANNUL( STATUS )

*  The header will be written to the new file as soon as it is created.
*  Furthermore this will create, map and initialise the output index and
*  cube.
      CALL MV4_MAPNEW( IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         WRITE( *, * ) 'Error opening the new map file.'
         GO TO 400
      END IF
      OUTOPN = .TRUE.


*  Convert prototype.
*  ==================

*  The VAX 6.3 version of the routine MAPOPEN does not seem to
*  distinguish between map formats 1 and 2. Even though spectral headers
*  are quite different between the two. Perhaps version 1 had IHEAD
*  always zero and never had a prototype.

      IF ( IHEAD .EQ. 1 ) THEN

*     Read the prototype.
         DO 1301 I = 1, 2
            J = ( I - 1 ) * 64 + 1
            CALL RFILE( UNIT, 0, ISHEAD(J), I, 128 )
 1301   CONTINUE

*     If version 2 or older.
         IF ( OLD_VERSION .LT. 3.0 ) THEN

*        Copy header and convert VAX bit patterns to native.
*        This code stolen from fv4.
            DO 1104 I = 1, 8
               IBUFER(1) = ISHEAD(I)
               CALL FV4_NATIVR(RBUFER(1))
               TSYS(I) = RBUFER(1)
               LOFREQ(I) = 0D0
               IFFREQ(I) = 0D0
 1104       CONTINUE
            IBUFER(1) = ISHEAD(41)
            CALL FV4_NATIVR(RBUFER(1))
            VLSR = RBUFER(1)
            VSL = 0.
            VES = 0.
            VTE = 0.
            AZ  = 0.
            IBUFER(1) = ISHEAD(42)
            CALL FV4_NATIVR(RBUFER(1))
            EL = 90. - RBUFER(1)
            DO 1105 I = 1, 8
               JFREST(I) = ISHEAD(42+I)
               CALL FV4_NATIVI(JFREST(I))
               JFCEN(I) = ISHEAD(50+I)
               CALL FV4_NATIVI(JFCEN(I))
               JFINC(I) = ISHEAD(58+I)
               CALL FV4_NATIVI(JFINC(I))
 1105       CONTINUE
            INTT = ISHEAD(67)
            CALL FV4_NATIVI(INTT)
            DO 1106 I = 1, 8
               ITREC(I) = ISHEAD(67+I)
               CALL FV4_NATIVI(ITREC(I))
               ITSKY(I) = ISHEAD(75+I)
               CALL FV4_NATIVI(ITSKY(I))
               ITTEL(I) = ISHEAD(83+I)
               CALL FV4_NATIVI(ITTEL(I))
               NPTS(I) = ISHEAD(91+I)
               CALL FV4_NATIVI(NPTS(I))
 1106       CONTINUE
            DO 1107 I = 100, 107
               CALL FV4_NATIVI( ISHEAD(I) )
 1107       CONTINUE
            CALL DMS_TO_RAD( ISHEAD(100), TREAL )
            RA = 15D0 * RAD2DG * TREAL
            CALL DMS_TO_RAD( ISHEAD(104), TREAL )
            DEC = RAD2DG * TREAL
            LSCAN = ISHEAD(108)
            CALL FV4_NATIVI(LSCAN)
            IMODE = ISHEAD(109)
            CALL FV4_NATIVI(IMODE)
            NQUAD = ISHEAD(110)
            CALL FV4_NATIVI(NQUAD)
            IST = ISHEAD(111)
            CALL FV4_NATIVI(IST)
            IEND = ISHEAD(112)
            CALL FV4_NATIVI(IEND)
            ICALZD = ISHEAD(113)
            CALL FV4_NATIVI(ICALZD)
            LSRFLG = ISHEAD(114)
            CALL FV4_NATIVI(LSRFLG)
            IQCEN = ISHEAD(115)
            CALL FV4_NATIVI(IQCEN)
            IBUFER(1) = ISHEAD(116)
            CALL FV4_NATIVI(IBUFER(1))
            DRA = IBUFER(1)
            IBUFER(1) = ISHEAD(117)
            CALL FV4_NATIVI(IBUFER(1))
            DDEC = IBUFER(1)
            ITITLE = CSHEAD(469:494)
            IDATE  = CSHEAD(495:503)
            IUTFLG = BSHEAD(504)
            ITIME  = CSHEAD(505:512)

*     Else if version 3.
         ELSE IF ( OLD_VERSION .LT. 4.0 ) THEN

*        Copy header and convert VAX bit patterns to native.
*        This code stolen from fv4.
            DO 1101 I = 1, 8
               IBUFER(1) = ISHEAD(I)
               CALL FV4_NATIVR(RBUFER(1))
               TSYS(I) = RBUFER(1)
               IBUFER(1) = ISHEAD(7+2*I)
               IBUFER(2) = ISHEAD(8+2*I)
               CALL FV4_NATIVD(DBUFER)
               LOFREQ(I) = DBUFER
               IBUFER(1) = ISHEAD(23+2*I)
               IBUFER(2) = ISHEAD(24+2*I)
               CALL FV4_NATIVD(DBUFER)
               IFFREQ(I) = DBUFER
 1101       CONTINUE
            IBUFER(1) = ISHEAD(41)
            CALL FV4_NATIVR(RBUFER(1))
            AZ = RBUFER(1)
            IBUFER(1) = ISHEAD(42)
            CALL FV4_NATIVR(RBUFER(1))
            EL = RBUFER(1)
            DO 1102 I = 1, 8
               JFREST(I) = ISHEAD(42+I)
               CALL FV4_NATIVI(JFREST(I))
               JFCEN(I) = ISHEAD(50+I)
               CALL FV4_NATIVI(JFCEN(I))
               JFINC(I) = ISHEAD(58+I)
               CALL FV4_NATIVI(JFINC(I))
 1102       CONTINUE
            INTT = ISHEAD(67)
            CALL FV4_NATIVI(INTT)
            DO 1103 I = 1, 8
               ITREC(I) = ISHEAD(67+I)
               CALL FV4_NATIVI(ITREC(I))
               ITSKY(I) = ISHEAD(75+I)
               CALL FV4_NATIVI(ITSKY(I))
               ITTEL(I) = ISHEAD(83+I)
               CALL FV4_NATIVI(ITTEL(I))
               NPTS(I) = ISHEAD(91+I)
               CALL FV4_NATIVI(NPTS(I))
 1103       CONTINUE
            IBUFER(1) = ISHEAD(100)
            IBUFER(2) = ISHEAD(101)
            CALL FV4_NATIVD(DBUFER)
            RA = DBUFER
            IBUFER(1) = ISHEAD(102)
            IBUFER(2) = ISHEAD(103)
            CALL FV4_NATIVD(DBUFER)
            DEC = DBUFER
            IBUFER(1) = ISHEAD(104)
            CALL FV4_NATIVR(RBUFER(1))
            VSL = RBUFER(1)
            IBUFER(1) = ISHEAD(105)
            CALL FV4_NATIVR(RBUFER(1))
            VES = RBUFER(1)
            IBUFER(1) = ISHEAD(106)
            CALL FV4_NATIVR(RBUFER(1))
            VTE = RBUFER(1)
            IBUFER(1) = ISHEAD(107)
            CALL FV4_NATIVR(RBUFER(1))
            VLSR = RBUFER(1)
            LSCAN = ISHEAD(108)
            CALL FV4_NATIVI(LSCAN)
            IMODE = ISHEAD(109)
            CALL FV4_NATIVI(IMODE)
            NQUAD = ISHEAD(110)
            CALL FV4_NATIVI(NQUAD)
            IST = ISHEAD(111)
            CALL FV4_NATIVI(IST)
            IEND = ISHEAD(112)
            CALL FV4_NATIVI(IEND)
            ICALZD = ISHEAD(113)
            CALL FV4_NATIVI(ICALZD)
            LSRFLG = ISHEAD(114)
            CALL FV4_NATIVI(LSRFLG)
            IQCEN = ISHEAD(115)
            CALL FV4_NATIVI(IQCEN)
            IBUFER(1) = ISHEAD(116)
            CALL FV4_NATIVR(RBUFER(1))
            DRA = RBUFER(1)
            IBUFER(1) = ISHEAD(117)
            CALL FV4_NATIVR(RBUFER(1))
            DDEC = RBUFER(1)
            ITITLE = CSHEAD(469:494)
            IDATE  = CSHEAD(495:503)
            IUTFLG = BSHEAD(504)
            ITIME  = CSHEAD(505:512)

*     Else (version 4.0).
         ELSE

*        Just a binary copy.
            DO 1302 I = 1, 128
               SCAN_HEADER(I) = ISHEAD(I)
 1302       CONTINUE

         END IF

*     The prototype is now in /STACKCOMM/ (SCAN_HEADER()). Write it to the
*     output file. This will also read it back into the /PROTOTYPE/
*     common block, but that does not concern us.
         CALL MV4_PROTWR( )

      END IF


*  Convert index.
*  ==============

*  Read index into output file. Includes conversion to native for format
*  before 4.0. Also includes conversion of -1000 to VAL__BADI.
      CALL MV4_CNVIDX( OLD_VERSION, UNIT, %VAL(CNF_PVAL(IDXPTR)) )


*  Convert data.
*  =============

*  Get a workspace to hold one spectrum as read from input.
*  For consistency should really be using IGETVM here
*  Note that if we switch to IGETVM we will not be able to assume
*  that CNF_PVAL is the correct thing to use here.
      CUBPTR = 0
      CALL PSX_MALLOC( 4*NPTS1*MSTEP*NSTEP, CUBPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         WRITE( *, * ) 'Failed to get work space for cube.'
         IFAIL = 51
         GO TO 400
      END IF

*  Read each input spectrum and write it to output. Includes conversion
*  to native for format before 4.0. Also includes conversion of
*  BADPIX_VAL to VAL__BADR.
      CALL MV4_CNVCUB( OLD_VERSION, UNIT, %VAL(CNF_PVAL(IDXPTR)),
     :     %VAL(CNF_PVAL(CUBPTR)) )


*  Tidy up.
*  ========

*  Close the input file.
 400  CONTINUE
      CLOSE( UNIT )

*  Starlink tidying.
 500  CONTINUE
      CALL PSX_FREE( CUBPTR, STATUS )
      IF ( OUTOPN ) CALL MV4_MAPCLS( IFAIL )
      CALL FIO_PUNIT( UNIT, STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END



      SUBROUTINE MV4_CNVIDX( OLD_VERSION, UNIT, INDEX )

*  Read index. Includes conversion to native for format
*  before 4.0. Also includes conversion of -1000 to VAL__BADI.

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MAPHD'

      REAL OLD_VERSION
      INTEGER UNIT
      INTEGER INDEX( MSTEP * NSTEP )

      INTEGER I

*  Read all but last index record from file. These records can be read
*  in full (128 INTEGER*2 words). These are 64*(NREDT-1) index
*  elements.
      DO 1 I = 1, NREDT - 1
         CALL RFILE( UNIT, 0, INDEX(1+64*(I-1)), I+2, 128 )
 1    CONTINUE

*  Read the remaining MSTEP*NSTEP-64*(NREDT-1) elements from last index
*  record. These are 2*(MSTEP*NSTEP-64*(NREDT-1)) INTEGER*2 words.
      I = MIN(128,2*(MSTEP*NSTEP-64*(NREDT-1)))
      CALL RFILE( UNIT, 0, INDEX(1+64*(NREDT-1)), NREDT+2, I )

*  If pre-4.0 format, convert from VAX binary to native.
      IF ( OLD_VERSION .LT. 4.0 ) THEN
         DO 2 I = 1, MSTEP * NSTEP
            CALL FV4_NATIVI( INDEX(I) )
 2       CONTINUE
      END IF

*  Any absence signals are converted to VAL__BADI.
      DO 3 I = 1, MSTEP * NSTEP
         IF ( INDEX(I) .EQ. 0 .OR. INDEX(I) .EQ. -1000 )
     :        INDEX(I) = VAL__BADI
 3    CONTINUE

      END



      SUBROUTINE MV4_CNVCUB( OLD_VERSION, UNIT, INDEX, CUBE )

*  Convert each input spectrum. Includes conversion to native for
*  format before 4.0. Also includes conversion of BADPIX_VAL to
*  VAL__BADR.

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPHD'

      REAL OLD_VERSION
      INTEGER UNIT
      INTEGER INDEX( MSTEP, NSTEP )
      REAL CUBE( NPTS1, MSTEP, NSTEP )

      INTEGER I, J, K, L
      INTEGER IST, IBST

*

*  Check each cube row. INDEX tells which spectrum to get, if any.
      DO 6 J = 1, NSTEP
         DO 5 I = 1, MSTEP

*        If spectrum (I,J) does exist.
            IF ( INDEX(I,J) .NE. VAL__BADI ) THEN

*           The index gives the position in the input file.
*           Work out the file blocks for RFILE.
               IST  = ( NPTS1 - 1 ) / 64 + 1
               IBST = 3 + NREDT + ( INDEX(I,J) - 1 ) * IST

*           Read the relevant file blocks. Make sure that only the
*           relevant part of the last relevant block is read. Otherwise
*           we would read garbage into part of the next row in the cube.
               DO 2 K = 1, IST
                  L = MIN( 128, 2*(NPTS1-(K-1)*64) )
                  CALL RFILE( UNIT,0, CUBE(1+(K-1)*64,I,J), IBST+K-1,L )
 2             CONTINUE

*           If pre-4.0 format, convert to native.
               IF ( OLD_VERSION .LT. 4.0 ) THEN
                  DO 3 K = 1, NPTS1
                     CALL FV4_NATIVR( CUBE(K,I,J) )
 3                CONTINUE
               END IF

*           Write the spectrum. Includes conversion between BADPIX_VAL
*           and VAL__BADR.
*           Note: This routine uses the file copy of the index, where an
*           HDS bad value denotes absent spectra. MV4_SPECWR assumes it
*           is given the memory copy of the index, where -1000 is used.
*           That does not matter here, since the call is made only if
*           INDEX(I,J) is neither and the spectrum exists.
               CALL MV4_SPECWR( I, J, INDEX, CUBE )

            END IF

 5       CONTINUE
 6    CONTINUE

      END
