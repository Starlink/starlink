      SUBROUTINE FV4_CNV321( IFAIL )
*+
*  Name:
*     FV4_CNV321

*  Purpose:
*     Convert a VAX binary file with Specx spectra to version 4 format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_CNV321( IFAIL )

*  Description:
*     This routine provides the Specx command "convert-vax-file". It
*     will convert an existing file in format versions 1, 2, or 3 to
*     format version 4. The whole file is converted, including deleted
*     scans in uncompressed files. (Specx deletes a spectrum by just
*     flagging it, it compresses a file by removing the flagged
*     spectra.)
*
*     Versions 1, 2, and 3 are VAX binary files, unformatted
*     direct-access files with a record length of 64 4-byte words (256
*     bytes) to be more precise. They were written from a BYTE array in
*     the VAX memory.
*
*     Version 4 is a portable format, implemented as an array of NDF
*     structures in a Starlink Data File.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global Specx status.

*  Notes:
*     The conversion of VAX binary formats is coded portably and in
*     Fortran. This is rather inefficient. Therefore this routine should
*     be used only for one-off conversions, not for every-day use.
*
*     This routine makes assumes that the last '/' in a file name
*     separates the name from the path. It is thus appropriate only for
*     Unix systems.

*  Prior Requirements:
*     The HDS system must have been started.
*     The NDF system must have been begun.

*  Side Effects:
*     In order to convert properly between machine-dependent binary
*     formats and between different Specx file format versions, this
*     routine may have to be compiled without optimisation.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02 Dec 1993 (hme):
*        Original version.
*     06 Dec 1993 (hme):
*        Write new file header and create first template in separate
*        routine.
*     10 Dec 1993 (hme):
*        The EQUIVALENCE for V1HEAD was wrong.
*        The DECstation does not like REC=(A*B), so have taken away the
*        redundanted parantheses.
*     17 Dec 1993 (hme):
*        Changed to INCLUDE 'STACKCOMM3', i.e. the old include file with
*        the alignment problems. Hopefully these problems are fixed at
*        run time by whatever machine. Tests on DECstation indicate that
*        it will work.
*     20 Dec 1993 (hme):
*        Cannot use STACKCOMM3 since NTOT, PUSH, POP, and FV4_SPEC2C use
*        the latest STACKCOMM. Now separate header (as read from file
*        into a byte array) from the header common block STACK.
*     02 Sep 1994 (hme):
*        RBUFER(I) was referenced twice (TSYS=..) when RBUFER(1) was
*        meant.
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

*  Global Variables:
      INCLUDE 'STACKCOMM'        ! Specx stack
      INCLUDE 'STAKPAR'          ! Specx stack control information
      BYTE BDATA( 1 )            ! To read data
      EQUIVALENCE ( DATA(1), BDATA(1) )

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Constants:
      DOUBLE PRECISION RAD2DG    ! Degrees per radian
      PARAMETER ( RAD2DG = 57.29577951D0 )

*  Local Variables:
      INTEGER I, J, K            ! Temporary integers
      INTEGER ISCAN              ! Loop variable
      INTEGER UNIT               ! Fortran unit number for input file
      INTEGER STATUS             ! Starlink status
      INTEGER JDEF               ! Returend by gen_getstr
      INTEGER IPOS(4)            ! Buffer for I2(4) -> R8 position
      REAL VFILE                 ! Input file version
      REAL TREAL                 ! Temporary REAL
      CHARACTER * ( 80 ) FILE1   ! Input file name
      CHARACTER * ( 80 ) FILE2   ! Output file name
      CHARACTER * ( DAT__SZLOC ) TOPLOC ! Output file top level
      CHARACTER * ( DAT__SZLOC ) SPXLOC ! Locator to array of NDFs
      CHARACTER * ( DAT__SZLOC ) TLOC( 3 ) ! Temporary locators

      BYTE BFHEAD( 256 )         ! Input file header
      INTEGER            NSCAN   ! Dto. as version 2 or 3 header
      INTEGER            NREC
      INTEGER            NSMAX
      CHARACTER * ( 12 ) NAME
      CHARACTER * ( 40 ) ID
      INTEGER            IREC1
      CHARACTER * (  8 ) VERSION
      EQUIVALENCE ( BFHEAD( 1), NSCAN   )
      EQUIVALENCE ( BFHEAD( 5), NREC    )
      EQUIVALENCE ( BFHEAD( 9), NSMAX   )
      EQUIVALENCE ( BFHEAD(13), NAME    )
      EQUIVALENCE ( BFHEAD(25), ID      )
      EQUIVALENCE ( BFHEAD(65), IREC1   )
      EQUIVALENCE ( BFHEAD(69), VERSION )
      INTEGER*2          NSCAN2X ! Dto. as version 1 header
      INTEGER*2          NREC2X
      INTEGER*2          NSMAX2X
      CHARACTER * ( 52 ) CHDATA
      INTEGER*2          IREC12X
      EQUIVALENCE ( BFHEAD( 1), NSCAN2X )
      EQUIVALENCE ( BFHEAD( 3), NREC2X  )
      EQUIVALENCE ( BFHEAD( 5), NSMAX2X )
      EQUIVALENCE ( BFHEAD( 7), CHDATA  )
      EQUIVALENCE ( BFHEAD(59), IREC12X )

      BYTE        BSHEAD( 512 )  ! Input scan header
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
      INTEGER NTOT               ! Sum(NPTS) 1st to nominated quadrant

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Get a file name. The user gives the input file with extension. For
*  output the extension must be stripped off so that HDS_NEW replaces it
*  with ".sdf".
      FILE1 = ' '
      CALL GEN_GETSTR( 'File name?', FILE1, ' ', FILE1, JDEF )
      FILE2 = FILE1
      J = CHR_LEN(FILE2)
      DO 1001 I = J, 1, -1
         IF ( FILE2(I:I) .EQ. '.' ) GO TO 1002
         IF ( FILE2(I:I) .EQ. '/' ) GO TO 1100
 1001 CONTINUE
 1002 CONTINUE
      IF ( I .NE. 0 ) FILE2(I:) = ' '
 1100 CONTINUE

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
         IFAIL = 10
         GO TO 500
      END IF

*  Read the input file header. Don't convert yet to native or version 3.
*  But extract the file format version.
      READ( UNIT, REC=1, IOSTAT=IFAIL ) ( BFHEAD(I), I = 1, 256 )
      IF ( IFAIL .NE. 0 ) THEN
         IFAIL = 38
         GO TO 400
      END IF
      READ( VERSION, '(1X,F3.1)' ) VFILE

*  Convert the file header to native. This means possibly reversing the
*  byte order in integers, and certainly the decoding of floating
*  variables. (There are no floating variables in the file header.)
*  Also convert version 1 file header to version 3. For this the order
*  of assignments is important, since we copy within the BFHEAD array.
      IF ( VFILE .LT. 1.99 ) THEN
         CALL FV4_NATIVW( NSCAN2X )
         CALL FV4_NATIVW( NREC2X  )
         CALL FV4_NATIVW( NSMAX2X )
         CALL FV4_NATIVW( IREC12X )
         IREC1 = IREC12X
         ID    = CHDATA(13:)
         NAME  = CHDATA(:12)
         NSMAX = NSMAX2X
         NREC  = NREC2X
         NSCAN = NSCAN2X
      ELSE
         CALL FV4_NATIVI( NSCAN )
         CALL FV4_NATIVI( NREC  )
         CALL FV4_NATIVI( NSMAX )
         CALL FV4_NATIVI( IREC1 )
      END IF

*  Open the output file. This is to be a new Starlink Data File. But we
*  first try to open the nominated file for read access.
      CALL HDS_OPEN( FILE2, 'READ', TOPLOC, STATUS )
      IF ( STATUS .NE. DAT__FILNF ) THEN
         WRITE( *, * ) 'Output file exists, no conversion!'
         IFAIL = 10
         GO TO 400
      END IF
      CALL ERR_ANNUL( STATUS )
      CALL HDS_NEW( FILE2, 'SPECXSP', 'SPECX_SPECTRA', 0, 0,
     :   TOPLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 10
         GO TO 400
      END IF

*  Write the file header, create the SPECTRUM array (length NSCAN+1) and
*  template spectrum.
      CALL FV4_FILINI( TOPLOC, NAME, ID, IREC1, NSCAN, SPXLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 10
         GO TO 400
      END IF

*  If stack is not empty, push stack.
      IF ( .NOT. XCLEAR ) CALL PUSH

*  Locate cell TLOC(1/2).
      J = 0
      CALL DAT_CELL( SPXLOC, 1, 1, TLOC(J+1), STATUS )

*  For each input spectrum.
*  I is the scan number (ignoring IREC1).
*  J is 0 or 1, such that TLOC(J+1) locates the cell to be filled and
*  TLOC(1-J+1) = TLOC(J) locates the next cell.
*  In the beginning J=0, thus TLOC(1) locates the cell and TLOC(2)
*  locates the next cell. On each iteration the loctors are swapped,
*  i.e. J=1-J. So on the first iteration J becomes 1, TLOC(2) is
*  suddenly TLOC(J+1), so what was the next cell becomes the cell.
      DO 1003 ISCAN = 1, NSCAN

*     If version 3.
         IF ( VFILE .GT. 2.99 ) THEN

*        Read scan header.
            READ( UNIT, REC=2*ISCAN,   IOSTAT=IFAIL )
     :            ( BSHEAD(K), K =   1, 256 )
            IF ( IFAIL .NE. 0 ) THEN
               IFAIL = 38
               GO TO 300
            END IF
            READ( UNIT, REC=2*ISCAN+1, IOSTAT=IFAIL )
     :            ( BSHEAD(K), K = 257, 512 )
            IF ( IFAIL .NE. 0 ) THEN
               IFAIL = 38
               GO TO 300
            END IF

*        Copy header and convert VAX bit patterns to native.
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

*     Else if version 2.
         ELSE IF ( VFILE .GT. 1.99 ) THEN

*        Read scan header. This is just like version 3.
            READ( UNIT, REC=2*ISCAN,   IOSTAT=IFAIL )
     :            ( BSHEAD(K), K =   1, 256 )
            IF ( IFAIL .NE. 0 ) THEN
               IFAIL = 38
               GO TO 300
            END IF
            READ( UNIT, REC=2*ISCAN+1, IOSTAT=IFAIL )
     :            ( BSHEAD(K), K = 257, 512 )
            IF ( IFAIL .NE. 0 ) THEN
               IFAIL = 38
               GO TO 300
            END IF

*        Copy header and convert VAX bit patterns to native.
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

*     Else (version 1).
         ELSE

*        Read scan header.
            READ( UNIT, REC=2*ISCAN, IOSTAT=IFAIL )
     :            ( BSHEAD(K), K = 1, 256 )
            IF ( IFAIL .NE. 0 ) THEN
               IFAIL = 38
               GO TO 300
            END IF

*        Convert scan header to native.
            DO 1007 I = 1, 51, 2
               CALL FV4_NATIVR( WSHEAD(I) )
 1007       CONTINUE
            DO 1008 I = 53, 69, 2
               CALL FV4_NATIVI( WSHEAD(I) )
 1008       CONTINUE
            DO 1009 I = 71, 106
               CALL FV4_NATIVW( WSHEAD(I) )
 1009       CONTINUE

*        Copy scan header.

*        Tsys R4(4) -> R4(8).
            SCAN_HEADER(41) = ISHEAD(1)
            SCAN_HEADER(42) = ISHEAD(2)
            SCAN_HEADER(43) = ISHEAD(3)
            SCAN_HEADER(44) = ISHEAD(4)
            TSYS(5) = 0.
            TSYS(6) = 0.
            TSYS(7) = 0.
            TSYS(8) = 0.

*        VLSR, R4 -> R4.
*        Other velocities unknown.
            SCAN_HEADER(107) = ISHEAD(25)
            VSL = 0.
            VES = 0.
            VTE = 0.

*        Azimuth unknown.
*        Elevation R4 -> R4, but old number is zenith distance.
            AZ = 0.
            SCAN_HEADER(40) = ISHEAD(26)
            EL              = 90. - EL

*        Centre frequencies I4(4) -> I4(8).
*        Frequency increments I4(4) -> I4(8).
            JFCEN(1) = ISHEAD(27)
            JFCEN(2) = ISHEAD(28)
            JFCEN(3) = ISHEAD(29)
            JFCEN(4) = ISHEAD(30)
            JFCEN(5) = 0
            JFCEN(6) = 0
            JFCEN(7) = 0
            JFCEN(8) = 0
            JFINC(1) = ISHEAD(31)
            JFINC(2) = ISHEAD(32)
            JFINC(3) = ISHEAD(33)
            JFINC(4) = ISHEAD(34)
            JFINC(5) = 0
            JFINC(6) = 0
            JFINC(7) = 0
            JFINC(8) = 0

*        Integration time I4 -> I4.
            INTT = ISHEAD(35)

*        Trec, Tsky, each I2(4) -> I4(8).
            ITREC(1) = WSHEAD(71)
            ITREC(2) = WSHEAD(72)
            ITREC(3) = WSHEAD(73)
            ITREC(4) = WSHEAD(74)
            ITREC(5) = 0
            ITREC(6) = 0
            ITREC(7) = 0
            ITREC(8) = 0
            ITSKY(1) = WSHEAD(75)
            ITSKY(2) = WSHEAD(76)
            ITSKY(3) = WSHEAD(77)
            ITSKY(4) = WSHEAD(78)
            ITSKY(5) = 0
            ITSKY(6) = 0
            ITSKY(7) = 0
            ITSKY(8) = 0

*        Ttel, number of channels, each I2(4) -> I4(8).
            ITTEL(1) = WSHEAD(79)
            ITTEL(2) = WSHEAD(80)
            ITTEL(3) = WSHEAD(81)
            ITTEL(4) = WSHEAD(82)
            ITTEL(5) = 0
            ITTEL(6) = 0
            ITTEL(7) = 0
            ITTEL(8) = 0
            NPTS(1) = WSHEAD(83)
            NPTS(2) = WSHEAD(84)
            NPTS(3) = WSHEAD(85)
            NPTS(4) = WSHEAD(86)
            NPTS(5) = 0
            NPTS(6) = 0
            NPTS(7) = 0
            NPTS(8) = 0

*        RA I2(4) -> I4(4) -> R8, hms -> deg.
*        Dec I2(4) -> I4(4) -> R8, DMS -> deg.
            IPOS(1) = WSHEAD(87)
            IPOS(2) = WSHEAD(88)
            IPOS(3) = WSHEAD(89)
            IPOS(4) = WSHEAD(90)
            CALL DMS_TO_RAD( IPOS, TREAL )
            RA = 15D0 * RAD2DG * TREAL
            IPOS(1) = WSHEAD(91)
            IPOS(2) = WSHEAD(92)
            IPOS(3) = WSHEAD(93)
            IPOS(4) = WSHEAD(94)
            CALL DMS_TO_RAD( IPOS, TREAL )
            DEC = RAD2DG * TREAL

*        Diverse I2 -> I4.
            LSCAN  = WSHEAD(95)
            IMODE  = WSHEAD(96)
            NQUAD  = WSHEAD(97)
            IST    = WSHEAD(98)
            IEND   = WSHEAD(99)
            ICALZD = WSHEAD(100)
            LSRFLG = WSHEAD(101)
            IQCEN  = WSHEAD(102)

*        Offset I2 -> R4.
            DRA  = WSHEAD(105)
            DDEC = WSHEAD(106)

*        The CHARACTER/BYTE data.
            ITITLE = CSHEAD(213:238)
            IDATE  = CSHEAD(239:247)
            IUTFLG = BSHEAD(248)
            ITIME  = CSHEAD(249:256)

*        Fill in unknown per quadrant information.
            DO 1011 I = 1, 8
               LOFREQ(I) = 0D0
               IFFREQ(I) = 0D0
               JFREST(I) = 0
 1011       CONTINUE
         END IF

*     Check that data will fit into stack.
         IF ( NTOT(NQUAD) .GT. LSTK - 128 ) THEN
            IFAIL = 25
            GO TO 300
         END IF

*     Read scan data.
         DO 1012 I = IST, IEND
            READ( UNIT, REC=I+1, IOSTAT=IFAIL )
     :         ( BDATA(K), K = 256*(I-IST)+1, 256*(I-IST+1) )
            IF ( IFAIL .NE. 0 ) THEN
               IFAIL = 38
               GO TO 300
            END IF
 1012    CONTINUE

*     Convert scan data to native.
         DO 1013 I = 1, NTOT(NQUAD)
            CALL FV4_NATIVR( DATA(I) )
 1013    CONTINUE

*     Locate next cell. Copy template from this cell to next cell.
*     This cell is TLOC(J+1), the next cell is TLOC(2-J), either is
*     TLOC(1) or TLOC(2). TLOC(3) is used temporarily to locate a
*     component to be copied.

         CALL DAT_CELL( SPXLOC, 1, ISCAN+1, TLOC(2-J), STATUS )
         CALL DAT_FIND( TLOC(J+1), 'DATA_ARRAY', TLOC(3), STATUS )
         CALL DAT_COPY( TLOC(3), TLOC(2-J), 'DATA_ARRAY', STATUS )
         CALL DAT_ANNUL( TLOC(3), STATUS )
         CALL DAT_FIND( TLOC(J+1), 'MORE', TLOC(3), STATUS )
         CALL DAT_COPY( TLOC(3), TLOC(2-J), 'MORE', STATUS )
         CALL DAT_ANNUL( TLOC(3), STATUS )

*     Put spectrum from X register into nominated cell.
         CALL FV4_SPEC2C( TLOC(J+1), STATUS )

*     Annul cell, swap TLOC.
         CALL DAT_ANNUL( TLOC(J+1), STATUS )
         J = 1 - J

*     Check Starlink status.
*     The error code 4 is not quite correct, the message will be
*     "No room in file, scan not inserted". But there will be Starlink
*     messages as well.
         IF ( STATUS .NE. SAI__OK ) THEN
            IFAIL = 4
            GO TO 300
         END IF
 1003 CONTINUE

*  Tidy up.
*  --------

*  If stack was not empty, pop stack. (XCLEAR is still the same as used
*  for deciding whether to PUSH.)
 300  CONTINUE
      IF ( .NOT. XCLEAR ) CALL POP

*  Close the input file.
 400  CONTINUE
      CLOSE( UNIT )

*  Starlink tidying.
*  Annull locator to template,
*  Annull locator to SPECTRUM array,
*  Close Starlink Data File,
*  Release input unit (closed above),
*  Flush error reports, end context.
 500  CONTINUE
      CALL DAT_ANNUL( TLOC(J+1), STATUS )
      CALL DAT_ANNUL( SPXLOC, STATUS )
      CALL HDS_CLOSE( TOPLOC, STATUS )
      CALL FIO_PUNIT( UNIT, STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
