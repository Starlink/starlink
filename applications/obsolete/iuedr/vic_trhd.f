      SUBROUTINE VIC_TRHD( TP, MAXLN, NREC, RECSIZ, STATUS )
*+
*  Name:
*     SUBROUTINE VIC_TRHD

*  Purpose:
*     Read VICAR tape header and write to file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL VIC_TRHD( TP, MAXLN, NREC, RECSIZ, STATUS )

*  Arguments:
*     TP = INTEGER (Given)
*        Source ("tape") descriptor.
*     MAXLN = INTEGER (Given)
*        Maximum number of header lines to be read.
*     NREC = INTEGER (Returned)
*        Number of records in image data.
*     RECSIZ = INTEGER (Returned)
*        Size in bytes of each image record.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     A sequence of tape blocks are read.
*     The block is converted from EBCDIC to ASCII character code.
*     If the tape block is larger than 360 bytes then we have a
*     blocked VICAR-like tape; in which case the first 360 bytes
*     of each 768 contain header text.
*     Each 360 byte block contains 5 lines of 72 characters.
*     The header lines are cleaned to remove all non-printable ASCII
*     characters.
*     The first line in the first block contains the record size (bytes)
*     and the number of image lines coded as integers; these are
*     decoded and returned.
*     A line ending in "L" indicates that this is the last
*     header block.
*     Much of this information is empirically based (especially
*     that about blocked tapes).
*     The first MAXLN lines are written to output.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     07-SEP-81 (JRG):
*       AT4 version.
*     26-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     22-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     01-OCT-92 (DMILLS):
*       IUEDR Vn. 3.0
*       ADAMised version to run on multiple hardware platforms
*       Added device independence (MAGTAPE/DISK/NFS-DISK)
*     15-JUL-94 (MJC):
*       IUEDR Vn. 3.1-1
*       Added inherited status check, leap-year support.
*     10-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMPSX'
      INCLUDE 'CMISAF'

*  Local Constants:
      INTEGER HDSIZE        ! Header block size.
      INTEGER LNPERB        ! Number of lines per 360 byte block.
      INTEGER LNSIZE        ! Size of header line (excluding terminator).
      INTEGER MAXBUF        ! Maximum tape block size.
      INTEGER VBSIZE        ! Blocking size for blocked tapes.
      PARAMETER ( HDSIZE = 360, LNPERB = 5, LNSIZE = 72,
     :            MAXBUF = 16384, VBSIZE = 768 )

*  Local Character Constants:
      INTEGER BIGL          ! ASCII "L"
      INTEGER BLANK         ! ASCII " "
      PARAMETER ( BIGL = 76, BLANK = 32 )

*  Arguments Given:
      INTEGER TP            ! Tape descriptor.
      INTEGER MAXLN         ! Maximum number of lines that can be read.

*  Arguments Returned:
      INTEGER NREC          ! Number of image lines (records).
      INTEGER RECSIZ        ! Size of image record (bytes).

*  Status:
      INTEGER STATUS        ! Global status.

*  Local Variables:
      BYTE BUF( MAXBUF )    ! Buffer to receive tape data.
      CHARACTER*( MAXBUF ) CBUF
      CHARACTER*( MAXBUF ) EBUF
      EQUIVALENCE ( CBUF, BUF )

      BYTE LINE( LNSIZE+1 ) ! Terminated header line.
      CHARACTER*( LNSIZE+1 ) CLBUF
      EQUIVALENCE ( CLBUF, LINE )

      BYTE SWPBUF

      INTEGER I
      INTEGER II
      INTEGER ITYPE        ! Character loop index.
      INTEGER IHEAD        ! Header block (within tape block) index.
      INTEGER ILINE        ! Line index within header block.
      INTEGER IOSTAT       ! File read status.
      INTEGER K            ! Loop index.
      INTEGER K1           ! Character pointer within header block.
      INTEGER K2           ! Character pointer within header block.
      INTEGER NBLOCK       ! Block count.
      INTEGER NHEAD        ! Number of header blocks per tape block.
      INTEGER NLINE        ! Line index from start of header.
      INTEGER FDN
      INTEGER NREAD        ! Number of bytes read.

      INTEGER DAYS( 12 )   ! Days in each month.

*   Initialisations:
      DATA DAYS / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NLINE = 0
      NBLOCK = 0

      DO WHILE ( .TRUE. )
         NBLOCK = NBLOCK + 1
         CALL IUE_DEVICE( 'READ', TP, FDN, BUF, MAXBUF, NREAD, STATUS )

*      Swap byte order (back) under SunOS.
         IF ( SYSNAME .EQ. 'SunOS' ) THEN
            DO I = 1, 2 * ( NREAD / 2 ), 2
               SWPBUF = BUF( I )
               BUF( I ) = BUF( I + 1 )
               BUF( I + 1 ) = SWPBUF
            END DO
         END IF
         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( .NOT. ISAFILE ) THEN
               CALL ERROUT( 'Error reading VICAR header block\\',
     :                      STATUS )
            END IF
            GO TO 999

         ELSE IF ( NREAD .EQ. HDSIZE ) THEN
            NHEAD = 1

         ELSE IF ( MOD( NREAD, VBSIZE ) .EQ. 0 ) THEN
            NHEAD = NREAD / 768

         ELSE
            CALL ERRSTR( 'block \\' )
            CALL ERRINT( NBLOCK )
            CALL ERRSTR( ' has illegal block size : \\' )
            CALL ERRINT( NREAD )
            CALL ERROUT( ' bytes\\', STATUS )
            GO TO 999
         END IF

*      Convert from EBCDIC to ASCII.
         EBUF = CBUF
         CALL CHR_ETOM( EBUF, CBUF )

         DO IHEAD = 1, NHEAD
            K2 = ( IHEAD - 1 ) * 768

            DO ILINE = 1, LNPERB
               NLINE = NLINE + 1
               K1 = K2 + 1
               K2 = K2 + LNSIZE
               I = 0

               DO K = K1, K2
                  IF ( BUF( K ) .LT. BLANK ) THEN
                     BUF( K ) = BLANK
                  END IF
                  I = I + 1
                  LINE( I ) = BUF( K )
               END DO

*            Terminate the string.
               CALL str_TERM( LNSIZE, LNSIZE + 1, LINE )

*            Remove non-printable ASCII characters.
               CALL str_CLEAN( LINE )

*            Extract image details from first header line.
               IF ( NLINE .EQ. 1 ) THEN

*               Number of image lines.
                  READ( CLBUF( 33:36 ), '(I4)' ) NREC
                  IF ( STATUS .NE. SAI__OK ) THEN
                     NREC = 0
                     STATUS = SAI__OK
                  END IF

*               Unblocked record size in bytes.
                  READ( CLBUF( 37:40 ), '(I4)' ) RECSIZ
                  IF ( STATUS .NE. SAI__OK ) THEN
                     RECSIZ = 0
                     STATUS = SAI__OK
                  END IF

*               IMAGE Number.
                  READ( CLBUF( 52:56 ), '(I5)' ) ISASEQ

                  IF ( STATUS .NE. SAI__OK ) THEN
                     NREC = 0
                     STATUS = SAI__OK
                  END IF

*               Which camera.
                  READ ( CLBUF( 50:50 ), '(I1)' ) ITYPE
                  IF ( ITYPE .EQ. 1 ) THEN
                     ISACAMERA( 1 ) = ICHAR( 'L' )
                     ISACAMERA( 2 ) = ICHAR( 'W' )
                     ISACAMERA( 3 ) = ICHAR( 'P' )

                  ELSE IF ( ITYPE .EQ. 2 ) THEN
                     ISACAMERA( 1 ) = ICHAR( 'L' )
                     ISACAMERA( 2 ) = ICHAR( 'W' )
                     ISACAMERA( 3 ) = ICHAR( 'R' )

                  ELSE IF ( ITYPE .EQ. 3 ) THEN
                     ISACAMERA( 1 ) = ICHAR( 'S' )
                     ISACAMERA( 2 ) = ICHAR( 'W' )
                     ISACAMERA( 3 ) = ICHAR( 'P' )

                  ELSE IF ( ITYPE .EQ. 4 ) THEN
                     ISACAMERA( 1 ) = ICHAR( 'S' )
                     ISACAMERA( 2 ) = ICHAR( 'W' )
                     ISACAMERA( 3 ) = ICHAR( 'R' )
                  END IF

                  IF ( STATUS .NE. SAI__OK ) THEN
                     NREC = 0
                     STATUS = SAI__OK
                  END IF

*               IMAGE resolution.
                  READ ( CLBUF( 51:51 ), '(I1)' ) ITYPE
                  IF ( ITYPE .EQ. 0 ) THEN
                     ISARES( 1 ) = ICHAR( 'H' )
                     ISARES( 2 ) = ICHAR( 'I' )

                  ELSE IF ( ITYPE .EQ. 1 ) THEN
                     ISARES( 1 ) = ICHAR( 'L' )
                     ISARES( 2 ) = ICHAR( 'O' )
                  END IF
                  ISARES( 3 ) = ICHAR( 'R' )
                  ISARES( 4 ) = ICHAR( 'E' )
                  ISARES( 5 ) = ICHAR( 'S' )

                  IF ( STATUS .NE. SAI__OK ) THEN
                     NREC = 0
                     STATUS = SAI__OK
                  END IF

*            Extract image details from second header line.
               ELSE IF ( NLINE .EQ. 2 ) THEN
                  IF ( LINE( 32 ) .EQ. ICHAR(',') ) THEN
                     READ ( CLBUF( 30:31 ), '(I2)' ) ISATIME
                     READ ( CLBUF( 33:35 ), '(I3)' ) ITYPE
                     ISATIME = ISATIME * 1000 + ITYPE

                  ELSE
                     READ ( CLBUF( 32:35 ), '(I4)' ) ISATIME
                  END IF
                  IF ( STATUS .NE. SAI__OK ) THEN
                     NREC = 0
                     STATUS = SAI__OK
                  END IF

*            Extract image details from third header line.
               ELSE IF ( NLINE .EQ. 3 ) THEN
                  I = 1
                  DO WHILE ( I.LT.72 .AND. LINE( I ).NE.ICHAR(',') )
                     I = I + 1
                  END DO
                  I = I + 1
                  DO WHILE ( I.LT.72 .AND. LINE( I ).EQ.ICHAR(' ') )
                     I = I + 1
                  END DO
                  DO II = 1, 16
                     ISANAME( II ) = ICHAR(' ')
                  END DO
                  II = 1
                  DO WHILE ( I.LT.72 .AND. LINE( I ).NE.ICHAR(',') )
                     ISANAME( II ) = LINE( I )
                     II = II + 1
                     I = I + 1
                  END DO

*            Extract image details from tenth header line.
               ELSE IF ( NLINE .EQ. 10 ) THEN

*               Year.
                  IOSTAT = SAI__OK
                  READ ( CLBUF( 1 : 2 ), '( I2 )', IOSTAT = IOSTAT,
     :                   ERR = 70 ) ISAYEAR
                  GO TO 71
  70              ISAYEAR = 0
  71              CONTINUE

*               Give extra day to Feb in leap year.
                  IF ( ( ( ISAYEAR + 3 ) / 4 ) .EQ. (ISAYEAR / 4) ) THEN
                     DAYS( 2 ) = 29
                  END IF

                  ISAYEAR = ISAYEAR + 1900
                  IOSTAT = SAI__OK
                  READ ( CLBUF( 3:5 ), '(I3)', IOSTAT = IOSTAT,
     :                   ERR = 80 ) ITYPE
                  GO TO 81
  80              ITYPE = 0
  81              CONTINUE

                  ISAMONTH = 0
                  DO WHILE ( ITYPE .GT. 0 )
                     ISAMONTH = ISAMONTH + 1
                     ITYPE = ITYPE - DAYS( ISAMONTH )
                  END DO
                  ISADAY = ITYPE + DAYS( ISAMONTH )
               END IF

*            Print selected header lines.
               IF ( MAXLN.LT.0 .OR. NLINE.LE.MAXLN ) THEN
                  CALL line_WRITS( '%p %s\\', LINE )
                  CALL PRTBUF( STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERROUT( 'print error\\', STATUS )
                     GO TO 999
                  END IF
               END IF

*            Check against last VICAR header line.
               IF ( LINE( LNSIZE ) .EQ. BIGL ) THEN
                  GO TO 999
               END IF
            END DO
         END DO
      END DO

 999  CONTINUE

      END
