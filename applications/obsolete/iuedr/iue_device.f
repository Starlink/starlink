      SUBROUTINE IUE_DEVICE( OPER, TP, FDN, RBUF, MAXSIZE, NREAD,
     :                       STATUS )
*+
*  Name:
*     SUBROUTINE IUE_DEVICE

*  Authors:
*     ???
*     MJC: Martin Clayton (UCL)
*     BKM: Brian McIlwrath (RAL)
*     MBT: Mark Taylor (Starlink)
*     {enter_new_authors_here}

*  History:
*     ??-???-?? (???)
*     08-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     10-OCT-96 (BKM)
*       Revise for Linux
*     09-JUL-99 (MBT)
*       Remove some OPEN keywords unsupported by g77.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT   NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*  Global Variables:
      INCLUDE 'CMPSX'
      INCLUDE 'CMTAPE'
      INCLUDE 'CMISAF'

*  Local Constants:
      INTEGER MAXDEVN
      PARAMETER (MAXDEVN=255)

*  Arguments Given:
      CHARACTER*( * ) OPER    ! Requested operation.
      INTEGER MAXSIZE         ! Size of RBUF array.

*  Given or returned depending on OPER:
      INTEGER TP              ! Tape descriptor.

*  Given and Returned:
      BYTE RBUF( MAXSIZE )    ! Storage for data on READ.

*  Arguments Returned:
      INTEGER FDN
      INTEGER NREAD           ! Number of bytes read on READ operation.

*  Status:
      INTEGER STATUS          ! Global status.

*  Local variables:
      INTEGER*2 NREAD2
      INTEGER ACTVAL          ! Parameter value count.
      INTEGER FILE            ! Tape file number.
      INTEGER FIOSTAT
      INTEGER LMAXSIZE
      INTEGER IOFF
      INTEGER I
      INTEGER IOSTAT          ! Local status.
      INTEGER RECL            ! Record length (system dependent).

      BYTE BCBUFFER( 6144 )
      CHARACTER*( 6144 ) CBUFFER
      EQUIVALENCE ( BCBUFFER, CBUFFER )

      BYTE B4BUF( 2 )
      INTEGER*2 I2BUF
      EQUIVALENCE ( I2BUF, B4BUF )

*  DRIVE and FILE parameters used to position tape.
      BYTE TAPE( MAXDEVN )    ! Tape drive logical name.
      CHARACTER*( MAXDEVN ) CTAPE
      EQUIVALENCE ( CTAPE, TAPE )

      BYTE RBUFFER( 1024 )
      BYTE SWPBUF

      INTEGER RECNO
      INTEGER RECPTR
      INTEGER S2PTR

      SAVE
*.

*  Check inherited global status except on CLOSE.
      IF ( OPER.NE.'CLOSE' .AND. STATUS.NE.SAI__OK ) RETURN

      FIOSTAT = SAI__OK

*  Open device.
      IF ( OPER .EQ. 'OPEN' ) THEN

*     GET parameter value for source of data.
 1       CTAPE = ' '
         STATUS = SAI__OK
         CALL CNPAR( 'DRIVE\\' , STATUS )
         CALL RDPARC( 'DRIVE\\', .FALSE., MAXDEVN, TAPE, ACTVAL,
     :                STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999
         END IF

*     Check if a file spec (has a . in it)
         ISAFILE = .FALSE.
         CTAPE( ACTVAL : ACTVAL ) = ' '
         DO I = 1, ACTVAL
            IF ( TAPE( I ) .EQ. ICHAR('.') ) THEN
               ISAFILE = .TRUE.
            END IF
         END DO

*     If not a file then assume it's a tape drive.
         IF ( .NOT. ISAFILE ) THEN
            CALL TAPE_FILE( FILE, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: accessing tape file\\', STATUS )
            END IF

*     Otherwise file open is a bit system dependent at the moment.
         ELSE
            IF ( SYSNAME .EQ. 'VMS' ) THEN
               CALL FIO_OPEN( CTAPE(  : ACTVAL ), 'READ', 'NONE', 0,
     :                        FDN, STATUS )
               CALL FIO_UNIT( FDN, TP, STATUS )

            ELSE
               CALL FIO_GUNIT( TP, STATUS )
               RECNO = 1
               RECPTR = 0
               RECL = 1024
               IF ( SYSNAME .EQ. 'OSF1' ) RECL = RECL / 4
               OPEN ( UNIT = TP, FILE = CTAPE( : ACTVAL ), TYPE = 'OLD',
     :                ACCESS = 'DIRECT', FORM = 'UNFORMATTED',
     :                RECL = RECL, IOSTAT = STATUS )
            END IF

*        Report any problem.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: accessing disk file\\', STATUS )
               IF ( SYSNAME .NE. 'VMS' ) THEN
                  CALL FIO_PUNIT( TP, FIOSTAT )
                  IF ( FIOSTAT .NE. SAI__OK ) THEN
                     CALL ERROUT( 'I/O unit release error\\', STATUS )
                     FIOSTAT = SAI__OK
                  END IF
               END IF
               GO TO 1
            END IF
         END IF

*  Read from device.
      ELSE IF ( OPER .EQ. 'READ' ) THEN
         LMAXSIZE = MAXSIZE
         IF ( ISAFILE ) THEN
            IF ( SYSNAME .EQ. 'VMS' ) THEN
               READ( TP, '( Q, A )', IOSTAT = IOSTAT )
     :               NREAD, CBUFFER( : NREAD )
               DO I = 1, NREAD
                  RBUF( I ) = BCBUFFER( I )
               END DO

            ELSE IF ( SYSNAME .EQ. 'SunOS' ) THEN
               IF ( RECPTR.EQ.0 .OR. RECPTR.EQ.1025 ) THEN
                  READ( TP, REC = RECNO, IOSTAT = IOSTAT, ERR = 122 )
     :                  RBUFFER
 122              CONTINUE
                  RECNO = RECNO + 1
                  RECPTR = 1
               END IF
               B4BUF( 2 ) = RBUFFER( RECPTR )
               B4BUF( 1 ) = RBUFFER( RECPTR + 1 )
               RECPTR = RECPTR + 2
               IF ( RECPTR .EQ. 1025 ) THEN
                  READ( TP, REC = RECNO, IOSTAT = IOSTAT, ERR = 123 )
     :                  RBUFFER
 123              CONTINUE
                  RECNO = RECNO + 1
                  RECPTR = 1
               END IF
               NREAD = I2BUF
               S2PTR = RECPTR
               IOFF = 0
               DO I = 1, NREAD, 2
                  RBUF( I + 1 ) = RBUFFER( RECPTR + I - 1 - IOFF )
                  RBUF( I ) = RBUFFER( RECPTR + I - IOFF )
                  S2PTR = S2PTR + 2
                  IF ( S2PTR .EQ. 1025 ) THEN
                     READ( TP, REC = RECNO, IOSTAT = IOSTAT,
     :                     ERR = 124 ) RBUFFER
 124                 CONTINUE
                     RECNO = RECNO + 1
                     RECPTR = 1
                     S2PTR = 1
                     IOFF = I + 1
                  END IF
               END DO
               RECPTR = S2PTR
               IF ( NREAD .EQ. 514 ) THEN
                  NREAD = 768
               END IF

            ELSE
               IF ( RECPTR.EQ.0 .OR. RECPTR.EQ.1025 ) THEN
                  READ( TP, REC = RECNO, IOSTAT = IOSTAT, ERR = 125 )
     :                  RBUFFER
 125              CONTINUE
                  RECNO = RECNO + 1
                  RECPTR = 1
               END IF
               B4BUF( 1 ) = RBUFFER( RECPTR )
               B4BUF( 2 ) = RBUFFER( RECPTR + 1 )
               RECPTR = RECPTR + 2
               IF ( RECPTR .EQ. 1025 ) THEN
                  READ( TP, REC = RECNO, IOSTAT = IOSTAT, ERR = 126 )
     :                  RBUFFER
 126              CONTINUE
                  RECNO = RECNO + 1
                  RECPTR = 1
               END IF
               NREAD = I2BUF
               DO I = 1, NREAD
                  RBUF( I ) = RBUFFER( RECPTR )
                  RECPTR = RECPTR + 1
                  IF ( RECPTR .EQ. 1025 ) THEN
                     READ( TP, REC = RECNO, IOSTAT = IOSTAT,
     :                     ERR = 127 ) RBUFFER
 127                 CONTINUE
                     RECPTR = 1
                     RECNO  = RECNO + 1
                  END IF
               END DO
            END IF

         ELSE
            IOSTAT = SAI__OK
            CALL MAG_READ( TCHAN, LMAXSIZE, RBUF, NREAD, IOSTAT )

*        Perform byte swap under SunOS.
            IF ( SYSNAME .EQ. 'SunOS' ) THEN
               DO I = 1, 2 * ( NREAD / 2 ), 2
                  SWPBUF = RBUF( I )
                  RBUF( I ) = RBUF( I + 1 )
                  RBUF( I + 1 ) = SWPBUF
               END DO
            END IF

*        Status check.
            IF ( IOSTAT .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( IOSTAT )
               CALL ERROUT( '\\', STATUS )
            END IF
         END IF

*  Close device.
      ELSE IF ( OPER .EQ. 'CLOSE' ) THEN
         IF ( ISAFILE ) THEN
            IF ( SYSNAME .EQ. 'VMS' ) THEN
               CALL FIO_CLOSE( FDN, STATUS )

            ELSE
               CLOSE( TP )
               CALL FIO_PUNIT( TP, FIOSTAT )
               IF ( FIOSTAT .NE. SAI__OK ) THEN
                  CALL ERROUT( 'I/O unit release error\\', STATUS )
               END IF
            END IF
         END IF

*  Unknown operation.
      ELSE
         CALL ERROUT( 'Error: unknown device operation\\', STATUS )
      END IF

 999  CONTINUE

      END
