      SUBROUTINE USR_LISTIUE( STATUS )
*+
*  Name:
*     SUBROUTINE USR_LISTIUE

*  Purpose:
*     Describe content of IUE tape or file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_LISTIUE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The tape is positioned at the start of the file specified by the
*     %FILE parameter on %DRIVE. If %FILE is not specified the
*     current file is adopted.  %NFILE files are read.
*     For each file the first %NLINE lines of the VICAR header are
*     printed and a standard block size analysis is performed on the
*     data part.
*     In the event of tape I/O errors, the  user (if available) will be
*     asked to provide a logical value to the %CONTINUE parameter.
*     The tape is left positioned at the start of the next file
*     unless the %SKIPNEXT parameter is FALSE.

*  Method:
*     The tape position is determined from the parameters %DRIVE and
*     %FILE using the TPFILE utility.
*     Each tape file is analysed using IUEFLAN.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-JAN-82 (JRG):
*       AT4 version.
*     02-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     02-JUN-89 (PCTR):
*       IUEDR Vn. 2.1
*       Conversion to SGP/16 style.
*       Addition of %IMSTAT parameter to invoke image statistics.
*     01-OCT-92 (DMILLS):
*       IUEDR Vn. 3.0
*       ADAMised version to run on multiple hardware platforms
*       Added call to IUE_DEVICE for device independent behaviour
*       eg, MAGTAPE,DISK,NFS-DISK
*     27-NOV-94 (MJC):
*       Forced IMSTAT to false for the present.  It has never been
*       documented and the output is not properly tabulated.
*     06-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*       Removed IMSTAT completely.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MAG_ERR'

*  Global Variables:
      INCLUDE 'CMTAPE'
      INCLUDE 'CMISAF'

*  Local Constants:
      INTEGER MAXDEVN
      PARAMETER ( MAXDEVN = 255 )

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Variables:
      INTEGER ACTVAL     ! Parameter value count.
      INTEGER FILE       ! Tape file number.
      INTEGER IFILE      ! Tape file counter.
      INTEGER NFILE      ! Number of files to be analysed.
      INTEGER NLINE      ! Number of VICAR header lines to be printed.
      INTEGER TP         ! Tape descriptor.
      INTEGER IDUM
      INTEGER FDN
      INTEGER I
      INTEGER ISTAT      ! Local status.

      LOGICAL SKIP       ! Whether skip to start of next file.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Open device.
      CALL IUE_DEVICE( 'OPEN', TP, FDN, IDUM, 1, I, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*   NLINE parameter.
      CALL RDPARI( 'NLINE\\', .FALSE., 1, NLINE, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'NLINE\\', STATUS )
         GO TO 999
      END IF

*   SKIPNEXT parameter.
      CALL RDPARL('SKIPNEXT\\', .FALSE., 1, SKIP, ACTVAL, STATUS)
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'SKIPNEXT\\', STATUS )
         GO TO 999
      END IF

*   NFILE parameter.
      CALL RDPARI( 'NFILE\\', .FALSE., 1, NFILE, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'NFILE\\', STATUS )
         GO TO 999

      ELSE
         CALL WRPARI( 'NFILE\\', 1, 1, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRPAR( 'NFILE\\' )
            CALL ERROUT( ': parameter write error\\', STATUS )
            GO TO 999
         END IF
      END IF

*   Analyse files.
      IF ( NFILE .NE. 0 ) THEN
         IFILE = 1
      END IF

      DO WHILE ( .TRUE. )
         CALL IUE_FLAN( TP, NLINE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( STATUS .EQ. MAG__EOF ) THEN
               STATUS = SAI__OK
               IF ( IFILE .LT. NFILE ) THEN
                  CALL LINE_WRITI( '%pOnly %i files found on tape.\\',
     :                             IFILE )
                  CALL PRTBUF( STATUS )
               END IF
            END IF
            GO TO 100
         END IF

         IF ( IFILE.LT.NFILE .OR. NFILE.LT.0 .OR. SKIP ) THEN
            IF ( .NOT. ISAFILE ) THEN
               ISTAT = SAI__OK
               CALL MAG_SKIP( TCHAN, 1, ISTAT )
               IF ( ISTAT .NE. SAI__OK ) THEN
                  CALL ERR_FLUSH( ISTAT )
                  CALL ERROUT( '\\', STATUS )
                  GO TO 100
               END IF
            END IF

            FILE = FILE + 1
         END IF

         IF ( NFILE.GT.0 .AND. IFILE.EQ.NFILE ) THEN
            GO TO 100

         ELSE
            IFILE = IFILE + 1
         END IF
      END DO

 100  CONTINUE

      CALL IUE_DEVICE( 'CLOSE', TP, FDN, IDUM, 1, I, STATUS )

 999  CONTINUE

      END
