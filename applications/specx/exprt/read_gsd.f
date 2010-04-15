*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*      12 May 2003 (timj)
*        SAVE DXYS (prevent uninitialized warning)
C-----------------------------------------------------------------

      SUBROUTINE READ_GSD (IERR)

C   Routine to read GSD file containing a single spectrum and map
C   it onto the SPECX data stack for normal use.

C   8-JUN-1991 REVAD::JFL - modified to handle GSD V5 (post-DAS) data format
C  22-NOV-1991 REVAD::JFL - modified to handle GSD V5.1 format
C  07-OCT-1995 Rachael@mrao - keep file open until new one needed.
C  13-MAY-1996 timj@jach  - modified so that re-reads if requested index
C                           is not available (eg during observing)

      IMPLICIT  NONE

C   Formal parameters

      INTEGER*4 IERR

      INTEGER*4 ADAM__OK
      PARAMETER (ADAM__OK=0)

      INTEGER*4 INDEX
      INTEGER*4 SCAN_LIST(64)
      INTEGER*4 ISTAT
      INTEGER*4 NO_SCANS
      INTEGER*4 OLDSCAN
      REAL*4    DXY(2)
      REAL*4    DXYS(2)
      CHARACTER PROMPT*256

      INCLUDE  'FLAGCOMM'
      INCLUDE  'STACKCOMM'
      INCLUDE  'GSD_VAR.INC'
      INCLUDE  'GSD_FILE.INC'
      INCLUDE  'STAKPAR'

C   Note that DXYS is the default prompt for the X&Y arcsec offsets.
C   It must be retained between calls to this routine so that the values
C   are retained from previous calls when making the default prompt.
      SAVE DXYS

      IERR = 0

C  Get a scan number from the terminal and make up a filename

      OLDSCAN = GSD_SCAN
      CALL GEN_GETI4 ('GSD scan number?',GSD_SCAN,'I4',GSD_SCAN,ISTAT)

C  Push existing data onto the stack to make room for the new scan

      IF (.NOT.XCLEAR) CALL PUSH
      IF (JTOP.EQ.0) JTOP = 1

C  Zero the existing data

      CALL INITHD
      CALL INIT_ARRAY (LSTK-128, DATA, 0.0)

C  Open the GSD file

      CALL SPECX_GSD_OPEN   (GSD_SCAN, IERR)
      IF (IERR.NE.0) GO TO 99

C  Report GSD version

      WRITE (ILOUT, '('' GSD version '', F4.1)') VERSION

C  Read file header according to version

      IF (VERSION .LT. 4.99) THEN

         CALL SPECX_GSD_V4_HEADER (IERR)
         IF (IERR .NE. 0) GOTO 98

      ELSE

         CALL SPECX_GSD_V5_HEADER (IERR)
         IF (IERR .NE. 0) GOTO 98

      END IF


C  Get data array as well
C  As for FROM-MAP - select by scan # or <CR> to select by sky position

      INDEX = 1

      NNSPEC = (NP-1)/(PPC*NCI) + 1

      IF (NNSPEC.GT.1) THEN

        CALL GEN_GETI4  ('Sequence # of spectrum in GSD file? '//
     &                   '(<CR> to select by position on sky)',
     &                   0, ' ', INDEX, ISTAT)

        IF (ISTAT.GT.0) THEN
          CALL GEN_GETR4A ('X & Y position offsets? (arcseconds)',
     &                     DXYS, 2, 'F6.2,'','',F6.2', DXYS, ISTAT)
          DXY(1) = DXYS(1)/DX
          DXY(2) = DXYS(2)/DY
          CALL SPECX_GSD_LOCATE (DXY, SCAN_LIST, NO_SCANS, IERR)

C     Just have to make sure that this file is not being written
          IF (IERR.NE.0) THEN
             PRINT *,'X,Y position not found...re-reading'
             IERR = 0

             CALL  SPECX_GSD_REOPEN (GSD_SCAN, IERR)
             IF (IERR.NE.0) GO TO 99
             CALL SPECX_GSD_LOCATE (DXY, SCAN_LIST, NO_SCANS, IERR)
             IF (IERR.NE.0) GO TO 98
          END IF

          IF (NO_SCANS.GT.1) THEN
            CALL RDGSD_MKPROMPT (PROMPT, NO_SCANS, SCAN_LIST)
            CALL GEN_GETI4 (PROMPT, SCAN_LIST(1), ' ', INDEX, ISTAT)
          ELSE
            INDEX = SCAN_LIST(1)
          END IF
        END IF

        IF (INDEX.GT.NGSDSPEC) THEN
C Really want to re-read GSD file once if requested INDEX is not
C present just in case file is still being written on telescope

           PRINT *,'INDEX too large...re-reading'
           IERR = 0

           CALL  SPECX_GSD_REOPEN (GSD_SCAN, IERR)
           IF (IERR.NE.0) GO TO 99

C Now really do the check to see if INDEX is too large
           IF (INDEX .GT. NGSDSPEC) THEN
              PRINT *,'INDEX still too large!'
              IERR = 2
              GO TO 98
           END IF
        END IF

      END IF

      WRITE (ITITLE(:9),'(I4.4,''.'',I4.4)',IOSTAT=ISTAT)
     &       GSD_SCAN,INDEX

C  Read data according to GSD version

      IF (VERSION .LT. 4.99) THEN
         CALL SPECX_GSD_V4_DATA (INDEX, 0, IERR)
      ELSE IF ((VERSION.GT.4.99) .AND. (VERSION.LT.5.01)) THEN
         CALL SPECX_GSD_V5_DATA (INDEX, 0, IERR)
      ELSE IF ((VERSION.GT.5.09) .AND. (VERSION.LT.5.11)) THEN
         CALL SPECX_GSD_V51_DATA (INDEX, 0, IERR)
      ELSE
         CALL SPECX_GSD_V52_DATA (INDEX, 0, IERR)
      END IF

   98 CONTINUE

C     Don't close file -- leave open in case needed again.
C     CALL SPECX_GSD_CLOSE (IERR)

C  Standard return

   99 IF (IERR.NE.0) THEN
        IF (.NOT.XCLEAR) CALL POP
      ELSE
        XCLEAR = .FALSE.
      END IF

      RETURN

      END

C-----------------------------------------------------------------

      SUBROUTINE RDGSD_MKPROMPT (PROMPT, NO_SCANS, SCAN_LIST)

      IMPLICIT NONE

C  Formal parameters

      CHARACTER PROMPT*(*)
      INTEGER*4 NO_SCANS
      INTEGER*4 SCAN_LIST(NO_SCANS)

C  Functions

      INTEGER*4 GEN_ILEN

C  Local variables

      INTEGER*4 I,J
      INTEGER*4 FIRST,LAST
      CHARACTER LINE*80

*     Print *,'NO_SCANS: ',NO_SCANS
*     Print *,'SCAN_LIST'
*     Print *,(SCAN_LIST(J),J=1,NO_SCANS)

      PROMPT = '"'' There are multiple scans at this position'',/,'
      DO I = 1, ((NO_SCANS -1)/12)+1
        FIRST  = 12*(I-1)+1
        LAST   = MIN (NO_SCANS, 12*I)
        LINE   = ' '
        WRITE (LINE,'(12(1X,I3))') (SCAN_LIST(J),J=FIRST,LAST)
        PROMPT =    PROMPT(:GEN_ILEN(PROMPT))
     &          // ''''
     &          // LINE(:GEN_ILEN(LINE))
     &          // ''',/,'
      END DO

      PROMPT = PROMPT(:GEN_ILEN(PROMPT))
     &           // '''$Which one do you want? ''"'

      RETURN
      END

C-----------------------------------------------------------------------
