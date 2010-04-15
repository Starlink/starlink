C-----------------------------------------------------------------

      SUBROUTINE READ_GSD_RAS (IERR)

C   Routine to read GSD file containing a single spectrum and map
C   it onto the SPECX data stack for normal use.

C   8-JUN-1991 REVAD::JFL - modified to handle GSD V5 (post-DAS) data format
C  22-NOV-1991 REVAD::JFL - modified to handle GSD V5.1 format
C  07-OCT-1995 Rachael@mrao - keep file open until new one needed.
C  13-DEC-1995 Timj@jach    - read all scans in at once.
C   4-JUN-1997 RPT@jach     - updated for DASMERGE wideband mode
C  20-SEP-2000 AJC@ral      - unused SCAN_LIST, NO_SCANS, DXY, DXYS, PROMPT

      IMPLICIT  NONE

C   Formal parameters

      INTEGER*4 IERR

      INTEGER*4 ADAM__OK
      PARAMETER (ADAM__OK=0)

      INTEGER   IFAIL
C      INTEGER*4 OUTFILE
      INTEGER   ICHECK

      INTEGER   NDROP            ! Number of channels to drop
      INTEGER   JDEF             ! GENLIB thing

      INTEGER*4 INDEX
      INTEGER*4 ISTAT
      INTEGER*4 OLDSCAN
      LOGICAL   ADJQUAD           ! Adjust quadrant DC offsets
      LOGICAL   DOMERGE           ! Do the das-merge
      LOGICAL   WIDEBAND          ! Wideband mode?

      INCLUDE  'FLAGCOMM'
      INCLUDE  'STACKCOMM'
      INCLUDE  'GSD_VAR.INC'
      INCLUDE  'GSD_FILE.INC'
      INCLUDE  'STAKPAR'

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

C ALWAYS reopen the GSD file if we are dealing with rasters

      IF (GSD_OPEN) CALL SPECX_GSD_CLOSE (IERR)

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

      NNSPEC = (NP-1)/(PPC*NCI) + 1

      IF (NNSPEC .EQ. 1) THEN
         WRITE(ILOUT,*) 'This scan only has one spectrum. ',
     +        'Please use READ-GSD-DATA'
         CALL POP
         RETURN
      END IF

C Inform user that SET-LIST-FILE might save time
      WRITE(ILOUT,*)
      WRITE(ILOUT,*) 'Use SET-LIST-FILE N to reduce output messages'
      WRITE(ILOUT,*)

C Ask for the file name
      IF(ICHECK(1,IFAIL).NE.1)   RETURN
      CALL GETFIL ('W', OUTFILE, IFAIL)
      IF (IFAIL.NE.0) THEN
         WRITE(ILOUT,*) 'File could not be accessed!'
         WRITE(ILOUT,*) 'Check that the file is open and has the'
         WRITE(ILOUT,*) 'correct file access'
         CALL POP
         RETURN
      END IF

C Write title into variable
      INDEX = 1
      WRITE (ITITLE(:9),'(I4.4,''.'',I4.4)',IOSTAT=ISTAT)
     &     GSD_SCAN,INDEX

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

C See if person wishes to do a DAS-MERGE (need to put a spectrum on the
C stack first so that header information is correct

      IF (NQUAD .GT. 1) THEN
         CALL GEN_YESNO ('Perform DAS-MERGE before writing ? ',.TRUE.,
     +        DOMERGE, JDEF)
      ELSE
         DOMERGE = .FALSE.
      END IF

      IF (DOMERGE) CALL DASMERGE(NDROP, ADJQUAD, WIDEBAND, IFAIL)

C Write spectrum to waiting Specx data-file

      IF (IFAIL.NE.0) RETURN
      CALL WRITESCAN (OUTFILE, IFAIL)


C Begin loop----------------------------------------
      DO INDEX = 2, NNSPEC

C  Push existing data onto the stack to make room for the new scan

         IF (.NOT.XCLEAR) CALL PUSH
         IF (JTOP.EQ.0) JTOP = 1


C Write title into variable
         WRITE (ITITLE(:9),'(I4.4,''.'',I4.4)',IOSTAT=ISTAT)
     &        GSD_SCAN,INDEX

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

C Do das-merge if required

         IF (DOMERGE) CALL DODASMERGE(NDROP, ADJQUAD, WIDEBAND, IFAIL)

C Write spectrum to waiting Specx data-file

         IF (IFAIL.NE.0) RETURN
         CALL WRITESCAN (OUTFILE, IFAIL)


      END DO

 98      CONTINUE


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
