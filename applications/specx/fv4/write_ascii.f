*  History:
*     23 Nov 1993 (hme):
*        Change common block to PR_SCAN.
*        This file exists twice.
*     02 Jan 1994 (rp):
*        Use function calls to IGETLUN, IFREELUN
C-----------------------------------------------------------------------

      SUBROUTINE WRITE_ASCII_DATA (XSCALE, IFAIL)

*  Subroutine to write current spectrum to an ascii file so that
*  data can be read and plotted by MONGO, etc

      IMPLICIT NONE

*  Formal parameters:

      REAL*4    XSCALE(*)
      INTEGER*4 IFAIL

      INCLUDE 'STACKCOMM'
      INCLUDE 'FLAGCOMM'

      INTEGER*4 I
      INTEGER*4 ISTAT
      INTEGER*4 LUN
      INTEGER*4 NOFF
      INTEGER*4 NQ
      CHARACTER FILENAME*40

*  Special common block

      INTEGER*4 NLINE
      COMMON /PR_SCAN/ NLINE

*  Functions

      LOGICAL*4 DOQUAD
      INTEGER*4 NTOT
      INTEGER*4 IGETLUN, IFREELUN

      IFAIL = 0

*  Get an output filename

      CALL GEN_GETSTR ('Name for output file?', FILENAME,
     &                 'A40', FILENAME, ISTAT)

*  Open the output file

      ISTAT = IGETLUN (LUN, 'write_ascii_data', .TRUE.)
      OPEN (LUN, FILE=FILENAME, ACCESS='SEQUENTIAL', STATUS='NEW',
     &      CARRIAGECONTROL='LIST')

*  Write the header information

      CALL PRSCAN (LUN, 1)

*  Calculate the X-axis values

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) RETURN

*  Write the data

      NLINE = NLINE + 1
      WRITE (LUN,'('' Line #, Channel #, X-value and data value:'')')
      DO NQ = 1, NQUAD
        IF (DOQUAD(NQ) .AND. NPTS(NQ).GT.0) THEN
          NLINE = NLINE + 1
          WRITE (LUN,'('' Quadrant # '',I1)') NQ
          NOFF = NTOT (NQ-1)
          DO I = 1, NPTS(NQ)
            NLINE = NLINE+1
            WRITE  (LUN, 100) NLINE, I, XSCALE(NOFF+I), DATA(NOFF+I)
  100       FORMAT (1X,I4,5X,I4,5X,F9.2,5X,E12.5)
          END DO
        END IF
      END DO

*  Close the output file

      CLOSE         (LUN)
      ISTAT = IFREELUN (LUN)

      RETURN
      END

C-----------------------------------------------------------------------
