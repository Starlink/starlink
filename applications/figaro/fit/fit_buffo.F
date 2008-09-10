#if HAVE_CONFIG_H
#  include "config.h"
#endif
C+
      SUBROUTINE FIT_BUFFO (STATUS)
C
C     F I T _ B U F F O
C
C     Writes the FITS header buffer (BUFF, in common) into the
C     main FITS output buffer (FBUFF).  If necessary, the buffer
C     will be written to tape.  Any non-printing characters are
C     set to blanks.
C
C     Parameter -   (">" input,  "!" modified,  "<" output)
C
C     (<) STATUS    (Integer) Return code.  0 => OK.  Non-zero codes
C                   represent an MTPCKG code caused by a tape I/O
C                   error.  This can be decoded by FIT_ERROR.
C
C     Common variables used -
C
C     (>) BUFF      (Character) The single line character buffer used
C                   for header keyword lines.
C     (<) FBUFF     (Byte array) The main FITS output buffer.
C     (!) FPTR      (Integer) The pointer to the next free byte in FBUFF.
C                   Should have been initialised by FIT_INIT.
C
C     BUFF is defined in the file COMB.INC, the others in the file
C     COMF.INC
C
C     Subroutines / functions used -
C
C     FIT_MWRIT      (FIT_ package) Writes FBUFF to tape.
C
C                                           KS / CIT 7th Feb 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
C
C     Common blocks
C
      INCLUDE 'COMB'
      INCLUDE 'COMF'
C
C     Local variables
C
      INTEGER BLANK,DEL,I,ICH
C
#if HAVE_BOZ
      DATA BLANK,DEL/Z'20',Z'7F'/
#elif HAVE_OLD_TYPELESS_BOZ
      DATA BLANK,DEL/'20'X,'7F'X/
#else
      Do not have X'' or ''X typeless BOZ
#endif
C
C     Copy BUFF into FBUFF, then write to tape if full.  (Note assumption
C     that FBUFF has been filled 80 bytes at a time, so will not fill in
C     the middle of the copy.)
C
      DO I=1,80
         ICH=ICHAR(BUFF(I:I))
         IF ((ICH.LT.BLANK).OR.(ICH.GE.DEL)) ICH=BLANK
         FBUFF(FPTR)=ICH
         FPTR=FPTR+1
      END DO
      IF (FPTR.GT.2880) THEN
         CALL FIT_MWRIT(STATUS)
         FPTR=1
      ELSE
         STATUS=0
      END IF
C
      END
