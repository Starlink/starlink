C+
      SUBROUTINE FIT_CLOSE (STATUS)
C
C     F I T _ C L O S E
C
C     Should be the last FIT_ routine called during the writing
C     of a single image.  Flushes the main I/O buffer to tape
C     or to disk, and if output is actually going to tape,
C     writes two file marks and leaves the tape positioned between
C     them.  Note that this does not close down the mag tape, so
C     this may be followed immediately by a call to FIT_HSTAN to
C     start writing a new image, without a call to FIT_INIT being
C     needed.  To finally close down the system, FIT_END should be
C     used. (The writing and backspacing over of the second file
C     mark can be suppressed by a prior call to FIT_NOTERM - see
C     comments for that routine for details.)
C
C     Parameters -  (">" input, "W" workspace, "<" output, "!" modified)
C
C     (<) STATUS    (Integer) Returned status code. 0 => OK, non-zero
C                   codes are I/O error codes, which may be decoded
C                   by FIT_ERROR.
C
C     Common variables used -
C
C     (!) FBUFF     (Byte array) Main I/O buffer.  If non-empty, will
C                   be filled with zeros and flushed to tape.
C     (!) FPTR      (Integer) Pointer to next free byte in FBUFF.
C     (>) MTUNIT    (Integer) I/O channel number for mag tape used.
C     (<) MTERR     (Character) description of last TIO_ error
C     (<) MTSTAT    (Integer) Last TIO_ error code.
C     (>) LUFILE    (Integer) Logical unit of disk file, if used.
C     (>) TBUFF     (Byte array) Is the large buffer in which logical
C                   blocks (built up in FBUFF) are blocked up into actual
C                   tape records.
C     (!) NOTERM    (Logical) If set, suppresses proper termination of tape.
C     (!) BCOUNT    (Integer) Is the number of logical records already held
C                   in TBUFF.
C
C                   All defined in the file COMF.INC, except MTERR
C                   which is in COMB.INC
C
C     Subroutines / functions used -
C
C     FIT_MWRIT     (FIT_ package) Writes FBUFF to tape.
C     TIO_MARK      (TIO_    "   ) Write EOF mark to tape
C     TIO_WRITE     ( "      "   ) Write data to tape
C     TIO_ERR       ( "      "   ) Determine if a status signals an error
C     TIO_SKIP      ( "      "   ) Skip over file marks on tape.
C     TIO_GETMSG    ( "      "   ) Decode TIO_ error code
C
C                                       KS / CIT 13th April 1984
C     Modified:
C
C     22nd Oct 1987.  Now uses TIO_ routines instead of MTPCKG.
C     30th Jan 1990.  KS/AAO.  Now supports blocked tape output.
C      5th Mar 1993.  KS/AAO.  Proper termination of the tape can now
C                     be suppressed.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
C
C     Common blocks
C
      INCLUDE 'COMF'
      INCLUDE 'COMB'
C
C     Functions
C
      LOGICAL TIO_ERR
C
C     Local variables
C
      INTEGER I,LENGTH,MARKS
C
C     Do we have to flush FBUFF?
C
      IF (FPTR.GT.1) THEN
         DO I=FPTR,2880
            FBUFF(I)=0
         END DO
         CALL FIT_MWRIT(STATUS)
         FPTR=1
         IF (STATUS.NE.0)  GO TO 600
      END IF
C
C     Disk or tape?
C
      IF (LUFILE.EQ.0) THEN
C
C        For tape, in blocked mode we may have to flush out the actual
C        tape buffer.
C
         IF (BCOUNT.NE.0) THEN
            CALL TIO_WRITE (MTUNIT,TBUFF,BCOUNT*2880,STATUS)
            BCOUNT=0
            IF (TIO_ERR(STATUS))  THEN
               CALL TIO_GETMSG(STATUS,MTERR,LENGTH)
               MTSTAT=STATUS
               GO TO 600
            ELSE
               STATUS=0
            END IF
         END IF
C
C        Write final file marks. Normally, write two and backspace over
C        the final one. If proper tape termination is suppressed, just
C        write one. If it was set, clear the 'NOTERM' flag.
C
         MARKS=2
         IF (NOTERM) MARKS=1
         DO I=1,MARKS
            CALL TIO_MARK(MTUNIT,STATUS)
            IF (TIO_ERR(STATUS))  THEN
               CALL TIO_GETMSG(STATUS,MTERR,LENGTH)
               MTSTAT=STATUS
               GO TO 600
            ELSE
               STATUS=0
            END IF
         END DO
         IF (NOTERM) THEN
            NOTERM=.FALSE.
         ELSE
            CALL TIO_SKIP(MTUNIT,-1,STATUS)
            IF (TIO_ERR(STATUS)) THEN
               CALL TIO_GETMSG(STATUS,MTERR,LENGTH)
               MTSTAT=STATUS
            ELSE
               STATUS=0
            END IF
         END IF
      END IF
C
  600 CONTINUE
      END
