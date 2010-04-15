C+
      SUBROUTINE FIT_MWRIT (STATUS)
C
C     F I T _ M W R I T
C
C     Writes the main FITS buffer, FBUFF, onto tape (or disk).  Note
C     this version does not perform the standard FITS clean-up
C     operations when the end-of-tape is encountered; instead, it
C     will return an end-of-tape I/O error.  A later version may
C     correct this.  Note that if the tape is being blocked, then
C     FBUFF is treated as a logical record, and is copied into the
C     buffer containing a number of such logical records that is
C     being built up, and so will actually be written to tape at this
C     point only if the tape buffer fills as a result of this call.
C
C     Parameters -   (">" input, "<" output)
C
C     (<) STATUS     (Integer) Returned status code.  0 => OK,
C                    non-zero values are I/O error codes which can be
C                    decoded by FIT_ERROR.
C
C     Common variables used -
C
C     (>) MTUNIT     (Integer) The I/O channel for the mag tape.
C     (>) FBUFF      (Byte array) The main FITS buffer.
C     (<) MTERR      (Character) Description of the last tape I/O error.
C     (<) MTSTAT     (Integer) Last TIO_ error code
C     (>) LUFILE     (Integer) Logical unit number for disk file, if used.
C     (!) TBUFF      (Byte array) Is the large buffer in which logical
C                    blocks (built up in FBUFF) are blocked up into actual
C                    tape records.
C     (>) TBLOCK     (Integer) Is the current blocking factor - number of
C                    logical records in each actual tape record.
C     (!) BCOUNT     (Integer) Is the number of logical records already held
C                    in TBUFF.
C     (!) RINDEX     (Integer) Disk file record index, if a direct access
C                    file is being written to. Zero otherwise.
C
C     MTERR is defined in COMB.INC, the others in COMF.INC
C
C     Subroutines / functions used -
C
C     GEN_MOVE       (GEN_ package) Fast byte copy between arrays
C     TIO_WRITE      (TIO_ package) Write data to a foreign mag tape
C     TIO_GETMSG     ( "      "   ) Decode TIO_ error code.
C     TIO_ERR        ( "      "   ) See if status indicates an error
C
C                                             KS / CIT 11th April 1984
C     Modified:
C
C     23rd Oct 1987  Now uses TIO routines instead of MTPCKG
C     30th Jan 1990  Code added to support writing of blocked FITS tapes.
C                    KS/AAO.
C     28th Apr 1993  Added RINDEX and possibility of direct access I/O. KS/AAO.
C      5th Jul 1993  Unused variables removed. KS/AAO.
C+
      IMPLICIT NONE
C
C     Parmeters
C
      INTEGER STATUS
C
C     Functions
C
      LOGICAL TIO_ERR
C
C     Common blocks
C
      INCLUDE 'COMB'
      INCLUDE 'COMF'
C
C     Local variables
C
      INTEGER LENGTH
      INTEGER TPTR
C
      IF (LUFILE.EQ.0) THEN
C
C        Tape output.  If records aren't being blocked, write
C        this one out directly.  Otherwise copy it into the larger
C        buffer and write that out if it's now full.
C
         IF (TBLOCK.EQ.1) THEN
C
C           Unblocked tape output.
C
            CALL TIO_WRITE(MTUNIT,FBUFF,2880,STATUS)
            IF (TIO_ERR(STATUS)) THEN
               CALL TIO_GETMSG(STATUS,MTERR,LENGTH)
               MTSTAT=STATUS
            ELSE
               STATUS=0
            END IF
         ELSE
C
C           Blocked tape output
C
            TPTR=BCOUNT*2880+1
            CALL GEN_MOVE (2880,FBUFF,TBUFF(TPTR))
            BCOUNT=BCOUNT+1
            IF (BCOUNT.EQ.TBLOCK) THEN
               CALL TIO_WRITE(MTUNIT,TBUFF,2880*BCOUNT,STATUS)
               BCOUNT=0
               IF (TIO_ERR(STATUS)) THEN
                  CALL TIO_GETMSG(STATUS,MTERR,LENGTH)
                  MTSTAT=STATUS
               ELSE
                  STATUS=0
               END IF
            END IF
         END IF
      ELSE
C
C        Output to disk.
C
         IF (RINDEX.GT.0) THEN
            WRITE (LUFILE,REC=RINDEX,IOSTAT=STATUS) FBUFF
            RINDEX=RINDEX+1
         ELSE
            WRITE (LUFILE,IOSTAT=STATUS) FBUFF
         END IF
         IF (STATUS.NE.0) THEN
            STATUS=-1
            MTERR='I/O error writing to disk file'
         END IF
      END IF
C
      END
