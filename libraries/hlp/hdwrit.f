      SUBROUTINE hlp_HDWRIT (STRING, IADR, J)
*+
*  - - - - - - -
*   H D W R I T
*  - - - - - - -
*
*  Direct-access write to the help library file.
*
*  Specified in INCLUDE:
*     LHBUF     i           length of help buffer
*
*  Given (in COMMON):
*     JHELP     i           state of HLP system: 1=open/write
*     LUHL      i           unit number for help library file
*     NCHH      i           number of characters in help file
*     IHBUF     i           address of HBUF within help file
*     HEOS      c*1         end-of-string character
*     HBUF      c*(LHBUF)   help buffer
*
*  Given (arguments):
*     STRING    c*(*)       string to write (see note)
*     IADR      i           character address to write to (1st = 0)
*
*  Returned (in COMMON):
*     HBUF      c*(LHBUF)   help buffer
*     IHBUF     i           address of HBUF within help file
*
*  Returned (arguments)
*     IADR      i           points to next character address in sequence
*     J         i           status:  0 = OK
*                                   -1 = HLP system in wrong state
*                                   -6 = attempt to write outside file
*                                   -3 = write error
*                                   -4 = read error
*
*  Notes:
*
*  1)  If STRING is the end-of-string character (optionally with
*      trailing spaces), the record which is output consists of a single
*      end-of-string.  If not, the record is STRING plus an
*      end-of-string character.  In this second case (the norm), STRING
*      must not contain any end-of-string characters, though no check is
*      made for this.
*
*  P.T.Wallace   Starlink   13 June 1995
*-

      IMPLICIT NONE

      CHARACTER*(*) STRING
      INTEGER IADR,J

      INCLUDE 'helpic'

*  Total number of characters to be written, including EOS
      INTEGER NC

*  Character index to STRING
      INTEGER IS

*  Character address
      INTEGER IC

*  Direct-access record number
      INTEGER IREC

*  Character index to output buffer BUF
      INTEGER I



*  Check that the HLP system is in the right state.
      IF (JHELP.NE.1) GO TO 9000

*  Total number of characters to be written, including EOS.
      IF (STRING.NE.HEOS) THEN
         NC=LEN(STRING)+1
      ELSE
         NC=1
      END IF

*  Check within the stated extent of the help library file.
      IF (IADR+NC.GT.NCHH) GO TO 9010

*  Output characters.
      DO IS=1,NC

*     Current character address.
         IC=IADR+IS-1

*     Is the current character within the current buffer?
         IF (IC.LT.IHBUF.OR.IC.GE.IHBUF+LHBUF.OR.IHBUF.LT.0) THEN

*        No: flush, unless no block is available.
            IF (IHBUF.GE.0) WRITE (UNIT=LUHL,FMT='(A)',
     :                             REC=1+IHBUF/LHBUF,ERR=9020) HBUF

*        Read the right record.
            IREC=1+IC/LHBUF
            READ (UNIT=LUHL,FMT='(A)',REC=IREC,ERR=9030) HBUF

*        Character address of start of buffer.
            IHBUF=LHBUF*(IREC-1)
         END IF

*     We have the correct record in the buffer: insert character.
         I=IC-IHBUF+1
         IF (IS.NE.NC) THEN
            HBUF(I:I)=STRING(IS:IS)
         ELSE
            HBUF(I:I)=HEOS
         END IF

*     Next character.
      END DO

*  Update the file pointer, set OK status and exit.
      IADR=IC+1
      J=0
      GO TO 9999

*  HLP system in wrong state
 9000 CONTINUE
      J=-1
      GO TO 9999

*  Attempt to write outside the file
 9010 CONTINUE
      J=-6
      GO TO 9999

*  Write error
 9020 CONTINUE
      J=-3
      GO TO 9999

*  Read error
 9030 CONTINUE
      J=-4

*  Exit.
 9999 CONTINUE

      END
