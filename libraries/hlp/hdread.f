      SUBROUTINE hlp_HDREAD (IADR, STRING, NC, J)
*+
*  - - - - - - -
*   H D R E A D
*  - - - - - - -
*
*  Direct-access read from the help library.
*
*  Specified in INCLUDE:
*     LHBUF     i          length of help buffer
*
*  Given (in COMMON):
*     JHELP     i          state of HLP system: 1=open/write
*     LUHL      i          unit number for help library file
*     IHBUF     i          address of HBUF within help library file
*     NCHH      i          number of characters in help library file
*     HBUF      c*(LHBUF)  help buffer
*     HEOS      c*1        end-of-string character
*
*  Given (argument):
*     IADR      i          character address within help file (1st = 0)
*
*  Returned (in COMMON):
*     HBUF      c*(LHBUF)  help buffer
*     IHBUF     i          address of HBUF within help file
*
*  Returned (argument)
*     IADR      i          points to next character in sequence
*     STRING    c*(*)      input record (not including end-of-string)
*     NC        i          length of record (0 or more)
*     J         i          status:  0 = OK
*                                  -1 = HLP system in wrong state
*                                  -4 = read error
*                                  -7 = attempt to read outside file
*                                  -8 = record overflows STRING (note 2)
*
*  Notes:
*
*  1)  No check is made that IADR points to the start of a record.
*
*  2)  If the record overflows STRING, the first LEN(STRING) characters
*      are stored in STRING, NC is set to LEN(STRING) and J is set to
*      -8.
*
*  P.T.Wallace   Starlink   13 June 1995
*-

      IMPLICIT NONE

      INTEGER IADR
      CHARACTER*(*) STRING
      INTEGER NC,J

      INCLUDE 'helpic'

*  Length of STRING
      INTEGER L

*  Character index to STRING
      INTEGER IS

*  Loop control
      LOGICAL MORE

*  Character address within help library file
      INTEGER IC

*  Direct-access record number
      INTEGER IREC

*  Character index to input buffer BUF
      INTEGER I

*  Single character
      CHARACTER C



*  Check that the HLP system is in the right state.
      IF (JHELP.NE.2) GO TO 9000

*  Preset status to OK.
      J=0

*  Length of STRING.
      L=LEN(STRING)

*  Fill buffer if uninitialized or past the specifed place.
      IF (IHBUF.LT.0.OR.IADR.LT.IHBUF) THEN
         IREC=1+IADR/LHBUF
         READ (UNIT=LUHL,FMT='(A)',REC=IREC,ERR=9020) HBUF

*     Character address of start of buffer.
         IHBUF=LHBUF*(IREC-1)
      END IF

*  Initialize STRING pointer.
      IS=1

*  Input characters until EOS or STRING full.
      MORE=.TRUE.
      DO WHILE (MORE)

*     Current character address.
         IC=IADR+IS-1

*     Error if beyond end of help library file.
         IF (IC.GE.NCHH) GO TO 9010

*     Is the current character within the current buffer?
         IF (IC.GE.IHBUF+LHBUF) THEN

*        No: read the next record.
            IREC=1+IC/LHBUF
            READ (UNIT=LUHL,FMT='(A)',REC=IREC,ERR=9020) HBUF

*        Character address of start of buffer.
            IHBUF=LHBUF*(IREC-1)
         END IF

*     We have the correct record in the buffer: extract character.
         I=IC-IHBUF+1
         C=HBUF(I:I)

*     EOS?
         IF (C.EQ.HEOS) THEN

*        Yes: done.
            MORE=.FALSE.
         ELSE

*        No: is STRING already full?
            IF (IS.LE.L) THEN

*           No: copy the character into STRING.
               STRING(IS:IS)=C
               IS=IS+1
            ELSE

*           STRING full: set error status.
               J=-8
            END IF
         END IF

*     Next character.
      END DO

*  Spacefill STRING.
      IF (IS.LE.L) STRING(IS:)=' '

*  Update IADR to point to next record.
      IADR=IC+1

*  Number of characters transferred.
      NC=IS-1

*  Exit.
      GO TO 9999

*  HLP system in wrong state
 9000 CONTINUE
      J=-1
      GO TO 9999

*  Attempt to read outside the file
 9010 CONTINUE
      J=-7
      GO TO 9999

*  Read error
 9020 CONTINUE
      J=-4

*  Exit.
 9999 CONTINUE

      END
