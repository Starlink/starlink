      SUBROUTINE INDAT (LUI,RECTYP,INBUF,APPROX,XARRAY,J1ARR,J2ARR,NFLD,
     :                  NAME)
*+
*
*     - - - - - -
*      I N D A T
*     - - - - - -
*
*  General purpose record input routine for ASTROM.
*
*  o  If new record not requested, return immediately.
*  o  Read chars into INBUF from LUI.
*  o  Ignore blank records
*  o  Eliminate leading spaces.
*  o  Detect and eliminate leading '~' (meaning approx).
*  o  Fold lowercase to uppercase except for comments.
*  o  Detect 'TIME', 'OBS', 'MET', 'COL', '/' and 'END' records.
*  o  Ignore records beginning '*'.
*  o  Use sla_DBJIN to decode up to 11 double precision numbers
*     into XARRAY.
*  o  Store sla_DBJIN status flags (for valid numbers only) in
*     J1ARR and J2ARR.
*  o  Return 10 character comment in NAME, left justified
*
*  Given:
*     LUI     i      unit for read
*
*  Given and returned:
*     RECTYP  c*(*)  space = next record needed
*
*  Returned:
*     INBUF   c*(*)  input buffer
*     APPROX  l      .TRUE. = leading '~'
*     XARRAY  d()    decoded input data
*     J1ARR   i()    number flags
*     J2ARR   i()    syntax flags
*     NFLD    i      number of valid numbers decoded
*     NAME    c*(*)  10 character name, left justified
*
*  RECTYP is returned with one of the following values:
*     'T'     TIME record
*     'O'     OBS record
*     'M'     MET record
*     'C'     COL record
*     '/'     end of sequence
*     'E'     end of file
*     '?'     other
*
*+

      IMPLICIT NONE

      INTEGER LUI
      CHARACTER*(*) RECTYP,INBUF
      LOGICAL APPROX
      DOUBLE PRECISION XARRAY(11)
      INTEGER J1ARR(11),J2ARR(11),NFLD
      CHARACTER NAME*10

      INTEGER L,NUC,I,IOUT,N,J1,J2,IERR
      CHARACTER T,C



*  Nothing to do if last record still unprocessed
      IF (RECTYP.NE.' ') GO TO 9999

*  Preset number of fields
      NFLD=0

*  Length of input buffer
      L=LEN(INBUF)

*  Flag non-trivial record yet to be accepted
      T='*'

*  Input and identify a data record

      DO WHILE (T.EQ.'*')
         READ (LUI,'(A)',IOSTAT=IERR) INBUF
         IF ( IERR .NE. 0 ) GOTO 999

*     Continue unless all blanks
         IF (INBUF.NE.' ') THEN

*        Strip leading spaces, look for "approx" flag, fold to uppercase,
*        eliminate TABs etc (n.b. ASCII dependent)

*        Reset '~' detected flag
            APPROX=.FALSE.
*        Initialise INBUF's output pointer
            IOUT=0
*        Calculate lowercase to uppercase offset
            NUC=ICHAR('A')-ICHAR('a')
*        Step INBUF's input pointer from first to last character
            DO I=1,L
*           Pick up the current input character
               C=INBUF(I:I)
*           If '~' and nothing output yet, set '~' flag
               IF (IOUT.EQ.0.AND.C.EQ.'~') THEN
                  APPROX=.TRUE.
*           If not a space and output has begun, increment output pointer
               ELSE IF (IOUT.GT.0.OR.C.NE.' ') THEN
                  IOUT=IOUT+1
*              If lowercase a-z, fold to uppercase
                  IF (C.GE.'a'.AND.C.LE.'z') THEN
                     C=CHAR(ICHAR(C)+NUC)
*              If TAB etc, substitute a space (assuming ASCII!)
                  ELSE IF (C.LT.' '.OR.C.GT.'~') THEN
                     C=' '
                  END IF
*              If not a '*', store the character
                  IF (C.NE.'*') THEN
                     INBUF(IOUT:IOUT)=C
*              Otherwise, spacefill up to the '*' and break from loop
                  ELSE
                     IF (IOUT.LT.I) INBUF(IOUT:I-1)=' '
                     GO TO 100
                  END IF
               END IF
*        Next input character
            END DO
 100        CONTINUE

*        Check for TIME record
            C=INBUF(1:1)
            IF (C.EQ.'T') THEN
               T='T'

*        Check for OBSERVATORY record
            ELSE IF (C.EQ.'O') THEN
               T='O'

*        Check for METEOROLOGICAL record
            ELSE IF (C.EQ.'M') THEN
               T='M'

*        Check for COLOUR record
            ELSE IF (C.EQ.'C') THEN
               T='C'

*        Check for end of sequence
            ELSE IF (C.EQ.'/') THEN
               T='/'

*        Check for end of file
            ELSE IF (C.EQ.'E') THEN
               T='E'

*        Terminate loop if not a comment
            ELSE IF (C.NE.'*'.AND.C.NE.' ') THEN
               T='?'

            END IF

         END IF

      END DO

*  Record type
      RECTYP=T

*  If not end, extract numeric data
      IF (RECTYP.NE.'/'.AND.RECTYP.NE.'E') THEN

*     Initialise buffer pointer
         N=1

*     If T, O, M or C record, look for first space
         IF (RECTYP.EQ.'T'.OR.
     :       RECTYP.EQ.'O'.OR.
     :       RECTYP.EQ.'M'.OR.
     :       RECTYP.EQ.'C') THEN
            DO WHILE (INBUF(N:N).NE.' '.AND.N.LT.L)
               N=N+1
            END DO
         END IF

*     Decode up to 11 double precision numbers
         DO I=1,11
            CALL sla_DBJIN(INBUF,N,XARRAY(I),J1,J2)

*        Proceed only if valid field decoded
            IF (J1.LE.0) THEN

*           Save status flags
               J1ARR(I)=J1
               J2ARR(I)=J2

*           Update count of valid fields
               NFLD=I

            END IF

         END DO

*     If we are pointing at '*', skip it
         IF (N.LE.L.AND.INBUF(N:N).EQ.'*') N=N+1

*     Skip any spaces preceding name
         DO WHILE (N.LT.L.AND.INBUF(N:N).EQ.' ')
            N=N+1
         END DO

*     Move name (if any) into NAME
         IF (N.LE.L) THEN
            NAME=INBUF(N:)
         ELSE
            NAME=' '
         END IF

      END IF

      GO TO 9999

*  End of file
 999  CONTINUE
      RECTYP='E'

*  Exit
 9999 CONTINUE

      END
