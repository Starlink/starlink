C*PGQINF -- inquire PGPLOT general information
C%void cpgqinf(const char *item, char *value, int *value_length);
C+
      SUBROUTINE PGQINF (ITEM, VALUE, LENGTH)
      CHARACTER*(*) ITEM, VALUE
      INTEGER LENGTH
C
C This routine can be used to obtain miscellaneous information about
C the PGPLOT environment. Input is a character string defining the
C information required, and output is a character string containing the
C requested information.
C
C The following item codes are accepted (note that the strings must
C match exactly, except for case, but only the first 8 characters are
C significant). For items marked *, PGPLOT must be in the OPEN state
C for the inquiry to succeed. If the inquiry is unsuccessful, either
C because the item code is not recognized or because the information
C is not available, a question mark ('?') is returned.
C
C   'VERSION'     - version of PGPLOT software in use.
C   'STATE'       - status of PGPLOT ('OPEN' if a graphics device
C                   is open for output, 'CLOSED' otherwise).
C   'USER'        - the username associated with the calling program.
C   'NOW'         - current date and time (e.g., '17-FEB-1986 10:04').
C   'DEVICE'    * - current PGPLOT device or file.
C   'FILE'      * - current PGPLOT device or file.
C   'TYPE'      * - device-type of the current PGPLOT device.
C   'DEV/TYPE'  * - current PGPLOT device and type, in a form which
C                   is acceptable as an argument for PGBEG.
C   'HARDCOPY'  * - is the current device a hardcopy device? ('YES' or
C                   'NO').
C   'TERMINAL'  * - is the current device the user's interactive
C                   terminal? ('YES' or 'NO').
C   'CURSOR'    * - does the current device have a graphics cursor?
C                   ('YES' or 'NO').
C   'SCROLL'    * - does current device have rectangle-scroll
C                   capability ('YES' or 'NO'); see PGSCRL.
C
C Arguments:
C  ITEM  (input)  : character string defining the information to
C                   be returned; see above for a list of possible
C                   values.
C  VALUE (output) : returns a character-string containing the
C                   requested information, truncated to the length 
C                   of the supplied string or padded on the right with 
C                   spaces if necessary.
C  LENGTH (output): the number of characters returned in VALUE
C                   (excluding trailing blanks).
C--
C 18-Feb-1988 - [TJP].
C 30-Aug-1988 - remove pseudo logical use of IER.
C 12-Mar-1992 - change comments for clarity.
C 17-Apr-1995 - clean up some zero-length string problems [TJP].
C  7-Jul-1995 - get cursor information directly from driver [TJP].
C 24-Feb-1997 - add SCROLL request.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER IER, L1, GRTRIM
      LOGICAL INTER, SAME
      CHARACTER*8 TEST
      CHARACTER*64 DEV1
C
C Initialize PGPLOT if necessary.
C
      CALL PGINIT
C
      CALL GRTOUP(TEST,ITEM)
      IF (TEST.EQ.'USER') THEN
          CALL GRUSER(VALUE, LENGTH)
          IER = 1
      ELSE IF (TEST.EQ.'NOW') THEN
          CALL GRDATE(VALUE, LENGTH)
          IER = 1
      ELSE IF (TEST.EQ.'VERSION') THEN
          VALUE = 'v5.2.2'
          LENGTH = 6
          IER = 1
      ELSE IF (TEST.EQ.'STATE') THEN
          IF (PGID.LT.1 .OR. PGID.GT.PGMAXD) THEN
             VALUE = 'CLOSED'
             LENGTH = 6
          ELSE IF (PGDEVS(PGID).EQ.0) THEN
             VALUE = 'CLOSED'
             LENGTH = 6
          ELSE
             VALUE = 'OPEN'
             LENGTH = 4
          END IF
          IER = 1
      ELSE IF (PGID.LT.1 .OR. PGID.GT.PGMAXD) THEN
          IER = 0
      ELSE IF (PGDEVS(PGID).EQ.0) THEN
          IER = 0
      ELSE IF (TEST.EQ.'DEV/TYPE') THEN
          CALL GRQDT(VALUE)
          LENGTH = GRTRIM(VALUE)
          IER = 0
          IF (LENGTH.GT.0) IER = 1
      ELSE IF (TEST.EQ.'DEVICE' .OR. TEST.EQ.'FILE') THEN
          CALL GRQDEV(VALUE, LENGTH)
          IER = 1
      ELSE IF (TEST.EQ.'TERMINAL') THEN
          CALL GRQDEV(DEV1, L1)
          IF (L1.GE.1) THEN
             CALL GRTTER(DEV1(1:L1), SAME)
          ELSE
             SAME = .FALSE.
          END IF
          IF (SAME) THEN
              VALUE = 'YES'
              LENGTH = 3
          ELSE
              VALUE = 'NO'
              LENGTH = 2
          END IF
          IER = 1
      ELSE IF (TEST.EQ.'TYPE') THEN
          CALL GRQTYP(VALUE,INTER)
          LENGTH = GRTRIM(VALUE)
          IER = 0
          IF (LENGTH.GT.0) IER = 1
      ELSE IF (TEST.EQ.'HARDCOPY') THEN
          CALL GRQTYP(VALUE,INTER)
          IF (INTER) THEN
              VALUE = 'NO'
              LENGTH = 2
          ELSE
              VALUE = 'YES'
              LENGTH = 3
          END IF
          IER = 1
      ELSE IF (TEST.EQ.'CURSOR') THEN
          CALL GRQCAP(DEV1)
          IF (DEV1(2:2).EQ.'N') THEN
              VALUE = 'NO'
              LENGTH = 2
          ELSE
              VALUE = 'YES'
              LENGTH = 3
          END IF
          IER = 1
      ELSE IF (TEST.EQ.'SCROLL') THEN
          CALL GRQCAP(DEV1)
          IF (DEV1(11:11).NE.'S') THEN
              VALUE = 'NO'
              LENGTH = 2
          ELSE
              VALUE = 'YES'
              LENGTH = 3
          END IF
          IER = 1
      ELSE
          IER = 0
      END IF
      IF (IER.NE.1) THEN
         VALUE = '?'
         LENGTH = 1
      ELSE IF (LENGTH.LT.1) THEN
         LENGTH = 1
         VALUE = ' '
      END IF
      END
