C*PGLDEV -- list available device types on standard output
C%void cpgldev(void);
C+
      SUBROUTINE PGLDEV
C
C Writes (to standard output) a list of all device types available in
C the current PGPLOT installation.
C
C Arguments: none.
C--
C 5-Aug-1986 - [AFT].
C 1-Aug-1988 - add version number [TJP].
C 24-Apr-1989 - add copyright notice [TJP].
C 13-Dec-1990 - changed warnings to messages [TJP].
C 26-Feb-1997 - revised description [TJP].
C 18-Mar-1997 - revised [TJP].
C-----------------------------------------------------------------------
      CHARACTER*16 GVER
      INTEGER L
      CHARACTER*10 T
      CHARACTER*64 D
      INTEGER I, N, TLEN, DLEN, INTER
C
C Initialize PGPLOT if necessary.
C
      CALL PGINIT
C
C Report version and copyright.
C
      CALL PGQINF('VERSION', GVER, L)
      CALL GRMSG('PGPLOT '//GVER(:L)//
     1           ' Copyright 1997 California Institute of Technology')
C
C Find number of device types.
C
      CALL PGQNDT(N)
C
C Loop through device-type list (twice).

      CALL GRMSG('Interactive devices:')
      DO 10 I=1,N
         CALL PGQDT(I, T, TLEN, D, DLEN, INTER)
         IF (TLEN.GT.0 .AND. INTER.EQ.1)
     :        CALL GRMSG('   '//T//' '//D(1:DLEN))
 10   CONTINUE
      CALL GRMSG('Non-interactive file formats:')
      DO 20 I=1,N
         CALL PGQDT(I, T, TLEN, D, DLEN, INTER)
         IF (TLEN.GT.0 .AND. INTER.EQ.0)
     :        CALL GRMSG('   '//T//' '//D(1:DLEN))
 20   CONTINUE
C
      END
