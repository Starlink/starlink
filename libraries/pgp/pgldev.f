C*PGLDEV -- list available device types
C%void cpgldev(void);
C+
      SUBROUTINE PGLDEV
C
C Writes a list to the terminal of all device types known to the
C current version of PGPLOT.
C
C Arguments: none.
C--
C 5-Aug-1986 - [AFT].
C 1-Aug-1988 - add version number [TJP].
C 24-Apr-1989 - add copyright notice [TJP].
C 13-Dec-1990 - changed warnings to messages [TJP].
C-----------------------------------------------------------------------
      CHARACTER*16 GVER
      INTEGER L
C
      CALL PGQINF('VERSION', GVER, L)
      CALL GRMSG('PGPLOT '//GVER(:L)//
     1           ' Copyright 1996 California Institute of Technology')
      CALL GRLDEV
      END
