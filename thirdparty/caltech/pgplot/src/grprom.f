C*GRPROM -- prompt user before clearing screen
C+
      SUBROUTINE GRPROM
C
C If the program is running under control of a terminal, display
C message and wait for the user to type <CR> before proceeding.
C
C Arguments:
C  none
C--
C 18-Aug-1994
C-----------------------------------------------------------------------
      INTEGER IER, L, GRGCOM
      CHARACTER*16 JUNK
C
      IER = GRGCOM(JUNK, 'Type <RETURN> for next page: ', L)
      END
