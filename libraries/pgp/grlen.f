      SUBROUTINE GRLEN (STRING, D)
*+
*     - - - - - - -
*       G R L E N     (GKS emulation of GRPCKG)
*     - - - - - - -
*
*   Calculate length of plotted string
*
*   Given
*      STRING     c      String
*
*   Returned
*      D          r      Length in absolute coordinates
*
*   Read from COMMON
*      GRCIDE      i      Current plotting device
*      GRXPIN      r()    Resolution (x)
*      GRYPIN      r()    Resolution (y)
*
*   D.L.Terrett  Starlink  Jul 1987  (after T.J.Pearson)
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'


      CHARACTER*(*) STRING
      REAL D

      LOGICAL UNUSED
      INTEGER XYGRID(300)
      INTEGER LIST(256), LX, IFNTLV, NLIST, I
      REAL FACTOR, RATIO, DX, FNTBAS, FNTFAC


      D = 0.0

*  Compute scaling and orientation
      FACTOR = GRCFAC(GRCIDE)/2.5
      RATIO = GRXPIN(GRCIDE)/GRYPIN(GRCIDE)
      FNTBAS = 0.0
      FNTFAC = 1.0
      IFNTLV = 0

*   Convert string to symbol numbers:
*   \\u and \\d escape sequences are converted to -1,-2
      CALL GRSYDS(LIST,NLIST,STRING,GRCFNT(GRCIDE))

*   Plot the string of characters
      DO 380 I = 1,NLIST
          IF (LIST(I).LT.0) THEN
            IF (LIST(I).EQ.-1) THEN
                IFNTLV = IFNTLV+1
                FNTBAS = FNTBAS + 16.0*FNTFAC
                FNTFAC = 0.6**IABS(IFNTLV)
            ELSE IF (LIST(I).EQ.-2) THEN
                IFNTLV = IFNTLV-1
                FNTFAC = 0.6**IABS(IFNTLV)
                FNTBAS = FNTBAS - 16.0*FNTFAC
            END IF
              GOTO 380
          END IF
          CALL GRSYXD(LIST(I),XYGRID,UNUSED)
          LX = XYGRID(5)-XYGRID(4)
          DX = FACTOR * LX * RATIO
          D = D + DX * FNTFAC
  380 CONTINUE

      END
