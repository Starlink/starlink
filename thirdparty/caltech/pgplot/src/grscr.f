C*GRSCR -- set color representation
C+
      SUBROUTINE GRSCR (CI, CR, CG, CB)
      INTEGER  CI
      REAL     CR, CG, CB
C
C GRPCKG: SET COLOUR REPRESENTATION -- define the colour to be
C associated with a colour index.  Ignored for devices which do not
C support variable colour or intensity.  On monochrome output
C devices (e.g. VT125 terminals with monochrome monitors), the
C monochrome intensity is computed from the specified Red, Green, Blue
C intensities as 0.30*R + 0.59*G + 0.11*B, as in US color television
C systems, NTSC encoding.  Note that most devices do not have an
C infinite range of colours or monochrome intensities available;
C the nearest available colour is used.
C
C Arguments:
C
C CI (integer, input): colour index. If the colour index is outside the
C       range available on the device, the call is ignored. Colour
C       index 0 applies to the background colour.
C CR, CG, CB (real, input): red, green, and blue intensities,
C       in range 0.0 to 1.0.
C--
C 20-Feb-1984 - [TJP].
C  5-Jun-1984 - add GMFILE device [TJP].
C  2-Jul-1984 - add REGIS device [TJP].
C  2-Oct-1984 - change use of map tables in Regis [TJP].
C 11-Nov-1984 - add code for /TK [TJP].
C  1-Sep-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - If needed, calls begin picture [AFT].
C 31-Aug-1994 - suppress call of begin picture [TJP].
C  1-Sep-1994 - use common data [TJP].
C 26-Jul-1995 - fix bug: some drivers would ignore a change to the
C               current color [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER   NBUF, LCHR
      REAL      RBUF(6)
      CHARACTER CHR
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSCR - Specified workstation is not open.')
      ELSE IF (CR.LT.0.0 .OR. CG.LT.0.0 .OR. CB.LT.0.0 .OR.
     1    CR.GT.1.0 .OR. CG.GT.1.0 .OR. CB.GT.1.0) THEN
          CALL GRWARN('GRSCR - Colour is outside range [0,1].')
      ELSE IF (CI.GE.GRMNCI(GRCIDE) .AND. CI.LE.GRMXCI(GRCIDE)) THEN
C         IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
          RBUF(1)=CI
          RBUF(2)=CR
          RBUF(3)=CG
          RBUF(4)=CB
          NBUF=4
          CALL GREXEC(GRGTYP,21,RBUF,NBUF,CHR,LCHR)
C         -- If this is the current color, reselect it in the driver.
          IF (CI.EQ.GRCCOL(GRCIDE)) THEN
             RBUF(1) = CI
             CALL GREXEC(GRGTYP,15,RBUF,NBUF,CHR,LCHR)
          END IF
      END IF
C
      END
