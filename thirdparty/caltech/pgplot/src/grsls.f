C*GRSLS -- set line style
C+
      SUBROUTINE GRSLS (IS)
      INTEGER IS
C
C GRPCKG: Set the line style for subsequent plotting on the current
C device. The different line styles are generated in hardware on
C some devices and by GRPCKG software for the other devices. Five
C different line styles are available, with the following codes:
C 1 (full line), 2 (dashed), 3 (dot-dash-dot-dash), 4 (dotted),
C 5 (dash-dot-dot-dot). The default is 1 (normal full line). Line
C style is ignored when drawing characters, which are always drawn with
C a full line.
C
C Argument:
C
C IS (input, integer): the line-style code for subsequent plotting on
C       the current device (in range 1-5).
C--
C  9-Feb-1983 - [TJP].
C  3-Jun-1984 - add GMFILE device [TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - If needed, calls begin picture [AFT].
C 19-Jan-1987 - fix bug in GREXEC call [TJP].
C 16-May-1989 - fix bug for hardware line dash [TJP].
C  1-Sep-1994 - do not call driver to get size and capabilities [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER I, L, IDASH, NBUF,LCHR
      REAL    RBUF(6),TMP
      CHARACTER*10 CHR
      REAL PATERN(8,5)
C
      DATA PATERN/ 8*10.0,
     1             8*10.0,
     2             8.0, 6.0, 1.0, 6.0, 8.0, 6.0, 1.0, 6.0,
     3             1.0, 6.0, 1.0, 6.0, 1.0, 6.0, 1.0, 6.0,
     4             8.0, 6.0, 1.0, 6.0, 1.0, 6.0, 1.0, 6.0 /
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSLS - no graphics device is active.')
          RETURN
      END IF
C
      I = IS
      IF (I.LT.1 .OR. I.GT.5) THEN
          CALL GRWARN('GRSLS - invalid line-style requested.')
          I = 1
      END IF
C
C Inquire if hardware dash is available.
C
      IDASH=0
      IF(GRGCAP(GRCIDE)(3:3).EQ.'D') IDASH=1
C
C Set up for hardware dash.
C
      IF(IDASH.NE.0) THEN
          GRDASH(GRCIDE) = .FALSE.
          IF (GRPLTD(GRCIDE)) THEN
              RBUF(1)=I
              NBUF=1
              CALL GREXEC(GRGTYP,19,RBUF,NBUF,CHR,LCHR)
          END IF
C
C Set up for software dash.
C
      ELSE
          IF (I.EQ.1) THEN
              GRDASH(GRCIDE) = .FALSE.
          ELSE
              GRDASH(GRCIDE) = .TRUE.
              GRIPAT(GRCIDE) = 1
              GRPOFF(GRCIDE) = 0.0
              TMP = GRYMXA(GRCIDE)/1000.
              DO 10 L=1,8
                  GRPATN(GRCIDE,L) = PATERN(L,I)*TMP
   10         CONTINUE
          END IF
      END IF
      GRSTYL(GRCIDE) = I
      END
