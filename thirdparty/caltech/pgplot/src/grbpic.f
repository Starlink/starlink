C*GRBPIC -- begin picture
C+
      SUBROUTINE GRBPIC
C
C GRPCKG (internal routine). Send a "begin picture" command to the
C device driver, and send commands to set deferred attributes (color,
C line width, etc.)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL RBUF(2)
      INTEGER NBUF, LCHR
      CHARACTER*20 CHR
C
      GRPLTD(GRCIDE) = .TRUE.
      IF (GRGTYP.GT.0) THEN
C         -- begin picture
          RBUF(1) = GRXMXA(GRCIDE)
          RBUF(2) = GRYMXA(GRCIDE)
          NBUF = 2
          CALL GREXEC(GRGTYP,11,RBUF,NBUF,CHR,LCHR)
C         -- set color index
          RBUF(1) = GRCCOL(GRCIDE)
          NBUF = 1
          CALL GREXEC(GRGTYP,15,RBUF,NBUF,CHR,LCHR)
C         -- set line width
          IF (GRGCAP(GRCIDE)(5:5).EQ.'T') THEN
              RBUF(1) = ABS(GRWIDT(GRCIDE))
              NBUF = 1
              CALL GREXEC(GRGTYP,22,RBUF,NBUF,CHR,LCHR)
          END IF
C         -- set hardware dashing
          IF (GRGCAP(GRCIDE)(3:3).EQ.'D') THEN
              RBUF(1) = GRSTYL(GRCIDE)
              NBUF = 1
              CALL GREXEC(GRGTYP,19,RBUF,NBUF,CHR,LCHR)
          END IF
      END IF
C
      END
