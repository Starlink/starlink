
C*GRPXPX -- Perform pixel operations using pixel primitive
C+
      SUBROUTINE GRPXPX (IA, IDIM, JDIM, I1, I2, J1, J2, X, Y)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      INTEGER IA(IDIM,JDIM)
      REAL    X, Y
C
C Arguments:
C  IA     (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  X, Y   (input)  : the lower left corner of the output region
C                    (device coordinates)
C--
C 16-Jan-1991 - [GvG]
*  4-Aug-1993 - Debugged by Remko Scharroo
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER     NSIZE
      PARAMETER   (NSIZE = 1280)
      REAL        RBUF(NSIZE + 2)
      REAL        WIDTH
      INTEGER     IC1, IC2
      INTEGER     I, J, L
      INTEGER     NBUF, LCHR
      CHARACTER*1 CHR

      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C
C Get allowable color range and pixel width
C
      CALL GRQCOL(IC1, IC2)
      CALL GREXEC(GRGTYP, 3, RBUF, NBUF, CHR, LCHR)
      WIDTH = RBUF(3)
      DO 30 J = J1, J2
C
C Compute Y coordinate for this line
C
         RBUF(2) = Y + (J - J1) * WIDTH
         I = I1
  10        L = 1
C
C Compute left X coordinate for this line segment
C
            RBUF(1) = X + (I - I1) * WIDTH
C
C Check color index
C
  20           IF (IA(I, J) .LT. IC1 .OR. IC2 .LT. IA(I, J)) THEN
                  RBUF(L + 2) = 1
               ELSE
                  RBUF(L + 2) = IA(I, J)
               ENDIF
               L = L + 1
               I = I + 1
C
C Still room in segment and something left?
C
            IF (L .LE. NSIZE .AND. I .LE. I2) GOTO 20
C
C Output segment
C
*           NBUF = L + 2 ! wrong ! should be: (RS)
            NBUF = L + 1
            CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
C
C Something left?
C
         IF (I .LE. I2) GOTO 10
  30  CONTINUE

      END
