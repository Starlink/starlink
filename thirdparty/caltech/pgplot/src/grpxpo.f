C*GRPXPO -- Emulate pixel operations using points
C+
      SUBROUTINE GRPXPO (IA, IDIM, JDIM, I1, I2, J1, J2, 
     1                   X1, X2, Y1, Y2)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      INTEGER IA(IDIM,JDIM)
      REAL    X1, X2, Y1, Y2
C
C Arguments:
C  IA     (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  X1, X2 (input)  : the horizontal range of the output region
C  Y1, Y2 (input)  : the vertical range of the output region
C--
C 16-Jan-1991 - [GvG]
C 28-Jun-1991
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER LW
      INTEGER I, J
      INTEGER ICOL, LSTCOL
C
C Save attributes
C
      CALL GRQLW(LW)
      CALL GRQCI(ICOL)
      CALL GRSLW(1)
      LSTCOL = ICOL
      DO 20 J = J1, J2
         DO 10 I = I1, I2
C
C Color changed?
C
            IF (IA(I, J) .NE. LSTCOL) THEN
               CALL GRSCI(IA(I, J))
               LSTCOL = IA(I, J)
            ENDIF
C
C Output dot
C
            CALL GRDOT0(X1 + (X2 - X1) * (I - I1 + 0.5) / (I2 - I1 + 1),
     1                  Y1 + (Y2 - Y1) * (J - J1 + 0.5) / (J2 - J1 + 1))
  10     CONTINUE
  20  CONTINUE
C
C Restore attributes
C
      CALL GRSCI(ICOL)
      CALL GRSLW(LW)
      END
