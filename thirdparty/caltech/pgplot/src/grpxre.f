C*GRPXRE -- Emulate pixel operations using rectangles
C+
      SUBROUTINE GRPXRE (IA, IDIM, JDIM, I1, I2, J1, J2, 
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
C 18-Jan-1991 - [GvG]
C-----------------------------------------------------------------------
      REAL YB, YT
      INTEGER I, J, ICOL, LSTCOL

C
C In some strange cases we can get bounds that are one greater than the
C size of the array. Presumably a rounding error but catch them here
C rather than attempting a proper debugging. Added by TIMJ.
C
      IF ( J2 .GT. JDIM ) THEN
         CALL GRWARN( 'GRPXRE: J2 Out of array bounds. Limiting range.')
         J2 = JDIM
      END IF
      IF ( I2 .GT. IDIM ) THEN
         CALL GRWARN( 'GRPXRE: I1 Out of array bounds. Limiting range.')
         I2 = IDIM
      END IF

C
C Save color attribute
C
      CALL GRQCI(ICOL)
      LSTCOL = ICOL
      DO 20 J = J1, J2
C
C Compute Y range for this index
C
         YB = Y1 + ((Y2 - Y1) * (J - J1)) / (J2 - J1 + 1)
         YT = Y1 + ((Y2 - Y1) * (J - J1 + 1)) / (J2 - J1 + 1)
         DO 10 I = I1, I2
C
C Need to change color?
C
            IF (IA(I, J) .NE. LSTCOL) THEN
               CALL GRSCI(IA(I, J))
               LSTCOL = IA(I, J)
            ENDIF
C
C Output rectangle
C
            CALL GRREC0(X1 + ((X2 - X1) * (I - I1)) / (I2 - I1 + 1), YB,
     1                  X1 + ((X2 - X1) * (I - I1 + 1)) / (I2 - I1 + 1),
     2                  YT)

  10     CONTINUE
  20  CONTINUE
C
C Restore color attribute
C
      CALL GRSCI(ICOL)
      END
