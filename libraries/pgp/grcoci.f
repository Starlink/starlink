	SUBROUTINE GRCOCI (A, IDIM, JDIM, I1, I2, J1, J2, A1, A2,
     :                   ICIWH, ICIBL, MODE, ICIA)
*+
*     - - - - - - - -
*       G R C O C I     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Converts an array of real numbers to an array of colour indexes
*
*   Given
*      A        r() The array to be plotted.
*      IDIM     i   The first dimension of array A.
*      JDIM     i   The second dimension of array A.
*      I1, I2   i   The inclusive range of the first index (I) to be plotted.
*      J1, J2   i   The inclusive range of the second index (J) to be plotted.
*      A1       r   The array value which is to appear the same as the
*                   background
*      A2       r   The array value which is to appear the same as the
*                   foreground
*      ICIBL    i   Colour index of background
*      ICIWH    i   Colour index of foreground
*      ICIA     r   Output colour index array
*
* N.B. Arguments are assumed to be valid (checked by PGGRAY).
*
*   D.L.Terrett  Starlink  Oct 1991
*   D.L.Terrett  Starlink  Feb 1995
*       Implement mode parameter for pgplot 5.0
*+
      IMPLICIT NONE
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MODE
      REAL    A(IDIM,JDIM)
      REAL    A1, A2, SFAC, SFACL, AV
      INTEGER ICIBL, ICIWH, ICIA(I2-I1+1,J2-J1+1)
      PARAMETER (SFAC=65000.0)

      INTEGER I, J, LEV

      SFACL = LOG(1.0+SFAC)

      DO 20 J = J1, J2
        DO 10 I = I1, I2
         AV = A(I,J)
         IF (A2.GT.A1) THEN
           AV = MIN(A2, MAX(A1,AV))
         ELSE
           AV = MIN(A1, MAX(A2,AV))
         END IF
*      Linear
         IF (MODE.EQ.0) THEN
           LEV = NINT((ICIWH*(A2-AV) + ICIBL*(AV-A1))/(A2-A1))
*      Logarithmic
         ELSE IF (MODE.EQ.1) THEN
           LEV = ICIWH + NINT((ICIBL-ICIWH)*
     :               LOG(1.0+SFAC*ABS((AV-A1)/(A2-A1)))/SFACL)
*      Square root
         ELSE IF (MODE.EQ.2) THEN
           LEV = ICIWH + NINT((ICIBL-ICIWH)*
     :                             SQRT(ABS((AV-A1)/(A2-A1))))
*      Some mode we don't know about.
         ELSE
           LEV = ICIWH
         END IF

        ICIA(I-I1+1,J-J1+1) = LEV
   10   CONTINUE
   20 CONTINUE

      END
