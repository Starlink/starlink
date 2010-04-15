      SUBROUTINE JTY_FFT2C(A,M,IFS,NITEMS)
c
c         FFT2C, one-dimensional finite complex fourier transform.
c
c     Fourier transform subroutine, programmed in system/360,
c     basic programming support, fortran iv. form c28-6504
c
c     Does either fourier synthesis,i.e.,computes complex fourier series
c     given a vector of n complex fourier amplitudes, or, given a vector
c     of complex data x does fourier analysis, computing amplitudes.
c     A is a complex vector of length n = 2**M complex nos. or 2*n real
c     M  is an integer 0.lt.M.le.13, set by user.
c     S is a vector S(j) = sin(2*pi*j/np ), j = 1,2,....,np/4-1,
c     computed by program.
c     IFS is a parameter to be set by user as follows-
c     IFS = 1 to set n = np = 2**m, set up sin table (if required),
c     and do a fourier transform, replacing the vector a by
c
c     x(j) = sum over k = 0,n-1 of A(k)*exp(2*pi*i/n)**(j*k),
c     j = 0,n-1, where i = sqrt(-1)
c
c     the  x's  are stored with   re x(j) in cell  2*j+1
c     and  im x(j)  in cell  2*j+2  for  j = 0,1,2,...,n-1.
c     the  A's  are stored in the same manner.
c
c     IFS = -1   to set  n = np = 2**m, set up sin table (if required),
c     and do an inverse fourier transform, taking the input vector A as x and
c     replacing it by the A satisfying the above fourier series.
c     (ie, conjugate, divide by n, and transform)
c
c     Note: as stated above, the maximum value of M for this program
c     is 13.    One may increase this limit by replacing 13 in
c     statement 3 below by log2 n, where n is the max. no. of
c     complex numbers one can store in high-speed core.  One must
c     also add more  do  statements to the binary sort routine
c     following statement  24  and change the equivalence statements
c     for the  k's. In addition, one must allocate more space for the
c     sine table.
c
c     Modified 26th May 1999 TCDA/RAL, Starlink. Dimension of A
c     now passed from calling program. Cures problems with Linux.
c
c     Modified 9th Oct. 2001 ACD/UoE, Starlink.  Explicitly initialise
c     local arrays K and S and variable MLAST to zero when the routine is
c     invoked.  (Note that previously MLAST was initialised in a DATA
c     statement, which is only invoked the first time the routine is called
c     and not subsequently.)  This fix appears to cure problems on Linux.

      REAL A
      DIMENSION A(NITEMS), S(2048), K(14)
      EQUIVALENCE (K(13),K1),(K(12),K2),(K(11),K3),(K(10),K4)
      EQUIVALENCE (K( 9),K5),(K( 8),K6),(K(7),K7),(K( 6),K8)
      EQUIVALENCE (K( 5),K9 ),(K( 4),K10),(K( 3),K11),(K( 2),K12)
      EQUIVALENCE (K( 1),K13),( K(1),N2)
      INTEGER MLAST
      INTEGER STATUS
      CHARACTER*(79) TEXT
      DATA MLAST /0/

      DO LOOP = 1, 14
         K(LOOP) = 0
      END DO

      DO LOOP = 1, 2048
         S(LOOP) = 0.0E0
      END DO

      MLAST = 0


      IF(M)2,2,3
    3 IF(M-13) 5,5,2
    2 IFERR = 1
      WRITE(TEXT,1000) M
      CALL PAR_WRUSER(TEXT,STATUS)
 1000 FORMAT(1X,'FFT2C: fft size out of range, logN =',I10)
      STOP
    1 RETURN
    5 IFERR = 0
      N = 2**M
      NP = N
      MP = M
      NT = N/4
      MT = M-2
      IF(M.NE.MLAST) GOTO 200
C     SCRAMBLE A, BY SANDE'S METHOD
   20 K(1) = 2*N
      DO 22 L = 2,M
   22 K(L) = K(L-1)/2
      DO 24 L = M,12
   24 K(L+1) = 2
C     NOTE EQUIVALENCE OF KL AND K(14-L)
C     BINARY SORT-
      IJ = 2
      DO 30 J1 = 2,K1,2
      DO 30 J2 = J1,K2,K1
      DO 30 J3 = J2,K3,K2
      DO 30 J4 = J3,K4,K3
      DO 30 J5 = J4,K5,K4
      DO 30 J6 = J5,K6,K5
      DO 30 J7 = J6,K7,K6
      DO 30 J8 = J7,K8,K7
      DO 30 J9 = J8,K9,K8
      DO 30 J10 = J9,K10,K9
      DO 30 J11 = J10,K11,K10
      DO 30 J12 = J11,K12,K11
      DO 30 JI = J12,K13,K12
      IF(IJ-JI)28,30,30
   28 T = A(IJ-1 )
      A(IJ-1) = A(JI-1)
      A(JI-1) = T
      T = A(IJ)
      A(IJ) = A(JI)
      A(JI) = T
   30 IJ = IJ+2
      IF(IFS)32,2,36
C     DOING FOURIER ANALYSIS,SO DIV. BY N AND CONJUGATE.
   32 FN = N
      DO 34 I = 1,N
      A(2*I-1) = A(2*I-1)/FN
   34 A(2*I) = -A(2*I)/FN
C     SPECIAL CASE- L = 1
   36 DO 40 I = 1,N,2
      T = A(2*I-1)
      A(2*I-1) = T + A(2*I+1)
      A(2*I+1) = T-A(2*I+1)
      T = A(2*I)
      A(2*I) = T + A(2*I+2)
   40 A(2*I+2) = T - A(2*I+2)
      IF(M-1) 2,1  ,50
C     SET FOR L = 2
   50 LEXP1 = 2
C     LEXP1 = 2**(L-1)
      LEXP = 8
C     LEXP = 2**(L+1)
      NPL = 2**MT
C     NPL = NP* 2**-L
   60 DO 130 L = 2,M
C     SPECIAL CASE- J = 0
      DO 80 I = 2,N2,LEXP
      I1 = I + LEXP1
      I2 = I1+ LEXP1
      I3 = I2+LEXP1
      T = A(I-1)
      A(I-1) = T +A(I2-1)
      A(I2-1) = T-A(I2-1)
      T = A(I)
      A(I) = T+A(I2)
      A(I2) = T-A(I2)
      T = -A(I3)
      TI = A(I3-1)
      A(I3-1) = A(I1-1) - T
      A(I3   ) = A(I1 )   - TI
      A(I1-1) = A(I1-1) +T
   80 A(I1)   = A(I1   )  +TI
      IF(L-2) 120,120,90
   90 KLAST = N2-LEXP
      JJ = NPL
      DO 110 J = 4,LEXP1,2
      NPJJ = NT-JJ
      UR = S(NPJJ)
      UI = S(JJ)
      ILAST = J+KLAST
      DO 100 I = J,ILAST,LEXP
      I1 = I+LEXP1
      I2 = I1+LEXP1
      I3 = I2+LEXP1
      T = A(I2-1)*UR-A(I2)*UI
      TI = A(I2-1)*UI+A(I2)*UR
      A(I2-1) = A(I-1)-T
      A(I2  ) = A(I   ) - TI
      A(I-1) = A(I-1)+T
      A(I)   = A(I)+TI
      T = -A(I3-1)*UI-A(I3)*UR
      TI = A(I3-1)*UR-A(I3)*UI
      A(I3-1) = A(I1-1)-T
      A(I3)   = A(I1  )-TI
      A(I1-1) = A(I1-1)+T
  100 A(I1)   = A(I1)   +TI
C     END OF I LOOP
  110 JJ = JJ+NPL
C     END OF J LOOP
  120 LEXP1 = 2*LEXP1
      LEXP = 2*LEXP
  130 NPL = NPL/2
C     END OF L LOOP
  140 IF(IFS)145,2,1
C     DOING FOURIER ANALYSIS. REPLACE A BY CONJUGATE.
  145 DO 150 I = 1,N
  150 A(2*I) = -A(2*I)
  160 GO TO 1
C     RETURN
C     MAKE TABLE OF S(J) = SIN(2*PI*J/NP),J = 1,2,....NT-1,NT = NP/4
  200 CONTINUE
      IF(MT) 260,260,205
  205 THETA = .7853981634
C     THETA = PI/2**(L+1)    FOR L = 1
  210 JSTEP = NT
C     JSTEP = 2**( MT-L+1 ) FOR L = 1
      JDIF = NT/2
C     JDIF = 2**(MT-L)  FOR L = 1
      S(JDIF) = SIN(THETA)
      IF (MT-2)260,220,220
  220 DO 250 L = 2,MT
      THETA = THETA/2.
      JSTEP2 = JSTEP
      JSTEP = JDIF
      JDIF = JDIF/2
      S(JDIF) = SIN(THETA)
      JC1 = NT-JDIF
      S(JC1) = COS(THETA)
      JLAST = NT-JSTEP2
      IF(JLAST-JSTEP)250,230,230
  230 DO 240 J = JSTEP,JLAST,JSTEP
      JC = NT-J
      JD = J+JDIF
  240 S(JD) = S(J)*S(JC1)+S(JDIF)*S(JC)
  250 CONTINUE
      MLAST = M
  260 IF(IFS)20,1,20
      END


