*+  TIM_FPOWER - Computes power spectrum using FFT
      SUBROUTINE TIM_FPOWER(NDAT,DATA,NV,STATUS)
*    Description :
*     Calls one of two FFT routines to calculate the power spectrum of
*     a dataset, which overwrites the data array.
*     If there are 2**n data points then FFA8 is used - otherwise
*     the flexible (but slower) routine FOURT is called.
*     Normalisation is  power=F*conj(F)=(wave amplitude/2)**2
*     where F(j)=(1/N)*sum{f(k)*exp(-i*2*pi*j*k/N)}.
*    Parameters :
*    Method :
*     All arrays are mapped so there is no size limitation.
*    Deficiencies :
*     The code is not up to standard
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*     18 Dec 85: Original
*     10/6/88:   DYN_ routines used for workspace (pla)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
	INTEGER NDAT			! Number of data values
*    Import/Export :
	REAL DATA(NDAT)			! Data values, returns power spectrum
*    Export :
	INTEGER NV			! Number of power values
					! ( =INT(NDAT/2)+1 )
*    Status :
	INTEGER STATUS

*    Local variables :
	INTEGER I
	INTEGER NN			! Nearest 2**N to NDAT
	INTEGER NARR			! Array size for FOURT
	INTEGER DPTR			! Pointer to FOURT data array
	INTEGER WPTR			! Pointer to FOURT work array

	REAL    HF			! Last element of power spectrum

	LOGICAL FAST			! FFA8 to be used?

	CHARACTER*(DAT__SZLOC) DLOC	! Data array locator
	CHARACTER*(DAT__SZLOC) WLOC	! Work array locator

*-
*   Status check
      IF (STATUS.NE.SAI__OK) RETURN

*   Is NDAT a power of 2?
      NN   = 2**( NINT( ALOG10(REAL(NDAT)) / ALOG10(2.0) ) )
      FAST = NN .EQ. NDAT
      NV   = INT( NDAT / 2 ) + 1

      IF (FAST) THEN
*      Use FFA8
         CALL TIM_FFA8( DATA, NDAT )
         DATA(1) = (DATA(1) / NDAT)**2
         HF      =(DATA(2)/NDAT)**2

         DO I = 2, NV - 1
	    DATA(I)=(DATA(2*I-1)/NDAT)**2+(DATA(2*I)/NDAT)**2

         END DO
         DATA(NV) = HF

      ELSE
*      Use FOURT - set up workspace and 'complex' data array
         NARR = 2 * NDAT

*      Get workspace
         CALL DYN_MAPR( 1, NARR, DPTR, STATUS )
         CALL DYN_MAPR( 1, NARR, WPTR, STATUS )

*      Check status
         IF (STATUS .NE. SAI__OK) GOTO 99

*      Copy data to 'complex' array and transform
         CALL TIM_FPOWER_COPY(DATA,NDAT,%VAL(DPTR),NARR)
         CALL TIM_FOURT(%VAL(DPTR),NDAT,1,-1,0,%VAL(WPTR))

*      Recover power spectrum
         CALL TIM_FPOWER_POWER(%VAL(DPTR),NARR,DATA,NDAT)

*      Free work space
         CALL DYN_UNMAP( DPTR, STATUS )
         CALL DYN_UNMAP( WPTR, STATUS )

      END IF

*   Exit
 99   IF (STATUS.NE.SAI__OK) THEN
         CALL AST_REXIT( 'TIM_FPOWER', STATUS )
      END IF

      END


*+
      SUBROUTINE TIM_FPOWER_COPY(DATA,NDAT,CDATA,NARR)
*
*	Copies real array DATA into alternate elements of CDATA to produce
*	a 'complex' array CDATA.
*
*-
      IMPLICIT NONE

      INTEGER NDAT, NARR, I
      REAL DATA(NDAT),CDATA(NARR)

      DO I = 1, NDAT
         CDATA(2*I-1) = DATA(I)
         CDATA(2*I) = 0.0
      END DO

      END


*+
      SUBROUTINE TIM_FPOWER_POWER(CDATA,NARR,POWER,NDAT)
*
*	Computes power spectrum up to the Nyquist frequency from complex
*	data values stored in CDATA.
*	(Also divides by NDAT**2 to give correct normalization.)
*
*-
      IMPLICIT NONE
      INTEGER NARR, NDAT, NV, I
      REAL CDATA(NARR),POWER(NDAT)

      NV = INT( NDAT / 2 ) + 1

      DO I = 1, NV
         POWER(I)=(CDATA(2*I-1)/NDAT)**2+(CDATA(2*I)/NDAT)**2
      END DO

      END



*+ TIM_FFA8 - fast Fourier analysis Radix 8 method
      SUBROUTINE TIM_FFA8( B, NPTS )
*    Description :
*     Fast fourier analysiss:  radix 8 method
*     see bergland in IEEE Trans. Audio & Electroacoustics
*     Vol. AU-17, No. 2, pp138-144
*    Method :
*     B		Replaced by its transform.  B(1) is the DC leval, B(2) is
*		the high frequency term, B(odd index>1) imaginary term
*		B(even index>2) real term.
*    Deficiencies :
*     The code is NOT to standard
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
C      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER                NPTS                          ! Number of datapoints
*    Import-Export :
      REAL                   B(NPTS)                       ! Data to be transformed
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
*-
      M = NINT( LOG( REAL(NPTS) ) / LOG(2.0) )
      N = 2**M
      N8POW = M / 3

      IF(M-N8POW*3-1) 30,20,10
10         NN=4
           INT=N/NN
           CALL TIM_DR4TR(INT,B(1),B(INT+1),B(2*INT+1),B(3*INT+1))
           GOTO 40
20       NN=2
           INT=N/NN
           CALL TIM_DR2TR(INT,B(1),B(INT+1))
           GOTO 40
30       NN=1
C--- PERFORM RADIX 8 ITERATIONS
40      IF(N8POW.LE.0) GOTO 60
      DO 50 IT=1,N8POW
           NN=NN*8
           INT=N/NN
           CALL TIM_DR8TR(INT,NN,B(1),B(INT+1),B(2*INT+1),B(3*INT+1),
     1          B(4*INT+1),B(5*INT+1),B(6*INT+1),B(7*INT+1),
     2          B(1),B(INT+1),B(2*INT+1),B(3*INT+1),B(4*INT+1),
     3          B(5*INT+1),B(6*INT+1),B(7*INT+1))
50         CONTINUE
60    CALL TIM_DORD1(M,B)
      CALL TIM_DORD2(M,B)
70    RETURN
      END
C*********
      SUBROUTINE TIM_DR2TR(INT,B0,B1)
C     IMPLICIT REAL*8(A-H,O-Z)
            DIMENSION B0(2),B1(2)
            DO 100 K=1,INT
                  T=B0(K)+B1(K)
                  B1(K)=B0(K)-B1(K)
                  B0(K)=T
100               CONTINUE
            RETURN
      END
C*********
      SUBROUTINE TIM_DR4TR(INT,B0,B1,B2,B3)
C     IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION B0(2),B1(2),B2(2),B3(2)
      DO 200 K=1,INT
           R0=B0(K)+B2(K)
           R1=B1(K)+B3(K)
           B2(K)=B0(K)-B2(K)
           B3(K)=B1(K)-B3(K)
           B0(K)=R0+R1
200         B1(K)=R0-R1
      RETURN
      END
C*********
      SUBROUTINE TIM_DR8TR(INT,NN,BR0,BR1,BR2,BR3,BR4,BR5,BR6,BR7,
     1     BI0,BI1,BI2,BI3,BI4,BI5,BI6,BI7)
C     IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION L(15),BR0(2),BR1(2),BR2(2),BR3(2),BR4(2),BR5(2)
     1     ,BR6(2),BR7(2),BI0(2),BI1(2),BI2(2),BI3(2),BI4(2),BI5(2),
     2     BI6(2),BI7(2)
      EQUIVALENCE (L15,L(1)),(L14,L(2)),(L13,L(3)),(L12,L(4)),
     1     (L11,L(5)),(L10,L(6)),(L9,L(7)),(L8,L(8)),(L7,L(9)),
     2     (L6,L(10)),(L5,L(11)),(L4,L(12)),(L3,L(13)),(L2,L(14)),
     3     (L1,L(15))
      L(1)=NN/8
      DO 10 K=2,15
           IF(L(K-1)-2) 11,12,13
11               L(K-1)=2
12           L(K)=2
             GOTO 10
13         L(K)=L(K-1)/2
10         CONTINUE
      PIOVN=3.14159265358979323846D0/FLOAT(NN)
      P7=0.70710678118654752440D0
      C22=.92387953251128675613D0
      S22=.38268343236508977172D0
      JI=3
      JL=2
      JR=2
C
      DO 70 J1=2,L1,2
      DO 70 J2=J1,L2,L1
      DO 70 J3=J2,L3,L2
      DO 70 J4=J3,L4,L3
      DO 70 J5=J4,L5,L4
      DO 70 J6=J5,L6,L5
      DO 70 J7=J6,L7,L6
      DO 70 J8=J7,L8,L7
      DO 70 J9=J8,L9,L8
      DO 70 J10=J9,L10,L9
      DO 70 J11=J10,L11,L10
      DO 70 J12=J11,L12,L11
      DO 70 J13=J12,L13,L12
           DO 70 J14=J13,L14,L13
      DO 70 JTHET=J14,L15,L14
           TH2=JTHET-2
           IF(TH2) 71,71,76
71         DO 72 K=1,INT
                T0=BR0(K)+BR4(K)
                T1=BR1(K)+BR5(K)
                T2=BR2(K)+BR6(K)
                T3=BR3(K)+BR7(K)
               T4=BR0(K)-BR4(K)
                T5=BR1(K)-BR5(K)
                T6=BR2(K)-BR6(K)
                T7=BR3(K)-BR7(K)
                BR2(K)=T0-T2
                BR3(K)=T1-T3
                T0=T0+T2
                T1=T1+T3
                BR0(K)=T0+T1
                BR1(K)=T0-T1
                PR=P7*(T5-T7)
                PI=P7*(T5+T7)
                BR4(K)=T4+PR
                BR7(K)=T6+PI
                BR6(K)=T4-PR
                BR5(K)=PI-T6
72         CONTINUE
           IF(NN-8) 70,70,73
73         K0=INT*8+1
           KL=K0+INT-1
           DO 75 K=K0,KL
                PR=P7*(BI2(K)-BI6(K))
                PI=P7*(BI2(K)+BI6(K))
                TR0=BI0(K)+PR
                TI0=BI4(K)+PI
                TR2=BI0(K)-PR
                TI2=BI4(K)-PI
                PR=P7*(BI3(K)-BI7(K))
                PI=P7*(BI3(K)+BI7(K))
                TR1=BI1(K)+PR
                TI1=BI5(K)+PI
                TR3=BI1(K)-PR
                TI3=BI5(K)-PI
                PR=TR1*C22-TI1*S22
                PI=TI1*C22+TR1*S22
                BI0(K)=TR0+PR
                BI6(K)=TR0-PR
                BI7(K)=PI+TI0
                BI1(K)=PI-TI0
                PR=-TR3*S22-TI3*C22
                PI= TR3*C22-TI3*S22
                BI2(K)=TR2+PR
                BI4(K)=TR2-PR
                BI5(K)=TI2+PI
                BI3(K)=PI-TI2
75              CONTINUE
           GOTO 70
76         ARG=TH2*PIOVN
           C1=COS(ARG)
           S1=SIN(ARG)
           C2=C1*C1-S1*S1
           S2=C1*S1*2.0D0
           C3=C1*C2-S1*S2
           S3=C2*S1+S2*C1
           C4=C2*C2-S2*S2
           S4=C2*S2*2.0D0
           C5=C2*C3-S2*S3
           S5=C3*S2+S3*C2
           C6=C3*C3-S3*S3
           S6=C3*S3*2.0D0
           C7=C3*C4-S3*S4
           S7=C4*S3+S4*C3
           INT8=INT*8
           J0=JR*INT8+1
          K0=JI*INT8+1
           JLAST=J0+INT-1
           DO 77 J=J0,JLAST
                KS=K0+J-J0
                DO 77 K=KS,KS
                TR1=BR1(J)*C1-BI1(K)*S1
                TI1=BR1(J)*S1+BI1(K)*C1
                TR2=BR2(J)*C2-BI2(K)*S2
                TI2=BR2(J)*S2+BI2(K)*C2
                TR3=BR3(J)*C3-BI3(K)*S3
                TI3=BR3(J)*S3+BI3(K)*C3
                TR4=BR4(J)*C4-BI4(K)*S4
                TI4=BR4(J)*S4+BI4(K)*C4
                TR5=BR5(J)*C5-BI5(K)*S5
                TI5=BR5(J)*S5+BI5(K)*C5
                TR6=BR6(J)*C6-BI6(K)*S6
                TI6=BR6(J)*S6+BI6(K)*C6
                TR7=BR7(J)*C7-BI7(K)*S7
                TI7=BR7(J)*S7+BI7(K)*C7
                T0=BR0(J)+TR4
                T1=BI0(K)+TI4
                TR4=BR0(J)-TR4
                TI4=BI0(K)-TI4
                T2=TR1+TR5
                T3=TI1+TI5
                TR5=TR1-TR5
                TI5=TI1-TI5
                T4=TR2+TR6
                T5=TI2+TI6
                TR6=TR2-TR6
                TI6=TI2-TI6
                T6=TR3+TR7
                T7=TI3+TI7
                TR7=TR3-TR7
                TI7=TI3-TI7
                TR0=T0+T4
                TI0=T1+T5
                TR2=T0-T4
                TI2=T1-T5
                TR1=T2+T6
                TI1=T3+T7
             TR3=T2-T6
                TI3=T3-T7
                T0=TR4-TI6
                T1=TI4+TR6
                T4=TR4+TI6
                T5=TI4-TR6
                T2=TR5-TI7
                T3=TI5+TR7
                T6=TR5+TI7
                T7=TI5-TR7
                BR0(J)=TR0+TR1
                BI7(K)=TI0+TI1
                BI6(K)=TR0-TR1
                BR1(J)=TI1-TI0
                BR2(J)=TR2-TI3
                BI5(K)=TI2+TR3
                BI4(K)=TR2+TI3
                BR3(J)=TR3-TI2
                PR=P7*(T2-T3)
                PI=P7*(T2+T3)
                BR4(J)=T0+PR
                BI3(K)=T1+PI
                BI2(K)=T0-PR
                BR5(J)=PI-T1
                PR=-P7*(T6+T7)
                PI=P7*(T6-T7)
                BR6(J)=T4+PR
                BI1(K)=T5+PI
                BI0(K)=T4-PR
                BR7(J)=PI-T5
77         CONTINUE
           JR=JR+2
              JI=JI-2
           IF(JI-JL) 78,78,70
78              JI=2*JR-1
                JL=JR
70     CONTINUE
      RETURN
      END
C*********
      SUBROUTINE TIM_DORD1(M,B)
C     IMPLICIT REAL*8(A-H,O-Z)
C           INPLACE REORDERING SUBROUTINES, TIM_DORD1, AND TIM_DORD2
            DIMENSION B(2)
            K=4
            KL=2
            N=2**M
            DO 94 J=4,N,2
                  IF(K-J)92,92,91
91                      T=B(J)
                        B(J)=B(K)
                        B(K)=T
92                K=K-2
                  IF(K-KL) 93,93,94
93                      K=2*J
                        KL=J
94                CONTINUE
            RETURN
      END
C*********
      SUBROUTINE TIM_DORD2(M,B)
C     IMPLICIT REAL*8(A-H,O-Z)
C                 THE SECOND INPLACE REORDERING ROUTINE
            DIMENSION L(20),B(2)
      EQUIVALENCE (L19,L(1)),(L18,L(2)),(L17,L(3)),(L16,L(
     1    4)),(L15,L(5)),(L14,L(6)),(L13,L(7)),(L12,L(8)),(L11,L(
     1      9)),(L10,L(10)),(L9,L(11)),(L8,L(12)),(L7,L(13)),(L6,L(
     1      14)),(L5,L(15)),(L4,L(16)),(L3,L(17)),(L2,L(18)),(L1,L(19))
            N=2**M
100         L(1)=N
            DO 101 K=2,M
101               L(K)=L(K-1)/2
            DO 102 K=M,19
102               L(K+1)=2
            IJ=2
            DO 103 J1=2,L1,2
             DO 103 J2=J1,L2,L1
              DO 103 J3=J2,L3,L2
               DO 103 J4=J3,L4,L3
                DO 103 J5=J4,L5,L4
                 DO 103 J6=J5,L6,L5
                  DO 103 J7=J6,L7,L6
                   DO 103 J8=J7,L8,L7
                    DO 103 J9=J8,L9,L8
                     DO 103 J10=J9,L10,L9
                      DO 103 J11=J10,L11,L10
                       DO 103 J12=J11,L12,L11
                        DO 103 J13=J12,L13,L12
                         DO 103 J14=J13,L14,L13
                          DO 103 J15=J14,L15,L14
                           DO 103 J16=J15,L16,L15
                            DO 103 J17=J16,L17,L16
                             DO 103 J18=J17,L18,L17
                              DO 103 JI=J18,L19,L18
                                    IF(IJ-JI) 108,103,103
108                                       T=B(IJ-1)
                                          B(IJ-1)=B(JI-1)
                                          B(JI-1)=T
                                          T=B(IJ)
                                          B(IJ)=B(JI)
                                          B(JI)=T
103                                 IJ=IJ+2
            RETURN
      END




*+  TIM_FOURT - fast Fourier Transform
      SUBROUTINE TIM_FOURT(DATA,NN,NDIM,KSIGN,IFORM,WORK)
C
C     THE COOLEY-TUKEY FAST FOURIER TRANSFORM IN USASI BASIC FORTRAN
C
C     (ISIGN CHANGED TO KSIGN,AUG 10,1970, L.L. ISIGN IS A LIBRARY FUNCTION)
C     TRANSFORM(K1,K2,...) = SUM(DATA(J1,J2,...)*EXP(ISIGN*2*PI*SQRT(-1)
C     *((J1-1)*(K1-1)/NN(1)+(J2-1)*(K2-1)/NN(2)+...))), SUMMED FOR ALL
C     J1, K1 BETWEEN 1 AND NN(1), J2, K2 BETWEEN 1 AND NN(2), ETC.
C     THERE IS NO LIMIT TO THE NUMBER OF SUBSCRIPTS.  DATA IS A
C     MULTIDIMENSIONAL COMPLEX ARRAY WHOSE REAL AND IMAGINARY
C     PARTS ARE ADJACENT IN STORAGE, SUCH AS FORTRAN IV PLACES THEM.
C     IF ALL IMAGINARY PARTS ARE ZERO (DATA ARE DISGUISED REAL), SET
C     IFORM TO ZERO TO CUT THE RUNNING TIME BY UP TO FORTY PERCENT.
C     OTHERWISE, IFORM = +1.  THE LENGTHS OF ALL DIMENSIONS ARE
C     STORED IN ARRAY NN, OF LENGTH NDIM.  THEY MAY BE ANY POSITIVE
C     INTEGERS, THO THE PROGRAM RUNS FASTER ON COMPOSITE INTEGERS, AND
C     ESPECIALLY FAST ON NUMBERS RICH IN FACTORS OF TWO.  ISIGN IS +1
C     OR -1.  IF A -1 TRANSFORM IS FOLLOWED BY A +1 ONE (OR A +1
C     BY A -1) THE ORIGINAL DATA REAPPEAR, MULTIPLIED BY NTOT (=NN(1)*
C     NN(2)*...).  TRANSFORM VALUES ARE ALWAYS COMPLEX, AND ARE RETURNED
C     IN ARRAY DATA, REPLACING THE INPUT.  IN ADDITION, IF ALL
C     DIMENSIONS ARE NOT POWERS OF TWO, ARRAY WORK MUST BE SUPPLIED,
C     COMPLEX OF LENGTH EQUAL TO THE LARGEST NON 2**K DIMENSION.
C     OTHERWISE, REPLACE WORK BY ZERO IN THE CALLING SEQUENCE.
C     NORMAL FORTRAN DATA ORDERING IS EXPECTED, FIRST SUBSCRIPT VARYING
C     FASTEST.  ALL SUBSCRIPTS BEGIN AT ONE.
C
C     RUNNING TIME IS MUCH SHORTER THAN THE NAIVE NTOT**2, BEING
C     GIVEN BY THE FOLLOWING FORMULA.  DECOMPOSE NTOT INTO
C     2**K2 * 3**K3 * 5**K5 * ....  LET SUM2 = 2*K2, SUMF = 3*K3 + 5*K5
C     + ... AND NF = K3 + K5 + ....  THE TIME TAKEN BY A MULTI-
C     DIMENSIONAL TRANSFORM ON THESE NTOT DATA IS T = T0 + NTOT*(T1+
C     T2*SUM2+T3*SUMF+T4*NF).  ON THE CDC 3300 (FLOATING POINT ADD TIME
C     OF SIX MICROSECONDS), T = 3000 + NTOT*(500+43*SUM2+68*SUMF+
C     320*NF) MICROSECONDS ON COMPLEX DATA.  IN ADDITION, THE
C     ACCURACY IS GREATLY IMPROVED, AS THE RMS RELATIVE ERROR IS
C     BOUNDED BY 3*2**(-B)*SUM(FACTOR(J)**1.5), WHERE B IS THE NUMBER
C     OF BITS IN THE FLOATING POINT FRACTION AND FACTOR(J) ARE THE
C     PRIME FACTORS OF NTOT.
C
C     PROGRAM BY NORMAN BRENNER FROM THE BASIC PROGRAM BY CHARLES
C     RADER.  RALPH ALTER SUGGESTED THE IDEA FOR THE DIGIT REVERSAL.
C     MIT LINCOLN LABORATORY, AUGUST 1967.  THIS IS THE FASTEST AND MOST
C     VERSATILE VERSION OF THE FFT KNOWN TO THE AUTHOR.  SHORTER PRO-
C     GRAMS FOUR1 AND FOUR2 RESTRICT DIMENSION LENGTHS TO POWERS OF TWO.
C     SEE-- IEEE AUDIO TRANSACTIONS (JUNE 1967), SPECIAL ISSUE ON FFT.
C
C     THE DISCRETE FOUIER TRANSFORM  PLACES THREE RESTRICTIONS UPON THE
C     DATA.
C     1.  THE NUMBER OF INPUT DATA AND THE NUMBER OF TRANSFORM VALUES
C     MUST BE THE SAME.
C     2.  BOTH THE INPUT DATA AND THE TRANSFORM VALUES MUST REPRESENT
C     EQUISPACED POINTS IN THEIR RESPECTIVE DOMAINS OF TIME AND
C     FREQUENCY.  CALLING THESE SPACINGS DELTAT AND DELTAF, IT MUST BE
C     TRUE THAT DELTAF=2*PI/(NN(I)*DELTAT).  OF COURSE, DELTAT NEED NOT
C     BE THE SAME FOR EVERY DIMENSION.
C     3.  CONCEPTUALLY AT LEAST, THE INPUT DATA AND THE TRANSFORM OUTPUT
C     REPRESENT SINGLE CYCLES OF PERIODIC FUNCTIONS.
C
C     EXAMPLE 1.  THREE-DIMENSIONAL FORWARD FOURIER TRANSFORM OF A
C     COMPLEX ARRAY DIMENSIONED 32 BY 25 BY 13 IN FORTRAN IV.
C     DIMENSION DATA(32,25,13),WORK(50),NN(3)
C     COMPLEX DATA
C     DATA NN/32,25,13/
C     DO 1 I=1,32
C     DO 1 J=1,25
C     DO 1 K=1,13
C  1  DATA(I,J,K)=COMPLEX VALUE
C     CALL FOURT(DATA,NN,3,-1,1,WORK)
C
C     EXAMPLE 2.  ONE-DIMENSIONAL FORWARD TRANSFORM OF A REAL ARRAY OF
C     LENGTH 64 IN FORTRAN II.
C     DIMENSION DATA(2,64)
C     DO 2 I=1,64
C     DATA(1,I)=REAL PART
C  2  DATA(2,I)=0.
C     CALL FOURT(DATA,64,1,-1,0,0)
C
*-
      DIMENSION DATA(1),NN(1),IFACT(32),WORK(1)
      TWOPI=6.283185307
      IF(NDIM-1)920,1,1
1     NTOT=2
      DO 2 IDIM=1,NDIM
      IF(NN(IDIM))920,920,2
2     NTOT=NTOT*NN(IDIM)
C
C     MAIN LOOP FOR EACH DIMENSION
C
      NP1=2
      DO 910 IDIM=1,NDIM
      N=NN(IDIM)
      NP2=NP1*N
      IF(N-1)920,900,5
C
C     FACTOR N
C
5     M=N
      NTWO=NP1
      IF=1
      IDIV=2
10    IQUOT=M/IDIV
      IREM=M-IDIV*IQUOT
      IF(IQUOT-IDIV)50,11,11
11    IF(IREM)20,12,20
12    NTWO=NTWO+NTWO
      M=IQUOT
      GO TO 10
20    IDIV=3
30    IQUOT=M/IDIV
      IREM=M-IDIV*IQUOT
      IF(IQUOT-IDIV)60,31,31
31    IF(IREM)40,32,40
32    IFACT(IF)=IDIV
      IF=IF+1
      M=IQUOT
      GO TO 30
40    IDIV=IDIV+2
      GO TO 30
50    IF(IREM)60,51,60
51    NTWO=NTWO+NTWO
      GO TO 70
60    IFACT(IF)=M
C
C     SEPARATE FOUR CASES--
C1. COMPLEX TRANSFORM OR REAL TRANSFORM FOR THE 4TH, 5TH,ETC.
C   DIMENSIONS.
C2. REAL TRANSFORM FOR THE 2ND OR 3RD DIMENSION.  METHOD--
C   TRANSFORM HALF THE DATA, SUPPLYING THE OTHER HALF BY CON-
C   JUGATE SYMMETRY.
C3. REAL TRANSFORM FOR THE 1ST DIMENSION, N ODD.  METHOD--
C   TRANSFORM HALF THE DATA AT EACH STAGE, SUPPLYING THE OTHER
C   HALF BY CONJUGATE SYMMETRY.
C4. REAL TRANSFORM FOR THE 1ST DIMENSION, N EVEN.  METHOD--
C   TRANSFORM A COMPLEX ARRAY OF LENGTH N/2 WHOSE REAL PARTS
C   ARE THE EVEN NUMBERED REAL VALUES AND WHOSE IMAGINARY PARTS
C   ARE THE ODD NUMBERED REAL VALUES.  SEPARATE AND SUPPLY
C   THE SECOND HALF BY CONJUGATE SYMMETRY.
C
70    NON2=NP1*(NP2/NTWO)
      ICASE=1
      IF(IDIM-4)71,90,90
71    IF(IFORM)72,72,90
72    ICASE=2
      IF(IDIM-1)73,73,90
73    ICASE=3
      IF(NTWO-NP1)90,90,74
74    ICASE=4
      NTWO=NTWO/2
      N=N/2
      NP2=NP2/2
      NTOT=NTOT/2
      I=3
      DO 80 J=2,NTOT
      DATA(J)=DATA(I)
80    I=I+2
90    I1RNG=NP1
      IF(ICASE-2)100,95,100
95    I1RNG=NP0*(1+NPREV/2)
C
C     SHUFFLE ON THE FACTORS OF TWO IN N.  AS THE SHUFFLING
C     CAN BE DONE BY SIMPLE INTERCHANGE, NO WORKING ARRAY IS NEEDED
C
100   IF(NTWO-NP1)600,600,110
110   NP2HF=NP2/2
      J=1
      DO 150 I2=1,NP2,NON2
      IF(J-I2)120,130,130
120   I1MAX=I2+NON2-2
      DO 125 I1=I2,I1MAX,2
      DO 125 I3=I1,NTOT,NP2
      J3=J+I3-I2
      TEMPR=DATA(I3)
      TEMPI=DATA(I3+1)
      DATA(I3)=DATA(J3)
      DATA(I3+1)=DATA(J3+1)
      DATA(J3)=TEMPR
125   DATA(J3+1)=TEMPI
130   M=NP2HF
140   IF(J-M)150,150,145
145   J=J-M
      M=M/2
      IF(M-NON2)150,140,140
150   J=J+M
C
C     MAIN LOOP FOR FACTORS OF TWO.  PERFORM FOURIER TRANSFORMS OF
C     LENGTH FOUR, WITH ONE OF LENGTH TWO IF NEEDED.  THE TWIDDLE FACTOR
C     W=EXP(ISIGN*2*PI*SQRT(-1)*M/(4*MMAX)).  CHECK FOR W=ISIGN*SQRT(-1)
C     AND REPEAT FOR W=ISIGN*SQRT(-1)*CONJUGATE(W).
C
      NON2T=NON2+NON2
      IPAR=NTWO/NP1
310   IF(IPAR-2)350,330,320
320   IPAR=IPAR/4
      GO TO 310
330   DO 340 I1=1,I1RNG,2
      DO 340 J3=I1,NON2,NP1
      DO 340 K1=J3,NTOT,NON2T
      K2=K1+NON2
      TEMPR=DATA(K2)
      TEMPI=DATA(K2+1)
      DATA(K2)=DATA(K1)-TEMPR
      DATA(K2+1)=DATA(K1+1)-TEMPI
      DATA(K1)=DATA(K1)+TEMPR
340   DATA(K1+1)=DATA(K1+1)+TEMPI
350   MMAX=NON2
360   IF(MMAX-NP2HF)370,600,600
370   LMAX=MAX0(NON2T,MMAX/2)
      IF(MMAX-NON2)405,405,380
380   THETA=-TWOPI*FLOAT(NON2)/FLOAT(4*MMAX)
      IF(KSIGN)400,390,390
390   THETA=-THETA
400   WR=COS(THETA)
      WI=SIN(THETA)
      WSTPR=-2.*WI*WI
      WSTPI=2.*WR*WI
405   DO 570 L=NON2,LMAX,NON2T
      M=L
      IF(MMAX-NON2)420,420,410
410   W2R=WR*WR-WI*WI
      W2I=2.*WR*WI
      W3R=W2R*WR-W2I*WI
      W3I=W2R*WI+W2I*WR
420   DO 530 I1=1,I1RNG,2
      DO 530 J3=I1,NON2,NP1
      KMIN=J3+IPAR*M
      IF(MMAX-NON2)430,430,440
430   KMIN=J3
440   KDIF=IPAR*MMAX
450   KSTEP=4*KDIF
      DO 520 K1=KMIN,NTOT,KSTEP
      K2=K1+KDIF
      K3=K2+KDIF
      K4=K3+KDIF
      IF(MMAX-NON2)460,460,480
460   U1R=DATA(K1)+DATA(K2)
      U1I=DATA(K1+1)+DATA(K2+1)
      U2R=DATA(K3)+DATA(K4)
      U2I=DATA(K3+1)+DATA(K4+1)
      U3R=DATA(K1)-DATA(K2)
      U3I=DATA(K1+1)-DATA(K2+1)
      IF(KSIGN)470,475,475
470   U4R=DATA(K3+1)-DATA(K4+1)
      U4I=DATA(K4)-DATA(K3)
      GO TO 510
475   U4R=DATA(K4+1)-DATA(K3+1)
      U4I=DATA(K3)-DATA(K4)
      GO TO 510
480   T2R=W2R*DATA(K2)-W2I*DATA(K2+1)
      T2I=W2R*DATA(K2+1)+W2I*DATA(K2)
      T3R=WR*DATA(K3)-WI*DATA(K3+1)
      T3I=WR*DATA(K3+1)+WI*DATA(K3)
      T4R=W3R*DATA(K4)-W3I*DATA(K4+1)
      T4I=W3R*DATA(K4+1)+W3I*DATA(K4)
      U1R=DATA(K1)+T2R
      U1I=DATA(K1+1)+T2I
      U2R=T3R+T4R
      U2I=T3I+T4I
      U3R=DATA(K1)-T2R
      U3I=DATA(K1+1)-T2I
      IF(KSIGN)490,500,500
490   U4R=T3I-T4I
      U4I=T4R-T3R
      GO TO 510
500   U4R=T4I-T3I
      U4I=T3R-T4R
510   DATA(K1)=U1R+U2R
      DATA(K1+1)=U1I+U2I
      DATA(K2)=U3R+U4R
      DATA(K2+1)=U3I+U4I
      DATA(K3)=U1R-U2R
      DATA(K3+1)=U1I-U2I
      DATA(K4)=U3R-U4R
520   DATA(K4+1)=U3I-U4I
      KMIN=4*(KMIN-J3)+J3
      KDIF=KSTEP
      IF(KDIF-NP2)450,530,530
530   CONTINUE
      M=MMAX-M
      IF(KSIGN)540,550,550
540   TEMPR=WR
      WR=-WI
      WI=-TEMPR
      GO TO 560
550   TEMPR=WR
      WR=WI
      WI=TEMPR
560   IF(M-LMAX)565,565,410
565   TEMPR=WR
      WR=WR*WSTPR-WI*WSTPI+WR
570   WI=WI*WSTPR+TEMPR*WSTPI+WI
      IPAR=3-IPAR
      MMAX=MMAX+MMAX
      GO TO 360
C
C     MAIN LOOP FOR FACTORS NOT EQUAL TO TWO.  APPLY THE TWIDDLE FACTOR
C     W=EXP(ISIGN*2*PI*SQRT(-1)*(J2-1)*(J1-J2)/(NP2*IFP1)), THEN
C     PERFORM A FOURIER TRANSFORM OF LENGTH IFACT(IF), MAKING USE OF
C     CONJUGATE SYMMETRIES.
C
600   IF(NTWO-NP2)605,700,700
605   IFP1=NON2
      IF=1
      NP1HF=NP1/2
610   IFP2=IFP1/IFACT(IF)
      J1RNG=NP2
      IF(ICASE-3)612,611,612
611   J1RNG=(NP2+IFP1)/2
      J2STP=NP2/IFACT(IF)
      J1RG2=(J2STP+IFP2)/2
612   J2MIN=1+IFP2
      IF(IFP1-NP2)615,640,640
615   DO 635 J2=J2MIN,IFP1,IFP2
      THETA=-TWOPI*FLOAT(J2-1)/FLOAT(NP2)
      IF(KSIGN)625,620,620
620   THETA=-THETA
625   SINTH=SIN(THETA/2.)
      WSTPR=-2.*SINTH*SINTH
      WSTPI=SIN(THETA)
      WR=WSTPR+1.
      WI=WSTPI
      J1MIN=J2+IFP1
      DO 635 J1=J1MIN,J1RNG,IFP1
      I1MAX=J1+I1RNG-2
      DO 630 I1=J1,I1MAX,2
      DO 630 I3=I1,NTOT,NP2
      J3MAX=I3+IFP2-NP1
      DO 630 J3=I3,J3MAX,NP1
      TEMPR=DATA(J3)
      DATA(J3)=DATA(J3)*WR-DATA(J3+1)*WI
630   DATA(J3+1)=TEMPR*WI+DATA(J3+1)*WR
      TEMPR=WR
      WR=WR*WSTPR-WI*WSTPI+WR
635   WI=TEMPR*WSTPI+WI*WSTPR+WI
640   THETA=-TWOPI/FLOAT(IFACT(IF))
      IF(KSIGN)650,645,645
645   THETA=-THETA
650   SINTH=SIN(THETA/2.)
      WSTPR=-2.*SINTH*SINTH
      WSTPI=SIN(THETA)
      KSTEP=2*N/IFACT(IF)
      KRANG=KSTEP*(IFACT(IF)/2)+1
      DO 698 I1=1,I1RNG,2
      DO 698 I3=I1,NTOT,NP2
      DO 690 KMIN=1,KRANG,KSTEP
      J1MAX=I3+J1RNG-IFP1
      DO 680 J1=I3,J1MAX,IFP1
      J3MAX=J1+IFP2-NP1
      DO 680 J3=J1,J3MAX,NP1
      J2MAX=J3+IFP1-IFP2
      K=KMIN+(J3-J1+(J1-I3)/IFACT(IF))/NP1HF
      IF(KMIN-1)655,655,665
655   SUMR=0.
      SUMI=0.
      DO 660 J2=J3,J2MAX,IFP2
      SUMR=SUMR+DATA(J2)
660   SUMI=SUMI+DATA(J2+1)
      WORK(K)=SUMR
      WORK(K+1)=SUMI
      GO TO 680
665   KCONJ=K+2*(N-KMIN+1)
      J2=J2MAX
      SUMR=DATA(J2)
      SUMI=DATA(J2+1)
      OLDSR=0.
      OLDSI=0.
      J2=J2-IFP2
670   TEMPR=SUMR
      TEMPI=SUMI
      SUMR=TWOWR*SUMR-OLDSR+DATA(J2)
      SUMI=TWOWR*SUMI-OLDSI+DATA(J2+1)
      OLDSR=TEMPR
      OLDSI=TEMPI
      J2=J2-IFP2
      IF(J2-J3)675,675,670
675   TEMPR=WR*SUMR-OLDSR+DATA(J2)
      TEMPI=WI*SUMI
      WORK(K)=TEMPR-TEMPI
      WORK(KCONJ)=TEMPR+TEMPI
      TEMPR=WR*SUMI-OLDSI+DATA(J2+1)
      TEMPI=WI*SUMR
      WORK(K+1)=TEMPR+TEMPI
      WORK(KCONJ+1)=TEMPR-TEMPI
680   CONTINUE
      IF(KMIN-1)685,685,686
685   WR=WSTPR+1.
      WI=WSTPI
      GO TO 690
686   TEMPR=WR
      WR=WR*WSTPR-WI*WSTPI+WR
      WI=TEMPR*WSTPI+WI*WSTPR+WI
690   TWOWR=WR+WR
      IF(ICASE-3)692,691,692
691   IF(IFP1-NP2)695,692,692
692   K=1
      I2MAX=I3+NP2-NP1
      DO 693 I2=I3,I2MAX,NP1
      DATA(I2)=WORK(K)
      DATA(I2+1)=WORK(K+1)
693   K=K+2
      GO TO 698
C
C     COMPLETE A REAL TRANSFORM IN THE 1ST DIMENSION, N ODD, BY CON-
C     JUGATE SYMMETRIES AT EACH STAGE.
C
695   J3MAX=I3+IFP2-NP1
      DO 697 J3=I3,J3MAX,NP1
      J2MAX=J3+NP2-J2STP
      DO 697 J2=J3,J2MAX,J2STP
      J1MAX=J2+J1RG2-IFP2
      J1CNJ=J3+J2MAX+J2STP-J2
      DO 697 J1=J2,J1MAX,IFP2
      K=1+J1-I3
      DATA(J1)=WORK(K)
      DATA(J1+1)=WORK(K+1)
      IF(J1-J2)697,697,696
696   DATA(J1CNJ)=WORK(K)
      DATA(J1CNJ+1)=-WORK(K+1)
697   J1CNJ=J1CNJ-IFP2
698   CONTINUE
      IF=IF+1
      IFP1=IFP2
      IF(IFP1-NP1)700,700,610
C
C     COMPLETE A REAL TRANSFORM IN THE 1ST DIMENSION, N EVEN, BY CON-
C     JUGATE SYMMETRIES.
C
700   GO TO (900,800,900,701),ICASE
701   NHALF=N
      N=N+N
      THETA=-TWOPI/FLOAT(N)
      IF(KSIGN)703,702,702
702   THETA=-THETA
703   SINTH=SIN(THETA/2.)
      WSTPR=-2.*SINTH*SINTH
      WSTPI=SIN(THETA)
      WR=WSTPR+1.
      WI=WSTPI
      IMIN=3
      JMIN=2*NHALF-1
      GO TO 725
710   J=JMIN
      DO 720 I=IMIN,NTOT,NP2
      SUMR=(DATA(I)+DATA(J))/2.
      SUMI=(DATA(I+1)+DATA(J+1))/2.
      DIFR=(DATA(I)-DATA(J))/2.
      DIFI=(DATA(I+1)-DATA(J+1))/2.
      TEMPR=WR*SUMI+WI*DIFR
      TEMPI=WI*SUMI-WR*DIFR
      DATA(I)=SUMR+TEMPR
      DATA(I+1)=DIFI+TEMPI
      DATA(J)=SUMR-TEMPR
      DATA(J+1)=-DIFI+TEMPI
720   J=J+NP2
      IMIN=IMIN+2
      JMIN=JMIN-2
      TEMPR=WR
      WR=WR*WSTPR-WI*WSTPI+WR
      WI=TEMPR*WSTPI+WI*WSTPR+WI
725   IF(IMIN-JMIN)710,730,740
730   IF(KSIGN)731,740,740
731   DO 735 I=IMIN,NTOT,NP2
735   DATA(I+1)=-DATA(I+1)
740   NP2=NP2+NP2
      NTOT=NTOT+NTOT
      J=NTOT+1
      IMAX=NTOT/2+1
745   IMIN=IMAX-2*NHALF
      I=IMIN
      GO TO 755
750   DATA(J)=DATA(I)
      DATA(J+1)=-DATA(I+1)
755   I=I+2
      J=J-2
      IF(I-IMAX)750,760,760
760   DATA(J)=DATA(IMIN)-DATA(IMIN+1)
      DATA(J+1)=0.
      IF(I-J)770,780,780
765   DATA(J)=DATA(I)
      DATA(J+1)=DATA(I+1)
770   I=I-2
      J=J-2
      IF(I-IMIN)775,775,765
775   DATA(J)=DATA(IMIN)+DATA(IMIN+1)
      DATA(J+1)=0.
      IMAX=IMIN
      GO TO 745
780   DATA(1)=DATA(1)+DATA(2)
      DATA(2)=0.
      GO TO 900
C
C     COMPLETE A REAL TRANSFORM FOR THE 2ND OR 3RD DIMENSION BY
C     CONJUGATE SYMMETRIES.
C
800   IF(I1RNG-NP1)805,900,900
805   DO 860 I3=1,NTOT,NP2
      I2MAX=I3+NP2-NP1
      DO 860 I2=I3,I2MAX,NP1
      IMIN=I2+I1RNG
      IMAX=I2+NP1-2
      JMAX=2*I3+NP1-IMIN
      IF(I2-I3)820,820,810
810   JMAX=JMAX+NP2
820   IF(IDIM-2)850,850,830
830   J=JMAX+NP0
      DO 840 I=IMIN,IMAX,2
      DATA(I)=DATA(J)
      DATA(I+1)=-DATA(J+1)
840   J=J-2
850   J=JMAX
      DO 860 I=IMIN,IMAX,NP0
      DATA(I)=DATA(J)
      DATA(I+1)=-DATA(J+1)
860   J=J-NP0
C
C     END OF LOOP ON EACH DIMENSION
C
900   NP0=NP1
      NP1=NP2
910   NPREV=N
920   RETURN
      END
