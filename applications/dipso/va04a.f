      SUBROUTINE VA04A
     :(X,E,N,F,ESCALE,IPRINT,ICON,MAXIT,IBUG,PROCEED)
      COMMON/UNIT/MTX,MTS,MT5,MT7,MT8,MT10
      COMMON /IDHELF/ ELFPRMPT
      DIMENSION W(500),X(1),E(1)
      LOGICAL PROCEED
      CHARACTER*10 ANS
      INTEGER IHX
      INTEGER ELFPRMPT
      DDMAG=0.1*ESCALE
      SCER=0.05/ESCALE
      JJ=N*N+N
      JJJ=JJ+N
      K=N+1
      NFCC=1
      IND=1
      INN=1
      DO 1 I=1,N
      DO 2 J=1,N
      W(K)=0.
      IF(I-J) 4,3,4
3     W(K)=ABS(E(I))
      W(I)=ESCALE
4     K=K+1
2      CONTINUE
1      CONTINUE
      ITERC=1
      ISGRAD=2
      CALL CALCFX(N,X,F,IBUG)
      FKEEP=ABS(F)+ABS(F)
5     ITONE=1
      FP=F
      SUM=0.
      IXP=JJ
      DO 6 I=1,N
      IXP=IXP+1
      W(IXP)=X(I)
6      CONTINUE
      IDIRN=N+1
      ILINE=1
7     DMAX=W(ILINE)
      DACC=DMAX*SCER
      DMAG=AMIN1(DDMAG,0.1*DMAX)
      DMAG=AMAX1(DMAG,20.*DACC)
      DDMAX=10.*DMAG
      GO TO (70,70,71),ITONE
70    DL=0.
      D=DMAG
      FPREV=F
      IS=5
      FA=F
      DA=DL
8     DD=D-DL
      DL=D
58    K=IDIRN
      DO 9 I=1,N
      X(I)=X(I)+DD*W(K)
      K=K+1
9      CONTINUE
      CALL CALCFX(N,X,F,IBUG)
C
      NFCC=NFCC+1
      GO TO (10,11,12,13,14,96),IS
14    IF(F-FA)15,16,24
16    IF(ABS(D)-DMAX) 17,17,18
17    D=D+D
      GO TO 8
18    IF(IBUG.EQ.1) WRITE(MTX,19)
      WRITE(6,19)
      GO TO 20
15    FB=F
      DB=D
      GO TO 21
24    FB=FA
      DB=DA
      FA=F
      DA=D
21    GO TO (83,23),ISGRAD
23    D=DB+DB-DA
      IS=1
      GO TO 8
83    D=0.5*(DA+DB-(FA-FB)/(DA-DB))
      IS=4
      IF((DA-D)*(D-DB)) 25,8,8
25    IS=1
      IF(ABS(D-DB)-DDMAX) 8,8,26
26    D=DB+SIGN(DDMAX,DB-DA)
      IS=1
      DDMAX=DDMAX+DDMAX
      DDMAG=DDMAG+DDMAG
      IF(DDMAX-DMAX) 8,8,27
27    DDMAX=DMAX
      GO TO 8
13    IF(F-FA) 28,23,23
28    FC=FB
      DC=DB
29    FB=F
      DB=D
      GO TO 30
12    IF(F-FB) 28,28,31
31    FA=F
      DA=D
      GO TO 30
11    IF(F-FB) 32,10,10
32    FA=FB
      DA=DB
      GO TO 29
71    DL=1.
      DDMAX=5.
      FA=FP
      DA=-1.
      FB=FHOLD
      DB=0.
      D=1.
10    FC=F
      DC=D
30    A=(DB-DC)*(FA-FC)
      B=(DC-DA)*(FB-FC)
      IF((A+B)*(DA-DC)) 33,33,34
33    FA=FB
      DA=DB
      FB=FC
      DB=DC
      GO TO 26
34    D=0.5*(A*(DB+DC)+B*(DA+DC))/(A+B)
      DI=DB
      FI=FB
      IF(FB-FC) 44,44,43
43    DI=DC
      FI=FC
44    GO TO (86,86,85),ITONE
85    ITONE=2
      GO TO 45
86    IF (ABS(D-DI)-DACC) 41,41,93
93    IF (ABS(D-DI)-0.03*ABS(D)) 41,41,45
45    IF ((DA-DC)*(DC-D)) 47,46,46
46    FA=FB
      DA=DB
      FB=FC
      DB=DC
      GO TO 25
47    IS=2
      IF((DB-D)*(D-DC)) 48,8,8
48    IS=3
      GO TO 8
41    F=FI
      D=DI-DL
      DD=SQRT((DC-DB)*(DC-DA)*(DA-DB)/(A+B))
      DO 49 I=1,N
      X(I)=X(I)+D*W(IDIRN)
      W(IDIRN)=DD*W(IDIRN)
      IDIRN=IDIRN+1
49    CONTINUE
      W(ILINE)=W(ILINE)/DD
      ILINE=ILINE+1
      IF(IPRINT-1) 51,50,51
C  50 IF(IBUG.EQ.1) WRITE(MTX,52)ITERC,NFCC,F,(X(I),I=1,N)
   50 IF(IBUG.EQ.1) WRITE(MTX,52)ITERC,F,(X(I),I=1,N)
C     WRITE(6,52) ITERC,NFCC,F,(X(I),I=1,N)
      WRITE(6,52) ITERC,F,(X(I),I=1,N)
      GO TO (51,53),IPRINT
51    GO TO (55,38),ITONE
55    IF(FPREV-F-SUM) 94,95,95
95    SUM=FPREV-F
      JIL=ILINE
94    IF(IDIRN-JJ) 7,7,84
84    GO TO (92,72),IND
92    FHOLD=F
      IS=6
      IXP=JJ
      DO 59 I=1,N
      IXP=IXP+1
      W(IXP)=X(I)-W(IXP)
59    CONTINUE
      DD=1.
      GO TO 58
96    GO TO (112,87),IND
112   IF(FP-F) 37,37,91
91    D=2.*(FP+F-2.*FHOLD)/(FP-F)**2
      IF(D*(FP-FHOLD-SUM)**2-SUM) 87,37,37
87    J=JIL*N+1
      IF(J-JJ) 60,60,61
60    DO 62 I=J,JJ
      K=I-N
      W(K)=W(I)
62    CONTINUE
      DO 97 I=JIL,N
      W(I-1)=W(I)
97    CONTINUE
61    IDIRN=IDIRN-N
      ITONE=3
      K=IDIRN
      IXP=JJ
      AAA=0.
      DO 65 I=1,N
      IXP=IXP+1
      W(K)=W(IXP)
      IF(AAA-ABS(W(K)/E(I))) 66,67,67
66    AAA=ABS(W(K)/E(I))
67    K=K+1
65    CONTINUE
      DDMAG=1.
      W(N)=ESCALE/AAA
      ILINE=N
      GO TO 7
37    IXP=JJ
      AAA=0.
      F=FHOLD
      DO 99 I=1,N
      IXP=IXP+1
      X(I)=X(I)-W(IXP)
      IF(AAA*ABS(E(I))-ABS(W(IXP))) 98,99,99
98    AAA=ABS(W(IXP)/E(I))
99    CONTINUE
      GO TO 72
38    AAA=AAA*(1.+DI)
      GO TO (72,106),IND
72    IF(IPRINT-2) 53,50,50
53    GO TO (109,88),IND
109   IF(AAA-0.1) 89,89,76
89    GO TO (20,116),ICON
116   IND=2
      GO TO (100,101),INN
100   INN=2
      K=JJJ
      DO 102 I=1,N
      K=K+1
      W(K)=X(I)
      X(I)=X(I)+10.*E(I)
102   CONTINUE
      FKEEP=F
      CALL CALCFX(N,X,F,IBUG)
      NFCC=NFCC+1
      DDMAG=0.
      GO TO 108
76    IF(F-FP) 35,78,78
78    IF(IBUG.EQ.1) WRITE(MTX,80)
      WRITE(6,80)
      GO TO 20
88    IND=1
35    DDMAG=0.4*SQRT(FP-F)
      ISGRAD=1
108   ITERC=ITERC+1
!
!   Updated to allow exit
!
!      IF (.NOT.PROCEED .AND. ITERC.LT.MAXIT) THEN
       IF (.NOT.PROCEED .AND. ELFPRMPT.GT.0) THEN
 1010     CONTINUE
          WRITE (*,
     :    '(''   ELFOPT:  continue, quit, or stop prompting'',
     :    '' (C/Q/S)?  '',$)')
 1000     CONTINUE
          READ (*,'(A10)',IOSTAT=IHX) ANS
          IF (IHX.NE.0) THEN
             WRITE (*,
     :    '(''   ELFOPT:  error reading, re-enter C Q or S:'',
     :    ''           '',$)')
             GO TO 1000
          ENDIF
          CALL SSTRIP(ANS)
          CALL DTOUPP(ANS)
          IF (ANS(1:1).EQ.'C') THEN
             CONTINUE
          ELSEIF (ANS(1:1).EQ.'Q') THEN
             MAXIT = 0
          ELSEIF (ANS(1:1).EQ.'S') THEN
             PROCEED = .TRUE.
          ELSE
             GO TO 1010
          ENDIF
       ENDIF
!
      IF(ITERC-MAXIT) 5,5,81
81    IF(IBUG.EQ.1) WRITE(MTX,82)MAXIT
      WRITE(6,82) MAXIT
      IF(F-FKEEP) 20,20,110
110   F=FKEEP
      DO 111 I=1,N
      JJJ=JJJ+1
      X(I)=W(JJJ)
111   CONTINUE
      GO TO 20
101   JIL=1
      FP=FKEEP
      IF(F-FKEEP) 105,78,104
104   JIL=2
      FP=F
      F=FKEEP
105   IXP=JJ
      DO 113 I=1,N
      IXP=IXP+1
      K=IXP+N
      GO TO (114,115),JIL
114   W(IXP)=W(K)
      GO TO 113
115   W(IXP)=X(I)
      X(I)=W(K)
113   CONTINUE
      JIL=2
      GO TO 92
106   IF(AAA-0.1) 20,20,107
20    RETURN
107   INN=1
      GO TO 35
19    FORMAT(5X,44HVA04A MAXIMUM CHANGE DOES NOT ALTER FUNCTION)
C  52 FORMAT(/1X,9HIteration,I5,I11,16H Function values,
C    16X,3HF =,E21.14/(3E24.14))
   52 FORMAT(1H ,'Iter.',I4,'; F=',1PE12.4,'; Vals=',3E12.4/
     : (6E12.4))
80    FORMAT(5X,37HVA04A ACCURACY LIMITED BY ERRORS IN F)
82    FORMAT(I5,30H ITERATIONS COMPLETED BY VA04A)
      END
