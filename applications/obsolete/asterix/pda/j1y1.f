      SUBROUTINE CALJY1(ARG,RESULT,JINT)
C---------------------------------------------------------------------
C
C This packet computes first-order Bessel functions of the first and
C   second kind (J1 and Y1), for real arguments X, where 0 < X <= XMAX
C   for Y1, and |X| <= XMAX for J1.  It contains two function-type
C   subprograms,  BESJ1  and  BESY1,  and one subroutine-type
C   subprogram,  CALJY1.  The calling statements for the primary
C   entries are:
C
C           Y = BESJ1(X)
C   and
C           Y = BESY1(X),
C
C   where the entry points correspond to the functions J1(X) and Y1(X),
C   respectively.  The routine  CALJY1  is intended for internal packet
C   use only, all computations within the packet being concentrated in
C   this one routine.  The function subprograms invoke  CALJY1  with
C   the statement
C           CALL CALJY1(ARG,RESULT,JINT),
C   where the parameter usage is as follows:
C
C      Function                  Parameters for CALJY1
C       call              ARG             RESULT          JINT
C
C     BESJ1(ARG)     |ARG| .LE. XMAX       J1(ARG)          0
C     BESY1(ARG)   0 .LT. ARG .LE. XMAX    Y1(ARG)          1
C
C   The main computation uses unpublished minimax rational
C   approximations for X .LE. 8.0, and an approximation from the
C   book  Computer Approximations  by Hart, et. al., Wiley and Sons,
C   New York, 1968, for arguments larger than 8.0   Part of this
C   transportable packet is patterned after the machine-dependent
C   FUNPACK program BESJ1(X), but cannot match that version for
C   efficiency or accuracy.  This version uses rational functions
C   that are theoretically accurate to at least 18 significant decimal
C   digits for X <= 8, and at least 18 decimal places for X > 8.  The
C   accuracy achieved depends on the arithmetic system, the compiler,
C   the intrinsic functions, and proper selection of the machine-
C   dependent constants.
C
C*******************************************************************
C
C Explanation of machine-dependent constants
C
C   XINF   = largest positive machine number
C   XMAX   = largest acceptable argument.  The functions AINT, SIN
C            and COS must perform properly for  ABS(X) .LE. XMAX.
C            We recommend that XMAX be a small integer multiple of
C            sqrt(1/eps), where eps is the smallest positive number
C            such that  1+eps > 1.
C   XSMALL = positive argument such that  1.0-(1/2)(X/2)**2 = 1.0
C            to machine precision for all  ABS(X) .LE. XSMALL.
C            We recommend that  XSMALL < sqrt(eps)/beta, where beta
C            is the floating-point radix (usually 2 or 16).
C
C     Approximate values for some important machines are
C
C                          eps      XMAX     XSMALL      XINF
C
C  CDC 7600      (S.P.)  7.11E-15  1.34E+08  2.98E-08  1.26E+322
C  CRAY-1        (S.P.)  7.11E-15  1.34E+08  2.98E-08  5.45E+2465
C  IBM PC (8087) (S.P.)  5.96E-08  8.19E+03  1.22E-04  3.40E+38
C  IBM PC (8087) (D.P.)  1.11D-16  2.68D+08  3.72D-09  1.79D+308
C  IBM 195       (D.P.)  2.22D-16  6.87D+09  9.09D-13  7.23D+75
C  UNIVAC 1108   (D.P.)  1.73D-18  4.30D+09  2.33D-10  8.98D+307
C  VAX 11/780    (D.P.)  1.39D-17  1.07D+09  9.31D-10  1.70D+38
C
C*******************************************************************
C*******************************************************************
C
C Error Returns
C
C  The program returns the value zero for  X .GT. XMAX, and returns
C    -XINF when BESLY1 is called with a negative or zero argument.
C
C
C Intrinsic functions required are:
C
C     ABS, AINT, COS, LOG, SIN, SQRT
C
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C  Latest modification: November 10, 1987
C
C--------------------------------------------------------------------
      INTEGER I,JINT
      DIMENSION PJ0(7),PJ1(8),PLG(4),PY0(7),PY1(9),P0(6),P1(6),
     1          QJ0(5),QJ1(7),QLG(4),QY0(6),QY1(8),Q0(6),Q1(6)
CS    REAL
      DOUBLE PRECISION
     1   ARG,AX,DOWN,EIGHT,FOUR,HALF,PI2,PJ0,PJ1,PLG,PROD,PY0,
     2   PY1,P0,P1,P17,QJ0,QJ1,QLG,QY0,QY1,Q0,Q1,RESJ,RESULT,
     3   RTPI2,R0,R1,THROV8,TWOPI,TWOPI1,TWOPI2,TWO56,UP,W,WSQ,
     4   XDEN,XINF,XMAX,XNUM,XSMALL,XJ0,XJ1,XJ01,XJ02,XJ11,XJ12,
     5   XY,XY0,XY01,XY02,XY1,XY11,XY12,Z,ZERO,ZSQ
C-------------------------------------------------------------------
C  Mathematical constants
C-------------------------------------------------------------------
CS    DATA EIGHT/8.0E0/,
CS   1     FOUR/4.0E0/,HALF/0.5E0/,THROV8/0.375E0/,
CS   2     PI2/6.3661977236758134308E-1/,P17/1.716E-1/
CS   3     TWOPI/6.2831853071795864769E+0/,ZERO/0.0E0/,
CS   4     TWOPI1/6.28125E0/,TWOPI2/1.9353071795864769253E-03/
CS   5     TWO56/256.0E+0/,RTPI2/7.9788456080286535588E-1/
      DATA EIGHT/8.0D0/,
     1     FOUR/4.0D0/,HALF/0.5D0/,THROV8/0.375D0/,
     2     PI2/6.3661977236758134308D-1/,P17/1.716D-1/
     3     TWOPI/6.2831853071795864769D+0/,ZERO/0.0D0/,
     4     TWOPI1/6.28125D0/,TWOPI2/1.9353071795864769253D-03/
     5     TWO56/256.0D+0/,RTPI2/7.9788456080286535588D-1/
C-------------------------------------------------------------------
C  Machine-dependent constants
C-------------------------------------------------------------------
CS    DATA XMAX/8.19E+03/,XSMALL/1.22E-09/,XINF/1.7E+38/
      DATA XMAX/1.07D+09/,XSMALL/9.31D-10/,XINF/1.7D+38/
C-------------------------------------------------------------------
C  Zeroes of Bessel functions
C-------------------------------------------------------------------
CS    DATA XJ0/3.8317059702075123156E+0/,XJ1/7.0155866698156187535E+0/,
CS   1     XY0/2.1971413260310170351E+0/,XY1/5.4296810407941351328E+0/,
CS   2     XJ01/ 981.0E+0/, XJ02/-3.2527979248768438556E-04/,
CS   3     XJ11/1796.0E+0/, XJ12/-3.8330184381246462950E-05/,
CS   4     XY01/ 562.0E+0/, XY02/ 1.8288260310170351490E-03/,
CS   5     XY11/1390.0E+0/, XY12/-6.4592058648672279948E-06/
      DATA XJ0/3.8317059702075123156D+0/,XJ1/7.0155866698156187535D+0/,
     1     XY0/2.1971413260310170351D+0/,XY1/5.4296810407941351328D+0/,
     2     XJ01/ 981.0D+0/, XJ02/-3.2527979248768438556D-04/,
     3     XJ11/1796.0D+0/, XJ12/-3.8330184381246462950D-05/,
     4     XY01/ 562.0D+0/, XY02/ 1.8288260310170351490D-03/,
     5     XY11/1390.0D+0/, XY12/-6.4592058648672279948D-06/
C-------------------------------------------------------------------
C  Coefficients for rational approximation to ln(x/a)
C--------------------------------------------------------------------
CS    DATA PLG/-2.4562334077563243311E+01,2.3642701335621505212E+02,
CS   1         -5.4989956895857911039E+02,3.5687548468071500413E+02/
CS    DATA QLG/-3.5553900764052419184E+01,1.9400230218539473193E+02,
CS   1         -3.3442903192607538956E+02,1.7843774234035750207E+02/
      DATA PLG/-2.4562334077563243311D+01,2.3642701335621505212D+02,
     1         -5.4989956895857911039D+02,3.5687548468071500413D+02/
      DATA QLG/-3.5553900764052419184D+01,1.9400230218539473193D+02,
     1         -3.3442903192607538956D+02,1.7843774234035750207D+02/
C-------------------------------------------------------------------
C  Coefficients for rational approximation of
C  J1(X) / (X * (X**2 - XJ0**2)),  XSMALL  <  |X|  <=  4.0
C--------------------------------------------------------------------
CS    DATA PJ0/9.8062904098958257677E+05,-1.1548696764841276794E+08,
CS   1       6.6781041261492395835E+09,-1.4258509801366645672E+11,
CS   2      -4.4615792982775076130E+03, 1.0650724020080236441E+01,
CS   3      -1.0767857011487300348E-02/
CS    DATA QJ0/5.9117614494174794095E+05, 2.0228375140097033958E+08,
CS   1       4.2091902282580133541E+10, 4.1868604460820175290E+12,
CS   2       1.0742272239517380498E+03/
      DATA PJ0/9.8062904098958257677D+05,-1.1548696764841276794D+08,
     1       6.6781041261492395835D+09,-1.4258509801366645672D+11,
     2      -4.4615792982775076130D+03, 1.0650724020080236441D+01,
     3      -1.0767857011487300348D-02/
      DATA QJ0/5.9117614494174794095D+05, 2.0228375140097033958D+08,
     1       4.2091902282580133541D+10, 4.1868604460820175290D+12,
     2       1.0742272239517380498D+03/
C-------------------------------------------------------------------
C  Coefficients for rational approximation of
C  J1(X) / (X * (X**2 - XJ1**2)),  4.0  <  |X|  <=  8.0
C-------------------------------------------------------------------
CS    DATA PJ1/4.6179191852758252280E+00,-7.1329006872560947377E+03,
CS   1       4.5039658105749078904E+06,-1.4437717718363239107E+09,
CS   2       2.3569285397217157313E+11,-1.6324168293282543629E+13,
CS   3       1.1357022719979468624E+14, 1.0051899717115285432E+15/
CS    DATA QJ1/1.1267125065029138050E+06, 6.4872502899596389593E+08,
CS   1       2.7622777286244082666E+11, 8.4899346165481429307E+13,
CS   2       1.7128800897135812012E+16, 1.7253905888447681194E+18,
CS   3       1.3886978985861357615E+03/
      DATA PJ1/4.6179191852758252280D+00,-7.1329006872560947377D+03,
     1       4.5039658105749078904D+06,-1.4437717718363239107D+09,
     2       2.3569285397217157313D+11,-1.6324168293282543629D+13,
     3       1.1357022719979468624D+14, 1.0051899717115285432D+15/
      DATA QJ1/1.1267125065029138050D+06, 6.4872502899596389593D+08,
     1       2.7622777286244082666D+11, 8.4899346165481429307D+13,
     2       1.7128800897135812012D+16, 1.7253905888447681194D+18,
     3       1.3886978985861357615D+03/
C-------------------------------------------------------------------
C  Coefficients for rational approximation of
C    (Y1(X) - 2 LN(X/XY0) J1(X)) / (X**2 - XY0**2),
C        XSMALL  <  |X|  <=  4.0
C--------------------------------------------------------------------
CS    DATA PY0/2.2157953222280260820E+05,-5.9157479997408395984E+07,
CS   1         7.2144548214502560419E+09,-3.7595974497819597599E+11,
CS   2         5.4708611716525426053E+12, 4.0535726612579544093E+13,
CS   3        -3.1714424660046133456E+02/
CS    DATA QY0/8.2079908168393867438E+02, 3.8136470753052572164E+05,
CS   1         1.2250435122182963220E+08, 2.7800352738690585613E+10,
CS   2         4.1272286200406461981E+12, 3.0737873921079286084E+14/
      DATA PY0/2.2157953222280260820D+05,-5.9157479997408395984D+07,
     1         7.2144548214502560419D+09,-3.7595974497819597599D+11,
     2         5.4708611716525426053D+12, 4.0535726612579544093D+13,
     3        -3.1714424660046133456D+02/
      DATA QY0/8.2079908168393867438D+02, 3.8136470753052572164D+05,
     1         1.2250435122182963220D+08, 2.7800352738690585613D+10,
     2         4.1272286200406461981D+12, 3.0737873921079286084D+14/
C--------------------------------------------------------------------
C  Coefficients for rational approximation of
C    (Y1(X) - 2 LN(X/XY1) J1(X)) / (X**2 - XY1**2),
C        4.0  <  |X|  <=  8.0
C--------------------------------------------------------------------
CS    DATA PY1/ 1.9153806858264202986E+06,-1.1957961912070617006E+09,
CS   1          3.7453673962438488783E+11,-5.9530713129741981618E+13,
CS   2          4.0686275289804744814E+15,-2.3638408497043134724E+16,
CS   3         -5.6808094574724204577E+18, 1.1514276357909013326E+19,
CS   4         -1.2337180442012953128E+03/
CS    DATA QY1/ 1.2855164849321609336E+03, 1.0453748201934079734E+06,
CS   1          6.3550318087088919566E+08, 3.0221766852960403645E+11,
CS   2          1.1187010065856971027E+14, 3.0837179548112881950E+16,
CS   3          5.6968198822857178911E+18, 5.3321844313316185697E+20/
      DATA PY1/ 1.9153806858264202986D+06,-1.1957961912070617006D+09,
     1          3.7453673962438488783D+11,-5.9530713129741981618D+13,
     2          4.0686275289804744814D+15,-2.3638408497043134724D+16,
     3         -5.6808094574724204577D+18, 1.1514276357909013326D+19,
     4         -1.2337180442012953128D+03/
      DATA QY1/ 1.2855164849321609336D+03, 1.0453748201934079734D+06,
     1          6.3550318087088919566D+08, 3.0221766852960403645D+11,
     2          1.1187010065856971027D+14, 3.0837179548112881950D+16,
     3          5.6968198822857178911D+18, 5.3321844313316185697D+20/
C-------------------------------------------------------------------
C  Coefficients for Hart,s approximation,  |X| > 8.0
C-------------------------------------------------------------------
CS    DATA P0/-1.0982405543459346727E+05,-1.5235293511811373833E+06,
CS   1         -6.6033732483649391093E+06,-9.9422465050776411957E+06,
CS   2         -4.4357578167941278571E+06,-1.6116166443246101165E+03/
CS    DATA Q0/-1.0726385991103820119E+05,-1.5118095066341608816E+06,
CS   1         -6.5853394797230870728E+06,-9.9341243899345856590E+06,
CS   2         -4.4357578167941278568E+06,-1.4550094401904961825E+03/
CS    DATA P1/ 1.7063754290207680021E+03, 1.8494262873223866797E+04,
CS   1          6.6178836581270835179E+04, 8.5145160675335701966E+04,
CS   2          3.3220913409857223519E+04, 3.5265133846636032186E+01/
CS    DATA Q1/ 3.7890229745772202641E+04, 4.0029443582266975117E+05,
CS   1          1.4194606696037208929E+06, 1.8194580422439972989E+06,
CS   2          7.0871281941028743574E+05, 8.6383677696049909675E+02/
      DATA P0/-1.0982405543459346727D+05,-1.5235293511811373833D+06,
     1         -6.6033732483649391093D+06,-9.9422465050776411957D+06,
     2         -4.4357578167941278571D+06,-1.6116166443246101165D+03/
      DATA Q0/-1.0726385991103820119D+05,-1.5118095066341608816D+06,
     1         -6.5853394797230870728D+06,-9.9341243899345856590D+06,
     2         -4.4357578167941278568D+06,-1.4550094401904961825D+03/
      DATA P1/ 1.7063754290207680021D+03, 1.8494262873223866797D+04,
     1          6.6178836581270835179D+04, 8.5145160675335701966D+04,
     2          3.3220913409857223519D+04, 3.5265133846636032186D+01/
      DATA Q1/ 3.7890229745772202641D+04, 4.0029443582266975117D+05,
     1          1.4194606696037208929D+06, 1.8194580422439972989D+06,
     2          7.0871281941028743574D+05, 8.6383677696049909675D+02/
C-------------------------------------------------------------------
C  Check for error conditions
C-------------------------------------------------------------------
      AX = ABS(ARG)
      IF ((JINT .EQ. 1) .AND. ((ARG .LE. ZERO) .OR.
     1   ((ARG .LT. HALF) .AND. (AX*XINF .LT. PI2)))) THEN
            RESULT = -XINF
            GO TO 2000
         ELSE IF (AX .GT. XMAX) THEN
            RESULT = ZERO
            GO TO 2000
      END IF
      IF (AX .GT. EIGHT) THEN
            GO TO 800
         ELSE IF (AX .LE. XSMALL) THEN
            IF (JINT .EQ. 0) THEN
                  RESULT = ARG * HALF
               ELSE
                  RESULT = -PI2 / AX
            END IF
            GO TO 2000
      END IF
C-------------------------------------------------------------------
C  Calculate J1 for appropriate interval, preserving
C     accuracy near the zero of J1
C-------------------------------------------------------------------
      ZSQ = AX * AX
      IF (AX .LE. FOUR) THEN
            XNUM = (PJ0(7) * ZSQ + PJ0(6)) * ZSQ + PJ0(5)
            XDEN = ZSQ + QJ0(5)
            DO 50 I = 1, 4
               XNUM = XNUM * ZSQ + PJ0(I)
               XDEN = XDEN * ZSQ + QJ0(I)
   50       CONTINUE
            PROD = ARG * ((AX - XJ01/TWO56) - XJ02) * (AX + XJ0)
         ELSE
            XNUM = PJ1(1)
            XDEN = (ZSQ + QJ1(7)) * ZSQ + QJ1(1)
            DO 220 I = 2, 6
               XNUM = XNUM * ZSQ + PJ1(I)
               XDEN = XDEN * ZSQ + QJ1(I)
  220       CONTINUE
            XNUM = XNUM * (AX - EIGHT) * (AX + EIGHT) + PJ1(7)
            XNUM = XNUM * (AX - FOUR) * (AX + FOUR) + PJ1(8)
            PROD = ARG * ((AX - XJ11/TWO56) - XJ12) * (AX + XJ1)
      END IF
      RESULT = PROD * (XNUM / XDEN)
      IF (JINT .EQ. 0) GO TO 2000
C-------------------------------------------------------------------
C  Calculate Y1.  First find  RESJ = pi/2 ln(x/xn) J1(x),
C    where xn is a zero of Y1
C-------------------------------------------------------------------
      IF (AX .LE. FOUR) THEN
            UP = (AX-XY01/TWO56)-XY02
            XY = XY0
         ELSE
            UP = (AX-XY11/TWO56)-XY12
            XY = XY1
      END IF
      DOWN = AX + XY
      IF (ABS(UP) .LT. P17*DOWN) THEN
            W = UP/DOWN
            WSQ = W*W
            XNUM = PLG(1)
            XDEN = WSQ + QLG(1)
            DO 320 I = 2, 4
               XNUM = XNUM*WSQ + PLG(I)
               XDEN = XDEN*WSQ + QLG(I)
  320       CONTINUE
            RESJ = PI2 * RESULT * W * XNUM/XDEN
         ELSE
            RESJ = PI2 * RESULT * LOG(AX/XY)
      END IF
C-------------------------------------------------------------------
C  Now calculate Y1 for appropriate interval, preserving
C     accuracy near the zero of Y1
C-------------------------------------------------------------------
      IF (AX .LE. FOUR) THEN
            XNUM = PY0(7) * ZSQ + PY0(1)
            XDEN = ZSQ + QY0(1)
            DO 340 I = 2, 6
               XNUM = XNUM * ZSQ + PY0(I)
               XDEN = XDEN * ZSQ + QY0(I)
  340       CONTINUE
         ELSE
            XNUM = PY1(9) * ZSQ + PY1(1)
            XDEN = ZSQ + QY1(1)
            DO 360 I = 2, 8
               XNUM = XNUM * ZSQ + PY1(I)
               XDEN = XDEN * ZSQ + QY1(I)
  360       CONTINUE
      END IF
      RESULT = RESJ + (UP*DOWN/AX) * XNUM / XDEN
      GO TO 2000
C-------------------------------------------------------------------
C  Calculate J1 or Y1 for |ARG|  >  8.0
C-------------------------------------------------------------------
  800 Z = EIGHT / AX
      W = AINT(AX/TWOPI) + THROV8
      W = (AX - W * TWOPI1) - W * TWOPI2
      ZSQ = Z * Z
      XNUM = P0(6)
      XDEN = ZSQ + Q0(6)
      UP = P1(6)
      DOWN = ZSQ + Q1(6)
      DO 850 I = 1, 5
         XNUM = XNUM * ZSQ + P0(I)
         XDEN = XDEN * ZSQ + Q0(I)
         UP = UP * ZSQ + P1(I)
         DOWN = DOWN * ZSQ + Q1(I)
  850 CONTINUE
      R0 = XNUM / XDEN
      R1 = UP / DOWN
      IF (JINT .EQ. 0) THEN
            RESULT = (RTPI2/SQRT(AX)) * (R0*COS(W) - Z*R1*SIN(W))
         ELSE
            RESULT = (RTPI2/SQRT(AX)) * (R0*SIN(W) + Z*R1*COS(W))
      END IF
      IF ((JINT .EQ. 0) .AND. (ARG .LT. ZERO)) RESULT = -RESULT
 2000 RETURN
C---------- Last card of CALJY1 ----------
      END
      FUNCTION BESJ1(X)
C--------------------------------------------------------------------
C
C This subprogram computes approximate values for Bessel functions
C   of the first kind of order zero for arguments  |X| <= XMAX
C   (see comments heading CALJY1).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
      DOUBLE PRECISION
     1   BESJ1,RESULT,X
C--------------------------------------------------------------------
      JINT=0
      CALL CALJY1(X,RESULT,JINT)
      BESJ1 = RESULT
      RETURN
C---------- Last card of BESJ1 ----------
      END
      FUNCTION BESY1(X)
C--------------------------------------------------------------------
C
C This subprogram computes approximate values for Bessel functions
C   of the second kind of order zero for arguments 0 < X <= XMAX
C   (see comments heading CALJY1).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
      DOUBLE PRECISION
     1   BESY1,RESULT,X
C--------------------------------------------------------------------
      JINT=1
      CALL CALJY1(X,RESULT,JINT)
      BESY1 = RESULT
      RETURN
C---------- Last card of BESY1 ----------
      END
