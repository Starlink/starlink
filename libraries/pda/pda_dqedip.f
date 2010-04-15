      SUBROUTINE PDA_DQEDIP(MEQUA,NVARS,MCON,IND,BL,BU,X,FJAC,LDFJAC,FB,
     .                  IGO,
     .                  IOPT,ROPT,IWA,WA,DX,XB,B,BB,BLB,BUB,INDB)
C***BEGIN PROLOGUE  DQEDIP
C***REFER TO  DQED
C***ROUTINES CALLED  DBOCLS,PDA_IDAMAX,DCOPY,PDA_DNRM2,D1MACH,
C                    CHRCNT,XERRWV
C***END PROLOGUE  DQEDIP
C  DBOCLS                  15      SUBROUTINE
C  PDA_IDAMAX    INTEGER        3      FUNCTION
C  DCOPY                    5      SUBROUTINE
C  PDA_DNRM2     REAL           3      FUNCTION
C  PDA_D1MACH    REAL           1      FUNCTION
C  CHRCNT                   2      SUBROUTINE
C  XERRWV                  10      SUBROUTINE
C DQEDIP:
C GLOSSARY OF VARIABLES. NOTATION:
C DUMMY-ARG A dummy argument, that is an argument to this prog. unit.
C /S$A$V$E/ SAV Denotes that this variable is local to the routine
C               and is saved between calls to it.
C INTEGER, REAL, DOUBLE PRECISION, LOGICAL, CHARACTER
C               The types of the variables.
C ADJ-ARR An adjustable array, that is an argument to this prog. unit.
C Name      Memory Status  Type     Argument   Uses and comments.
C                                    Status
C ----      -------------  ----     --------   ------------------
C ALB        /S$A$V$E/ SAV REAL
C ALFAC      /S$A$V$E/ SAV REAL
C ALPHA      /S$A$V$E/ SAV REAL
C AUB        /S$A$V$E/ SAV REAL
C B          DUMMY-ARG     REAL      ADJ-ARY
C BB         DUMMY-ARG     REAL      ADJ-ARY
C BBOOST     /S$A$V$E/ SAV REAL
C BL         DUMMY-ARG     REAL      ADJ-ARY
C BLB        DUMMY-ARG     REAL      ADJ-ARY
C BOLD       /S$A$V$E/ SAV REAL
C BU         DUMMY-ARG     REAL      ADJ-ARY
C BUB        DUMMY-ARG     REAL      ADJ-ARY
C CHG        /S$A$V$E/ SAV REAL
C CHGFAC     /S$A$V$E/ SAV REAL
C COLNRM     /S$A$V$E/ SAV REAL
C C1516      /S$A$V$E/ SAV REAL
C DX         DUMMY-ARG     REAL      ADJ-ARY
C IPLS       /S$A$V$E/ SAV INTEGER
C IPRINT     /S$A$V$E/ SAV INTEGER
C ITERS      /S$A$V$E/ SAV INTEGER
C ITMAX      /S$A$V$E/ SAV INTEGER
C IWA        DUMMY-ARG     INTEGER   ADJ-ARY
C J          /S$A$V$E/ SAV INTEGER
C JP         /S$A$V$E/ SAV INTEGER
C K          /S$A$V$E/ SAV INTEGER
C KL         /S$A$V$E/ SAV INTEGER
C KP         /S$A$V$E/ SAV INTEGER
C LDFJAC     DUMMY-ARG     INTEGER
C LEVEL      /S$A$V$E/ SAV INTEGER
C  EDIT on 950228-1300. REMOVE references to:
C LINPRB     /S$A$V$E/ SAV INTEGER
C LP         /S$A$V$E/ SAV INTEGER
C LPDIFF     /S$A$V$E/ SAV INTEGER
C MCON       DUMMY-ARG     INTEGER
C MEQUA      DUMMY-ARG     INTEGER
C MODE       /S$A$V$E/ SAV INTEGER
C NALL       /S$A$V$E/ SAV INTEGER
C NERR       /S$A$V$E/ SAV INTEGER
C NEWBST     /S$A$V$E/ SAV LOGICAL
C NEWOPT     /S$A$V$E/ SAV LOGICAL
C NMESS      /S$A$V$E/ SAV INTEGER
C NOUT       /S$A$V$E/ SAV INTEGER
C NVARS      DUMMY-ARG     INTEGER
C ONE        /S$A$V$E/ SAV REAL
C PASSB      /S$A$V$E/ SAV LOGICAL
C DXNRM      /S$A$V$E/ SAV REAL
C FB         DUMMY-ARG     REAL
C FC         /S$A$V$E/ SAV REAL
C FJAC       DUMMY-ARG     REAL      ADJ-ARY
C FL         /S$A$V$E/ SAV REAL
C FULNWT     /S$A$V$E/ SAV LOGICAL
C GVAL       /S$A$V$E/ SAV REAL
C ICASE      /S$A$V$E/ SAV INTEGER
C IFLAG      /S$A$V$E/ SAV INTEGER
C IGO        DUMMY-ARG     INTEGER
C IGOIOV     /S$A$V$E/ SAV INTEGER
C IGOPOA     /S$A$V$E/ SAV INTEGER
C IGOTFC     /S$A$V$E/ SAV INTEGER
C IGOTNC     /S$A$V$E/ SAV INTEGER
C IND        DUMMY-ARG     INTEGER   ADJ-ARY
C INDB       DUMMY-ARG     INTEGER   ADJ-ARY
C IOPT       DUMMY-ARG     INTEGER   ADJ-ARY
C PB         /S$A$V$E/ SAV REAL
C PD         /S$A$V$E/ SAV REAL
C PV         /S$A$V$E/ SAV REAL
C RB         /S$A$V$E/ SAV REAL
C RDUM       /S$A$V$E/ SAV REAL
C RETREA     /S$A$V$E/ SAV LOGICAL
C RG         /S$A$V$E/ SAV REAL
C RNORMC     /S$A$V$E/ SAV REAL
C ROPT       DUMMY-ARG     REAL      ADJ-ARY
C SEMIBG     /S$A$V$E/ SAV REAL
C T          /S$A$V$E/ SAV REAL
C TERM       /S$A$V$E/ SAV LOGICAL
C TOLD       /S$A$V$E/ SAV REAL
C TOLF       /S$A$V$E/ SAV REAL
C TOLP       /S$A$V$E/ SAV REAL
C TOLSNR     /S$A$V$E/ SAV REAL
C TOLX       /S$A$V$E/ SAV REAL
C TWO        /S$A$V$E/ SAV REAL
C T2         /S$A$V$E/ SAV REAL
C WA         DUMMY-ARG     REAL      ADJ-ARY
C X          DUMMY-ARG     REAL      ADJ-ARY
C XB         DUMMY-ARG     REAL      ADJ-ARY
C XMESS      /S$A$V$E/ SAV CHAR*128
C ZERO       /S$A$V$E/ SAV REAL
C
C     REVISED 870204-1100
C     REVISED YYMMDD-HHMM
      DOUBLE PRECISION BL(*),BU(*),X(*),FJAC(LDFJAC,*)
      DOUBLE PRECISION DX(*),XB(*),B(*),BB(*)
      DOUBLE PRECISION BLB(*),BUB(*),ROPT(*),WA(*)
      DOUBLE PRECISION ALB,ALFAC,ALPHA,AUB,BBOOST
      DOUBLE PRECISION BOLD,CHG,CHGFAC,COLNRM
      DOUBLE PRECISION DXNRM,C1516,FB,FC,FL
      DOUBLE PRECISION RG,RB,PB,PD
      DOUBLE PRECISION GVAL,ONE,PV
      DOUBLE PRECISION RNORMC,SEMIBG,T,TOLD,TOLF,TOLP
      DOUBLE PRECISION TOLSNR,TOLX,TWO,T2,ZERO
      DOUBLE PRECISION PDA_D1MACH,PDA_DNRM2
      REAL RDUM
      INTEGER IND(*),INDB(*),IOPT(*),IWA(*)
      CHARACTER XMESS*128
      INTEGER PDA_I1MACH, PDA_IDAMAX
      EXTERNAL PDA_I1MACH, PDA_IDAMAX, PDA_D1MACH, PDA_DNRM2
C     OPTIONS..
C     1    SET THE PRINTED OUTPUT OFF/ON.  REQUIRES TWO ENTRIES
C          IN IOPT(*).  IF IOPT(*+1)=0, NO PRINTING; =1 PRINT.
C          (THIS PRINTOUT SHOWS VARIOUS QUANTITIES ASSOCIATED
C          WITH THE NONLINEAR PROBLEM BEING SOLVED. GOOD FOR
C          DEBUGGING A PROBLEM IN CASE OF TROUBLE.
C     2    SET THE MAXIMUM NUMBER OF INTERATIONS.  REQUIRES TWO ENTRIES
C          IN IOPT(*).  USE IOPT(*+1)= MAXIMUM NUMBER OF ITERATIONS.
C     3    PASS INITIAL BOUNDS FOR THE TRUST REGION TO THE NONLINEAR
C          SOLVER. REQUIRES TWO ENTRIES IN IOPT(*). USE IOPT(*+1) AS A
C          POINTER INTO ROPT(*) FOR START OF THE NVARS BOUNDS.
C  EDIT on 950228-1300:
C      LOGICAL RETREA,TERM,FULNWT,PASSB,NEWBST,NEWOPT,LINPRB
       LOGICAL RETREA,TERM,FULNWT,PASSB,NEWBST,NEWOPT

C Modified PWD. Need to explicitly SAVE for some compilers. This
C saves all local variables as the prologue indicates most should
C be saved
       SAVE
      DATA IFLAG/0/
C--PROCEDURES--
C -NAME------TYPE--------ARGS------CLASS-----
C
C  DBOCLS                  15      SUBROUTINE
C  PDA_IDAMAX    INTEGER        3      FUNCTION
C  DCOPY                    5      SUBROUTINE
C  PDA_DNRM2     REAL           3      FUNCTION
C  PDA_D1MACH    REAL           1      FUNCTION
C  CHRCNT                   2      SUBROUTINE
C  XERRWV                  10      SUBROUTINE
      GO TO (50),IFLAG
*
      ZERO = 0.D0
      ONE = 1.D0
      TWO = 2.D0
C     DO(PROCESS OPTION ARRAY)
      ASSIGN 10 TO IGOPOA
      GO TO 470
*
   10 CONTINUE
C     DO(INITIALIZE OTHER VALUES)
      ASSIGN 20 TO IGOIOV
      GO TO 450
*
   20 CONTINUE
C     SET SO X(*)-DX(*) UPDATE WILL BE CORRECT FIRST TIME.
      DX(1) = ZERO
      CALL PDA_DCOPY(NVARS,DX,0,DX,1)
      K = 0
C     PDA_D1MACH(2)="INFINITY" ON THIS MACHINE.
      FB = PDA_D1MACH(2)
      DXNRM = FB
      FL = ZERO
C
C     LINEAR PROBLEM RESIDUAL.
      PV = ZERO
      RETREA = .FALSE.
      FULNWT = .FALSE.
      TERM = .FALSE.
   30 CONTINUE
      IF ( .NOT. RETREA) ITERS = ITERS + 1
      IF (RETREA) THEN
C     MUST RETREAT TO BEST X VALUES.
          K = 0
          KL = -1
          FL = FB
          CALL PDA_DCOPY(NVARS,XB,1,X,1)
*
      ELSE
          KL = K
          DO 40 J = 1,NVARS
             X(J) = X(J) - DX(J)
   40     CONTINUE
          IF (TERM) THEN
              IFLAG = 0
              GO TO 390
*
          END IF
*
      END IF
*
      IGO = 1
      IFLAG = 1
C     DO(RETURN TO USER PROGRAM UNIT)
      GO TO 440
*
   50 CONTINUE
      FC = PDA_DNRM2(MEQUA,FJAC(MCON+1,NVARS+1),1)
C     DO(TEST FOR CONVERGENCE)
      ASSIGN 60 TO IGOTFC
      GO TO 400
*
   60 CONTINUE
      IF (TERM) THEN
          IFLAG = 0
          GO TO 390
*
      END IF
*
      NEWBST = FC .LT. FB .OR. (MCON.GT.0 .AND.ITERS.EQ.2)
      IF (NEWBST) K = 0
      IF (K.EQ.0) THEN
          RG = ZERO
          PB = ZERO
          PD = PDA_D1MACH(2)
C     WANT TO POSITION AT BEST X VALUES.
          FB = FC
C     DO CASE(2-KL,3)
          GO TO (70,90,110),2 - KL
*
          GO TO 120
*
   70     CONTINUE
C     CASE 1
C     IMMEDIATELY GOT A NEW BEST X.
          IF (T2.LE.0.25D0) THEN
              BBOOST = ONE
              CHG = MAX(4.D0*T2,.1D0)
          END IF
*
          DO 80 J = 1,NVARS
             BB(J) = CHG*BB(J)
   80     CONTINUE
C     THIS CODE FOR ALPHA HAS THE FOLLOWING EFFECT.
C     IF FC .EQ. PV, THEN ALPHA=ALFAC.
C     IF FC**2 .EQ. PV*FL THEN ALPHA=2.-1./ALFAC
C     IF FC**2 IS MUCH LARGER THAN PV*FL, THEN ALPHA=1.
          T = FC - PV
          IF (T.EQ.ZERO) THEN
              ALPHA = ALFAC
*
          ELSE
              ALPHA = (PV* (FL-PV))/ (FC+PV)/ (ALFAC-ONE)
              ALPHA = (ABS(T)+ALFAC*ALPHA)/ (ABS(T)+ALPHA)
          END IF
*
          ALFAC = 1.5D0*ALPHA
          BBOOST = MIN(1.5D0*ALPHA*BBOOST,SEMIBG)
          GO TO 140
*
   90     CONTINUE
C     CASE 2
C     AT THE INITIAL X.
          ALFAC = 256.D0
          DO 100 J = 1,NVARS
             IF ( .NOT. PASSB) BB(J) = -X(J)
             IF (BB(J).EQ.ZERO) THEN
                 COLNRM = PDA_DNRM2(MEQUA,FJAC(MCON+1,J),1)
                 IF (COLNRM.NE.ZERO) BB(J) = -FC/COLNRM
             END IF
*
             IF (BB(J).EQ.ZERO) BB(J) = -ONE
             XB(J) = X(J)
             B(J) = BB(J)
  100     CONTINUE
          ALPHA = ONE
          BBOOST = 0.5D0
C     EXIT IF
          GO TO 170
*
  110     CONTINUE
C     CASE 3
C     RETREAT TO BEST X.
          IF (ALFAC.NE.256.D0) THEN
              ALPHA = MIN(ONE/ALFAC,.25D0)
              ALFAC = 1.25D0
*
          ELSE
              ALPHA = .25D0*ALPHA
          END IF
*
          BBOOST = .25D0
          GO TO 140
*
  120     CONTINUE
C     CASE OTHER
C     NOT IMMEDIATELY A BEST X.
          RB = ZERO
          DO 130 J = 1,NVARS
             RB = MAX(RB,ABS((XB(J)-X(J))/BB(J)))
  130     CONTINUE
          ALPHA = RB
          ALFAC = TWO
          BBOOST = (8.D0/7.D0+RG)/ (2.D0/7.D0+RG)
C     END CASE
  140     CONTINUE
          DO 150 J = 1,NVARS
             DX(J) = XB(J) - X(J)
             IF (DX(J).EQ.ZERO) THEN
                 B(J) = ALPHA*BB(J)
*
             ELSE
                 XB(J) = X(J)
                 B(J) = SIGN(ALPHA*BB(J),DX(J)) + BBOOST*DX(J)
             END IF
*
            BB(J) = SIGN(MIN(SQRT(PDA_D1MACH(2)),ABS(B(J))),B(J))
  150     CONTINUE
*
      ELSE
C     COMPUTE A GAUGE FOR RETREATING IF DID NOT
C     GET A NEW BEST.
          IF (K.EQ.1) THEN
              PB = PV
*              PD = .5D0* (FB-PB)* ((FB+PB)/FB)
              PD = 1.5D0* (FB+PB* (PB/FB)) - 4.D0*PB
          END IF
*
          ALPHA = (.5D0*FC+FL)/ (FC+FL)
          CHG = MIN(ALPHA*CHG,T2)
          CHG=MAX(CHG,.1D0)
          DO 160 J = 1,NVARS
             B(J) = CHG*B(J)
             IF (K.EQ.1) BB(J) = B(J)
  160     CONTINUE
      END IF
*
  170 CONTINUE
C     DO(TEST FOR CONVERGENCE)
      ASSIGN 180 TO IGOTFC
      GO TO 400
*
  180 CONTINUE
      IF (TERM) THEN
          IFLAG = 0
          GO TO 390
*
      END IF
*
      K = K + 1
C     SOLVE LINEAR BOUNDED PROBLEM.
      DO 240 J = 1,NVARS
         IF (B(J).LT.ZERO) THEN
             ALB = B(J)
             IF (DX(J).EQ.ZERO) THEN
C     THIS CASE IS REQD. TO AVOID USING BUB(*) AT THE INITIAL PT.
                 AUB = -C1516*ALB
*
             ELSE
                 AUB = MIN(-C1516*ALB,-DX(J)+BUB(J))
             END IF
*
         ELSE
             AUB = B(J)
             IF (DX(J).EQ.ZERO) THEN
                 ALB = -C1516*AUB
*
             ELSE
                 ALB = MAX(-C1516*AUB,-DX(J)+BLB(J))
             END IF
*
         END IF
C Edit on 950228-1300:
C      IF(LINPRB) THEN
C          AUB=PDA_D1MACH(2)
C          ALB=-AUB
C      END IF

C     RESTRICT THE STEP FURTHER IF USER GIVES BOUNDS.
         ICASE = IND(J)
C     DO CASE(ICASE,4)
         GO TO (190,200,210,220),ICASE
*
  190    CONTINUE
C     CASE 1
         AUB = MIN(AUB,X(J)-BL(J))
         GO TO 230
*
  200    CONTINUE
C     CASE 2
         ALB = MAX(ALB,X(J)-BU(J))
         GO TO 230
*
  210    CONTINUE
C     CASE 3
         AUB = MIN(AUB,X(J)-BL(J))
         ALB = MAX(ALB,X(J)-BU(J))
         GO TO 230
*
  220    CONTINUE
C     CASE 4
C     END CASE
  230    CONTINUE
         BLB(J) = ALB
C     THIS NEXT LINE IS TO GUARANTEE THAT THE LOWER BOUND
C     IS .LE. THE UPPER BOUND.
         AUB = MAX(AUB,ALB)
         BUB(J) = AUB
         INDB(J) = 3
  240 CONTINUE
C     SEE IF USER HAS GIVEN GENERAL CONSTRAINTS.
      DO 300 J = NVARS + 1,NALL
         ICASE = IND(J)
         GVAL = FJAC(J-NVARS,NVARS+1)
C     DO CASE(ICASE,4)
         GO TO (250,260,270,280),ICASE
*
  250    CONTINUE
C     CASE 1
         BLB(J) = - (GVAL-BL(J))
         INDB(J) = 1
         GO TO 290
*
  260    CONTINUE
C     CASE 2
         BUB(J) = - (GVAL-BU(J))
         INDB(J) = 2
         GO TO 290
*
  270    CONTINUE
C     CASE 3
         BLB(J) = - (GVAL-BL(J))
         BUB(J) = - (GVAL-BU(J))
         INDB(J) = 3
         GO TO 290
*
  280    CONTINUE
C     CASE 4
         INDB(J) = 4
C     END CASE
  290    CONTINUE
  300 CONTINUE
C     SOLVE THE LEAST SQUARES PROBLEM WITH BOUNDS AND LINEAR
C     CONSTRAINTS.  THESE BOUNDS CAN COME FROM THE USER OR
C     THE ALGORITHM.
      CALL PDA_DBOCLS(FJAC,LDFJAC,MCON,MEQUA,NVARS,BLB,BUB,INDB,
     .     IOPT(IPLS),DX,RNORMC,PV,MODE,WA,IWA)
      IF (IPRINT.GT.0) THEN
          WRITE (NOUT,9011) ITERS,FC,PV,K,KL,FB,ALPHA,BBOOST
          WRITE (NOUT,9001) '  +X=', (X(J),J=1,NVARS)
          WRITE (NOUT,9001) ' +XB=', (XB(J),J=1,NVARS)
          WRITE (NOUT,9001) ' +DX=', (DX(J),J=1,NALL)
          WRITE (NOUT,9001) ' + B=', (B(J),J=1,NVARS)
          WRITE (NOUT,9001) ' +LB=', (BLB(J),J=1,NALL)
          WRITE (NOUT,9001) ' +UB=', (BUB(J),J=1,NALL)
          WRITE (NOUT,'('' +///END OF ITERATION.///'')')
      END IF
C     TEST FOR NOISE IN LINEAR PROBLEM SOLN.
      TERM=MCON.EQ.0.AND.(PV.GE.FC)
      TERM=.FALSE.
      IF (TERM) THEN
          IF (IPRINT.GT.0) THEN
              WRITE (NOUT,9021) PV,FC
          END IF
*
          CALL PDA_DCOPY(NVARS,XB,1,X,1)
          IGO = 4
          IFLAG = 0
          GO TO 390
*
      END IF
*
      RG = MAX(RG, (PV-PB)/PD)
      IF ( .NOT. RETREA) THEN
          CHG = ONE
          T2 = ZERO
          DO 360 J = 1,NVARS
             BOLD = B(J)
             T = DX(J)/BOLD
C    IF USER GIVES BOUNDS, AND THESE BOUNDS ARE HIT,
C    DO NOT DETRACT FROM DECLARING A FULL NEWTON STEP.
             ICASE = IND(J)
             GO TO (310,320,330,340),ICASE
C     CASE 1
  310        ALB = (X(J)-BL(J))/BOLD
             AUB = -SEMIBG
             GO TO 350
C     CASE 2
  320        AUB = (X(J)-BU(J))/BOLD
             ALB = -SEMIBG
             GO TO 350
C     CASE 3
  330        ALB = (X(J)-BL(J))/BOLD
             AUB = (X(J)-BU(J))/BOLD
             GO TO 350
C     CASE 4
  340        CONTINUE
             ALB = -SEMIBG
             AUB = -SEMIBG
C     END CASE
  350        CONTINUE
             IF (T.EQ.ONE) THEN
                 T2 = ONE
                 B(J) = BOLD + BOLD
                 CHG = CHG*CHGFAC
*
             ELSE
                 IF (ABS(T).LT..25D0 .AND. DX(J).NE.ZERO) THEN
                     B(J) = SIGN(.25D0*BOLD,DX(J)) + 3.D0*DX(J)
*
                 ELSE
                     B(J) = SIGN(BOLD,DX(J))
                 END IF
*
             END IF
C     THIS TEST AVOIDS THE USER BOUNDS IN DECLARING A NEWTON STEP.
             IF (ABS(ALB-T).GE..01D0*ABS(T) .AND. ABS(AUB-T).GE..01D0*
     .           ABS(T)) THEN
                 IF (T.GT.ZERO) THEN
                     T2 = MAX(T2,T)
*
                 ELSE
                     T2 = MAX(T2,-T/C1516)
                 END IF
*
             END IF
*
  360     CONTINUE
          FULNWT = T2 .LT. .99D0
          FL = FC
          DXNRM = ABS(DX(PDA_IDAMAX(NVARS,DX,1)))
C     DO BLOCK
C     TEST FOR SMALL ABSOLUTE CHANGE IN X VALUES.
          TERM = DXNRM .LT. TOLD .AND. FULNWT
          IF (TERM) THEN
              IGO = 5
C     EXIT BLOCK
              GO TO 370
*
          END IF
*
          TERM = DXNRM .LT. PDA_DNRM2(NVARS,X,1)*TOLX .AND. FULNWT
          TERM = TERM .AND. (ITERS.GT.1)
          IF (TERM) THEN
              IGO = 6
C     EXIT BLOCK
              GO TO 370
*
          END IF
C     EXIT IF
          GO TO 380
C     END BLOCK
  370     CONTINUE
          GO TO 30
*
      END IF
*
  380 CONTINUE
      GO TO 30
*
  390 CONTINUE
C     DO(RETURN TO USER PROGRAM UNIT)
      GO TO 440
C     PROCEDURE(TEST FOR CONVERGENCE)
  400 CONTINUE
C     TEST FOR SMALL FUNCTION NORM.
      TERM = FC .LE. TOLF .OR. TERM
C     IF HAVE CONSTRAINTS MUST ALLOW AT LEAST ONE MOVE.
      TERM = TERM .AND. (MCON.EQ.0 .OR. ITERS.GT.1)
      IF (TERM) THEN
          IGO = 2
C     EXIT PROCEDURE
          GO TO 420
*
      END IF
C     DO(TEST FOR NO CHANGE)
      ASSIGN 410 TO IGOTNC
      GO TO 430
*
  410 CONTINUE
      TERM = TERM .AND. .NOT. RETREA
      IF (TERM) THEN
          IGO = 3
C     EXIT PROCEDURE
          GO TO 420
*
      END IF
*
      TERM = ITERS .GE. ITMAX
      IF (TERM) THEN
          IGO = 7
      END IF
C     END PROCEDURE
  420 CONTINUE
      GO TO IGOTFC
C     PROCEDURE(TEST FOR NO CHANGE)
  430 CONTINUE
      T = SQRT(MAX(ZERO, (FL-PV)* (FL+PV)))
      TERM = (ABS(FB-FC).LE.TOLSNR*FB) .AND. (T.LE.PV*TOLP)
      TERM = TERM .AND. (ABS(FC-FL).LE.FB*TOLSNR)
      TERM = TERM .AND. FULNWT
C     END PROCEDURE
      GO TO IGOTNC
C     PROCEDURE(RETURN TO USER PROGRAM UNIT)
  440 CONTINUE
      RETURN
C     END PROCEDURE
C     PROCEDURE(INITIALIZE OTHER VALUES)
  450 CONTINUE
C Edit on 950228-1300:
C      LINPRB = IGO.EQ.0
      ITERS = 0
      NALL = MCON + NVARS
      CHGFAC = TWO** (-ONE/REAL(NVARS))
      C1516 = 15.D0/16.D0
      SEMIBG = 1.D10
C     MAKE SURE THAT VARIABLES SATISFY THE BOUNDS AND CONSTRAINTS.
      DO 460 J = 1,NALL
         BLB(J) = BL(J)
         BUB(J) = BU(J)
         INDB(J) = IND(J)
  460 CONTINUE
C     END PROCEDURE
      GO TO IGOIOV
C     PROCEDURE(PROCESS OPTION ARRAY)
  470 CONTINUE
      IPRINT = 0
C     PDA_I1MACH(2)=STANDARD OUTPUT UNIT.
      NOUT = PDA_I1MACH(2)
C     PDA_D1MACH(4)=RELPR=MACHINE REL. PREC.
      T = PDA_D1MACH(4)
      TOLF = T
      TOLX = TOLF
      TOLD = TOLF
      TOLSNR = 1.D-3
      TOLP = 1.D-3
      ITMAX = 18
      PASSB = .FALSE.
      LEVEL = 1
      IPLS = 0
      LPDIFF = 0
      LP = 1
  480 CONTINUE
      LP = LP + LPDIFF
      LPDIFF = 2
      KP = IOPT(LP)
      NEWOPT = KP .GT. 0
      JP = ABS(KP)
C     SEE IF THIS IS THE LAST OPTION..
      IF (JP.EQ.99) THEN
          IF (NEWOPT) THEN
C     THE POINTER TO THE START OF OPTIONS FOR THE LINEAR
C     SOLVER MUST SATISFY THE REQUIREMENTS FOR THAT OPTION ARRAY.
              IF (IPLS.EQ.0) IPLS = LP
              GO TO 490
*
          ELSE
              LPDIFF = 1
              GO TO 480
*
          END IF
*
      END IF
C     CHANGE PRINT OPTION.
      IF (JP.EQ.1) THEN
          IF (NEWOPT) IPRINT = IOPT(LP+1)
          GO TO 480
*
      END IF
C     SEE IF MAX. NUMBER OF ITERATIONS CHANGING.
      IF (JP.EQ.2) THEN
          IF (NEWOPT) ITMAX = IOPT(LP+1)
          GO TO 480
*
      END IF
C     SEE IF BOUNDS FOR THE TRUST REGION ARE BEING PASSED.
      IF (JP.EQ.3) THEN
          IF (NEWOPT) THEN
              CALL PDA_DCOPY(NVARS,ROPT(IOPT(LP+1)),1,BB,1)
              PASSB = .TRUE.
          END IF
*
          GO TO 480
*
      END IF
C     CHANGE TOLERANCE ON THE LENGTH OF THE RESIDUALS.
      IF (JP.EQ.4) THEN
          IF (NEWOPT) TOLF = ROPT(IOPT(LP+1))
          GO TO 480
*
      END IF
C     CHANGE TOLERANCE ON THE NORM OF THE RELATIVE
C     CHANGE TO THE PARAMETERS.
      IF (JP.EQ.5) THEN
          IF (NEWOPT) TOLX = ROPT(IOPT(LP+1))
          GO TO 480
*
      END IF
C     CHANGE TOLERANCE ON ABSOLUTE CHANGE TO THE PARAMETERS.
      IF (JP.EQ.6) THEN
          IF (NEWOPT) TOLD = ROPT(IOPT(LP+1))
          GO TO 480
*
      END IF
*
      IF (JP.EQ.7) THEN
C     CHANGE TOLERANCE FOR RELATIVE AGREEMENT BETWEEN
C     BEST FUNCTION NORM, LAST FUNCTION NORM AND THE
C     CURRENT FUNCTION NORM.
          IF (NEWOPT) TOLSNR = ROPT(IOPT(LP+1))
          GO TO 480
*
      END IF
*
      IF (JP.EQ.8) THEN
C     CHANGE TOLERANCE FOR AGREEMENT BETWEEN PREDICTED
C     VALUE OF RESIDUAL NORM AND THE PREVIOUS VALUE OF
C     THE RESIDUAL NORM.
          IF (NEWOPT) TOLP = ROPT(IOPT(LP+1))
          GO TO 480
*
      END IF
C     CHANGE THE PRINT LEVEL IN THE ERROR PROCESSOR.
      IF (JP.EQ.9) THEN
          IF (NEWOPT) LEVEL = IOPT(LP+1)
          GO TO 480
*
      END IF
C     PASS AN OPTION ARRAY TO THE CONSTRAINED LINEAR SOLVER.
C     THIS OPTION IS A POINTER TO THE START OF THE OPTION
C     ARRAY FOR THE SUBPROGRAM.
      IF (JP.EQ.10) THEN
          IF (NEWOPT) IPLS = IOPT(LP+1)
          GO TO 480
*
      END IF
C     MOVE THE PROCESSING POINTER BY THE VALUE IN THE
C     NEXT ENTRY OF THE OPTION ARRAY.  THIS DEVICE IS
C     INCLUDED SO THAT PASSING OPTIONS TO LOWER LEVEL
C     SUBROUTINES IS EASY TO DO.
      IF (JP.EQ.11) THEN
          IF (NEWOPT) LPDIFF = IOPT(LP+1)
          GO TO 480
*
      END IF
C     SAW AN OPTION (OR GARBAGE) THAT IS NOT ON THE LIST.
      XMESS =
     .'DQEDIP. INVALID OPTION PROCESSED. I1=IOPT(*) ENTRY. I2=IOPT(I1).'
      NERR = 08
      IGO = 16
      CALL PDA_CHRCNT(XMESS,NMESS)
      CALL PDA_XERRWV(XMESS,NMESS,NERR,LEVEL,2,LP,IOPT(LP),0,RDUM,RDUM)
      IFLAG = 0
C     DO(RETURN TO USER PROGRAM UNIT)
      GO TO 440
C     END PROCEDURE
  490 CONTINUE
      GO TO IGOPOA
*
 9001 FORMAT (A4,1P10D12.4/ (4X,10D12.4))
 9011 FORMAT ('0+ITER.=',I3,' FC=',1PD10.4,' PV=',1PD10.4,' K=',I4,
     .  ' KL=',I4,' FB=',1PD10.4/12X,'AL=',1PD14.4,' BB=',1PD14.4)
 9021 FORMAT (' LINEAR RESIDUAL.GE.CURRENT F. QUITTING.',1P2D12.5)
      END
