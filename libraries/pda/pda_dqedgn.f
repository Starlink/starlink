      SUBROUTINE PDA_DQEDGN(MEQUA,NVARS,MCON,IND,BL,BU,X,FJAC,LDFJAC,
     .                       FNORM,IGO,IOPT,ROPT,IWA,WA)
C***BEGIN PROLOGUE  DQEDGN
C***REFER TO  DQED
C***ROUTINES CALLED  DQEDIP
C***END PROLOGUE  DQEDGN
C  DQEDGN:
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
C BL         DUMMY-ARG     REAL      ADJ-ARY Model lower bounds
C BU         DUMMY-ARG     REAL      ADJ-ARY Model upper bounds
C FJAC       DUMMY-ARG     REAL      ADJ-ARY Model Jacobian array
C FNORM      DUMMY-ARG     REAL              Model residual norm
C IGO        DUMMY-ARG     INTEGER           direct model action
C IND        DUMMY-ARG     INTEGER   ADJ-ARY Model bound indicators
C IOPT       DUMMY-ARG     INTEGER   ADJ-ARY Option array
C IWA        DUMMY-ARG     INTEGER   ADJ-ARY Working array
C LDFJAC     DUMMY-ARG     INTEGER           Row dim of FJAC(*,*)
C MB         /S$A$V$E/ SAV INTEGER           Pointer to B(*)
C MBB        /S$A$V$E/ SAV INTEGER           Pointer to BB(*)
C MBLB       /S$A$V$E/ SAV INTEGER           Pointer to BLB(*)
C MBUB       /S$A$V$E/ SAV INTEGER           Pointer to BLB(*)
C MCON       DUMMY-ARG     INTEGER           Number, model constraints
C MDX        /S$A$V$E/ SAV INTEGER           Pointer to DX(*)
C MEQUA      DUMMY-ARG     INTEGER           Number, model equations
C MINDB      /S$A$V$E/ SAV INTEGER           Pointer to INDB(*)
C MIWA       /S$A$V$E/ SAV INTEGER           Pointer to IWA(*)
C MWA        /S$A$V$E/ SAV INTEGER           Pointer to WA(*)
C MXB        /S$A$V$E/ SAV INTEGER           Pointer to XB(*)
C NALL       /S$A$V$E/ SAV INTEGER           NVARS+MEQUA
C NVARS      DUMMY-ARG     INTEGER           Number, user variables
C ROPT       DUMMY-ARG     REAL      ADJ-ARY Option array data
C WA         DUMMY-ARG     REAL      ADJ-ARY Working array
C
C     REVISED 870204-1100
C     REVISED YYMMDD-HHMM
      DOUBLE PRECISION BL(*),BU(*),X(*),FJAC(LDFJAC,*),FNORM
      DOUBLE PRECISION ROPT(*),WA(*)
      INTEGER IND(*),IOPT(*),IWA(*)
C     ALLOCATE BLOCKS OF WORKING STORAGE TO LOGICAL ARRAYS.
      NALL = MCON + NVARS
      MDX = 1
      MXB = MDX + 2*NALL + 2
      MB = MXB + NVARS
      MBB = MB + NVARS
      MBLB = MBB + NVARS
      MBUB = MBLB + NALL
      MWA = MBUB + NALL
C
      MINDB = 1
      MIWA = MINDB + NALL + NVARS
      CALL PDA_DQEDIP(MEQUA,NVARS,MCON,IND,BL,BU,X,FJAC,LDFJAC,FNORM,
     .            IGO,
     .            IOPT,ROPT,IWA(MIWA),WA(MWA),WA(MDX),WA(MXB),WA(MB),
     .            WA(MBB),WA(MBLB),WA(MBUB),IWA(MINDB))
C     THESE DEFINE THE AMOUNT OF STORAGE FOR THE DOUBLE PRECISION AND
C     INTEGER WORK ARRAYS, WA(*) AND IWA(*).
      MWA = MWA + 6*NVARS + 5*MCON
      MIWA = MIWA + 2*NALL
C     TOTAL WORKING STORAGE IN WA(*)=
C          9*NALL + 4*NVARS = 9*MCON + 13*NVARS.
C     TOTAL WORKING STORAGE IN IWA(*)=
C          3*NALL + NV      = 3*MCON +4*NVARS.
      RETURN
      END
