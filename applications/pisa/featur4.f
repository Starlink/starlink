      SUBROUTINE FEATUR4(XMODEL,IMS,JCOUNT,MPX,SIGMA,RANGE,IEXTRA,EDGE)
C     *** FEATUR  looks at difference map for blend and sticks in more
C     ***         images if necessary.


C  Changes:
C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK. Removed INTEGER*2 variable CEXT
C        not used. Changed INTEGER*2 references to INTEGER*4

C     ..Parameters..
      INCLUDE 'PSA1_PAR'        ! PISA parameters

C     .. Scalar Arguments ..
      REAL EDGE,RANGE,SIGMA
      INTEGER IEXTRA,IMS,JCOUNT,MPX
C     ..
C     .. Array Arguments ..
      REAL*4 XMODEL(PIXLIM)
C     ..
C     .. Scalars in Common ..
      REAL CONST,OFFSET,PHRESH
      INTEGER IANAL,IPIX,ISTART,ISTOP,IXH,IXL,NBIT,NPT,NWORD
C     ..
C     .. Arrays in Common ..
      REAL*4 PARM(16,IMNUM),XCORDL(IMNUM),XCORDS(IMNUM),YCORDL(IMNUM),
     +     YCORDS(IMNUM)
      INTEGER*4 IJIST(PIXLIM),ILIST(IMLIM),JJIST(PIXLIM),JLIST(IMLIM),
     +     KJIST(PIXLIM),KLIST(IMLIM),NJIST(PIXLIM),SILIST(PIXLIM),
     +     SJLIST(PIXLIM),SKLIST(PIXLIM)
C     ..
C     .. Local Scalars ..
      REAL PK,SIGNIF,SIGSQ,SS,THRESH,TT,XX,XYDIS,YY
      INTEGER I,ICOUNT,IH,IL,J,MOBJ,NUMIM
C     ..
C     .. Local Arrays ..
      REAL*4 SEXT(IMNUM),XEXT(IMNUM),YEXT(IMNUM)
C     ..
C     .. Common blocks ..
      COMMON /AN/IJIST,JJIST,KJIST,NJIST,SILIST,SJLIST,SKLIST,NPT
      COMMON /FIL/ISTART,ISTOP,NWORD,IXL,IXH
      COMMON /OV/ILIST,JLIST,KLIST,PHRESH,IPIX,PARM,NBIT,CONST,OFFSET,
     +     IANAL
      COMMON /ST/XCORDS,YCORDS,XCORDL,YCORDL
C     ..
      IEXTRA = 0
      THRESH = 2.0*SIGMA
      SIGSQ = SIGMA**2
      IL = IMS
      IH = IMS + JCOUNT - 1
 10   CONTINUE
C     *** grab pixels for analysis
      NPT = 0
      DO 20 I = IL,IH
         SIGNIF = SQRT(1.0+MAX0(0,KLIST(I))/SIGSQ)
         IF (KLIST(I)-XMODEL(I-IL+1).GT.THRESH*SIGNIF) THEN
            NPT = NPT + 1
            IF (NPT.GT.PIXLIM) THEN
               GOTO 30
            ELSE
               SILIST(NPT) = ILIST(I)
               SJLIST(NPT) = JLIST(I)
               SKLIST(NPT) = KLIST(I) - NINT(XMODEL(I-IL+1))
            ENDIF
         ENDIF
 20   CONTINUE
      GOTO 40

 30   THRESH = THRESH + SIGMA
      GOTO 10
C     type *,' No. of pixels above threshold =',npt
 40   IF (NPT.LT.MPX) THEN
         IEXTRA = 0
      ELSE
         CALL ANALYS4
 50      CONTINUE
C     *** find separate images their coordinates intensities etc.
         MOBJ = NJIST(1)
         ICOUNT = 1
         TT = KJIST(1)
         XX = IJIST(1)*TT
         YY = JJIST(1)*TT
         PK = TT
         SS = TT
         NUMIM = 0
         DO 60 I = 2,NPT
            IF (MOBJ.EQ.NJIST(I)) THEN
               ICOUNT = ICOUNT + 1
               TT = KJIST(I)
               XX = XX + IJIST(I)*TT
               YY = YY + JJIST(I)*TT
               PK = AMAX1(PK,TT)
               SS = SS + TT
            ENDIF

            IF ((MOBJ.EQ.NJIST(I).AND.I.EQ.NPT) .OR.
     +           .NOT.MOBJ.EQ.NJIST(I)) THEN
C     *** reject small bits
               IF (ICOUNT.GE.MPX) THEN
                  NUMIM = NUMIM + 1
                  IF (NUMIM+NBIT.GT.IMNUM) THEN
                     GOTO 70

                  ELSE

                     XEXT(NUMIM) = XX/SS
                     YEXT(NUMIM) = YY/SS
                     SEXT(NUMIM) = SS/ (1.0-THRESH/PK)
                  ENDIF

               ENDIF

               IF (I.NE.NPT) THEN
                  ICOUNT = 1
                  TT = KJIST(I)
                  XX = IJIST(I)*TT
                  YY = JJIST(I)*TT
                  PK = TT
                  SS = TT
                  MOBJ = NJIST(I)
               ENDIF

            ENDIF

 60      CONTINUE
         GOTO 80

 70      MPX = MPX + 1
         GOTO 50

 80      CONTINUE
         IF (NUMIM.EQ.0) THEN
            IEXTRA = 0

         ELSE
C     ***
C     *** Now check thro list and see if any can be removed
C     ***
            DO 90 I = 1,NUMIM
               IF (XEXT(I).LT.ISTART+EDGE .OR.
     +              XEXT(I).GT.ISTOP-EDGE .OR.
     +              YEXT(I).LT.IXL+EDGE .OR. YEXT(I).GT.IXH-EDGE) THEN
                  SEXT(I) = -1.0

               ELSE

                  DO 100 J = 1,NBIT
                     IF (PARM(1,J).GE.0.5) THEN
                        XYDIS = (PARM(4,J)-XEXT(I))**2 +
     +                       (PARM(2,J)-YEXT(I))**2
                        IF (XYDIS.LT.RANGE) GOTO 110
                     ENDIF

 100              CONTINUE

                  GOTO 90

 110              SEXT(I) = -1.0
               ENDIF

 90         CONTINUE
C     *** store in master object lists
            DO 120 I = 1,NUMIM
               IF (SEXT(I).GE.0.0) THEN
                  NBIT = NBIT + 1
                  IEXTRA = IEXTRA + 1
                  PARM(1,NBIT) = SEXT(I)
                  PARM(2,NBIT) = YEXT(I)
                  PARM(4,NBIT) = XEXT(I)
                  XCORDS(NBIT) = XEXT(I)
                  YCORDS(NBIT) = YEXT(I)
                  XCORDL(NBIT) = XEXT(I)
                  YCORDL(NBIT) = YEXT(I)
               ENDIF

 120        CONTINUE
         ENDIF

      ENDIF

      END
