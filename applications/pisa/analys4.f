      SUBROUTINE ANALYS4

C     *** ANALYS  does isophotal analysis on lists from OVERLP
C     ***         these lists only contain pixels above threshold.

C  Changes:
C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK. INTEGER*2 references changed to
C        INTEGER*4.

C     ..Parameters..
      INCLUDE 'PSA1_PAR'        ! PISA parameters

C     .. Scalars in Common ..
      INTEGER NPT
C     ..
C     .. Arrays in Common ..
      INTEGER*4 IJIST(PIXLIM),JJIST(PIXLIM),KJIST(PIXLIM),NJIST(PIXLIM),
     +     SILIST(PIXLIM),SJLIST(PIXLIM),SKLIST(PIXLIM)
C     ..
C     .. Local Scalars ..
      INTEGER I,IDEL1,IDEL2,IJC,IJCOLD,IJCPRE,IOLD,J,K,KL,KREF,KU,L,N,
     +     NEWNUM,NOBJ,NUMB
C     ..
C     .. Common blocks ..
      COMMON /AN/IJIST,JJIST,KJIST,NJIST,SILIST,SJLIST,SKLIST,NPT
C     ..

      IJC = 0
      IJCOLD = 0
      IJCPRE = 0
C     *** sort pixel lists on i
      CALL SORTI34(SILIST,SJLIST,SKLIST,NPT)
      IOLD = SILIST(1)
      NOBJ = 0
      DO 10 N = 1,NPT
C     *** is i j neighbour to any in ijlist
         I = SILIST(N)
         J = SJLIST(N)
         IF (I.NE.IOLD) THEN
            IOLD = I
            IJCOLD = IJCPRE
            IJCPRE = IJC
         ENDIF

         IF (IJC.NE.0) THEN
            KL = IJCOLD + 1
            KREF = KL
            KU = IJC
            IF (KL.LE.KU) THEN
 20            CONTINUE
               DO 30 K = KL,KU
                  IDEL1 = IJIST(K) - I
                  IDEL2 = JJIST(K) - J
                  IF (IDEL1**2+IDEL2**2.LE.1) GOTO 40
 30            CONTINUE
               GOTO 50
C     *** addition to present group
 40            IF (KL.EQ.KREF) THEN

                  IJC = IJC + 1
                  IJIST(IJC) = I
                  JJIST(IJC) = J
                  KJIST(IJC) = SKLIST(N)
                  NJIST(IJC) = NJIST(K)
                  KL = K + 1
                  IF (KL.GT.KU) THEN
                     GOTO 10

                  ELSE
                     GOTO 20

                  ENDIF

               ELSE
                  NUMB = NJIST(K)
                  NEWNUM = NJIST(IJC)
                  IF (NUMB.NE.NEWNUM) THEN
C     *** check for branched objects
                     DO 60 L = 1,KU
                        IF (NJIST(L).EQ.NUMB) NJIST(L) = NEWNUM
 60                  CONTINUE
                  ENDIF

                  KL = K + 1
                  IF (KL.GT.KU) THEN
                     GOTO 10

                  ELSE
                     GOTO 20

                  ENDIF

               ENDIF

 50            CONTINUE
               IF (KL.NE.KREF) GOTO 10
            ENDIF

         ENDIF
C     *** new object
         NOBJ = NOBJ + 1
         IJC = IJC + 1
         IJIST(IJC) = I
         JJIST(IJC) = J
         KJIST(IJC) = SKLIST(N)
         NJIST(IJC) = NOBJ
 10   CONTINUE
C     *** sort objects in ascending order on njist
      CALL SORTIN4(NJIST,IJIST,JJIST,KJIST,IJC)

      END
