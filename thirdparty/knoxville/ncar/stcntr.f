      SUBROUTINE STCNTR (Z,L,M,N,CONV)
C
      SAVE
      DIMENSION       Z(L,N)
C
C THIS ROUTINE FINDS THE BEGINNINGS OF ALL CONTOUR LINES AT LEVEL CONV.
C FIRST THE EDGES ARE SEARCHED FOR LINES INTERSECTING THE EDGE (OPEN
C LINES) THEN THE INTERIOR IS SEARCHED FOR LINES WHICH DO NOT INTERSECT
C THE EDGE (CLOSED LINES).  BEGINNINGS ARE STORED IN IR TO PREVENT RE-
C TRACING OF LINES.  IF IR IS FILLED, THE SEARCH IS STOPPED FOR THIS
C CONV.
C
      COMMON /ISOSR6/ IX         ,IY         ,IDX        ,IDY        ,
     1                IS         ,ISS        ,NP         ,CV         ,
     2                INX(8)     ,INY(8)     ,IR(500)    ,NR
      COMMON /ISOSR7/ IENTRY     ,IONES
      COMMON /ISOSR9/ BIG        ,IXBIT
C
C  PACK X AND Y
C
      IPXY(I1,J1) = ISHIFT(I1,IXBIT)+J1
C
      IENTRY = 0
      NP = 0
      CV = CONV
C
C THE FOLLOWING CODE SHOULD BE RE-ENABLED IF THIS ROUTINE IS USED FOR
C GENERAL CONTOURING
C
C     ISS=0
C     DO 2 IP1=2,M
C     I=IP1-1
C     IF(Z(I,1).GE.CV.OR.Z(IP1,1).LT.CV) GO TO 1
C     IX=IP1
C     IY=1
C     IDX=-1
C     IDY=0
C     IS=1
C     CALL DRLINE(Z,L,M,N)
C   1 IF(Z(IP1,N).GE.CV.OR.Z(I,N).LT.CV) GO TO 2
C     IX=I
C     IY=N
C     IDX=1
C     IDY=0
C     IS=5
C     CALL DRLINE(Z,L,M,N)
C   2 CONTINUE
C     DO 4 JP1=2,N
C     J=JP1-1
C     IF(Z(M,J).GE.CV.OR.Z(M,JP1).LT.CV) GO TO 3
C     IX=M
C     IY=JP1
C     IDX=0
C     IDY=-1
C     IS=7
C     CALL DRLINE(Z,L,M,N)
C   3 IF(Z(1,JP1).GE.CV.OR.Z(1,J).LT.CV) GO TO 4
C     IX=1
C     IY=J
C     IDX=0
C     IDY=1
C     IS=3
C     CALL DRLINE(Z,L,M,N)
C   4 CONTINUE
C
      ISS = 1
      DO  40 JP1=3,N
         J = JP1-1
         DO  30 IP1=2,M
            I = IP1-1
            IF (Z(I,J).GE.CV .OR. Z(IP1,J).LT.CV) GO TO  30
            IXY = IPXY(IP1,J)
            IF (NP .EQ. 0) GO TO  20
            DO  10 K=1,NP
               IF (IR(K) .EQ. IXY) GO TO  30
   10       CONTINUE
   20       NP = NP+1
            IF (NP .GT. NR) RETURN
            IR(NP) = IXY
            IX = IP1
            IY = J
            IDX = -1
            IDY = 0
            IS = 1
            CALL DRCNTR (Z,L,M,N)
   30    CONTINUE
   40 CONTINUE
      RETURN
      END
