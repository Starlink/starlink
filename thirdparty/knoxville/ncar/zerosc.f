      SUBROUTINE ZEROSC
      SAVE
C
      COMMON /ISOSR2/ LX         ,NX         ,NY         ,ISCR(8,128),
     1                ISCA(8,128)
C
C ZERO BOTH SCRENE MODELS.
C
      DO  20 I=1,LX
         DO  10 J=1,NY
            ISCR(I,J) = 0
            ISCA(I,J) = 0
   10    CONTINUE
   20 CONTINUE
      RETURN
      END
