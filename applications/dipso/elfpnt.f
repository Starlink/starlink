       FUNCTION ELFPNT(N,X,IF)
       COMMON/DAT4/XV,FXV,ARP,WVF,CFLX,IXV,NXV,NPROF
       COMMON/DAT4A/ITXT
       COMMON/DEBUG/NY
       DIMENSION XV(5,1000),FXV(5,1000),ARP(5),IXV(5),NXV(5),ITXT(5)
       DIMENSION WVF(5),CFLX(5)
       CHARACTER*40 ITXT
       IF=0
       NP=NXV(N)
       ELFPNT=0.
       IF(X.GT.XV(N,NP).OR.X.LT.XV(N,1)) THEN
         IF=1
         RETURN
       ENDIF
       DO I=2,NP
       IF(X.LE.XV(N,I)) THEN
       G=(FXV(N,I)-FXV(N,I-1))/(XV(N,I)-XV(N,I-1))
       ELFPNT=FXV(N,I-1) + G*(X-XV(N,I-1))
       RETURN
       ENDIF
       ENDDO
       RETURN
       END
