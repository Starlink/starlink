      SUBROUTINE ELFPL
      COMMON/DAT4/XV,FXV,ARP,WVF,CFLX,IXV,NXV,NPROF
      COMMON/DAT4A/ITXT
      CHARACTER*40 ITXT
      DIMENSION XV(5,1000),FXV(5,1000),ARP(5),IXV(5),NXV(5),ITXT(5)
      DIMENSION WVF(5),CFLX(5)
      WRITE(6,102)
      WRITE(6,103)
      IF(NPROF.NE.0) THEN
        DO I=1,NPROF
          NP=I+5
          WRITE(6,100) NP,ITXT(I)
        ENDDO
      ELSE
        WRITE(6,101)
      ENDIF
  100 FORMAT ('         Profile',I3,':',5X,A40)
  101 FORMAT ('         Input profile stack is empty')
  102 FORMAT ('   ELFPL:  Profile  1:',5X,'Gaussian')
  103 FORMAT ('         Profile  2:',5X,'Triangular')
      RETURN
      END
