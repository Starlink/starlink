      SUBROUTINE SOFAR(A,P,INDX,IREL,NPAR,NDIM)

      INCLUDE 'KUSE_COM'   ! Declares common block KUSE holding K1 and K2

      COMMON/DEBUG/NY

      DIMENSION A(80),P(10),INDX(80),IREL(80)

  200 FORMAT(1X,'Line   Parameter    Option   IREL     Value'/)
  201 FORMAT(1X,I3,5X,A9,2X,A9,I4,F12.3)
  203 FORMAT(1X,'No line specifications present so far')
  204 FORMAT(/)

      WRITE(6,200)
      INIT=0
      NDI4=4*NDIM
      DO K=1,NDI4
        INIT=INIT+INDX(K)
      ENDDO
      IF(INIT.EQ.0) THEN
        WRITE(6,203)
      ELSE
        DO K=1,4
          DO I=1,NDIM
            IP=(K-1)*NDIM+I
            J=INDX(IP)
            IF(J.NE.0) WRITE(6,201) I,K2(K),K1(J),IREL(IP),A(IP)
          ENDDO
        ENDDO
      ENDIF
      IF(NPAR.NE.0) THEN
        NP=NPAR-1
        WRITE(6,202) NP
  202   FORMAT(//3X,'Degree of background polynomial is',I3)
      ENDIF
      WRITE(6,204)

      RETURN
      END
