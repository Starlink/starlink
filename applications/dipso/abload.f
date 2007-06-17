      SUBROUTINE ABLOAD(IP,SUBCHK)

      INCLUDE 'KUSE_COM'   ! Declares common block KUSE holding K1 and K2

      LOGICAL SUBCHK
      COMMON/DAT2/A,B,P,S,FLX,INDX,IREL,LP,NL,IAB
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      DIMENSION A(80),B(80),P(10),INDX(80),IREL(80),LP(20)
      DIMENSION FLX(20)
  100 FORMAT (3X,A9,' of line',I4,' not specified')
      SUBCHK = .TRUE.
      IAB=1
      IF(NL.EQ.0) THEN
C       SUBCHK = .FALSE.
        IAB=0
        RETURN
      ENDIF
      ND=-NDIM
      DO I=1,4
        ND=ND+NDIM
        DO J=1,NL
          IF(INDX(J+ND).EQ.0.OR.INDX(J+ND).EQ.1) THEN
            IAB=0
            IF(IP.NE.0) THEN
              WRITE(6,100) K2(I),J
            ENDIF
          ENDIF
          IF(INDX(J+ND).EQ.2) B(J+ND)=A(J+ND)
        ENDDO
        DO J=1,NL
          IF(INDX(J+ND).EQ.3) THEN
            K=IREL(J+ND)
            IF(I.EQ.3) THEN
              B(J+ND)=B(K+ND)*A(J+ND)
            ELSE
              B(J+ND)=B(K+ND)+A(J+ND)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      END
