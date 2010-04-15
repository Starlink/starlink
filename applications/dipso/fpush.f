      SUBROUTINE  FPUSH(MAXPT,MAXBRK,X,Y,NPT,BRKS,NBRK,N1,N2,SUBCHK)

      REAL X(MAXPT),Y(MAXPT),XN(5000),YN(5000)
      INTEGER BRKS(MAXBRK),BRKSN(10)
      LOGICAL OK
      character*18 pushcm
      character*2 number(20)
      COMMON/DAT1/FL,W,WS,WV,NF
      COMMON/DAT2/A,B,P,S,FLX,INDX,IREL,LP,NL,IAB
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      COMMON/DAT3/FLMAX
      COMMON/DEBUG/ NY
      DIMENSION FL(1000),W(1000),A(80),B(80),P(10),FLX(20)
      DIMENSION INDX(80),IREL(80),LP(20)
      LOGICAL SUBCHK

      data number/' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9','10',
     :            '11','12','13','14','15','16','17','18','19','20'/

      SUBCHK = .TRUE.
      IF(NY.GT.0) PRINT *,'SR. FPUSH N1,N2',N1,N2
      IF(NPT.EQ.0) THEN
        SUBCHK=.FALSE.
        WRITE(*,
     :     '(''   ELFPUSH:  nothing pushed, DIPSO current is empty'')')
        RETURN
      ENDIF
      NSZ=5000
      NBSZ=10
      NDI3=3*NDIM
      NB=BRKS(1)
      DX=(X(NB)-X(1))/NB
      DX=DX/3.
      NPT1=(X(NPT)-X(1))/DX
      DX=(X(NPT)-X(1))/NPT1
      NPT1=NPT1+1
!      PRINT *,'  ELFPUSH:  number of points = ',NPT1
      IF(N1.EQ.0) THEN
        DO IF=1,NPT1
          XN(IF)=X(1)+(IF-1)*DX
          YN(IF)=0.
          IF(NPAR.NE.0) THEN
            XX=1.
            DO J=1,NPAR
              YN(IF)=YN(IF)+XX*P(J)
              XX=XX*(XN(IF)-WS)
            ENDDO
          ENDIF
          IF(NL.NE.0) THEN
           DO J=1,NL
             YN(IF)=YN(IF)+B(NDI2+J)*
     :                    ELFG(XN(IF),B(J),B(NDIM+J),B(NDI3+J))
           ENDDO
          ENDIF
          YN(IF)=FLMAX/100*YN(IF)
        ENDDO
        BRKSN(1)=NPT1
        CALL UPUSH(NSZ,XN,YN,NPT1,NBSZ,BRKSN,1,'FIT',WORV,OK)
        IF (.NOT.OK) THEN
           SUBCHK = .FALSE.
           WRITE (*,'(''   ELFPUSH:  error on pushing'')')
           RETURN
        ENDIF
      ELSE
        BRKSN(1)=NPT1
        IL1=N1
        IF(IL1.GT.NL) THEN
          WRITE (*,
     :    '(''   ELFPUSH:  component'',I4,'' does not exist'')') IL1
          SUBCHK = .FALSE.
          RETURN
        ENDIF
        IL2=N2
        IF(IL2.GT.NL) THEN
          WRITE (*,
     :    '(''   ELFPUSH:  range includes non-existent components'')')
          SUBCHK = .FALSE.
          RETURN
        ENDIF
        IF(N2.EQ.0) IL2=N1
          IF(NPAR.NE.0) THEN
            DO IF=1,NPT1
              XN(IF)=X(1)+(IF-1)*DX
              YN(IF)=0.
                XX=1.
                DO J=1,NPAR
                  YN(IF)=YN(IF)+XX*P(J)
                  XX=XX*(XN(IF)-WS)
                ENDDO
              YN(IF)=FLMAX/100*YN(IF)
            ENDDO
            CALL UPUSH(NSZ,XN,YN,NPT1,NBSZ,BRKSN,1,'POLY',WORV,OK)
            IF (.NOT.OK) THEN
               WRITE (*,
     :         '(''   ELFPUSH:  error pushing'')')
               SUBCHK = .FALSE.
               RETURN
            ENDIF
          ENDIF
          DO J=IL1,IL2
            DO IF=1,NPT1
              XN(IF)=X(1)+(IF-1)*DX
              YN(IF)=0.
              YN(IF)=YN(IF)+B(NDI2+J)*
     :                   ELFG(XN(IF),B(J),B(NDIM+J),B(NDI3+J))
              YN(IF)=FLMAX/100*YN(IF)
            ENDDO
          pushcm='PROFILE OF LINE '//number(J)
          CALL UPUSH(NSZ,XN,YN,NPT1,NBSZ,BRKSN,1,pushcm,WORV,OK)
          IF (.NOT.OK) THEN
             WRITE (*,
     :       '(''   ELFPUSH:  error pushing'')')
             SUBCHK = .FALSE.
             RETURN
          ENDIF
          ENDDO
      ENDIF
      RETURN
      END
