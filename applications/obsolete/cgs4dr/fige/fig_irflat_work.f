      SUBROUTINE FIG_IRFLAT_WORK(ND,SUM,NUM,NX,DETS,DATA)
      IMPLICIT NONE
      INTEGER ND,NX
      REAL SUM(ND),DATA(NX)
      INTEGER NUM(ND),DETS(NX)

      INTEGER ID,IX,NS,IGNORE
      REAL SS

      CHARACTER*13 ICH_CI,ICH_CF

      SS=0.0
      NS=0
      DO ID=1,ND
          IF (NUM(ID) .EQ. 0) THEN
              SUM(ID)=0.0
          ELSE
              SUM(ID)=SUM(ID)/NUM(ID)
              SS=SS+SUM(ID)
              NS=NS+1
          ENDIF
      ENDDO
      SS=SS/NS
      DO ID=1,ND
          SUM(ID)=SUM(ID)/SS
          CALL PAR_WRUSER(ICH_CI(ID)//ICH_CF(SUM(ID)),IGNORE)
      ENDDO
      DO IX=1,NX
          DATA(IX)=SUM(DETS(IX))
      ENDDO
      END
