C-----------------------------------------------------------------------

      SUBROUTINE HANNSP(NQ,BUF,IFAIL)

      LOGICAL   DOQUAD
      DIMENSION BUF(1),CFUN(3)
      INCLUDE   'STACKCOMM'

      CFUN(1)=0.25
      CFUN(2)=0.50
      CFUN(3)=0.25

      IFAIL=0
      CALL QLIM(NQ,NQ1,NQ2)
      DO NQQ=NQ1,NQ2
       IF(DOQUAD(NQQ)) THEN
        CALL CONVOL(CFUN,3,BUF,NQQ)
       END IF
      END DO
      CALL TRUNC(NQ,1)

      RETURN
      END


